import os
import subprocess
import tempfile
import time
import signal
import shutil
import platform
import sys
import re
from datetime import datetime, timedelta
from dotenv import load_dotenv
import concurrent.futures
import psutil
import threading

# Load environment variables from the .env file
load_dotenv()

# Lock mechanism for per-iteration directory creation to prevent race conditions
# Multiple hypotheses (h1, h2, h3, h4) may execute simultaneously for the same iteration
# Use a lock to serialize directory creation per (rt_id, iteration) pair
_iteration_locks = {}
_lock_lock = threading.Lock()

def get_iteration_lock(rt_id, iteration):
    """Get a threading lock specific to this iteration to prevent race conditions.
    
    When multiple workers execute hypotheses for the same iteration concurrently,
    only one should manage directory creation at a time. Others wait.
    This prevents concurrent directory creation errors on network filesystems.
    """
    with _lock_lock:
        key = (rt_id, iteration)
        if key not in _iteration_locks:
            _iteration_locks[key] = threading.Lock()
        return _iteration_locks[key]

# Global flag for graceful shutdown
stop_execution = False

def signal_handler(sig, frame):
    """Handle keyboard interrupt (Ctrl+C) gracefully."""
    global stop_execution
    print("\n\n" + "="*60)
    print("⚠ STOP SIGNAL RECEIVED")
    print("="*60)
    print("Will stop after current do-file completes...")
    print("Press Ctrl+C again to force immediate exit.")
    print("="*60 + "\n")
    stop_execution = True

# Register the signal handler
signal.signal(signal.SIGINT, signal_handler)

# Constants
ROOT_DIR = os.path.abspath(".")
DOFILE_DIR = os.path.join(ROOT_DIR, "data", "raw", "dofiles_consistency_checked")
LOG_DIR = os.path.join(ROOT_DIR, "data", "derived", "5_idiosyncrasy_logs")
CSV_DIR = os.path.join(ROOT_DIR, "data", "derived", "5_idiosyncrasy_results")

# Create main execution log directory
EXEC_LOG_DIR = os.path.join(ROOT_DIR, "logs", "5_idiosyncrasy_execution_logs")
if not os.path.exists(EXEC_LOG_DIR):
    os.makedirs(EXEC_LOG_DIR)

# New configurable iteration bounds (inclusive)
START_ITERATION = 0
END_ITERATION = 1000 

# Set STATA_EXECUTABLE based on user and platform
current_user = os.getlogin().lower()
current_platform = platform.system().lower()

if current_user == "dalbrecht":
    if current_platform == "windows":
        STATA_EXECUTABLE = "C:/Program Files/StataNow19/StataMP-64.exe"
    elif current_platform == "linux":
        STATA_EXECUTABLE = "/usr/local/stata19/stata-mp"
    else:
        STATA_EXECUTABLE = os.getenv("STATA_EXECUTABLE", "stata")  # fallback to env or 'stata'
else:
    STATA_EXECUTABLE = os.getenv("STATA_EXECUTABLE", "stata")  # fallback to env or 'stata'

# Create the log and csv directories if they don't exist
if not os.path.exists(LOG_DIR):
    os.makedirs(LOG_DIR)
if not os.path.exists(CSV_DIR):
    os.makedirs(CSV_DIR)

def extract_rt_id(filename):
    """Extract rt_id from filename (format: YYYYMMDD_HHMMSS_XXXXXX_hX.do)"""
    # Find position of _h1.do, _h2.do, _h3.do, or _h4.do
    for h in ['_h1.do', '_h2.do', '_h3.do', '_h4.do']:
        if h in filename:
            pos = filename.find(h)
            # Extract 6 characters before the _hX.do part
            if pos >= 6:
                return filename[pos-6:pos]
    return "unknown"

def extract_hypothesis(filename):
    """Extract hypothesis number from filename (format: YYYYMMDD_HHMMSS_XXXXXX_hX.do)"""
    # Find _h1.do, _h2.do, _h3.do, or _h4.do
    for h_num in ['1', '2', '3', '4']:
        if f'_h{h_num}.do' in filename:
            return h_num
    return "unknown"

def get_completed_iterations(rt_id, start_iter, end_iter):
    """
    Check which iterations are already completed for a given rt_id.
    An iteration is only complete when ALL 4 hypotheses have result files.
    Returns a set of completed iteration numbers.
    """
    rt_csv_dir = os.path.join(CSV_DIR, rt_id)
    if not os.path.exists(rt_csv_dir):
        return set()
    
    # Track which (iteration, hypothesis) pairs have results
    iteration_hypothesis_pairs = set()
    pattern = re.compile(r'idiosyncrasy_.*_h([1-4])_results_iter(\d+)\.csv')
    
    for filename in os.listdir(rt_csv_dir):
        match = pattern.match(filename)
        if match:
            hypothesis = int(match.group(1))
            iteration = int(match.group(2))
            if start_iter <= iteration <= end_iter:
                iteration_hypothesis_pairs.add((iteration, hypothesis))
    
    # An iteration is complete only if all 4 hypotheses exist
    completed = set()
    for iteration in range(start_iter, end_iter + 1):
        hypotheses_present = {h for (it, h) in iteration_hypothesis_pairs if it == iteration}
        if hypotheses_present == {1, 2, 3, 4}:
            completed.add(iteration)
    
    return completed

def get_missing_iterations(rt_id, start_iter, end_iter):
    """
    Get list of missing iterations for a given rt_id.
    Returns a sorted list of missing iteration numbers.
    """
    completed = get_completed_iterations(rt_id, start_iter, end_iter)
    expected = set(range(start_iter, end_iter + 1))
    missing = sorted(expected - completed)
    return missing

def is_rt_id_complete(rt_id, start_iter, end_iter):
    """
    Check if all iterations are complete for a given rt_id.
    Returns True if all iterations exist, False otherwise.
    """
    completed = get_completed_iterations(rt_id, start_iter, end_iter)
    expected = set(range(start_iter, end_iter + 1))
    return completed == expected

def get_jobs_missing_logs(rt_id, start_iter, end_iter):
    """
    Get set of (iteration, hypothesis) pairs that have CSV results but NO corresponding log files.
    These are jobs that completed successfully (produced CSV) but their logs were lost or not preserved.
    Returns a set of (iteration, hypothesis) tuples.
    """
    jobs_with_csv = set()
    jobs_with_logs = set()
    
    # Scan CSV directory for result files
    rt_csv_dir = os.path.join(CSV_DIR, rt_id)
    if os.path.exists(rt_csv_dir):
        pattern = re.compile(r'idiosyncrasy_.*_h([1-4])_results_iter(\d+)\.csv')
        for filename in os.listdir(rt_csv_dir):
            # Skip subdirectories (e.g., .job_* directories)
            if os.path.isdir(os.path.join(rt_csv_dir, filename)):
                continue
            match = pattern.match(filename)
            if match:
                hyp = int(match.group(1))
                iter_num = int(match.group(2))
                if start_iter <= iter_num <= end_iter:
                    jobs_with_csv.add((iter_num, hyp))
    
    # Scan log directory for log files
    rt_log_dir = os.path.join(LOG_DIR, rt_id)
    if os.path.exists(rt_log_dir):
        # Log file pattern: idiosyncrasy_YYYYMMDD_HHMMSS_*_hN_iterN.log
        # More flexible pattern that handles varying filename formats
        log_pattern = re.compile(r'idiosyncrasy_.*_h([1-4])_iter(\d+)\.log')
        for filename in os.listdir(rt_log_dir):
            # Only look at files, not directories
            if os.path.isdir(os.path.join(rt_log_dir, filename)):
                continue
            match = log_pattern.match(filename)
            if match:
                hyp = int(match.group(1))
                iter_num = int(match.group(2))
                if start_iter <= iter_num <= end_iter:
                    jobs_with_logs.add((iter_num, hyp))
    
    # Return jobs that have CSV but no log
    missing = jobs_with_csv - jobs_with_logs
    
    # Debug output
    if missing:
        print(f"\n[DEBUG] RT_ID {rt_id}: Found {len(jobs_with_csv)} jobs with CSV, {len(jobs_with_logs)} with logs, {len(missing)} missing logs")
        print(f"[DEBUG]   Missing log jobs (sample): {sorted(list(missing))[:10]}")
    
    return missing


def move_log_file(file_path, rt_id, iteration, temp_log_name=None):
    """Move log file from DOFILE_DIR to rt_id subdirectory. 
    Returns: (success, log_was_found)
      - (True, True): Log was found and successfully moved
      - (True, False): Log was NOT found, but job is considered OK (CSV exists)
      - (False, X): Log move operation FAILED (should re-queue job)
    """
    # Create rt_id subdirectory in LOG_DIR
    rt_log_dir = os.path.join(LOG_DIR, rt_id)
    if not os.path.exists(rt_log_dir):
        try:
            os.makedirs(rt_log_dir, exist_ok=True)
        except Exception as e:
            print(f"[LOG-DEBUG] Warning: could not create log dir {rt_log_dir}: {e}")
    
    if temp_log_name:
        original_log_file = os.path.join(DOFILE_DIR, temp_log_name)
        log_file_name = f"idiosyncrasy_{os.path.basename(file_path).replace('.do', f'_iter{iteration}.log')}"
        new_log_file = os.path.join(rt_log_dir, log_file_name)
    else:
        base_name = f"idiosyncrasy_{os.path.basename(file_path).replace('.do', f'_iter{iteration}.log')}"
        log_file_name = base_name
        original_log_file = os.path.join(DOFILE_DIR, log_file_name.replace(f'_iter{iteration}', ''))
        new_log_file = os.path.join(rt_log_dir, log_file_name)
    
    # Wait for Stata to finish writing and release the log file (up to 10 seconds)
    max_wait = 10
    found_log = None
    search_start_time = time.time()
    
    for wait_attempt in range(max_wait):
        # First check in DOFILE_DIR (where Stata should write it)
        if os.path.exists(original_log_file):
            try:
                # Test if file is readable and not locked (use binary mode to avoid encoding issues)
                with open(original_log_file, 'rb') as f:
                    f.read(1)  # Try to read at least 1 byte
                found_log = original_log_file
                elapsed = time.time() - search_start_time
                print(f"[LOG-DEBUG] Found log file: {os.path.basename(original_log_file)} (attempt {wait_attempt + 1}/{max_wait}, elapsed {elapsed:.2f}s)")
                break
            except (IOError, PermissionError) as e:
                print(f"[LOG-DEBUG] File exists but not accessible yet ({type(e).__name__}), retrying... (attempt {wait_attempt + 1}/{max_wait})")
                if wait_attempt < max_wait - 1:
                    time.sleep(1)
                    continue
        
        if wait_attempt < max_wait - 1:
            if wait_attempt == 0:
                print(f"[LOG-DEBUG] Waiting for log file: {os.path.basename(original_log_file)} (searching up to {max_wait}s)")
            time.sleep(1)
        else:
            elapsed = time.time() - search_start_time
            print(f"[LOG-DEBUG] Log file still not found after {wait_attempt + 1} attempts ({elapsed:.2f}s elapsed): {os.path.basename(original_log_file)}")
    
    # If not found in direct location, try fallback pattern search
    if not found_log and temp_log_name:
        elapsed = time.time() - search_start_time
        print(f"[LOG-DEBUG] Expected log file not found after {elapsed:.2f}s: {os.path.basename(original_log_file)}")
        print(f"[LOG-DEBUG] Searching for any matching temp log in {DOFILE_DIR}...")
        # Extract hypothesis from temp_log_name if possible (format: temp_rt_id_hN_iterN_pid.log)
        hyp_match = re.search(r'_h([1-4])_', temp_log_name)
        hyp_filter = f'h{hyp_match.group(1)}' if hyp_match else None
        # Fallback: search for ANY temp_*.log file matching this job
        try:
            matching_logs = []
            for fname in os.listdir(DOFILE_DIR):
                if fname.startswith('temp_') and fname.endswith('.log'):
                    # Check if this log matches our rt_id and iteration (and hypothesis if available)
                    if rt_id in fname and f'iter{iteration}' in fname:
                        if hyp_filter and hyp_filter not in fname:
                            continue  # Skip if hypothesis doesn't match
                        candidate = os.path.join(DOFILE_DIR, fname)
                        try:
                            with open(candidate, 'rb') as f:
                                f.read(1)
                            matching_logs.append(candidate)
                            print(f"[LOG-DEBUG] Found matching log file: {fname}")
                        except (IOError, PermissionError):
                            pass
            # Use the first matching log found
            if matching_logs:
                found_log = matching_logs[0]
                elapsed_fallback = time.time() - search_start_time
                print(f"[LOG-DEBUG] Selected log via fallback: {os.path.basename(found_log)} (total search {elapsed_fallback:.2f}s)")
            else:
                print(f"[LOG-DEBUG] No matching temp logs found in fallback search")
        except Exception as e:
            elapsed = time.time() - search_start_time
            print(f"[LOG-DEBUG] Error during fallback search (after {elapsed:.2f}s): {e}")
    
    if not found_log:
        elapsed = time.time() - search_start_time
        print(f"⚠ [LOG-DEBUG] Log file not found after {elapsed:.2f}s ({max_wait}s max wait): {temp_log_name if temp_log_name else log_file_name}")
        print(f"  [LOG-DEBUG] Searched in: {DOFILE_DIR}")
        
        # DEBUG: List all files in DOFILE_DIR to help diagnose
        try:
            all_files = os.listdir(DOFILE_DIR)
            log_files = [f for f in all_files if f.endswith('.log')]
            temp_files = [f for f in all_files if f.startswith('temp_')]
            print(f"  [LOG-DEBUG] Files in DOFILE_DIR: {len(all_files)} total")
            print(f"  [LOG-DEBUG]   - .log files: {len(log_files)}")
            print(f"  [LOG-DEBUG]   - temp_ files: {len(temp_files)}")
            if log_files:
                print(f"  [LOG-DEBUG]   - Sample logs (first 5): {log_files[:5]}")
        except Exception as e:
            print(f"  [LOG-DEBUG] Could not list DOFILE_DIR: {e}")
        
        print(f"  [LOG-DEBUG] (This is OK - Stata in silent mode may not create log files)")
        return (True, False)  # Log not found, but job is OK (CSV exists)
    
    original_log_file = found_log  # Use the location where we actually found it

    if original_log_file and os.path.exists(original_log_file):
        elapsed_found = time.time() - search_start_time
        print(f"[LOG-DEBUG] Attempting to move log file (found after {elapsed_found:.2f}s)")
        print(f"[LOG-DEBUG]   From: {original_log_file}")
        print(f"[LOG-DEBUG]   To: {new_log_file}")
        
        move_start = time.time()
        for attempt in range(10):
            try:
                # Ensure target directory exists
                os.makedirs(os.path.dirname(new_log_file), exist_ok=True)
                
                if os.path.exists(new_log_file):
                    os.remove(new_log_file)
                
                # Verify source file is readable before moving
                with open(original_log_file, 'rb') as f:
                    test_read = f.read(100)
                
                # Use shutil.move instead of os.rename for better cross-filesystem support
                shutil.move(original_log_file, new_log_file)
                move_time = time.time() - move_start
                print(f"[LOG-DEBUG] ✓ Successfully moved log file to {os.path.join(rt_id, log_file_name)} (move took {move_time:.2f}s)")
                return (True, True)  # Log found and moved successfully
            except (PermissionError, OSError) as e:
                if attempt < 9:
                    elapsed_move = time.time() - move_start
                    print(f"[LOG-DEBUG] Move attempt {attempt+1}/10 failed after {elapsed_move:.2f}s: {type(e).__name__}: {e}")
                    print(f"[LOG-DEBUG]   Retrying in 1s...")
                    time.sleep(1)
                else:
                    elapsed_move = time.time() - move_start
                    print(f"⚠ [LOG-DEBUG] ✗ Failed to move log file after {attempt+1} attempts ({elapsed_move:.2f}s total)")
                    print(f"  [LOG-DEBUG] From: {original_log_file}")
                    print(f"  [LOG-DEBUG] To: {new_log_file}")
                    print(f"  [LOG-DEBUG] Final error: {type(e).__name__}: {e}")
                    return (False, True)  # Log found but FAILED to move - job should be re-queued
    else:
        if original_log_file:
            print(f"⚠ [LOG-DEBUG] Log file {os.path.basename(original_log_file)} existed but disappeared before move")
        else:
            print(f"⚠ [LOG-DEBUG] Log file {temp_log_name if temp_log_name else log_file_name} was never found")
        return (True, False)  # Log not found, but job is OK (CSV exists)

def rename_csv_file(do_file_name, rt_id, iteration):
    """
    Rename ONLY the result file produced by this specific do-file (hypothesis),
    to avoid cross-renaming when multiple hypotheses finish concurrently.

    Expected original file: hX_results.csv where X is 1..4 derived from do_file_name.
    Target file: {do_file_name}_results_iter{iteration}.csv
    
    Uses retries to handle Windows filesystem lag on network drives where files
    may not be immediately visible to os.path.exists() even though Stata wrote them.
    
    Returns True on success, False on failure.
    """
    # The rt_id subdirectory should already exist (created by Stata via $OUT)
    rt_csv_dir = os.path.join(CSV_DIR, rt_id)
    
    if not os.path.exists(rt_csv_dir):
        print(f"CSV subdirectory {rt_csv_dir} not found. Creating it...")
        os.makedirs(rt_csv_dir, exist_ok=True)
    
    # Extract hypothesis from do_file_name (expects suffix _h1/_h2/_h3/_h4)
    m = re.search(r"_h([1-4])$", do_file_name)
    if not m:
        print(f"Warning: could not determine hypothesis from do_file_name '{do_file_name}'. Skipping rename.")
        return False
    hyp = m.group(1)
    src_name = f"h{hyp}_results.csv"
    
    # CRITICAL FIX: Add aggressive retries for finding CSV file
    # On network drives under parallel load with 7 workers, files can take 10+ seconds to become visible
    # Using extended retries with exponential backoff to handle worst-case filesystem lag
    max_search_retries = 40  # Increased from 15 to handle sustained network filesystem lag
    search_wait = 0.5  # Start with 500ms

    src = None
    csv_search_start = time.time()
    total_search_wait = 0
    
    for search_attempt in range(max_search_retries):
        # Look for the result file: first in per-job subdir (new behavior), then in main dir (backward compatibility)
        job_subdir = os.path.join(rt_csv_dir, f".job_{rt_id}_{iteration}_h{hyp}")
        candidates = [
            os.path.join(job_subdir, src_name),
            os.path.join(rt_csv_dir, src_name),
        ]
        
        for cand in candidates:
            if os.path.exists(cand):
                src = cand
                csv_search_time = time.time() - csv_search_start
                if search_attempt > 0:
                    print(f"[{rt_id}] ✓ CSV found (attempt {search_attempt + 1}/{max_search_retries}, {csv_search_time:.2f}s total, {total_search_wait:.2f}s wait)")
                break
        
        if src is not None:
            break
        
        # Not found yet - retry with exponential backoff
        if search_attempt < max_search_retries - 1:
            if search_attempt == 0:
                print(f"[{rt_id}] CSV not yet visible, will retry (max {max_search_retries} attempts)")
            time.sleep(search_wait)
            total_search_wait += search_wait
            search_wait = min(search_wait * 1.5, 2.0)  # Cap at 2s
    
    if src is None:
        # Also check for any h{N}_results.csv files that might be from other iterations (race condition scenario)
        # List all files to help debug
        csv_search_time = time.time() - csv_search_start
        if os.path.exists(rt_csv_dir):
            existing_files = os.listdir(rt_csv_dir)
            print(f"[{rt_id}] ERROR: CSV file not found after {max_search_retries} attempts ({csv_search_time:.2f}s total, {total_search_wait:.2f}s wait)")
            print(f"[{rt_id}]   Expected: {src_name}")
            print(f"[{rt_id}]   In: {rt_csv_dir}")
            print(f"[{rt_id}]   Files found ({len(existing_files)} total): {existing_files[:10]}")
            if len(existing_files) > 10:
                print(f"[{rt_id}]     ... and {len(existing_files) - 10} more files")
            # Check for Stata error logs
            dofile_dir = os.path.join(os.path.dirname(__file__), '..', 'data', 'raw', 'dofiles_consistency_checked')
            if os.path.exists(dofile_dir):
                stderr_files = [f for f in os.listdir(dofile_dir) if f.startswith('.stata_stderr_')]
                stdout_files = [f for f in os.listdir(dofile_dir) if f.startswith('.stata_stdout_')]
                if stderr_files:
                    print(f"[{rt_id}]   Stata error logs available: {stderr_files[-5:]} (showing last 5)")
                if stdout_files:
                    print(f"[{rt_id}]   Stata output logs available: {stdout_files[-5:]} (showing last 5)")
        else:
            print(f"[{rt_id}] ERROR: CSV directory not found after {max_search_retries} attempts ({csv_search_time:.2f}s total, {total_search_wait:.2f}s wait): {rt_csv_dir}")
        return False

    new_file_name = f"idiosyncrasy_{do_file_name}_results_iter{iteration}.csv"
    dst = os.path.join(rt_csv_dir, new_file_name)
    try:
        if os.path.exists(dst):
            os.remove(dst)
        # Move/rename to final location
        shutil.move(src, dst)
        print(f"Renamed {src_name} to {new_file_name} in {rt_id}/ (from {os.path.basename(os.path.dirname(src)) if os.path.dirname(src) != rt_csv_dir else 'main dir'})")
        
        # Clean up the job-specific subdirectory after successful move
        if os.path.dirname(src) != rt_csv_dir:
            job_subdir_path = os.path.dirname(src)
            try:
                if os.path.exists(job_subdir_path) and os.path.isdir(job_subdir_path):
                    shutil.rmtree(job_subdir_path)
                    print(f"Cleaned up job subdirectory: {os.path.basename(job_subdir_path)}")
            except Exception as cleanup_err:
                print(f"Warning: Could not remove job subdirectory {job_subdir_path}: {cleanup_err}")
        
        return True
    except Exception as e:
        print(f"Failed to rename {src_name} -> {new_file_name} in {rt_id}/: {e}")
        return False

def cleanup_results_directory():
    """Remove files from CSV_DIR that don't match the pattern idiosyncrasy_*_results.csv"""
    if not os.path.exists(CSV_DIR):
        print(f"Results directory {CSV_DIR} not found.")
        return
    
    files_removed = []
    for file_name in os.listdir(CSV_DIR):
        file_path = os.path.join(CSV_DIR, file_name)
        # Skip if not a file
        if not os.path.isfile(file_path):
            continue
        
        # Check if file matches the expected pattern
        if not (file_name.startswith("idiosyncrasy_") and file_name.endswith("_results.csv")):
            try:
                os.remove(file_path)
                files_removed.append(file_name)
            except Exception as e:
                print(f"  Could not delete {file_name}: {e}")
    
    if files_removed:
        print(f"Cleaned up {len(files_removed)} unwanted file(s) from results directory:")
        for file in files_removed:
            print(f"  Deleted: {file}")

def cleanup_iteration_data(rt_id, iteration, hypothesis=None):
    """Delete iteration/rt_id-specific raw, derived, and temp data directories after each do-file run."""
    # Build directory patterns - if hypothesis provided, use specific pattern, otherwise use wildcard
    if hypothesis:
        patterns = [
            os.path.join(ROOT_DIR, "data", "raw_confidential", f"95%_iter_{rt_id}_h{hypothesis}_{iteration}"),
            os.path.join(ROOT_DIR, "data", "raw_confidential", f"5%_iter_{rt_id}_h{hypothesis}_{iteration}"),
            os.path.join(ROOT_DIR, "data", "raw_confidential", f"100%_iter_{rt_id}_h{hypothesis}_{iteration}"),
            os.path.join(ROOT_DIR, "data", "raw_confidential", f"temp_{rt_id}_h{hypothesis}_{iteration}"),
            os.path.join(ROOT_DIR, "data", "derived_confidential", f"95%_iter_{rt_id}_h{hypothesis}_{iteration}"),
            os.path.join(ROOT_DIR, "data", "derived_confidential", f"5%_iter_{rt_id}_h{hypothesis}_{iteration}")
        ]
    else:
        # Legacy: clean up all hypothesis directories for this rt_id and iteration
        patterns = [
            os.path.join(ROOT_DIR, "data", "raw_confidential", f"95%_iter_{rt_id}_{iteration}"),
            os.path.join(ROOT_DIR, "data", "raw_confidential", f"5%_iter_{rt_id}_{iteration}"),
            os.path.join(ROOT_DIR, "data", "raw_confidential", f"100%_iter_{rt_id}_{iteration}"),
            os.path.join(ROOT_DIR, "data", "raw_confidential", f"temp_{rt_id}_{iteration}"),
            os.path.join(ROOT_DIR, "data", "derived_confidential", f"95%_iter_{rt_id}_{iteration}"),
            os.path.join(ROOT_DIR, "data", "derived_confidential", f"5%_iter_{rt_id}_{iteration}")
        ]
    
    for d in patterns:
        if os.path.exists(d):
            try:
                shutil.rmtree(d)
                print(f"Deleted directory: {d}")
            except Exception as e:
                print(f"Could not delete directory {d}: {e}")

def execute_stata_do_file(file_path, rt_id, iteration, use_setup=False, setup_file=None, progress_tracker=None):
    original_dir = os.getcwd()
    temp_file_path = None
    temp_log_name = None
    stata_process = None
    do_file_to_run = None
    stata_temp_dir = None
    job_start_time = time.time()
    
    # Extract hypothesis from filename
    hypothesis = extract_hypothesis(os.path.basename(file_path))
    worker_pid = os.getpid()
    file_basename = os.path.basename(file_path)
    
    print(f"\n[Worker {worker_pid}] JOB START: {file_basename} (rt_id={rt_id}, iter={iteration}, h{hypothesis})")
    print(f"[Worker {worker_pid}] Timestamp: {datetime.now().isoformat()}")
    
    if progress_tracker:
        progress_tracker.start_file(file_basename, iteration)
    
    try:
        os.chdir(DOFILE_DIR)
        if not os.path.exists(STATA_EXECUTABLE):
            raise FileNotFoundError(f"Stata executable not found at {STATA_EXECUTABLE}. Please ensure the path is correct.")
        
        # Configure Stata's internal temp directory to avoid spaces in paths
        # Windows Stata fails with unquoted paths containing spaces when using tempfile
        # Solution: Use a temp location without spaces (8.3 short names or /tmp)
        env = os.environ.copy()
        try:
            use_base = None
            
            # On Windows: use Windows short temp path or C:/tmp (without spaces)
            if platform.system().lower() == 'windows':
                # Try to use C:\TEMP (8.3 compatible, no spaces)
                # Or create in C:\MD_STATA_TMP (no spaces in path)
                windows_safe_tmp = "C:\\MD_STATA_TMP"
                try:
                    os.makedirs(windows_safe_tmp, exist_ok=True)
                    use_base = windows_safe_tmp
                except Exception as e:
                    # Fallback: try to get system temp and convert to short path
                    try:
                        import ctypes
                        from ctypes import wintypes
                        # Use GetShortPathName to convert to 8.3 format
                        GetShortPathName = ctypes.windll.kernel32.GetShortPathNameW
                        GetShortPathName.argtypes = [wintypes.LPCWSTR, wintypes.LPWSTR, wintypes.DWORD]
                        GetShortPathName.restype = wintypes.DWORD
                        
                        system_temp = os.environ.get('TEMP', 'C:\\TEMP')
                        buff_size = GetShortPathName(system_temp, None, 0)
                        if buff_size:
                            buff = ctypes.create_unicode_buffer(buff_size)
                            GetShortPathName(system_temp, buff, buff_size)
                            use_base = buff.value
                        else:
                            use_base = windows_safe_tmp
                    except Exception as e2:
                        print(f"Warning: could not get short path: {e2}")
                        use_base = windows_safe_tmp
            else:
                # On Unix/Linux: use /tmp for better performance
                use_base = "/tmp"
            
            if use_base is None:
                use_base = "/tmp" if platform.system().lower() != 'windows' else "C:\\TEMP"

            dir_suffix = f"stata_md_{rt_id}_{iteration}_h{hypothesis}_{os.getpid()}"
            stata_temp_dir = os.path.join(use_base, dir_suffix)
            os.makedirs(stata_temp_dir, exist_ok=True)
        except Exception as e:
            print(f"Warning: could not prepare Stata temp dir: {e}")
            # Fallback to system temp
            try:
                stata_temp_dir = os.path.join(tempfile.gettempdir(), f"stata_md_{rt_id}_{iteration}_h{hypothesis}_{os.getpid()}")
                os.makedirs(stata_temp_dir, exist_ok=True)
            except Exception as e2:
                print(f"Warning: could not create fallback temp dir: {e2}")
                stata_temp_dir = None

        # Ensure the per-job OUT directory exists BEFORE Stata attempts to write results
        # Path: data/derived/5_idiosyncrasy_results/{rt_id}/.job_{rt_id}_{iteration}_h{hypothesis}
        # Python is responsible for creating all directories with robust retry logic.
        # Stata setup file only verifies they exist (no creation to avoid race conditions).
        
        # CRITICAL FIX #2: Use per-iteration lock to prevent race conditions
        # When multiple hypotheses (h1, h2, h3, h4) execute for the same iteration concurrently,
        # only one should create directories at a time. Others wait for the lock.
        # This prevents concurrent mkdir errors and filesystem visibility issues on network drives.
        iteration_lock = get_iteration_lock(rt_id, iteration)
        lock_acquired_time = time.time()
        print(f"[Worker {worker_pid}] Attempting to acquire directory creation lock for iteration {iteration}...")
        sys.stdout.flush()
        
        with iteration_lock:
            lock_wait_time = time.time() - lock_acquired_time
            if lock_wait_time > 0.5:
                print(f"[Worker {worker_pid}] ✓ Acquired lock (waited {lock_wait_time:.2f}s)")
            sys.stdout.flush()
            
            try:
                # First ensure parent CSV_DIR exists
                os.makedirs(CSV_DIR, exist_ok=True)
                
                # Then ensure rt_id subdirectory exists
                rt_csv_dir = os.path.join(CSV_DIR, rt_id)
                os.makedirs(rt_csv_dir, exist_ok=True)
                
                # Finally create the job-specific subdirectory with aggressive retries
                job_out_dir = os.path.join(rt_csv_dir, f".job_{rt_id}_{iteration}_h{hypothesis}")
                
                # Create with extended retries to handle Windows filesystem race conditions
                max_retries = 15
                retry_wait = 0.3  # Start with 300ms
                dir_create_start = time.time()
                total_dir_wait = 0
                for attempt in range(max_retries):
                    try:
                        os.makedirs(job_out_dir, exist_ok=True)
                        
                        # Verify it actually exists with explicit stat call (forces filesystem flush)
                        try:
                            stat_result = os.stat(job_out_dir)
                            if stat_result.st_size >= 0:  # Just verify stat succeeded
                                dir_create_time = time.time() - dir_create_start
                                if attempt > 0:
                                    print(f"[Worker {worker_pid}] ✓ OUT dir verified (attempt {attempt+1}/{max_retries}, {dir_create_time:.2f}s total, {total_dir_wait:.2f}s wait): {job_out_dir}")
                                break
                        except (OSError, FileNotFoundError):
                            if attempt < max_retries - 1:
                                print(f"[Worker {worker_pid}] Directory created but not yet visible, waiting {retry_wait*1000:.0f}ms (attempt {attempt+1}/{max_retries})...")
                                time.sleep(retry_wait)
                                total_dir_wait += retry_wait
                                retry_wait = min(retry_wait * 1.5, 1.0)  # Exponential backoff capped at 1s
                            else:
                                raise RuntimeError(f"Directory {job_out_dir} created but could not be verified after {max_retries} attempts")
                    except OSError as os_err:
                        if attempt < max_retries - 1:
                            print(f"[Worker {worker_pid}] OS error creating directory (attempt {attempt+1}/{max_retries}): {os_err}")
                            time.sleep(retry_wait)
                            total_dir_wait += retry_wait
                            retry_wait = min(retry_wait * 1.5, 1.0)
                        else:
                            raise RuntimeError(f"Failed to create {job_out_dir} after {max_retries} attempts: {os_err}")
                else:
                    # Loop completed without break = verification failed on last attempt
                    if not os.path.isdir(job_out_dir):
                        raise RuntimeError(f"Directory {job_out_dir} still not visible after {max_retries} attempts and {total_dir_wait:.1f}s total wait")
                
                # Post-creation wait: Give Windows filesystem time to propagate to all processes
                # This is critical for parallel execution where multiple processes need to see the same directory
                print(f"[Worker {worker_pid}] Waiting 1s for filesystem propagation before launching Stata...")
                time.sleep(1.0)
                
            except Exception as e:
                print(f"ERROR: could not create OUT dir {job_out_dir}: {e}")
                raise RuntimeError(f"Failed to create OUT directory for job: {e}")

        # Ensure the per-rt_id log directory exists to receive moved logs
        try:
            # First ensure parent LOG_DIR exists
            os.makedirs(LOG_DIR, exist_ok=True)
            # Then ensure rt_id subdirectory exists
            rt_log_dir = os.path.join(LOG_DIR, rt_id)
            os.makedirs(rt_log_dir, exist_ok=True)
            # Verify it actually exists
            if not os.path.isdir(rt_log_dir):
                raise RuntimeError(f"Directory {rt_log_dir} still does not exist after creation attempt")
        except Exception as e:
            print(f"Warning: could not create log dir {rt_log_dir}: {e}")
        
        # Set environment variables used by Stata for temp files
        env['TMPDIR'] = stata_temp_dir
        env['STATATMP'] = stata_temp_dir
        env['TMP'] = stata_temp_dir
        env['TEMP'] = stata_temp_dir
        
        if use_setup and setup_file:
            # Create a temporary do-file with a unique name based on rt_id, hypothesis, and iteration
            # This prevents conflicts when running multiple Stata instances in parallel
            temp_basename = f"temp_{rt_id}_h{hypothesis}_iter{iteration}_{os.getpid()}.do"
            temp_file_path = os.path.join(DOFILE_DIR, temp_basename)
            
            with open(temp_file_path, 'w', encoding='utf-8') as temp_do:
                # Pass iteration, rt_id, hypothesis as globals to Stata
                # ROOT will be automatically derived by the setup file from c(pwd)
                # This approach (used in bootstrap script too) avoids issues with long paths containing spaces
                # Note: When using -b flag, Stata automatically creates a log file
                # so we don't need to manually manage log opening/closing
                temp_do.write(f'global rt_id_iter "{rt_id}"\n')
                temp_do.write(f'global iteration "{iteration}"\n')
                temp_do.write(f'global hypothesis "{hypothesis}"\n')
                temp_do.write(f'display "--- STARTING: rt_id={rt_id}, h{hypothesis}, iter={iteration} ---"\n')
                temp_do.write(f'do "{os.path.basename(setup_file)}"\n')
                temp_do.write(f'display "--- EXECUTING: {os.path.basename(file_path)} ---"\n')
                temp_do.write(f'do "{os.path.basename(file_path)}"\n')
                temp_do.write(f'display "--- COMPLETED: rt_id={rt_id}, h{hypothesis}, iter={iteration} ---"\n')
            
            do_file_to_run = temp_basename
            do_file_name = os.path.basename(file_path)[:-3]
            temp_log_name = temp_basename.replace('.do', '.log')
        else:
            do_file_to_run = os.path.basename(file_path)
            do_file_name = os.path.basename(file_path)[:-3]
        
        stata_exec_start = time.time()
        print(f"[Worker {worker_pid}] STATA LAUNCH: {do_file_to_run} with PID tracking...")
        # Use -e flag for non-interactive mode and CREATE_NO_WINDOW on Windows to completely hide Stata windows
        # Additional measures: SW_HIDE startup info prevents any window from appearing
        if platform.system().lower() == 'windows':
            # On Windows, use multiple strategies to hide windows
            import subprocess
            creationflags = subprocess.CREATE_NO_WINDOW if hasattr(subprocess, 'CREATE_NO_WINDOW') else 0
            
            # Additional startup info to hide window
            startupinfo = subprocess.STARTUPINFO()
            startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
            startupinfo.wShowWindow = subprocess.SW_HIDE
            
            # Capture output to files for debugging
            stata_stdout_file = os.path.join(DOFILE_DIR, f".stata_stdout_{os.getpid()}_{int(time.time()*1000)}.txt")
            stata_stderr_file = os.path.join(DOFILE_DIR, f".stata_stderr_{os.getpid()}_{int(time.time()*1000)}.txt")
            with open(stata_stdout_file, 'w') as stdout_f, open(stata_stderr_file, 'w') as stderr_f:
                stata_process = subprocess.Popen(
                    [STATA_EXECUTABLE, "-e", "-b", "do", do_file_to_run], 
                    env=env,
                    cwd=DOFILE_DIR,
                    creationflags=creationflags,
                    startupinfo=startupinfo,
                    stdout=stdout_f,
                    stderr=stderr_f
                )
        else:
            # On Linux/Unix, -b is sufficient
            stata_process = subprocess.Popen([STATA_EXECUTABLE, "-b", "do", do_file_to_run], env=env, cwd=DOFILE_DIR)
        print(f"[Worker {worker_pid}] Stata process spawned: PID {stata_process.pid}")
        
        return_code = stata_process.wait()  # Wait for process to complete
        stata_exec_time = time.time() - stata_exec_start
        stata_pid = stata_process.pid
        stata_process = None  # Mark process as cleaned up
        
        print(f"[Worker {worker_pid}] Stata process {stata_pid} exited with code {return_code}")
        sys.stdout.flush()
        print(f"[Worker {worker_pid}] Stata execution: {stata_exec_time:.2f}s")
        sys.stdout.flush()
        
        stata_error = None
        if return_code != 0:
            stata_error = subprocess.CalledProcessError(return_code, [STATA_EXECUTABLE, "-b", "do", do_file_to_run])
            print(f"[Worker {worker_pid}] ⚠ Stata exited with error code {return_code} - log file will be preserved for debugging")
        
        # CRITICAL: Wait for Stata to fully write and close files on network drives
        # Network filesystems under 7-worker parallel load need extended sync time
        # Testing (Dec 11-16) shows 10s insufficient; increasing to 15s to handle worst-case propagation
        # With 7 parallel workers creating directories simultaneously, metadata can take 10-15s to become visible
        sync_start = time.time()
        print(f"[Worker {worker_pid}] Waiting 15s for file system sync (network propagation under parallel load)...")
        sys.stdout.flush()
        time.sleep(15)
        sync_time = time.time() - sync_start
        print(f"[Worker {worker_pid}] File system sync complete ({sync_time:.2f}s)")
        sys.stdout.flush()
        
        # CRITICAL: Always move log file FIRST, before any other operations
        # This ensures the log is preserved even if CSV creation fails or Stata errored
        # Log file is needed for debugging failed iterations
        log_start = time.time()
        print(f"[Worker {worker_pid}] LOG MOVE: Starting move_log_file for {file_basename}...")
        print(f"[Worker {worker_pid}]   - rt_id: {rt_id}, iteration: {iteration}, hypothesis: {hypothesis}")
        print(f"[Worker {worker_pid}]   - temp_log_name: {temp_log_name}")
        print(f"[Worker {worker_pid}]   - Expected destination: {os.path.join(LOG_DIR, rt_id)}")
        sys.stdout.flush()
        log_moved_success, log_was_found = move_log_file(file_path, rt_id, iteration, temp_log_name=temp_log_name)
        log_time = time.time() - log_start
        if log_moved_success:
            if log_was_found:
                print(f"[Worker {worker_pid}] LOG MOVE: ✓ Success ({log_time:.2f}s)")
            else:
                print(f"[Worker {worker_pid}] LOG MOVE: ⚠ Log not found, continuing ({log_time:.2f}s) - CSV exists so job is OK")
        else:
            print(f"[Worker {worker_pid}] LOG MOVE: ✗ Failed ({log_time:.2f}s) - Could not move log file - JOB WILL BE RE-QUEUED")
        sys.stdout.flush()
        
        # If log move failed (not just log missing, but actual move operation failed), raise exception
        # This will cause the job to be re-queued by the executor
        if not log_moved_success:
            raise RuntimeError(f"Log file move operation failed for {file_basename}. Job will be re-queued.")
        
        # If Stata errored, raise exception after log is safely moved
        # This allows the log to be preserved even for failed jobs
        if stata_error:
            raise stata_error
        
        print(f"[Worker {worker_pid}] Stata execution complete, now handling results...")
        sys.stdout.flush()
        
        # Rename CSV file - if this fails, it's a critical error
        csv_start = time.time()
        print(f"[Worker {worker_pid}] CSV RENAME: Starting rename_csv_file({do_file_name}, {rt_id}, {iteration})")
        sys.stdout.flush()
        csv_moved = rename_csv_file(do_file_name, rt_id, iteration)
        csv_time = time.time() - csv_start
        if not csv_moved:
            raise RuntimeError(f"Failed to move/rename CSV result file for {do_file_name}")
        print(f"[Worker {worker_pid}] CSV RENAME: Success (search+move: {csv_time:.2f}s)")
        sys.stdout.flush()
        
        cleanup_results_directory()
        
        job_end_time = time.time()
        total_job_time = job_end_time - job_start_time
        print(f"[Worker {worker_pid}] JOB COMPLETE: {file_basename} in {total_job_time:.2f}s (Stata: {stata_exec_time:.2f}s, Sync: {sync_time:.2f}s, CSV: {csv_time:.2f}s, Log: {log_time:.2f}s)")
        sys.stdout.flush()
        
        if progress_tracker:
            progress_tracker.complete_file()
            
    except subprocess.CalledProcessError as e:
        print(f"[Worker {worker_pid}] FAILED: STATA EXECUTION FAILED: {file_path}")
        print(f"[Worker {worker_pid}] Return code: {e.returncode}")
        print(f"[Worker {worker_pid}] Command: {e.cmd}")
        # Try to read and display Stata error output if available
        try:
            if platform.system().lower() == 'windows' and 'stata_stderr_file' in locals():
                if os.path.exists(stata_stderr_file):
                    with open(stata_stderr_file, 'r') as f:
                        stderr_content = f.read()
                        if stderr_content.strip():
                            print(f"[Worker {worker_pid}] STATA STDERR OUTPUT:")
                            print(stderr_content[:1000])  # First 1000 chars
                if os.path.exists(stata_stdout_file):
                    with open(stata_stdout_file, 'r') as f:
                        stdout_content = f.read()
                        if stdout_content.strip():
                            print(f"[Worker {worker_pid}] STATA STDOUT OUTPUT (last 500 chars):")
                            print(stdout_content[-500:])  # Last 500 chars
        except Exception as err:
            print(f"[Worker {worker_pid}] Could not read Stata output: {err}")
        cleanup_results_directory()  # Clean up even if execution failed
        # Note: log file was already moved before raising the exception above
        # No need to move it again here
        if progress_tracker:
            progress_tracker.complete_file()  # Still count as completed even if failed
        raise  # Re-raise the exception
    finally:
        os.chdir(original_dir)
        
        # Ensure Stata process is fully terminated
        if stata_process is not None:
            try:
                print(f"[Worker {worker_pid}] CLEANUP: Terminating Stata process PID {stata_process.pid}...")
                stata_process.terminate()
                stata_process.wait(timeout=5)
                print(f"[Worker {worker_pid}] CLEANUP: Process terminated gracefully")
            except subprocess.TimeoutExpired:
                print(f"[Worker {worker_pid}] CLEANUP: Process did not terminate gracefully. Force killing PID {stata_process.pid}...")
                try:
                    stata_process.kill()
                    stata_process.wait(timeout=2)
                    print(f"[Worker {worker_pid}] CLEANUP: Process force-killed successfully")
                except Exception as e:
                    print(f"[Worker {worker_pid}] CLEANUP: Failed to kill process: {e}")
            except Exception as e:
                print(f"[Worker {worker_pid}] CLEANUP: Error terminating process: {e}")
        
        # Kill any remaining Stata processes spawned by this worker (safety check)
        try:
            if do_file_to_run:
                orphaned_count = 0
                for proc in psutil.process_iter(['pid', 'name', 'cmdline']):
                    try:
                        name = proc.info.get('name', '').lower()
                        if 'stata' in name:
                            # Check if this is from our temp file
                            cmdline = proc.info.get('cmdline') or []
                            if any(do_file_to_run in arg for arg in cmdline):
                                pid = proc.info['pid']
                                p = psutil.Process(pid)
                                p.terminate()
                                print(f"[Worker {worker_pid}] CLEANUP: Killed orphaned Stata process PID {pid}")
                                orphaned_count += 1
                    except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
                        pass
                if orphaned_count > 0:
                    print(f"[Worker {worker_pid}] CLEANUP: Found and killed {orphaned_count} orphaned Stata process(es)")
        except Exception as e:
            print(f"[Worker {worker_pid}] CLEANUP: Warning - Could not check for orphaned processes: {e}")
        
        # Only clean up temp files for this specific job (rt_id, iteration, hypothesis)
        # BUT: Don't delete temp log files here - move_log_file handles their preservation
        # We check for their existence before cleanup to preserve failed job logs
        if hypothesis:
            temp_prefix = f"temp_{rt_id}_h{hypothesis}_iter{iteration}_"
            # Temp files are in DOFILE_DIR (dofiles_consistency_checked)
            try:
                for fname in os.listdir(DOFILE_DIR):
                    # Skip .log files - they may need to be moved for debugging
                    # Only delete the temp .do file
                    if fname.startswith(temp_prefix) and fname.endswith('.do'):
                        fpath = os.path.join(DOFILE_DIR, fname)
                        try:
                            os.remove(fpath)
                            print(f"Deleted temp do file: {fpath}")
                        except Exception as e:
                            print(f"Could not delete temp do file {fpath}: {e}")
            except Exception as e:
                print(f"Warning: Could not list DOFILE_DIR for cleanup: {e}")
        if temp_file_path and os.path.exists(temp_file_path):
            try:
                os.remove(temp_file_path)
            except Exception as e:
                print(f"Could not delete temp do file {temp_file_path}: {e}")
        
        # Clean up Stata diagnostic output files (stderr, stdout captures)
        if platform.system().lower() == 'windows':
            try:
                for fname in os.listdir(DOFILE_DIR):
                    if fname.startswith('.stata_stdout_') or fname.startswith('.stata_stderr_'):
                        fpath = os.path.join(DOFILE_DIR, fname)
                        try:
                            os.remove(fpath)
                        except Exception:
                            pass  # Ignore cleanup errors
            except Exception:
                pass  # Ignore listing errors
        
        # Clean the Stata temp directory
        try:
            if stata_temp_dir and os.path.isdir(stata_temp_dir):
                shutil.rmtree(stata_temp_dir, ignore_errors=True)
                print(f"Deleted Stata temp dir: {stata_temp_dir}")
        except Exception as e:
            print(f"Warning: Could not delete Stata temp dir: {e}")
        
        # Clean up empty job-specific subdirectories if they still exist (means Stata failed to produce output)
        try:
            rt_csv_dir = os.path.join(CSV_DIR, rt_id)
            if os.path.exists(rt_csv_dir):
                for item in os.listdir(rt_csv_dir):
                    item_path = os.path.join(rt_csv_dir, item)
                    if os.path.isdir(item_path) and item.startswith('.job_'):
                        # Check if directory is empty
                        try:
                            if not os.listdir(item_path):  # Empty directory
                                os.rmdir(item_path)
                                print(f"Cleaned up empty job folder: {item}")
                        except Exception:
                            pass  # Not empty or other error, leave it
        except Exception as e:
            print(f"Warning: Could not clean empty job folders: {e}")
        
        # Final job completion log
        job_total_time = time.time() - job_start_time
        print(f"[Worker {worker_pid}] JOB COMPLETE: {file_basename} in {job_total_time:.2f}s")

class ProgressTracker:
    def __init__(self, total_files, start_iter, end_iter):
        self.total_files = total_files
        self.start_iter = start_iter
        self.end_iter = end_iter
        self.num_iterations = end_iter - start_iter + 1
        self.total_executions = total_files * self.num_iterations
        self.completed_files = 0
        self.start_time = datetime.now()
        self.file_times = []
        
    def start_file(self, filename, iteration):
        self.current_file = filename
        self.current_iteration = iteration
        self.file_start_time = datetime.now()
        
    def complete_file(self):
        file_duration = (datetime.now() - self.file_start_time).total_seconds()
        self.file_times.append(file_duration)
        self.completed_files += 1
        self._print_progress()
        
    def _print_progress(self):
        elapsed_time = datetime.now() - self.start_time
        remaining_executions = self.total_executions - self.completed_files
        
        # Calculate statistics
        avg_time = sum(self.file_times) / len(self.file_times) if self.file_times else 0
        min_time = min(self.file_times) if self.file_times else 0
        max_time = max(self.file_times) if self.file_times else 0
        
        # Estimate remaining time
        if avg_time > 0:
            estimated_remaining = timedelta(seconds=avg_time * remaining_executions)
        else:
            estimated_remaining = timedelta(0)
        
        # Calculate current iteration progress
        files_in_current_iter = self.completed_files % self.total_files
        if files_in_current_iter == 0 and self.completed_files > 0:
            files_in_current_iter = self.total_files
        relative_iter_index = (self.completed_files - 1) // self.total_files
        current_iter_value = self.start_iter + relative_iter_index
        if self.completed_files == 0:
            current_iter_value = self.start_iter

        print(f"\n{'='*60}")
        print(f"PROGRESS REPORT")
        print(f"{'='*60}")
        print(f"Total iterations: {self.num_iterations} (range {self.start_iter}..{self.end_iter})")
        print(f"Current iteration: {current_iter_value} of {self.end_iter}")
        print(f"Files in current iteration: {files_in_current_iter}/{self.total_files}")
        print(f"Overall progress: {self.completed_files}/{self.total_executions} ({(self.completed_files/self.total_executions)*100:.1f}%)")
        print(f"Time elapsed: {str(elapsed_time).split('.')[0]}")
        print(f"Average time per file: {avg_time:.2f}s")
        print(f"Minimum time per file: {min_time:.2f}s")
        print(f"Maximum time per file: {max_time:.2f}s")
        print(f"Estimated time remaining: {str(estimated_remaining).split('.')[0]}")
        print(f"{'='*60}\n")

def cleanup_orphaned_temp_files():
    """
    Remove any orphaned temp_*.do and temp_*.log files from previous interrupted runs.
    These files can cause parallelization issues by being grouped under 'unknown' rt_id.
    
    SAFETY: Only removes temp files if their embedded PID is no longer running.
    This prevents deleting files in use by other workers.
    
    Should be called at script startup before any execution begins.
    """
    if not os.path.exists(DOFILE_DIR):
        return
    
    # Pattern: temp_rtid_iterN_pid.do or temp_rtid_iterN_pid.log
    temp_pattern = re.compile(r'^temp_.*_(\d+)\.(do|log)$')
    all_temp_files = [f for f in os.listdir(DOFILE_DIR) if temp_pattern.match(f)]
    
    if not all_temp_files:
        print("OK: No temp files found - ready to start\n")
        return
    
    # Check which temp files are truly orphaned (PID no longer exists)
    orphaned_files = []
    active_files = []
    
    for file in all_temp_files:
        match = temp_pattern.match(file)
        if match:
            pid = int(match.group(1))
            try:
                # Check if process exists (Windows-safe using psutil)
                if psutil.pid_exists(pid):
                    # Process exists - file is still active
                    active_files.append((file, pid))
                else:
                    # Process doesn't exist - file is orphaned
                    orphaned_files.append(file)
            except (OSError, psutil.NoSuchProcess):
                # Process lookup failed - treat as orphaned to be safe
                orphaned_files.append(file)
    
    # Report status
    if active_files:
        print("\n" + "="*60)
        print("ℹ ACTIVE TEMP FILES (will NOT delete)")
        print("="*60)
        print(f"Found {len(active_files)} temp file(s) from running processes:")
        for file, pid in active_files[:5]:
            print(f"  - {file} (PID {pid} is running)")
        if len(active_files) > 5:
            print(f"  ... and {len(active_files) - 5} more")
        print("="*60 + "\n")
    
    if orphaned_files:
        print("\n" + "="*60)
        print("⚠ ORPHANED TEMP FILES DETECTED")
        print("="*60)
        print(f"Found {len(orphaned_files)} orphaned temp file(s) from previous run:")
        for f in sorted(orphaned_files)[:10]:  # Show first 10
            print(f"  - {f}")
        if len(orphaned_files) > 10:
            print(f"  ... and {len(orphaned_files) - 10} more")
        print("\nThese files can prevent proper parallel execution.")
        print("Cleaning up...")
        
        removed_count = 0
        failed_files = []
        for file in orphaned_files:
            file_path = os.path.join(DOFILE_DIR, file)
            try:
                if os.path.isfile(file_path):
                    os.remove(file_path)
                    removed_count += 1
            except Exception as e:
                print(f"  ⚠ Could not delete {file}: {e}")
                failed_files.append((file, str(e)))
        
        print(f"Removed {removed_count}/{len(orphaned_files)} orphaned temp file(s)")
        if failed_files:
            print(f"⚠ Failed to remove {len(failed_files)} file(s)")
        print("="*60 + "\n")
    else:
        if not active_files:
            print("OK: No temp files found - ready to start\n")
        else:
            print("OK: No orphaned temp files - ready to start\n")

def get_dofiles_checked_files():
    """Return a set of files currently in DOFILE_DIR."""
    return set(os.listdir(DOFILE_DIR))

def remove_new_files(initial_files, rt_id=None):
    """Remove any files in DOFILE_DIR that were not present in initial_files.
    If rt_id is provided, only remove files belonging to that rt_id."""
    current_files = set(os.listdir(DOFILE_DIR))
    new_files = current_files - initial_files
    if rt_id:
        # Filter to only files matching this rt_id (temp_{rt_id}_h*_iter* or temp_{rt_id}_iter*)
        new_files = {f for f in new_files if f.startswith(f"temp_{rt_id}_")}
    if new_files:
        print(f"\nCleaning up {len(new_files)} new file(s) in dofiles_consistency_checked:")
        for file in new_files:
            file_path = os.path.join(DOFILE_DIR, file)
            try:
                if os.path.isfile(file_path):
                    os.remove(file_path)
                    print(f"  Deleted: {file}")
            except Exception as e:
                print(f"  Could not delete {file}: {e}")
    else:
        if rt_id:
            print(f"No new files for rt_id {rt_id} to clean up in dofiles_consistency_checked.")
        else:
            print("No new files to clean up in dofiles_consistency_checked.")

def run_single_job(job_info):
    """
    Run a single do-file iteration job.
    Job is a tuple: (rt_id, file_name, iteration, setup_file, initial_dofiles)
    This enables dynamic work distribution across workers - same as bootstrap approach.
    
    Returns: (rt_id, file_name, iteration, success, error_message)
      - success=True: Job completed successfully
      - success=False: Job failed, should NOT be re-queued (hard failure)
      - success='requeue': Job should be re-queued (soft failure, e.g., log move failed)
    """
    rt_id, file_name, iteration, setup_file, initial_dofiles = job_info
    file_path = os.path.join(DOFILE_DIR, file_name)
    
    # Extract hypothesis from file_name
    hypothesis = extract_hypothesis(file_name)
    
    print(f"\n[Worker {os.getpid()}] Executing {file_name} (rt_id: {rt_id}, iter: {iteration}, h{hypothesis})")
    
    try:
        execute_stata_do_file(file_path, rt_id, iteration, use_setup=True, setup_file=setup_file, progress_tracker=None)
        remove_new_files(initial_dofiles, rt_id=rt_id)
        cleanup_iteration_data(rt_id, iteration, hypothesis)
        print(f"[Worker {os.getpid()}] Successfully completed {file_name}")
        return rt_id, file_name, iteration, True, None
    except RuntimeError as e:
        # Check if this is a log move failure (which should trigger re-queuing)
        if "Log file move operation failed" in str(e):
            print(f"[Worker {os.getpid()}] ⚠ Log file operation failed in {file_name} - JOB WILL BE RE-QUEUED")
            print(f"[Worker {os.getpid()}] Error: {e}")
            try:
                cleanup_iteration_data(rt_id, iteration, hypothesis)
            except Exception as cleanup_err:
                print(f"[Worker {os.getpid()}] Error during cleanup: {cleanup_err}")
            # Return 'requeue' status instead of False to indicate this should be re-queued
            return rt_id, file_name, iteration, 'requeue', str(e)
        else:
            # Other RuntimeError - treat as hard failure
            print(f"[Worker {os.getpid()}] Error in {file_name} (rt_id: {rt_id}, iter: {iteration}): {e}")
            import traceback
            traceback.print_exc()
            try:
                cleanup_iteration_data(rt_id, iteration, hypothesis)
            except Exception as cleanup_err:
                print(f"[Worker {os.getpid()}] Error during cleanup: {cleanup_err}")
            return rt_id, file_name, iteration, False, str(e)
    except Exception as e:
        print(f"[Worker {os.getpid()}] Error in {file_name} (rt_id: {rt_id}, iter: {iteration}): {e}")
        import traceback
        traceback.print_exc()
        try:
            cleanup_iteration_data(rt_id, iteration, hypothesis)
        except Exception as cleanup_err:
            print(f"[Worker {os.getpid()}] Error during cleanup: {cleanup_err}")
        return rt_id, file_name, iteration, False, str(e)

def run_rt_id_iteration(rt_id, do_files, setup_file, initial_dofiles, iteration, total_iterations):
    """DEPRECATED: Legacy function kept for compatibility. Use run_single_job for dynamic execution."""
    # This function is no longer used but kept for reference
    pass

def run_rt_id_group(rt_id, do_files, setup_file, initial_dofiles, start_iter, end_iter, skip_complete=False):
    """DEPRECATED: Legacy function kept for compatibility. Use run_rt_id_iteration for parallel execution."""
    # This function is no longer used but kept for reference
    pass

def confirm_iteration_bounds():
    """Ask user to confirm or modify iteration bounds."""
    global START_ITERATION, END_ITERATION
    print("\n" + "="*60)
    print("ITERATION RANGE CONFIGURATION")
    print("="*60)
    print(f"Current configured range: START_ITERATION={START_ITERATION}, END_ITERATION={END_ITERATION}")
    print("Press Enter to keep a value, or type a new integer.")
    try:
        new_start = input(f"Enter start iteration [{START_ITERATION}]: ").strip()
        if new_start != "":
            START_ITERATION = int(new_start)
        new_end = input(f"Enter end iteration   [{END_ITERATION}]: ").strip()
        if new_end != "":
            END_ITERATION = int(new_end)
        if START_ITERATION > END_ITERATION:
            print("Start iteration greater than end iteration. Reverting to defaults.")
            START_ITERATION, END_ITERATION = 0, 2  # Default range
        print(f"Final iteration range: {START_ITERATION} .. {END_ITERATION} (total {END_ITERATION - START_ITERATION + 1})")
        print("="*60 + "\n")
    except Exception as e:
        print(f"Invalid input ({e}). Using defaults {START_ITERATION} .. {END_ITERATION}.\n")

def format_duration(seconds):
    days = int(seconds // 86400)
    hours = int((seconds % 86400) // 3600)
    minutes = int((seconds % 3600) // 60)
    secs = seconds % 60
    parts = []
    if days > 0:
        parts.append(f"{days}d")
    if hours > 0 or days > 0:
        parts.append(f"{hours}h")
    if minutes > 0 or hours > 0 or days > 0:
        parts.append(f"{minutes}m")
    parts.append(f"{secs:.2f}s")
    return " ".join(parts)

class TeeOutput:
    """Write output to both terminal and log file.
    
    NOTE: Worker process output from ProcessPoolExecutor may not be captured
    because each worker runs in a separate process with separate stdout/stderr.
    The log file will contain parent process logs and progress updates, but
    detailed worker logs like [Worker PID] messages appear only in terminal.
    This is a limitation of Python's ProcessPoolExecutor architecture.
    """
    def __init__(self, log_file):
        self.terminal = sys.stdout
        self.log = open(log_file, 'a', encoding='utf-8', buffering=1)  # line buffered
    
    def write(self, message):
        self.terminal.write(message)
        self.log.write(message)
    
    def flush(self):
        self.terminal.flush()
        self.log.flush()
    
    def close(self):
        self.log.close()

def prompt_for_setup_choice():
    """
    Interactively prompt user to choose between presampled and standard setup.
    
    Returns tuple: (setup_file_path, setup_type)
      - setup_type: 'presampled' or 'standard'
      - setup_file_path: full path to the setup .do file to use
    """
    # Define network paths for display
    network_data_raw = r"M:\user\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData\data_raw"
    network_data_derived = r"M:\user\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData\data_derived"
    
    print("\n" + "="*90)
    print("DATA PREPARATION METHOD SELECTION".center(90))
    print("="*90)
    print()
    print("1. PRESAMPLED SETUP (RECOMMENDED) [DEFAULT]")
    print("   └─ Copies pre-sampled iteration-specific datasets from network storage")
    print(f"   ├─ Source (raw data): {network_data_raw}")
    print(f"   ├─ Source (derived):  {network_data_derived}")
    print("   ├─ Fast: No on-demand sampling needed, data is pre-prepared and ready")
    print("   ├─ Isolated: Each job (rt_id×hypothesis×iteration) gets own data copy")
    print("   ├─ Safe: Jobs cannot interfere with each other's raw data")
    print("   └─ When: You have pre-sampled data available on network storage")
    print()
    print("2. STANDARD SETUP (On-Demand Sampling)")
    print("   └─ Generates fresh 5%-samples on-demand for each iteration")
    print("   ├─ Source: Raw SOEP_v37 data (100%-sample)")
    print("   ├─ Slow: Performs sampling from full SOEP dataset for each iteration")
    print("   ├─ Process: Creates 100%, 95%, 5% samples using stratified random sampling")
    print("   ├─ Seeds: Uses iteration number to generate different sample per iteration")
    print("   └─ When: You need on-demand sampling without pre-prepared datasets")
    print()
    print("="*90)
    
    choice = input("Select setup [1/2] (default=1): ").strip()
    
    if choice in ('', '1'):
        setup_type = 'presampled'
        presampled_setup = os.path.join(DOFILE_DIR, "5%-idiosyncrasy_Setup_Presampled.do")
        
        if not os.path.exists(presampled_setup):
            print(f"ERROR: Presampled setup file not found: {presampled_setup}")
            print("Falling back to standard setup.")
            return (os.path.join(DOFILE_DIR, "5%-idiosyncrasy_Setup.do"), 'standard')
        
        # Ask if user wants custom network paths
        print("\n" + "-"*60)
        print("Network data location configuration:")
        print("-"*60)
        default_data_raw = r"M:\user\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData\data_raw"
        default_data_derived = r"M:\user\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData\data_derived"
        
        print(f"Default raw data path:")
        print(f"  {default_data_raw}")
        print(f"\nDefault derived data path:")
        print(f"  {default_data_derived}")
        print()
        
        use_defaults = input("Use default network paths? [Y/n]: ").strip().lower()
        if use_defaults in ('', 'y', 'yes'):
            print(f"Using default paths:")
            print(f"  data_raw: {default_data_raw}")
            print(f"  data_derived: {default_data_derived}")
            return (presampled_setup, setup_type)
        else:
            # Ask for custom paths
            print("\nEnter custom network paths (or press Enter to skip and use defaults):")
            custom_data_raw = input(f"Path to data_raw [{default_data_raw}]: ").strip()
            if not custom_data_raw:
                custom_data_raw = default_data_raw
            
            custom_data_derived = input(f"Path to data_derived [{default_data_derived}]: ").strip()
            if not custom_data_derived:
                custom_data_derived = default_data_derived
            
            # Verify paths exist
            if not os.path.exists(custom_data_raw):
                print(f"WARNING: data_raw path does not exist: {custom_data_raw}")
            if not os.path.exists(custom_data_derived):
                print(f"WARNING: data_derived path does not exist: {custom_data_derived}")
            
            # Create a temporary presampled setup file with custom paths
            print(f"\nCreating temporary setup file with custom paths...")
            temp_setup = os.path.join(DOFILE_DIR, "5%-idiosyncrasy_Setup_Presampled_TEMP.do")
            
            try:
                with open(presampled_setup, 'r') as f:
                    setup_content = f.read()
                
                # Replace network paths in the setup file
                setup_content = setup_content.replace(
                    r'M:\user\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData\data_raw',
                    custom_data_raw
                )
                setup_content = setup_content.replace(
                    r'M:\user\dalbrecht\Research Projects\ManyDaughters_IdiosyncrasySampleData\data_derived',
                    custom_data_derived
                )
                
                with open(temp_setup, 'w') as f:
                    f.write(setup_content)
                
                print(f"✓ Temporary setup file created: {temp_setup}")
                return (temp_setup, setup_type)
            except Exception as e:
                print(f"ERROR: Could not create temporary setup file: {e}")
                print("Using default presampled paths.")
                return (presampled_setup, setup_type)
    
    else:  # choice == '2' or other input
        setup_type = 'standard'
        standard_setup = os.path.join(DOFILE_DIR, "5%-idiosyncrasy_Setup.do")
        
        if not os.path.exists(standard_setup):
            print(f"ERROR: Standard setup file not found: {standard_setup}")
            return (None, None)
        
        print(f"Using standard setup: {standard_setup}")
        return (standard_setup, setup_type)

def execute_files():
    global stop_execution

    # Setup logging to file
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    log_filename = f"execution_log_{timestamp}.txt"
    log_filepath = os.path.join(EXEC_LOG_DIR, log_filename)
    
    # Redirect stdout to both terminal and log file
    tee_stdout = TeeOutput(log_filepath)
    tee_stderr = TeeOutput(log_filepath)
    original_stdout = sys.stdout
    original_stderr = sys.stderr
    sys.stdout = tee_stdout
    sys.stderr = tee_stderr
    
    print(f"Execution log started: {datetime.now()}")
    print(f"Log file: {log_filepath}")
    print("=" * 60)

    try:
        # Clean up any orphaned temp files from previous interrupted runs
        cleanup_orphaned_temp_files()
        
        overall_start = time.time()  # Start timing

        confirm_iteration_bounds()  # NEW: interactive confirmation
        
        # Ask user if they want to skip already completed rt_ids
        print("=" * 60)
        skip_complete_input = input("Skip already completed jobs (rt_id*hypothesis*iteration)? [Y/n]: ").strip().lower()
        skip_complete = skip_complete_input in ('', 'y', 'yes')
        print(f"Skip complete mode: {'ENABLED' if skip_complete else 'DISABLED'}")
        
        # Ask user if they want to rerun jobs with missing log files
        print("=" * 60)
        rerun_missing_logs_input = input("Rerun jobs with CSV results but no log files? [y/N]: ").strip().lower()
        rerun_missing_logs = rerun_missing_logs_input in ('y', 'yes')
        print(f"Rerun missing logs mode: {'ENABLED' if rerun_missing_logs else 'DISABLED'}")
        print("=" * 60 + "\n")

        # Prompt for setup choice (presampled vs standard)
        setup_file, setup_type = prompt_for_setup_choice()
        if setup_file is None:
            print("ERROR: Could not select a valid setup file. Exiting.")
            return
        print(f"\n✓ Using {setup_type} setup")
        print("=" * 60 + "\n")

        initial_dofiles = get_dofiles_checked_files()
        do_files = [f for f in sorted(os.listdir(DOFILE_DIR))
                    if f.endswith('.do')
                    and not f.endswith('_Setup.do')
                    and 'Setup' not in f  # Exclude all setup files
                    and not f.startswith('0_')]  # Exclude main orchestration scripts
        total_files = len(do_files)
        num_iters_effective = END_ITERATION - START_ITERATION + 1
        print(f"Found {total_files} do-files to execute (excluding setup files)")
        print(f"Iteration range: {START_ITERATION} .. {END_ITERATION} (total {num_iters_effective})")
        print(f"Setup file to prepend: {setup_file}")
        print(f"\n💡 TIP: Press Ctrl+C to stop gracefully after current do-file completes\n")
        if not os.path.exists(setup_file):
            print(f"ERROR: Setup file not found: {setup_file}")
            return

        # --- GROUP DO-FILES BY RT_ID ---
        rt_id_to_files = {}
        for file_name in do_files:
            rt_id = extract_rt_id(file_name)
            if rt_id not in rt_id_to_files:
                rt_id_to_files[rt_id] = []
            rt_id_to_files[rt_id].append(file_name)
        
        print(f"Total RT_IDs: {len(rt_id_to_files)}")
        print(f"(Job-level skipping will be applied during queue building)\n")

        # --- DYNAMIC QUEUE-BASED PARALLEL EXECUTION (same as bootstrap) ---
        # Create individual jobs for each (rt_id, file_name, iteration) tuple
        # Workers pull jobs from queue as they become available - maximizes CPU utilization
        try:
            num_cores = psutil.cpu_count(logical=False)
        except Exception:
            num_cores = os.cpu_count() or 2
        
        # Build job queue: one job per (rt_id, hypothesis_file, iteration)
        from collections import deque
        job_queue = deque()
        total_jobs = 0
        skipped_jobs = 0
        
        for rt_id, files in rt_id_to_files.items():
            # Get all completed (iteration, hypothesis) pairs for this rt_id
            completed_pairs = set()
            if skip_complete:
                rt_csv_dir = os.path.join(CSV_DIR, rt_id)
                if os.path.exists(rt_csv_dir):
                    # Pattern: idiosyncrasy_*_hX_results_iterN.csv
                    pattern = re.compile(r'idiosyncrasy_.*_h([1-4])_results_iter(\d+)\.csv')
                    for filename in os.listdir(rt_csv_dir):
                        # Skip subdirectories
                        if os.path.isdir(os.path.join(rt_csv_dir, filename)):
                            continue
                        match = pattern.match(filename)
                        if match:
                            hyp = int(match.group(1))
                            iter_num = int(match.group(2))
                            if START_ITERATION <= iter_num <= END_ITERATION:
                                completed_pairs.add((iter_num, hyp))
            
            # Get jobs missing logs (if enabled)
            missing_logs_pairs = set()
            if rerun_missing_logs:
                missing_logs_pairs = get_jobs_missing_logs(rt_id, START_ITERATION, END_ITERATION)
            
            # Build jobs only for missing (iteration, hypothesis) combinations
            iterations_to_run = list(range(START_ITERATION, END_ITERATION + 1))
            
            for iteration in iterations_to_run:
                for file_name in files:
                    # Extract hypothesis from file_name
                    hyp_match = re.search(r"_h([1-4])\.do$", file_name)
                    if hyp_match:
                        hyp = int(hyp_match.group(1))
                        
                        # Priority 1: Check if this job has missing logs AND rerun is enabled
                        if rerun_missing_logs and (iteration, hyp) in missing_logs_pairs:
                            # Rerun this job to regenerate its log file
                            job_queue.append((rt_id, file_name, iteration, setup_file, initial_dofiles))
                            total_jobs += 1
                            continue
                        
                        # Priority 2: Check if this specific job (rt_id, iteration, hyp) is already fully done (has both CSV and log)
                        if skip_complete and (iteration, hyp) in completed_pairs and (iteration, hyp) not in missing_logs_pairs:
                            # Skip this job - result already exists WITH log
                            skipped_jobs += 1
                            continue
                        
                        # Priority 3: If not skipping complete, or if it's a new job, add it to queue
                        job_queue.append((rt_id, file_name, iteration, setup_file, initial_dofiles))
                        total_jobs += 1
        
        # Calculate total expected jobs based on actual hypothesis files available
        num_rt_ids = len(rt_id_to_files)
        # Count actual hypothesis files across all RT_IDs
        total_hypothesis_files = sum(len(files) for files in rt_id_to_files.values())
        num_iterations = END_ITERATION - START_ITERATION + 1
        total_expected_jobs = total_hypothesis_files * num_iterations
        
        print(f"\n{'='*60}")
        print(f"DYNAMIC QUEUE-BASED PARALLEL EXECUTION")
        print(f"{'='*60}")
        print(f"Physical cores detected: {num_cores}")
        print(f"Total expected jobs: {total_expected_jobs} ({num_rt_ids} rt_ids × {total_hypothesis_files} hypothesis files × {num_iterations} iterations)")
        print(f"Total jobs in queue: {total_jobs} (new/missing jobs)")
        if skip_complete and skipped_jobs > 0:
            print(f"Skipped {skipped_jobs} job(s) with existing results")
        
        max_workers = max(1, min(total_jobs, (num_cores or 2) - 1))
        
        # Optional override via env var MD_MAX_WORKERS
        try:
            env_override = int(os.getenv('MD_MAX_WORKERS', '').strip()) if os.getenv('MD_MAX_WORKERS') else None
            if env_override is not None and env_override > 0:
                max_workers = max(1, min(max_workers, env_override))
        except Exception:
            pass
        
        # Prompt user to choose worker count
        print(f"Max available workers: {max_workers} (limited by {(num_cores or 2) - 1} cores)")
        user_workers = input(f"Workers [press Enter for {max_workers}]: ").strip()
        if user_workers:
            try:
                chosen = int(user_workers)
                if chosen < 1:
                    chosen = 1
                if chosen > max_workers:
                    chosen = max_workers
                max_workers = chosen
            except ValueError:
                print("Invalid input. Using default maximum.")
        
        print(f"Using {max_workers} parallel workers")
        print(f"Expected speedup: ~{min(max_workers, total_jobs)}x")
        print(f"{'='*60}\n")
        
        # Prompt for job submission stagger delay
        print(f"{'='*60}")
        print(f"JOB SUBMISSION STAGGER")
        print(f"{'='*60}")
        print(f"Stagger delay reduces network contention when copying presampled data")
        print(f"by submitting jobs sequentially rather than all at once.")
        print(f"Example: 7 workers with 300s stagger = 7 jobs starting over ~30 minute period")
        stagger_input = input(f"Stagger jobs by X seconds [press Enter for 300]: ").strip()
        stagger_delay = 300
        if stagger_input:
            try:
                stagger_delay = float(stagger_input)
                if stagger_delay < 0:
                    stagger_delay = 0
                    print("Invalid input. Using stagger delay of 0 (no stagger).")
            except ValueError:
                print("Invalid input. Using default 300 second (5 minute) stagger.")
        if stagger_delay > 0:
            print(f"Using {stagger_delay}s stagger between job submissions")
        else:
            print(f"No stagger: all jobs submitted immediately")
        print(f"{'='*60}\n")
        
        if total_jobs == 0:
            print("No jobs to execute. All work complete!")
            return
        
        completed_jobs = 0
        failed_jobs = []
        requeued_jobs = []
        job_start_time = time.time()
        submission_start_time = time.time()
        
        with concurrent.futures.ProcessPoolExecutor(max_workers=max_workers) as executor:
            # Submit all jobs to executor
            future_to_job = {}
            for job_idx, job in enumerate(job_queue):
                # Optional stagger delay between job submissions to reduce network contention
                if stagger_delay > 0 and job_idx > 0:
                    time.sleep(stagger_delay)
                future = executor.submit(run_single_job, job)
                future_to_job[future] = job
            
            # Keep track of jobs pending re-submission
            pending_requeue = []
            
            # Process completed jobs as they finish
            for future in concurrent.futures.as_completed(future_to_job):
                job = future_to_job[future]
                rt_id, file_name, iteration = job[0], job[1], job[2]
                
                try:
                    rt_done, file_done, iter_done, success, error = future.result()
                    completed_jobs += 1
                    
                    if success == 'requeue':
                        # Job needs to be re-queued due to soft failure (e.g., log move failure)
                        print(f"🔄 Job re-queued: {file_done} (rt_id: {rt_done}, iter: {iter_done}) - {error}")
                        requeued_jobs.append((rt_done, file_done, iter_done))
                        # Submit the job again
                        new_future = executor.submit(run_single_job, job)
                        future_to_job[new_future] = job
                        # Reset counter since this job will be processed again
                        completed_jobs -= 1
                    elif not success:
                        # Hard failure - don't re-queue
                        failed_jobs.append((rt_done, file_done, iter_done, error))
                        print(f"❌ Job failed: {file_done} (rt_id: {rt_done}, iter: {iter_done})")
                    
                    # Progress update every 5 jobs or at completion
                    if completed_jobs % 5 == 0 or completed_jobs == total_jobs:
                        elapsed = time.time() - job_start_time
                        avg_time = elapsed / completed_jobs if completed_jobs > 0 else 0
                        remaining = total_jobs - completed_jobs
                        est_remaining = avg_time * remaining if avg_time > 0 else 0
                        
                        print(f"\n{'='*60}")
                        print(f"PROGRESS: {completed_jobs}/{total_jobs} jobs completed ({100*completed_jobs/total_jobs:.1f}%)")
                        if requeued_jobs:
                            print(f"Re-queued: {len(requeued_jobs)} job(s)")
                        if failed_jobs:
                            print(f"Failed: {len(failed_jobs)} job(s)")
                        print(f"Elapsed: {format_duration(elapsed)}")
                        print(f"Estimated remaining: {format_duration(est_remaining)}")
                        print(f"{'='*60}\n")
                        
                except Exception as e:
                    print(f"❌ Unexpected error processing job {file_name} (rt_id: {rt_id}, iter: {iteration}): {e}")
                    failed_jobs.append((rt_id, file_name, iteration, str(e)))
                    import traceback
                    traceback.print_exc()

        overall_elapsed = time.time() - overall_start
        hours = int(overall_elapsed // 3600)
        minutes = int((overall_elapsed % 3600) // 60)
        seconds = overall_elapsed % 60
        
        print("\n" + "="*60)
        print("EXECUTION COMPLETE")
        print("="*60)
        print(f"Completed iterations {START_ITERATION}..{END_ITERATION}")
        print(f"Total jobs: {total_jobs}")
        print(f"Successful: {completed_jobs - len(failed_jobs)}")
        print(f"Re-queued: {len(requeued_jobs)} job(s)")
        print(f"Failed: {len(failed_jobs)}")
        print(f"Total wall time: {hours:02d}h {minutes:02d}m {seconds:05.2f}s ({overall_elapsed:.2f} seconds)")
        
        if requeued_jobs:
            print("\n" + "="*60)
            print(f"RE-QUEUED JOBS ({len(requeued_jobs)} total):")
            print("="*60)
            for rt_id, file_name, iteration in requeued_jobs:
                print(f"  🔄 {file_name} (rt_id: {rt_id}, iter: {iteration})")
            print("="*60)
        
        if failed_jobs:
            print("\n" + "="*60)
            print(f"FAILED JOBS ({len(failed_jobs)} total):")
            print("="*60)
            for rt_id, file_name, iteration, error in failed_jobs:
                print(f"\n❌ {file_name}")
                print(f"   RT_ID: {rt_id}, Iteration: {iteration}")
                print(f"   Error: {error}")
            print("\n" + "="*60)
            print("DEBUGGING TIPS:")
            print("1. Check worker logs above for detailed execution traces")
            print("2. Look for [Worker PID] markers to track specific job progress")
            print("3. Search for 'STATA LAUNCH', 'CSV RENAME', 'LOG MOVE' for file operation timing")
            print("4. Check data/derived/5_idiosyncrasy_logs/{rt_id}/ for Stata output logs")
            print("5. Check data/derived/5_idiosyncrasy_results/{rt_id}/ for result files")
            print("="*60)
        
        print("="*60 + "\n")

    except KeyboardInterrupt:
        print("\n\n" + "="*60)
        print("❌ EXECUTION FORCE STOPPED")
        print("="*60)
        print("Program terminated immediately.")
        print("="*60 + "\n")
        raise
    except Exception as e:
        print(f"An error occurred: {e}")
        import traceback
        traceback.print_exc()
    finally:
        # Clean up temporary presampled setup file if it was created
        temp_presampled_setup = os.path.join(DOFILE_DIR, "5%-idiosyncrasy_Setup_Presampled_TEMP.do")
        if os.path.exists(temp_presampled_setup):
            try:
                os.remove(temp_presampled_setup)
                print(f"Cleaned up temporary setup file: {temp_presampled_setup}")
            except Exception as e:
                print(f"Warning: Could not remove temporary setup file: {e}")
        
        # Restore original stdout/stderr and close log file
        print("\n" + "=" * 60)
        print(f"Execution log ended: {datetime.now()}")
        print(f"Log saved to: {log_filepath}")
        sys.stdout = original_stdout
        sys.stderr = original_stderr
        tee_stdout.close()
        tee_stderr.close()

def emergency_stop():
    """
    Emergency stop function to kill all related processes:
    - Parent Python process running this script
    - All worker processes spawned by ProcessPoolExecutor
    - All Stata instances spawned by the workers
    
    Uses multiple strategies to ensure all processes are terminated:
    1. SIGTERM (graceful shutdown) for Python
    2. taskkill for Stata on Windows (force terminate)
    3. SIGKILL (force kill) as fallback
    4. Zombie process detection
    """
    import subprocess
    import os
    
    print("\n" + "="*60)
    print("🛑 EMERGENCY STOP INITIATED")
    print("="*60)
    
    script_name = "04_prepare_idiosyncrasy_index_5.py"  
    current_pid = os.getpid()
    
    try:
        python_pids = []
        stata_pids = []
        
        # DETECTION PHASE - find all related processes
        print("\n[DETECTION] Scanning for related processes...")
        for proc in psutil.process_iter(['pid', 'name', 'cmdline', 'status']):
            try:
                pid = proc.info['pid']
                if pid == current_pid:
                    continue
                
                name = proc.info.get('name', '').lower()
                cmdline = proc.info.get('cmdline') or []
                status = proc.info.get('status', '')
                cmdline_str = ' '.join(cmdline) if cmdline else ''
                
                # Find Python processes
                if 'python' in name or 'pythonw' in name:
                    # Check multiple conditions for better matching
                    is_our_process = (
                        any(script_name in arg for arg in cmdline) or
                        'spawn_main' in cmdline_str or
                        'multiprocessing' in cmdline_str or
                        any('ManyDaughters' in arg for arg in cmdline)
                    )
                    
                    if is_our_process:
                        python_pids.append(pid)
                        print(f"  [PYTHON] PID {pid} ({status})")
                
                # Find Stata processes
                elif 'stata' in name or 'stataMP' in name or 'stataIC' in name or 'stataSE' in name:
                    stata_pids.append(pid)
                    print(f"  [STATA] PID {pid} ({status})")
                    
            except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
                pass
        
        print(f"\nDetected {len(python_pids)} Python process(es), {len(stata_pids)} Stata process(es)")
        
        if not python_pids and not stata_pids:
            print("✓ No related processes found")
            print("="*60 + "\n")
            return
        
        # Remove this script's own process from the kill list
        python_pids = [pid for pid in python_pids if pid != current_pid]
        print(f"(Excluding current process {current_pid})")
        sys.stdout.flush()
        
        if python_pids:
            print(f"Will terminate {len(python_pids)} other Python process(es)")
            sys.stdout.flush()
        
        # TERMINATION PHASE 1 - Send SIGTERM to Python processes
        if python_pids:
            print(f"\n[TERMINATE] Sending SIGTERM to {len(python_pids)} Python process(es)...")
            sys.stdout.flush()
            for pid in python_pids:
                try:
                    psutil.Process(pid).terminate()
                    print(f"  ✓ SIGTERM sent to PID {pid}")
                    sys.stdout.flush()
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    pass
                except Exception as e:
                    print(f"  ⚠ SIGTERM failed for PID {pid}: {type(e).__name__}")
                    sys.stdout.flush()
            
            print("[WAIT] Waiting for Python processes to terminate...")
            sys.stdout.flush()
            time.sleep(2)
            print("[WAIT] Resuming...")
            sys.stdout.flush()
        
        # TERMINATION PHASE 2 - Kill Stata processes (Windows taskkill is most reliable on Windows)
        if stata_pids:
            print(f"\n[TERMINATE] Terminating {len(stata_pids)} Stata process(es)...")
            sys.stdout.flush()
            for pid in stata_pids:
                killed = False
                print(f"  [Attempting] PID {pid}...")
                sys.stdout.flush()
                
                # On Windows, use taskkill as primary method
                if platform.system().lower() == 'windows':
                    try:
                        # First try /F (force) with /T (tree - kill child processes)
                        print(f"    [taskkill] Trying /F /T...")
                        sys.stdout.flush()
                        result = subprocess.run(
                            ['taskkill', '/PID', str(pid), '/F', '/T'],
                            capture_output=True,
                            timeout=5,
                            check=False
                        )
                        if result.returncode == 0:
                            print(f"  ✓ Killed PID {pid} (taskkill /F /T)")
                            sys.stdout.flush()
                            killed = True
                        else:
                            # Try again with just /F if /T failed
                            print(f"    [taskkill] /T failed (code {result.returncode}), trying /F...")
                            sys.stdout.flush()
                            result = subprocess.run(
                                ['taskkill', '/PID', str(pid), '/F'],
                                capture_output=True,
                                timeout=5,
                                check=False
                            )
                            if result.returncode == 0:
                                print(f"  ✓ Killed PID {pid} (taskkill /F)")
                                sys.stdout.flush()
                                killed = True
                            else:
                                print(f"  ⚠ taskkill /F returned code {result.returncode} for PID {pid}")
                                sys.stdout.flush()
                    except subprocess.TimeoutExpired:
                        print(f"    [taskkill] Timeout for PID {pid}")
                        sys.stdout.flush()
                    except FileNotFoundError:
                        print(f"    [taskkill] Command not found, trying psutil fallback")
                        sys.stdout.flush()
                    except Exception as e:
                        print(f"    [taskkill] Error for PID {pid}: {type(e).__name__}: {e}")
                        sys.stdout.flush()
                
                # Fallback for non-Windows or if taskkill didn't work
                if not killed:
                    try:
                        print(f"    [psutil] Trying kill()...")
                        sys.stdout.flush()
                        proc = psutil.Process(pid)
                        proc.kill()  # SIGKILL
                        print(f"  ✓ SIGKILL sent to PID {pid} (psutil)")
                        sys.stdout.flush()
                        killed = True
                    except psutil.NoSuchProcess:
                        print(f"  ℹ PID {pid} already terminated")
                        sys.stdout.flush()
                        killed = True
                    except psutil.AccessDenied:
                        print(f"  ❌ Access denied killing PID {pid}")
                        sys.stdout.flush()
                    except Exception as e:
                        print(f"  ❌ Failed to kill PID {pid}: {type(e).__name__}: {e}")
                        sys.stdout.flush()
            
            print("[WAIT] Waiting for Stata processes to terminate...")
            sys.stdout.flush()
            time.sleep(2)
            print("[WAIT] Resuming...")
            sys.stdout.flush()
        
        # VERIFICATION PHASE - Check what's still running
        print(f"\n[CHECK] Verifying process termination...")
        sys.stdout.flush()
        remaining_python = []
        remaining_stata = []
        
        for pid in python_pids:
            try:
                proc = psutil.Process(pid)
                if proc.is_running() and proc.status() != psutil.STATUS_ZOMBIE:
                    remaining_python.append(pid)
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
        
        for pid in stata_pids:
            try:
                proc = psutil.Process(pid)
                if proc.is_running() and proc.status() != psutil.STATUS_ZOMBIE:
                    remaining_stata.append(pid)
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
        
        print(f"  After initial termination: {len(remaining_python)} Python, {len(remaining_stata)} Stata still running")
        sys.stdout.flush()
        
        # Force kill any remaining processes
        all_remaining = remaining_python + remaining_stata
        if all_remaining:
            print(f"\n[FORCE-KILL] Attempting to force kill {len(all_remaining)} remaining process(es)...")
            sys.stdout.flush()
            for pid in all_remaining:
                killed = False
                
                # Windows taskkill as primary
                if platform.system().lower() == 'windows':
                    try:
                        result = subprocess.run(
                            ['taskkill', '/PID', str(pid), '/F'],
                            capture_output=True,
                            timeout=5,
                            check=False
                        )
                        if result.returncode == 0:
                            print(f"  ✓ Force killed PID {pid} (taskkill /F)")
                            sys.stdout.flush()
                            killed = True
                    except Exception:
                        pass
                
                # Fallback: psutil kill
                if not killed:
                    try:
                        psutil.Process(pid).kill()
                        print(f"  ✓ Force killed PID {pid} (psutil)")
                        sys.stdout.flush()
                        killed = True
                    except Exception:
                        pass
                
                if not killed:
                    print(f"  ⚠ Could not kill PID {pid}")
                    sys.stdout.flush()
            
            time.sleep(2)
        
        # FINAL VERIFICATION
        print(f"\n[VERIFICATION] Final process check...")
        sys.stdout.flush()
        still_running = []
        
        for proc in psutil.process_iter(['pid', 'name', 'cmdline', 'status']):
            try:
                pid = proc.info['pid']
                if pid == current_pid:
                    continue
                
                name = proc.info.get('name', '').lower()
                cmdline = proc.info.get('cmdline') or []
                status = proc.info.get('status', '')
                
                # Check Python
                if ('python' in name or 'pythonw' in name) and any('ManyDaughters' in arg for arg in cmdline):
                    still_running.append((pid, 'Python', status))
                # Check Stata
                elif 'stata' in name or 'stataMP' in name or 'stataIC' in name or 'stataSE' in name:
                    still_running.append((pid, 'Stata', status))
                    
            except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
                pass
        
        # Report results
        print("="*60)
        sys.stdout.flush()
        if not still_running:
            print("✓ SUCCESS: All processes terminated")
        else:
            print(f"⚠ {len(still_running)} process(es) still present:")
            for pid, ptype, status in still_running[:15]:
                print(f"  - {ptype} PID {pid} ({status})")
            if len(still_running) > 15:
                print(f"  ... and {len(still_running) - 15} more")
            print("\nTroubleshooting:")
            print("  1. Some processes may be stuck (I/O or system lock)")
            print("  2. Close Stata windows manually if they persist")
            print("  3. Check Windows Task Manager (Ctrl+Shift+Esc)")
            print("  4. Restart PowerShell or terminal if needed")
        print("="*60 + "\n")
        sys.stdout.flush()
        
    except Exception as e:
        print(f"\n⚠ Warning during emergency stop: {e}")
        import traceback
        traceback.print_exc()
        print("="*60 + "\n")
        sys.stdout.flush()

if __name__ == "__main__":
    # Check if emergency stop requested via command line argument
    if len(sys.argv) > 1 and sys.argv[1] in ['--stop', '--emergency-stop', 'stop']:
        emergency_stop()
    else:
        execute_files()
