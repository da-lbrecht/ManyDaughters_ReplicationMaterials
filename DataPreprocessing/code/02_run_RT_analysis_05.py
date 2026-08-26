import os
import subprocess
import tempfile
import time
import signal
import shutil
from datetime import datetime, timedelta
from dotenv import load_dotenv

# Load environment variables from the .env file
load_dotenv()

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
DOFILE_DIR = os.path.join(
    ROOT_DIR, "data", "raw", "dofiles_consistency_checked", "dofiles_analysis"
)
LOG_DIR = os.path.join(ROOT_DIR, "data", "derived", "5_logs")
CSV_DIR = os.path.join(ROOT_DIR,"data", "derived", "5_results")
SAMPLE_5_DIR = os.path.join(ROOT_DIR, "data", "derived_confidential", "5%-sample")
SAMPLE_5_PROVIDED_DIR = os.path.join(ROOT_DIR, "data", "derived_confidential", "5%-sample_provided")
RAW_SAMPLE_DIR = os.path.join(ROOT_DIR, "data", "raw_confidential", "5%-sample")
RAW_SAMPLE_ORIGINAL_DIR = os.path.join(ROOT_DIR, "data", "raw_confidential", "5%-sample_original")

# Set STATA_EXECUTABLE based on user
if os.getlogin().lower() == "dalbrecht":
    STATA_EXECUTABLE = "C:/Program Files/StataNow19/StataMP-64.exe"
else:
    STATA_EXECUTABLE = os.getenv("STATA_EXECUTABLE", "stata")  # fallback to env or 'stata'

# Create the log and csv directories if they don't exist (should already exist, but safe)
if not os.path.exists(LOG_DIR):
    os.makedirs(LOG_DIR)
if not os.path.exists(CSV_DIR):
    os.makedirs(CSV_DIR)

def move_log_file(file_path, temp_log_name=None):
    if temp_log_name:
        # If a temp log file was created (from a temp do-file), move and rename it
        original_log_file = os.path.join(DOFILE_DIR, temp_log_name)
        log_file_name = os.path.basename(file_path).replace('.do', '.log')
        new_log_file = os.path.join(LOG_DIR, log_file_name)
    else:
        log_file_name = os.path.basename(file_path).replace('.do', '.log')
        original_log_file = os.path.join(DOFILE_DIR, log_file_name)
        new_log_file = os.path.join(LOG_DIR, log_file_name)
    if os.path.exists(original_log_file):
        for attempt in range(5):
            try:
                if os.path.exists(new_log_file):
                    os.remove(new_log_file)
                os.rename(original_log_file, new_log_file)
                print(f"Moved log file to {new_log_file}")
                break
            except PermissionError:
                if attempt < 4:
                    time.sleep(0.5)
                else:
                    print(f"Failed to move log file after several attempts: {original_log_file}")
    else:
        print(f"Log file {original_log_file} not found.")

def rename_csv_file(do_file_name):
    if not os.path.exists(CSV_DIR):
        print(f"CSV directory {CSV_DIR} not found.")
        return
    for file_name in os.listdir(CSV_DIR):
        if file_name.startswith("h") and "_results.csv" in file_name:
            trimmed_name = file_name[3:]  # remove first three characters
            new_file_name = f"repro_{do_file_name}_{trimmed_name}"
            src = os.path.join(CSV_DIR, file_name)
            dst = os.path.join(CSV_DIR, new_file_name)
            if os.path.exists(dst):
                os.remove(dst)
            os.rename(src, dst)
            print(f"Renamed {file_name} to {new_file_name}")

def cleanup_results_directory():
    """Remove files from CSV_DIR that don't match the pattern repro_*_results.csv"""
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
        if not (file_name.startswith("repro_") and file_name.endswith("_results.csv")):
            try:
                os.remove(file_path)
                files_removed.append(file_name)
            except Exception as e:
                print(f"  Could not delete {file_name}: {e}")
    
    if files_removed:
        print(f"Cleaned up {len(files_removed)} unwanted file(s) from results directory:")
        for file in files_removed:
            print(f"  Deleted: {file}")

def execute_stata_do_file(file_path, use_setup=False, setup_file=None, progress_tracker=None):
    original_dir = os.getcwd()
    temp_file_path = None
    temp_log_name = None
    
    if progress_tracker:
        progress_tracker.start_file(os.path.basename(file_path))
    
    try:
        os.chdir(DOFILE_DIR)  # Back to code/preprocessing
        if not os.path.exists(STATA_EXECUTABLE):
            raise FileNotFoundError(f"Stata executable not found at {STATA_EXECUTABLE}. Please ensure the path is correct.")
        if use_setup and setup_file:
            # Create a temporary do-file in code/preprocessing
            with tempfile.NamedTemporaryFile(delete=False, suffix='.do', dir=DOFILE_DIR, mode='w', encoding='utf-8') as temp_do:
                temp_do.write(f'do "{os.path.basename(setup_file)}"\n')
                temp_do.write(f'do "{os.path.basename(file_path)}"\n')
                temp_file_path = temp_do.name
            do_file_to_run = os.path.basename(temp_file_path)
            do_file_name = os.path.basename(file_path)[:-3]
            temp_log_name = do_file_to_run.replace('.do', '.log')
        else:
            do_file_to_run = os.path.basename(file_path)
            do_file_name = os.path.basename(file_path)[:-3]
        result = subprocess.run([STATA_EXECUTABLE, "-e", "do", do_file_to_run], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        print(f"Executed {file_path} successfully.")
        rename_csv_file(do_file_name)
        cleanup_results_directory()
        
        if progress_tracker:
            progress_tracker.complete_file()
            
    except subprocess.CalledProcessError as e:
        print(f"Error executing {file_path}: {e}")
        cleanup_results_directory()  # Clean up even if execution failed
        if progress_tracker:
            progress_tracker.complete_file()  # Still count as completed even if failed
    finally:
        os.chdir(original_dir)
        move_log_file(file_path, temp_log_name=temp_log_name)
        if temp_file_path and os.path.exists(temp_file_path):
            os.remove(temp_file_path)

class ProgressTracker:
    def __init__(self, total_files):
        self.total_files = total_files
        self.completed_files = 0
        self.start_time = datetime.now()
        self.file_times = []
        
    def start_file(self, filename):
        self.current_file = filename
        self.file_start_time = datetime.now()
        
    def complete_file(self):
        file_duration = (datetime.now() - self.file_start_time).total_seconds()
        self.file_times.append(file_duration)
        self.completed_files += 1
        self._print_progress()
        
    def _print_progress(self):
        elapsed_time = datetime.now() - self.start_time
        remaining_files = self.total_files - self.completed_files
        
        # Calculate statistics
        avg_time = sum(self.file_times) / len(self.file_times) if self.file_times else 0
        min_time = min(self.file_times) if self.file_times else 0
        max_time = max(self.file_times) if self.file_times else 0
        
        # Estimate remaining time
        if avg_time > 0:
            estimated_remaining = timedelta(seconds=avg_time * remaining_files)
        else:
            estimated_remaining = timedelta(0)
            
        print(f"\n{'='*60}")
        print(f"PROGRESS REPORT")
        print(f"{'='*60}")
        print(f"Total do-files: {self.total_files}")
        print(f"Completed: {self.completed_files}")
        print(f"Remaining: {remaining_files}")
        print(f"Time elapsed: {str(elapsed_time).split('.')[0]}")
        print(f"Average time per file: {avg_time:.2f}s")
        print(f"Minimum time per file: {min_time:.2f}s")
        print(f"Maximum time per file: {max_time:.2f}s")
        print(f"Estimated time remaining: {str(estimated_remaining).split('.')[0]}")
        print(f"Progress: {self.completed_files}/{self.total_files} ({(self.completed_files/self.total_files)*100:.1f}%)")
        print(f"{'='*60}\n")

def sync_5_percent_sample():
    """Ensure 5%-sample directory matches 5%-sample_provided directory."""
    print("\n" + "="*60)
    print("SYNCING 5%-SAMPLE DIRECTORY")
    print("="*60)
    
    if not os.path.exists(SAMPLE_5_PROVIDED_DIR):
        print(f"ERROR: Source directory not found: {SAMPLE_5_PROVIDED_DIR}")
        raise FileNotFoundError(f"5%-sample_provided directory not found")
    
    # Create target directory if it doesn't exist
    if not os.path.exists(SAMPLE_5_DIR):
        os.makedirs(SAMPLE_5_DIR)
        print(f"Created directory: {SAMPLE_5_DIR}")
    
    # Get files in both directories
    provided_files = set(os.listdir(SAMPLE_5_PROVIDED_DIR))
    current_files = set(os.listdir(SAMPLE_5_DIR))
    
    # Files to remove (in current but not in provided)
    files_to_remove = current_files - provided_files
    # Files to copy (in provided but not in current, or modified)
    files_to_copy = []
    
    for file in provided_files:
        src_path = os.path.join(SAMPLE_5_PROVIDED_DIR, file)
        dst_path = os.path.join(SAMPLE_5_DIR, file)
        
        # Skip if not a file
        if not os.path.isfile(src_path):
            continue
            
        # Copy if file doesn't exist or has different modification time
        if file not in current_files:
            files_to_copy.append(file)
        else:
            src_mtime = os.path.getmtime(src_path)
            dst_mtime = os.path.getmtime(dst_path)
            if abs(src_mtime - dst_mtime) > 1:  # Allow 1 second difference
                files_to_copy.append(file)
    
    # Remove extra files
    for file in files_to_remove:
        file_path = os.path.join(SAMPLE_5_DIR, file)
        if os.path.isfile(file_path):
            os.remove(file_path)
            print(f"  Removed: {file}")
    
    # Copy needed files
    for file in files_to_copy:
        src_path = os.path.join(SAMPLE_5_PROVIDED_DIR, file)
        dst_path = os.path.join(SAMPLE_5_DIR, file)
        shutil.copy2(src_path, dst_path)  # copy2 preserves metadata
        print(f"  Copied: {file}")
    
    if not files_to_remove and not files_to_copy:
        print("  ✓ Directories already in sync")
    else:
        print(f"  ✓ Sync complete: {len(files_to_copy)} copied, {len(files_to_remove)} removed")
    
    print("="*60 + "\n")

def sync_raw_5_percent_sample():
    """Ensure raw 5%-sample directory matches 5%-sample_original directory."""
    print("\n" + "="*60)
    print("SYNCING RAW 5%-SAMPLE DIRECTORY")
    print("="*60)
    
    if not os.path.exists(RAW_SAMPLE_ORIGINAL_DIR):
        print(f"ERROR: Source directory not found: {RAW_SAMPLE_ORIGINAL_DIR}")
        raise FileNotFoundError(f"5%-sample_original directory not found")
    
    # Create target directory if it doesn't exist
    if not os.path.exists(RAW_SAMPLE_DIR):
        os.makedirs(RAW_SAMPLE_DIR)
        print(f"Created directory: {RAW_SAMPLE_DIR}")
    
    # Get files in both directories
    provided_files = set(os.listdir(RAW_SAMPLE_ORIGINAL_DIR))
    current_files = set(os.listdir(RAW_SAMPLE_DIR))
    
    # Files to remove (in current but not in provided)
    files_to_remove = current_files - provided_files
    # Files to copy (in provided but not in current, or modified)
    files_to_copy = []
    
    for file in provided_files:
        src_path = os.path.join(RAW_SAMPLE_ORIGINAL_DIR, file)
        dst_path = os.path.join(RAW_SAMPLE_DIR, file)
        
        # Skip if not a file
        if not os.path.isfile(src_path):
            continue
            
        # Copy if file doesn't exist or has different modification time
        if file not in current_files:
            files_to_copy.append(file)
        else:
            src_mtime = os.path.getmtime(src_path)
            dst_mtime = os.path.getmtime(dst_path)
            if abs(src_mtime - dst_mtime) > 1:  # Allow 1 second difference
                files_to_copy.append(file)
    
    # Remove extra files
    for file in files_to_remove:
        file_path = os.path.join(RAW_SAMPLE_DIR, file)
        if os.path.isfile(file_path):
            os.remove(file_path)
            print(f"  Removed: {file}")
    
    # Copy needed files
    for file in files_to_copy:
        src_path = os.path.join(RAW_SAMPLE_ORIGINAL_DIR, file)
        dst_path = os.path.join(RAW_SAMPLE_DIR, file)
        shutil.copy2(src_path, dst_path)  # copy2 preserves metadata
        print(f"  Copied: {file}")
    
    if not files_to_remove and not files_to_copy:
        print("  ✓ Directories already in sync")
    else:
        print(f"  ✓ Sync complete: {len(files_to_copy)} copied, {len(files_to_remove)} removed")
    
    print("="*60 + "\n")

def get_dofiles_checked_files():
    """Return a set of files currently in DOFILE_DIR."""
    return set(os.listdir(DOFILE_DIR))

def remove_new_files(initial_files):
    """Remove any files in DOFILE_DIR that were not present in initial_files."""
    current_files = set(os.listdir(DOFILE_DIR))
    new_files = current_files - initial_files
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
        print("No new files to clean up in dofiles_consistency_checked.")

def execute_files():
    global stop_execution

    try:
        # Sync 5%-sample directory before starting
        sync_5_percent_sample()

        # Sync raw 5%-sample directory before starting
        sync_raw_5_percent_sample()

        # Track initial files in dofiles_consistency_checked
        initial_dofiles = get_dofiles_checked_files()

        # Setup file to prepend to all other do-files
        setup_file = os.path.join(DOFILE_DIR, "5%-sample_Setup.do")

        # Get all do-files except the setup files
        do_files = [f for f in sorted(os.listdir(DOFILE_DIR))
                   if f.endswith('.do')
                   and not f.endswith('_Setup.do')]  # Exclude both setup files

        total_files = len(do_files)
        print(f"Found {total_files} do-files to execute (excluding setup files)")
        print(f"Setup file to prepend: {setup_file}")
        print(f"\n💡 TIP: Press Ctrl+C to stop gracefully after current do-file completes\n")

        if not os.path.exists(setup_file):
            print(f"ERROR: Setup file not found: {setup_file}")
            return

        progress_tracker = ProgressTracker(total_files)

        # Execute all do-files with setup prepended
        for file_name in do_files:
            # Check if stop was requested
            if stop_execution:
                print("\n" + "="*60)
                print("⚠ EXECUTION STOPPED BY USER")
                print("="*60)
                print(f"Completed: {progress_tracker.completed_files}/{total_files} do-files")
                print(f"Remaining: {total_files - progress_tracker.completed_files} do-files")
                print("="*60 + "\n")
                return

            file_path = os.path.join(DOFILE_DIR, file_name)
            print(f"\nExecuting {file_name} with setup prepended...")
            execute_stata_do_file(file_path, use_setup=True, setup_file=setup_file, progress_tracker=progress_tracker)

            # Clean up any new files created in DOFILE_DIR
            remove_new_files(initial_dofiles)

            # Restore raw data after each dofile
            sync_raw_5_percent_sample()

            # Restore derived sample after each dofile
            sync_5_percent_sample()

        print("\n" + "="*60)
        print("EXECUTION COMPLETE")
        print("="*60)
        
        # Final cleanup of results directory
        cleanup_results_directory()

    except KeyboardInterrupt:
        # Handle force quit (second Ctrl+C)
        print("\n\n" + "="*60)
        print("❌ EXECUTION FORCE STOPPED")
        print("="*60)
        print("Program terminated immediately.")
        print("="*60 + "\n")
        raise
    except Exception as e:
        print(f"An error occurred: {e}")

if __name__ == "__main__":
    execute_files()
