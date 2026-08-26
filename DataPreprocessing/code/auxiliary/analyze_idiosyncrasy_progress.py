#!/usr/bin/env python3
"""
Analyze idiosyncrasy analysis progress by scanning result files.

This script examines data/derived/5_idiosyncrasy_results to determine which
rt_id/hypothesis combinations have completed iterations, and reports:
- Per rt_id: iteration counts for each hypothesis (h1, h2, h3, h4)
- Per hypothesis summary statistics (min, max, mean completed iterations)
- Overall completion status

Usage:
    python code/auxiliary/analyze_idiosyncrasy_progress.py
    python code/auxiliary/analyze_idiosyncrasy_progress.py --verbose
    python code/auxiliary/analyze_idiosyncrasy_progress.py --incomplete-only
"""

import os
import re
import argparse
from collections import defaultdict

# Configuration
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.abspath(os.path.join(SCRIPT_DIR, "..", ".."))
RESULTS_DIR = os.path.join(PROJECT_ROOT, "data", "derived", "5_idiosyncrasy_results")

# Expected total number of RT_IDs in the project
TOTAL_EXPECTED_RT_IDS = 120

# Pattern to extract rt_id, hypothesis, and iteration from filenames
# Expected format: YYYYMMDD_HHMMSS_rtid_hN_results_iterN.csv
PATTERN = re.compile(r'.*_([a-zA-Z0-9]+)_h([1-4])_results_iter(\d+)\.csv')

def collect_results():
    """Scan results directory and collect completion data.
    Returns: dict[rt_id][hypothesis] -> set of completed iterations
    """
    rt_data = defaultdict(lambda: {'h1': set(), 'h2': set(), 'h3': set(), 'h4': set()})
    
    if not os.path.exists(RESULTS_DIR):
        print(f"ERROR: Results directory not found: {RESULTS_DIR}")
        return rt_data
    
    for rt_id_dir in os.listdir(RESULTS_DIR):
        dir_path = os.path.join(RESULTS_DIR, rt_id_dir)
        if not os.path.isdir(dir_path):
            continue
        
        for filename in os.listdir(dir_path):
            match = PATTERN.match(filename)
            if match:
                rt_id = match.group(1)
                hyp_num = match.group(2)
                hypothesis = f'h{hyp_num}'
                iteration = int(match.group(3))
                rt_data[rt_id][hypothesis].add(iteration)
    
    return rt_data


def determine_expected_range(rt_data):
    """Determine expected iteration range based on actual data."""
    all_iters = set()
    for rt_id in rt_data:
        for hyp in ['h1', 'h2', 'h3', 'h4']:
            all_iters.update(rt_data[rt_id][hyp])
    
    if not all_iters:
        return 0, 2, 3  # Default: 0-2 (3 iterations)
    
    min_iter = min(all_iters)
    max_iter = max(all_iters)
    total_expected = max_iter - min_iter + 1
    
    return min_iter, max_iter, total_expected


def print_per_rt_id_report(rt_data, min_iter, max_iter, total_expected, incomplete_only=False):
    """Print per-rt_id hypothesis completion table."""
    print("=" * 100)
    print("PER RT_ID HYPOTHESIS COMPLETION")
    print("=" * 100)
    print(f"Expected iteration range: {min_iter} - {max_iter} ({total_expected} iterations)")
    print(f"Total RT_IDs in project: {TOTAL_EXPECTED_RT_IDS}")
    print(f"RT_IDs with results: {len(rt_data)}")
    if len(rt_data) < TOTAL_EXPECTED_RT_IDS:
        print(f"⚠ Missing {TOTAL_EXPECTED_RT_IDS - len(rt_data)} RT_IDs (no results found at all)")
    print("=" * 100)
    print(f"{'RT_ID':<10} {'h1':<12} {'h2':<12} {'h3':<12} {'h4':<12} {'Status':<20}")
    print("-" * 100)
    
    complete_rt_ids = 0
    incomplete_rt_ids = 0
    
    for rt_id in sorted(rt_data.keys()):
        h1_count = len(rt_data[rt_id]['h1'])
        h2_count = len(rt_data[rt_id]['h2'])
        h3_count = len(rt_data[rt_id]['h3'])
        h4_count = len(rt_data[rt_id]['h4'])
        
        all_complete = all(
            len(rt_data[rt_id][h]) == total_expected 
            for h in ['h1', 'h2', 'h3', 'h4']
        )
        
        if all_complete:
            status = "✓ COMPLETE"
            complete_rt_ids += 1
            if incomplete_only:
                continue
        else:
            status = "INCOMPLETE"
            incomplete_rt_ids += 1
        
        print(f"{rt_id:<10} {h1_count:<12} {h2_count:<12} {h3_count:<12} {h4_count:<12} {status:<20}")
    
    print("-" * 100)
    print(f"RT_IDs found: {len(rt_data)}/{TOTAL_EXPECTED_RT_IDS} (Complete: {complete_rt_ids}, Incomplete: {incomplete_rt_ids})")
    if len(rt_data) < TOTAL_EXPECTED_RT_IDS:
        print(f"⚠ {TOTAL_EXPECTED_RT_IDS - len(rt_data)} RT_IDs have no results at all")
    print("=" * 100)
    print()
    
    return complete_rt_ids, incomplete_rt_ids


def print_hypothesis_summary(rt_data, total_expected):
    """Print summary statistics per hypothesis."""
    print("=" * 100)
    print("PER-HYPOTHESIS SUMMARY STATISTICS")
    print("=" * 100)
    print(f"Total RT_IDs in project: {TOTAL_EXPECTED_RT_IDS}")
    print(f"RT_IDs with results: {len(rt_data)}")
    print("=" * 100)
    
    for hyp in ['h1', 'h2', 'h3', 'h4']:
        counts = [len(rt_data[rt_id][hyp]) for rt_id in rt_data]
        
        if not counts:
            continue
        
        min_count = min(counts)
        max_count = max(counts)
        mean_count = sum(counts) / len(counts)
        complete_count = sum(1 for c in counts if c == total_expected)
        incomplete_count = len(counts) - complete_count
        
        print(f"\n{hyp.upper()}:")
        print(f"  RT_IDs analyzed:           {len(counts)}/{TOTAL_EXPECTED_RT_IDS}")
        print(f"  Min iterations completed:  {min_count}")
        print(f"  Max iterations completed:  {max_count}")
        print(f"  Mean iterations completed: {mean_count:.1f}")
        print(f"  RT_IDs with all {total_expected} iters: {complete_count}/{len(counts)} ({100*complete_count/len(counts):.1f}%)")
        print(f"  RT_IDs incomplete:         {incomplete_count}/{len(counts)} ({100*incomplete_count/len(counts):.1f}%)")
        
        # Show distribution
        if incomplete_count > 0:
            zero_count = sum(1 for c in counts if c == 0)
            partial_count = sum(1 for c in counts if 0 < c < total_expected)
            if zero_count > 0:
                print(f"    - No iterations:   {zero_count}")
            if partial_count > 0:
                print(f"    - Partial (1-{total_expected-1}): {partial_count}")
    
    print("=" * 100)
    print()


def print_missing_details(rt_data, min_iter, max_iter, verbose=False):
    """Print detailed missing iterations for incomplete rt_ids."""
    expected = set(range(min_iter, max_iter + 1))
    
    print("=" * 100)
    print("INCOMPLETE RT_IDs - DETAILED VIEW")
    print("=" * 100)
    print(f"{'RT_ID':<10} {'Hyp':<5} {'Completed':<12} {'Missing Iterations':<70}")
    print("-" * 100)
    
    has_incomplete = False
    
    for rt_id in sorted(rt_data.keys()):
        rt_has_incomplete = False
        
        for hyp in ['h1', 'h2', 'h3', 'h4']:
            iterations = rt_data[rt_id][hyp]
            completed_count = len(iterations)
            
            if completed_count < len(expected):
                if not rt_has_incomplete:
                    # First incomplete hypothesis for this rt_id
                    rt_has_incomplete = True
                    has_incomplete = True
                
                missing = sorted(expected - iterations)
                missing_str = ', '.join(map(str, missing))
                
                if len(missing_str) > 67:
                    missing_str = missing_str[:64] + "..."
                
                print(f"{rt_id:<10} {hyp:<5} {completed_count:<12} {missing_str:<70}")
        
        if rt_has_incomplete:
            print()  # Blank line between rt_ids
    
    if not has_incomplete:
        print("✓ All RT_IDs complete for all hypotheses!")
    
    print("=" * 100)
    print()


def main():
    parser = argparse.ArgumentParser(
        description="Analyze idiosyncrasy analysis progress by scanning result files."
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Show detailed missing iterations for incomplete runs"
    )
    parser.add_argument(
        "-i", "--incomplete-only",
        action="store_true",
        help="Show only incomplete RT_IDs in the per-rt_id table"
    )
    
    args = parser.parse_args()
    
    # Collect data
    rt_data = collect_results()
    
    if not rt_data:
        print("No idiosyncrasy analysis results found.")
        return 1
    
    # Determine expected iteration range
    min_iter, max_iter, total_expected = determine_expected_range(rt_data)
    
    # Print per-rt_id report
    complete_rt_ids, incomplete_rt_ids = print_per_rt_id_report(
        rt_data, min_iter, max_iter, total_expected, 
        incomplete_only=args.incomplete_only
    )
    
    # Print hypothesis summary statistics
    print_hypothesis_summary(rt_data, total_expected)
    
    # Print detailed missing iterations if verbose
    if args.verbose:
        print_missing_details(rt_data, min_iter, max_iter, verbose=True)
    
    return 0


if __name__ == "__main__":
    exit(main())
