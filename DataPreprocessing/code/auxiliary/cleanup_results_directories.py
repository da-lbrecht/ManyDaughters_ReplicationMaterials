#!/usr/bin/env python3
"""
Clean up results and log directories by removing files with incorrect extensions.

Expected structure:
- Log directories: Only .log files in RT subdirectories
- Results directories: Only .csv files in RT subdirectories

The script will always:
1. Scan directories and list all non-compliant files
2. Ask for confirmation before deletion

Usage:
    python code/auxiliary/cleanup_results_directories.py           # Scan, show files, ask for confirmation
    python code/auxiliary/cleanup_results_directories.py --dry-run # Scan and show files only (no deletion prompt)
"""

import os
import argparse
from pathlib import Path

# Configuration
SCRIPT_DIR = Path(__file__).parent.absolute()
PROJECT_ROOT = SCRIPT_DIR.parent.parent

DIRECTORIES_TO_CHECK = {
    "95_bootstrap_logs": {
        "path": PROJECT_ROOT / "data" / "derived" / "95_bootstrap_logs",
        "expected_extension": ".log",
        "description": "Bootstrap log files"
    },
    "95_bootstrap_results": {
        "path": PROJECT_ROOT / "data" / "derived" / "95_bootstrap_results",
        "expected_extension": ".csv",
        "description": "Bootstrap result files"
    },
    "5_idiosyncrasy_logs": {
        "path": PROJECT_ROOT / "data" / "derived" / "5_idiosyncrasy_logs",
        "expected_extension": ".log",
        "description": "Idiosyncrasy log files"
    },
    "5_idiosyncrasy_results": {
        "path": PROJECT_ROOT / "data" / "derived" / "5_idiosyncrasy_results",
        "expected_extension": ".csv",
        "description": "Idiosyncrasy result files"
    }
}


def format_size(size_bytes):
    """Format file size in human-readable format."""
    for unit in ['B', 'KB', 'MB', 'GB']:
        if size_bytes < 1024.0:
            return f"{size_bytes:.2f} {unit}"
        size_bytes /= 1024.0
    return f"{size_bytes:.2f} TB"


def scan_directory(base_path, expected_extension):
    """
    Scan directory for files that don't match expected extension.
    Returns list of (file_path, size_bytes) tuples.
    """
    unwanted_files = []
    
    if not base_path.exists():
        print(f"  ⚠ Directory not found: {base_path}")
        return unwanted_files
    
    # Iterate through RT subdirectories
    for rt_dir in base_path.iterdir():
        if not rt_dir.is_dir():
            # Files at top level (not in RT subdirectories) are also unwanted
            size = rt_dir.stat().st_size
            unwanted_files.append((rt_dir, size, "top-level file"))
            continue
        
        # Check files within RT subdirectory
        for file_path in rt_dir.iterdir():
            if file_path.is_file():
                if not file_path.suffix.lower() == expected_extension.lower():
                    size = file_path.stat().st_size
                    unwanted_files.append((file_path, size, f"wrong extension ({file_path.suffix})"))
    
    return unwanted_files


def main():
    parser = argparse.ArgumentParser(
        description="Clean up results and log directories by removing files with incorrect extensions."
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Scan and show non-compliant files without prompting for deletion"
    )
    
    args = parser.parse_args()
    
    print("=" * 100)
    print("SCANNING DIRECTORIES FOR NON-COMPLIANT FILES")
    print("=" * 100)
    print()
    
    all_unwanted_files = []
    total_size = 0
    
    # Scan each directory
    for dir_name, config in DIRECTORIES_TO_CHECK.items():
        print(f"Checking: {config['description']}")
        print(f"  Path: {config['path']}")
        print(f"  Expected extension: {config['expected_extension']}")
        
        unwanted = scan_directory(config['path'], config['expected_extension'])
        
        if unwanted:
            print(f"  Found {len(unwanted)} non-compliant file(s):")
            for file_path, size, reason in unwanted:
                rel_path = file_path.relative_to(PROJECT_ROOT)
                print(f"    ✗ {rel_path} ({format_size(size)}) - {reason}")
                all_unwanted_files.append(file_path)
                total_size += size
        else:
            print(f"  ✓ All files compliant")
        
        print()
    
    # Summary
    print("=" * 100)
    print("SUMMARY")
    print("=" * 100)
    print(f"Total non-compliant files: {len(all_unwanted_files)}")
    print(f"Total size: {format_size(total_size)}")
    print()
    
    if not all_unwanted_files:
        print("✓ No files to delete. All directories are clean!")
        return 0
    
    # Always ask for confirmation (unless dry-run)
    if args.dry_run:
        print("DRY RUN MODE - No files will be deleted.")
        return 0
    
    print("=" * 100)
    print("⚠ WARNING: The files listed above will be permanently deleted.")
    print("=" * 100)
    confirm = input("Do you want to delete these files? Type 'yes' to confirm: ").strip().lower()
    
    if confirm != "yes":
        print("Deletion cancelled. No files were deleted.")
        return 1
    
    # Delete files
    print()
    print("Deleting files...")
    deleted_count = 0
    failed_count = 0
    
    for file_path in all_unwanted_files:
        try:
            file_path.unlink()
            print(f"  ✓ Deleted: {file_path.relative_to(PROJECT_ROOT)}")
            deleted_count += 1
        except Exception as e:
            print(f"  ✗ Failed to delete {file_path.relative_to(PROJECT_ROOT)}: {e}")
            failed_count += 1
    
    print()
    print("=" * 100)
    print(f"Deleted: {deleted_count} file(s)")
    if failed_count > 0:
        print(f"Failed: {failed_count} file(s)")
    print("=" * 100)
    
    return 0 if failed_count == 0 else 1


if __name__ == "__main__":
    exit(main())
