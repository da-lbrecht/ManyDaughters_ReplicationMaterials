#!/usr/bin/env python3
"""
Emergency Stop Script for Idiosyncrasy Index Analysis

This script provides a convenient way to stop all running idiosyncrasy analysis processes,
including the parent Python process, worker processes, and Stata instances.

Usage:
    python code/auxiliary/stop_04_idiosyncrasy.py
    
    or
    
    python code/04_prepare_idiosyncrasy_index_5.py --stop
"""

import sys
import os
import importlib.util

# The wrapper lives in code/auxiliary; the numbered runner lives in code.
code_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, code_dir)

# Import module with numeric prefix using importlib
module_path = os.path.join(code_dir, '04_prepare_idiosyncrasy_index_5.py')
spec = importlib.util.spec_from_file_location("idiosyncrasy_module", module_path)
idiosyncrasy_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(idiosyncrasy_module)

if __name__ == "__main__":
    idiosyncrasy_module.emergency_stop()
