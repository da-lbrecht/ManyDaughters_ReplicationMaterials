#!/usr/bin/env python3
"""
Estimate parallelization efficiency from idiosyncrasy execution logs.

Analyzes log file timestamps to identify execution batches and calculate
realistic efficiency factors for use in execution time predictions.

Usage:
    python code/auxiliary/estimate_idiosyncrasy_parallelization_efficiency.py
"""
import os
import re
import json
from datetime import datetime
from collections import defaultdict
from pathlib import Path
from typing import Dict, List
import statistics


class IdiosyncrasyLogAnalyzer:
    """Analyze idiosyncrasy log files to estimate parallelization efficiency."""
    
    def __init__(self, log_dir: str):
        """Initialize analyzer with log directory."""
        self.log_dir = Path(log_dir)
        self.log_files = []
        self.batches = []
        self.efficiency_estimates = {}
        
    def discover_logs(self) -> List[Dict]:
        """Discover and parse all idiosyncrasy log files."""
        log_pattern = re.compile(
            r'idiosyncrasy_(\d{8})_(\d{6})_([0-9A-Za-z]+)_h(\d)_iter(\d+)\.log'
        )
        
        logs = []
        
        for rt_id_dir in self.log_dir.iterdir():
            if not rt_id_dir.is_dir():
                continue
            
            for log_file in rt_id_dir.glob('*.log'):
                match = log_pattern.match(log_file.name)
                if not match:
                    continue
                
                date_str = match.group(1)
                time_str = match.group(2)
                rt_id = match.group(3)
                hyp = int(match.group(4))
                iteration = int(match.group(5))
                
                # Parse timestamp from filename
                try:
                    start_time = datetime.strptime(f"{date_str}_{time_str}", "%Y%m%d_%H%M%S")
                except ValueError:
                    continue
                
                # Get file modification time (job end)
                stat = log_file.stat()
                mtime = datetime.fromtimestamp(stat.st_mtime)
                
                # Calculate elapsed time
                elapsed_seconds = (mtime - start_time).total_seconds()
                
                # Sanity check
                if elapsed_seconds <= 0 or elapsed_seconds > 86400:
                    continue
                
                logs.append({
                    'rt_id': rt_id,
                    'hypothesis': hyp,
                    'iteration': iteration,
                    'start_time': start_time,
                    'end_time': mtime,
                    'elapsed_seconds': elapsed_seconds,
                    'logfile': str(log_file.name)
                })
        
        print(f"Discovered {len(logs)} valid log files")
        self.log_files = sorted(logs, key=lambda x: x['start_time'])
        return logs
    
    def identify_batches(self, time_window_seconds: int = 60) -> List[Dict]:
        """Identify execution batches by clustering jobs with similar start times."""
        if not self.log_files:
            return []
        
        batches = []
        current_batch = []
        batch_start_time = self.log_files[0]['start_time']
        
        for log in self.log_files:
            time_since_batch_start = (log['start_time'] - batch_start_time).total_seconds()
            
            if time_since_batch_start <= time_window_seconds:
                current_batch.append(log)
            else:
                if current_batch:
                    batches.append(self._create_batch_dict(current_batch))
                current_batch = [log]
                batch_start_time = log['start_time']
        
        if current_batch:
            batches.append(self._create_batch_dict(current_batch))
        
        print(f"Identified {len(batches)} execution batches")
        self.batches = batches
        return batches
    
    def _create_batch_dict(self, batch_logs: List[Dict]) -> Dict:
        """Create a batch dictionary from a list of logs."""
        start_times = [log['start_time'] for log in batch_logs]
        end_times = [log['end_time'] for log in batch_logs]
        elapsed_times = [log['elapsed_seconds'] for log in batch_logs]
        rt_ids = set(log['rt_id'] for log in batch_logs)
        
        batch_start = min(start_times)
        batch_end = max(end_times)
        batch_duration = (batch_end - batch_start).total_seconds()
        
        return {
            'batch_id': batch_start.strftime('%Y%m%d_%H%M%S'),
            'start_time': batch_start,
            'end_time': batch_end,
            'num_jobs': len(batch_logs),
            'unique_rt_ids': len(rt_ids),
            'batch_duration_seconds': batch_duration,
            'job_durations_seconds': elapsed_times,
            'mean_job_duration': statistics.mean(elapsed_times),
            'median_job_duration': statistics.median(elapsed_times),
        }
    
    def estimate_core_counts(self) -> Dict[str, int]:
        """Estimate the number of cores used for each batch."""
        core_estimates = {}
        
        for batch in self.batches:
            num_jobs = batch['num_jobs']
            mean_duration = batch['mean_job_duration']
            batch_duration = batch['batch_duration_seconds']
            
            if num_jobs == 1:
                estimated_cores = 1
            else:
                serial_time = num_jobs * mean_duration
                if batch_duration > 0:
                    speedup = serial_time / batch_duration
                    realistic_efficiency = 0.15
                    estimated_cores = int(speedup / realistic_efficiency)
                    estimated_cores = max(1, min(estimated_cores, 256))
                else:
                    estimated_cores = 1
            
            core_estimates[batch['batch_id']] = estimated_cores
            batch['estimated_cores'] = estimated_cores
        
        return core_estimates
    
    def calculate_efficiency(self) -> Dict[str, Dict]:
        """Calculate parallelization efficiency for each batch."""
        if not self.batches:
            return {}
        
        efficiency_results = {}
        
        for batch in self.batches:
            batch_id = batch['batch_id']
            num_jobs = batch['num_jobs']
            num_cores = batch['estimated_cores']
            mean_duration = batch['mean_job_duration']
            batch_duration = batch['batch_duration_seconds']
            
            serial_time = num_jobs * mean_duration
            parallel_time = batch_duration
            
            if parallel_time > 0:
                achieved_speedup = serial_time / parallel_time
            else:
                achieved_speedup = 1.0
            
            if num_cores > 0:
                efficiency = achieved_speedup / num_cores
            else:
                efficiency = 0.0
            
            efficiency = min(efficiency, 1.0)
            efficiency = max(efficiency, 0.0)
            
            efficiency_results[batch_id] = {
                'num_jobs': num_jobs,
                'estimated_cores': num_cores,
                'mean_job_duration_sec': mean_duration,
                'batch_duration_sec': batch_duration,
                'serial_time_sec': serial_time,
                'parallel_time_sec': parallel_time,
                'achieved_speedup': achieved_speedup,
                'theoretical_cores': num_cores,
                'efficiency': efficiency,
            }
            
            batch['efficiency'] = efficiency
            batch['achieved_speedup'] = achieved_speedup
            batch['theoretical_cores'] = num_cores
        
        self.efficiency_estimates = efficiency_results
        return efficiency_results
    
    def generate_summary_statistics(self) -> Dict:
        """Generate summary statistics across all batches."""
        if not self.efficiency_estimates:
            return {}
        
        efficiencies = [stats['efficiency'] for stats in self.efficiency_estimates.values()]
        core_counts = [batch['estimated_cores'] for batch in self.batches]
        
        summary = {
            'total_batches': len(self.batches),
            'total_jobs': sum(batch['num_jobs'] for batch in self.batches),
            'overall_mean_efficiency': statistics.mean(efficiencies),
            'overall_median_efficiency': statistics.median(efficiencies),
            'overall_stdev_efficiency': statistics.stdev(efficiencies) if len(efficiencies) > 1 else 0.0,
            'core_counts': {
                'mean': statistics.mean(core_counts) if core_counts else 0,
                'median': statistics.median(core_counts) if core_counts else 0,
                'min': min(core_counts) if core_counts else 0,
                'max': max(core_counts) if core_counts else 0,
            },
            'analysis_timestamp': datetime.now().isoformat(),
        }
        
        efficiency_by_cores = defaultdict(list)
        for batch in self.batches:
            cores = batch['estimated_cores']
            eff = batch['efficiency']
            efficiency_by_cores[cores].append(eff)
        
        summary['efficiency_by_core_count'] = {
            int(cores): {
                'mean': statistics.mean(effs),
                'stdev': statistics.stdev(effs) if len(effs) > 1 else 0.0,
                'num_samples': len(effs),
            }
            for cores, effs in sorted(efficiency_by_cores.items())
        }
        
        return summary
    
    def save_results(self, output_path: str) -> None:
        """Save analysis results to JSON file."""
        output_file = Path(output_path)
        output_file.parent.mkdir(parents=True, exist_ok=True)
        
        batches_serializable = []
        for batch in self.batches:
            batch_data = batch.copy()
            batch_data['start_time'] = batch_data['start_time'].isoformat()
            batch_data['end_time'] = batch_data['end_time'].isoformat()
            batches_serializable.append(batch_data)
        
        efficiency_serializable = {}
        for batch_id, stats in self.efficiency_estimates.items():
            efficiency_serializable[batch_id] = stats
        
        summary = self.generate_summary_statistics()
        
        output_data = {
            'summary': summary,
            'batches': batches_serializable,
            'efficiency_estimates': efficiency_serializable,
        }
        
        with open(output_file, 'w') as f:
            json.dump(output_data, f, indent=2)
        
        print(f"Results saved to: {output_file}")
    
    def print_summary(self) -> None:
        """Print analysis summary to console."""
        if not self.batches:
            print("No batches found to analyze.")
            return
        
        summary = self.generate_summary_statistics()
        
        print("\n" + "="*80)
        print("IDIOSYNCRASY PARALLELIZATION EFFICIENCY ANALYSIS SUMMARY")
        print("="*80)
        print(f"Total batches analyzed:     {summary['total_batches']}")
        print(f"Total jobs analyzed:        {summary['total_jobs']}")
        print(f"\nOverall efficiency metrics:")
        print(f"  Mean efficiency:          {summary['overall_mean_efficiency']:.4f}")
        print(f"  Median efficiency:        {summary['overall_median_efficiency']:.4f}")
        print(f"  Std Dev efficiency:       {summary['overall_stdev_efficiency']:.4f}")
        
        print(f"\nCore count statistics:")
        cores = summary['core_counts']
        print(f"  Mean cores:               {cores['mean']:.1f}")
        print(f"  Median cores:             {cores['median']:.1f}")
        print(f"  Range:                    {cores['min']} - {cores['max']}")
        
        print(f"\nEfficiency by core count:")
        for core_count in sorted(summary['efficiency_by_core_count'].keys()):
            stats = summary['efficiency_by_core_count'][core_count]
            print(f"  {core_count:3d} cores: "
                  f"mean={stats['mean']:.4f}, "
                  f"stdev={stats['stdev']:.4f}, "
                  f"n={stats['num_samples']}")
        
        print("\n" + "="*80)


def main():
    """Main entry point."""
    project_root = Path(__file__).resolve().parents[2]
    log_dir = project_root / "data" / "derived" / "5_idiosyncrasy_logs"
    output_dir = project_root / "out"
    output_file = output_dir / "idiosyncrasy_parallelization_efficiency.json"
    
    if not log_dir.exists():
        print(f"Error: Log directory not found: {log_dir}")
        return 1
    
    print(f"\n{'='*80}")
    print("IDIOSYNCRASY PARALLELIZATION EFFICIENCY ANALYZER")
    print(f"{'='*80}\n")
    print(f"Log directory: {log_dir}")
    print(f"Output file:  {output_file}")
    
    analyzer = IdiosyncrasyLogAnalyzer(str(log_dir))
    
    print("\n[1/5] Discovering log files...")
    logs = analyzer.discover_logs()
    
    if not logs:
        print("Error: No valid log files found")
        return 1
    
    print(f"      Found {len(logs)} log files spanning from")
    print(f"      {logs[0]['start_time']} to {logs[-1]['start_time']}")
    
    print("\n[2/5] Identifying execution batches...")
    batches = analyzer.identify_batches(time_window_seconds=60)
    
    print("\n[3/5] Estimating core counts...")
    cores = analyzer.estimate_core_counts()
    
    print("\n[4/5] Calculating efficiency...")
    efficiency = analyzer.calculate_efficiency()
    
    print("\n[5/5] Saving results...")
    analyzer.save_results(str(output_file))
    
    print("\n")
    analyzer.print_summary()
    
    return 0


if __name__ == '__main__':
    exit(main())
