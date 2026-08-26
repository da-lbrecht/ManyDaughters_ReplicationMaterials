#!/usr/bin/env python3
"""
Live-updating Idiosyncrasy Analysis Progress Dashboard

Displays idiosyncrasy analysis progress in a web browser with automatic refresh.
Uses Plotly Dash to create an interactive dashboard that updates every 30 seconds.

The dashboard shows:
- Overall completion statistics
- Per-hypothesis progress bars
- Heatmap of rt_id completion by hypothesis
- Missing RT_IDs summary

Usage:
    python code/auxiliary/analyze_idiosyncrasy_progress_dashboard.py
    
Then open http://localhost:8051 in your browser.
Press Ctrl+C in terminal to stop the server.
"""
import os
import re
import plotly.graph_objects as go
from collections import defaultdict
from datetime import datetime
import statistics
from pathlib import Path
from typing import Dict, List

try:
    import dash
    from dash import dcc, html, dash_table
    from dash.dependencies import Input, Output
    import plotly.express as px
except ImportError:
    print("ERROR: Required packages not installed.")
    print("Please install: pip install dash plotly")
    exit(1)

# Configuration
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.abspath(os.path.join(SCRIPT_DIR, "..", ".."))
RESULTS_DIR = os.path.join(PROJECT_ROOT, "data", "derived", "5_idiosyncrasy_results")

# Expected total number of RT_IDs in the project
TOTAL_EXPECTED_RT_IDS = 120

# Pattern to extract rt_id, hypothesis, and iteration from filenames
PATTERN = re.compile(r'.*_([a-zA-Z0-9]+)_h([1-4])_results_iter(\d+)\.csv')

# RT_IDs to exclude from heatmap view
EXCLUDED_RT_IDS = {'015w1n', '016nbt', '01f8uy', '01iw87', '01vtfa', '01xrXX', '01ygl2'}

# Auto-refresh interval (milliseconds)
REFRESH_INTERVAL = 30000  # 30 seconds

# Directory containing log files for execution time analysis
LOG_DIR = os.path.join(PROJECT_ROOT, "data", "derived", "5_idiosyncrasy_logs")


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
        
        if not self.log_dir.exists():
            return logs
        
        for rt_id_dir in self.log_dir.iterdir():
            if not rt_id_dir.is_dir():
                continue
            
            for log_file in rt_id_dir.glob('*.log'):
                match = log_pattern.match(log_file.name)
                if not match:
                    continue
                
                date_str = match.group(1)
                time_str = match.group(2)
                
                try:
                    start_time = datetime.strptime(f"{date_str}_{time_str}", "%Y%m%d_%H%M%S")
                except ValueError:
                    continue
                
                stat = log_file.stat()
                mtime = datetime.fromtimestamp(stat.st_mtime)
                
                elapsed_seconds = (mtime - start_time).total_seconds()
                
                if elapsed_seconds <= 0 or elapsed_seconds > 86400:
                    continue
                
                logs.append({
                    'start_time': start_time,
                    'end_time': mtime,
                    'elapsed_seconds': elapsed_seconds,
                })
        
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
        
        self.batches = batches
        return batches
    
    def _create_batch_dict(self, batch_logs: List[Dict]) -> Dict:
        """Create a batch dictionary from a list of logs."""
        start_times = [log['start_time'] for log in batch_logs]
        end_times = [log['end_time'] for log in batch_logs]
        elapsed_times = [log['elapsed_seconds'] for log in batch_logs]
        
        batch_start = min(start_times)
        batch_end = max(end_times)
        batch_duration = (batch_end - batch_start).total_seconds()
        
        return {
            'num_jobs': len(batch_logs),
            'batch_duration_seconds': batch_duration,
            'job_durations_seconds': elapsed_times,
            'mean_job_duration': statistics.mean(elapsed_times),
        }
    
    def estimate_core_counts(self) -> None:
        """Estimate the number of cores used for each batch."""
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
            
            batch['estimated_cores'] = estimated_cores
    
    def calculate_efficiency(self) -> Dict[int, float]:
        """Calculate parallelization efficiency for each batch."""
        if not self.batches:
            return {}
        
        efficiency_by_batch = {}
        
        for i, batch in enumerate(self.batches):
            num_jobs = batch['num_jobs']
            num_cores = batch['estimated_cores']
            mean_duration = batch['mean_job_duration']
            batch_duration = batch['batch_duration_seconds']
            
            serial_time = num_jobs * mean_duration
            
            if batch_duration > 0:
                achieved_speedup = serial_time / batch_duration
            else:
                achieved_speedup = 1.0
            
            if num_cores > 0:
                efficiency = achieved_speedup / num_cores
            else:
                efficiency = 0.0
            
            efficiency = min(efficiency, 1.0)
            efficiency = max(efficiency, 0.0)
            
            self.efficiency_estimates[i] = efficiency
            batch['efficiency'] = efficiency
            efficiency_by_batch[i] = efficiency
        
        return efficiency_by_batch
    
    def get_overall_efficiency(self) -> float:
        """Return the mean efficiency across all batches."""
        if not self.efficiency_estimates:
            return 0.065  # Default fallback
        
        efficiencies = list(self.efficiency_estimates.values())
        return statistics.mean(efficiencies) if efficiencies else 0.065

    def get_efficiency_by_core_count(self, core_counts: List[int] = None) -> Dict[int, float]:
        """Calculate theoretical efficiency for different core counts.
        
        For each core count, calculates the average efficiency across all batches
        as if the system had that many cores available.
        
        Args:
            core_counts: List of core counts to evaluate. Defaults to [1, 8, 16, 32, 74, 128]
            
        Returns:
            Dictionary mapping core_count -> mean_efficiency
        """
        if core_counts is None:
            core_counts = [1, 8, 16, 32, 74, 128]
        
        if not self.batches:
            return {cores: 0.065 for cores in core_counts}
        
        efficiency_by_cores = {}
        
        for cores in core_counts:
            efficiencies = []
            for batch in self.batches:
                serial_time = batch['num_jobs'] * batch['mean_job_duration']
                batch_duration = batch['batch_duration_seconds']
                
                if batch_duration > 0:
                    achieved_speedup = serial_time / batch_duration
                    efficiency = achieved_speedup / cores
                    efficiency = min(efficiency, 1.0)  # Cap at 100%
                    efficiency = max(efficiency, 0.0)  # Floor at 0%
                else:
                    efficiency = 0.0
                
                efficiencies.append(efficiency)
            
            if efficiencies:
                efficiency_by_cores[cores] = statistics.mean(efficiencies)
            else:
                efficiency_by_cores[cores] = 0.0
        
        return efficiency_by_cores


def parse_stata_log(filepath):
    """Extract execution time from a single Stata log file.
    
    For batch mode logs without timestamps, estimates execution time from
    file modification time minus creation time.
    """
    try:
        # Get log file creation time (when Stata started)
        log_ctime = os.path.getctime(filepath)
        
        # Get log file modification time (when Stata finished)
        log_mtime = os.path.getmtime(filepath)
        
        # Calculate elapsed time in minutes
        elapsed_sec = log_mtime - log_ctime
        
        # Sanity check: elapsed time should be positive and reasonable (< 24 hours)
        if elapsed_sec <= 0 or elapsed_sec > 86400:
            return None
        
        return elapsed_sec / 60.0  # Convert to minutes
    except:
        pass
    return None


def calculate_mean_execution_time():
    """Calculate mean execution time from log files."""
    times = []
    
    if not os.path.exists(LOG_DIR):
        return 0.0
    
    for root, dirs, files in os.walk(LOG_DIR):
        for fname in files:
            if fname.endswith('.log') and not fname.endswith('_Setup.log'):
                fpath = os.path.join(root, fname)
                elapsed = parse_stata_log(fpath)
                if elapsed:
                    times.append(elapsed)
    
    if times:
        return sum(times) / len(times)
    else:
        return 0.0


def calculate_parallelization_efficiency():
    """Calculate parallelization efficiency from log files using integrated analyzer."""
    try:
        analyzer = IdiosyncrasyLogAnalyzer(LOG_DIR)
        logs = analyzer.discover_logs()
        
        if not logs or len(logs) < 10:
            # Not enough data to estimate, use default
            return 0.065
        
        analyzer.identify_batches(time_window_seconds=60)
        analyzer.estimate_core_counts()
        analyzer.calculate_efficiency()
        
        return analyzer.get_overall_efficiency()
    except Exception as e:
        print(f"Warning: Could not calculate efficiency from logs: {e}")
        return 0.065  # Default fallback


def calculate_parallelization_efficiency_by_cores():
    """Calculate parallelization efficiency by core count from log files.
    
    Returns:
        Dictionary mapping core_count -> mean_efficiency for different core counts.
        If insufficient data, returns default values for each core count.
    """
    default_by_cores = {1: 1.0, 8: 0.065, 16: 0.065, 32: 0.065, 74: 0.065, 128: 0.065}
    
    try:
        analyzer = IdiosyncrasyLogAnalyzer(LOG_DIR)
        logs = analyzer.discover_logs()
        
        if not logs or len(logs) < 10:
            # Not enough data to estimate, use defaults
            return default_by_cores
        
        analyzer.identify_batches(time_window_seconds=60)
        analyzer.estimate_core_counts()
        analyzer.calculate_efficiency()
        
        return analyzer.get_efficiency_by_core_count()
    except Exception as e:
        print(f"Warning: Could not calculate efficiency by cores from logs: {e}")
        return default_by_cores


def create_execution_time_table(mean_time, efficiency_by_cores=None):
    """Create execution time estimates table.
    
    Uses empirical efficiency factor calculated from log file analysis.
    This accounts for I/O contention, Stata licensing, system overhead, etc.
    
    Args:
        mean_time: Mean execution time per job in minutes
        efficiency_by_cores: Dict mapping core_count -> efficiency. If None, calculated from logs.
    """
    TOTAL_RT_IDS = 120
    HYPOTHESES_PER_RT = 4
    TOTAL_JOBS_PER_ITER = TOTAL_RT_IDS * HYPOTHESES_PER_RT
    
    # Get efficiency for different core counts
    if efficiency_by_cores is None:
        efficiency_by_cores = calculate_parallelization_efficiency_by_cores()
    
    core_counts = sorted(efficiency_by_cores.keys())
    iteration_counts = [1, 10, 100, 1000]
    
    table_data = []
    
    for n_iter in iteration_counts:
        total_jobs = TOTAL_JOBS_PER_ITER * n_iter
        serial_time_min = total_jobs * mean_time
        
        row = {
            'Iterations': n_iter,
            'Total Jobs': total_jobs
        }
        
        for cores in core_counts:
            if cores == 1:
                parallel_time_min = serial_time_min
            else:
                # Use empirical efficiency for this core count
                efficiency = efficiency_by_cores[cores]
                # actual_speedup = cores * efficiency (e.g., 8 cores at 6.5% efficiency = 0.52x speedup)
                actual_speedup = cores * efficiency
                if actual_speedup > 0:
                    parallel_time_min = serial_time_min / actual_speedup
                else:
                    parallel_time_min = serial_time_min
            
            # Format output
            if parallel_time_min < 60:
                time_str = f"{parallel_time_min:.1f} min"
            elif parallel_time_min < 1440:
                hours = parallel_time_min / 60
                time_str = f"{hours:.1f} hrs"
            else:
                days = parallel_time_min / 1440
                time_str = f"{days:.1f} days"
            
            row[f'{cores} cores'] = time_str
        
        table_data.append(row)
    
    return dash_table.DataTable(
        data=table_data,
        columns=[{'name': col, 'id': col} for col in table_data[0].keys()],
        style_cell={'textAlign': 'center', 'padding': '10px'},
        style_header={'backgroundColor': '#3498db', 'color': 'white', 'fontWeight': 'bold'},
        style_data_conditional=[
            {'if': {'column_id': 'Iterations'}, 'fontWeight': 'bold'},
            {'if': {'row_index': 'odd'}, 'backgroundColor': '#f9f9f9'}
        ]
    )


def collect_results():
    """Scan results directory and collect completion data for CSV files."""
    rt_data = defaultdict(lambda: {'h1': set(), 'h2': set(), 'h3': set(), 'h4': set()})
    
    if not os.path.exists(RESULTS_DIR):
        return rt_data
    
    # First, initialize all 120 RT_IDs with empty sets (to show 0% completion)
    for rt_id_dir in os.listdir(RESULTS_DIR):
        dir_path = os.path.join(RESULTS_DIR, rt_id_dir)
        if os.path.isdir(dir_path):
            rt_data[rt_id_dir]  # Initialize with default empty sets
    
    # Then, collect actual completion data
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


def collect_logs():
    """Scan log directory and collect log file completion data."""
    rt_data = defaultdict(lambda: {'h1': set(), 'h2': set(), 'h3': set(), 'h4': set()})
    
    if not os.path.exists(LOG_DIR):
        return rt_data
    
    # Log file pattern: idiosyncrasy_YYYYMMDD_HHMMSS_RTID_hN_iterN.log
    log_pattern = re.compile(r'idiosyncrasy_\d{8}_\d{6}_([a-zA-Z0-9]+)_h([1-4])_iter(\d+)\.log')
    
    # Scan all log files
    for rt_id_dir in os.listdir(LOG_DIR):
        dir_path = os.path.join(LOG_DIR, rt_id_dir)
        if not os.path.isdir(dir_path):
            continue
        
        # Initialize rt_id entry if not seen before
        if rt_id_dir not in rt_data:
            rt_data[rt_id_dir]  # Initialize with default empty sets
        
        for filename in os.listdir(dir_path):
            match = log_pattern.match(filename)
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
        return 0, 2, 1000  # Default: 0-2 (3 iterations)
    
    min_iter = min(all_iters)
    max_iter = max(all_iters)
    total_expected = max_iter - min_iter + 1
    
    return min_iter, max_iter, total_expected


def calculate_statistics(rt_data, total_expected):
    """Calculate summary statistics."""
    stats = {
        'timestamp': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
        'total_rt_ids': TOTAL_EXPECTED_RT_IDS,
        'rt_ids_with_data': len(rt_data),
        'rt_ids_missing': TOTAL_EXPECTED_RT_IDS - len(rt_data),
        'complete_rt_ids': 0,
        'incomplete_rt_ids': 0,
        'total_expected_iters': total_expected,
        'max_iterations_completed': 0,
        'total_executions': 0,
        'hypotheses': {}
    }
    
    # Count complete RT_IDs and track max iterations
    for rt_id in rt_data:
        all_complete = all(
            len(rt_data[rt_id][h]) == total_expected 
            for h in ['h1', 'h2', 'h3', 'h4']
        )
        if all_complete:
            stats['complete_rt_ids'] += 1
        else:
            stats['incomplete_rt_ids'] += 1
        
        # Track max iterations completed across all hypotheses
        max_iters = max(len(rt_data[rt_id][h]) for h in ['h1', 'h2', 'h3', 'h4'])
        stats['max_iterations_completed'] = max(stats['max_iterations_completed'], max_iters)
    
    # Calculate total executions (sum of all iterations across all rt_ids and hypotheses)
    for rt_id in rt_data:
        for hyp in ['h1', 'h2', 'h3', 'h4']:
            stats['total_executions'] += len(rt_data[rt_id][hyp])
    
    # Initialize per-hypothesis max and min iterations tracker
    stats['max_iterations_per_hyp'] = {'h1': 0, 'h2': 0, 'h3': 0, 'h4': 0}
    stats['min_iterations_per_hyp'] = {'h1': 0, 'h2': 0, 'h3': 0, 'h4': 0}
    
    # Per-hypothesis statistics
    for hyp in ['h1', 'h2', 'h3', 'h4']:
        counts = [len(rt_data[rt_id][hyp]) for rt_id in rt_data]
        
        if counts:
            min_count = min(counts)
            max_count = max(counts)
            mean_count = sum(counts) / len(counts)
            complete_count = sum(1 for c in counts if c == total_expected)
            total_iterations = sum(counts)
            expected_total = TOTAL_EXPECTED_RT_IDS * total_expected
            
            stats['hypotheses'][hyp] = {
                'min': min_count,
                'max': max_count,
                'mean': mean_count,
                'complete': complete_count,
                'incomplete': len(counts) - complete_count,
                'total_rt_ids': len(counts),
                'total_iterations': total_iterations,
                'expected_total': expected_total
            }
            
            stats['max_iterations_per_hyp'][hyp] = max_count
            stats['min_iterations_per_hyp'][hyp] = min_count
        else:
            stats['hypotheses'][hyp] = {
                'min': 0, 'max': 0, 'mean': 0.0,
                'complete': 0, 'incomplete': 0, 'total_rt_ids': 0,
                'total_iterations': 0, 'expected_total': 0
            }
    
    return stats


def create_heatmap(rt_data, total_expected, title_suffix=""):
    """Create completion heatmap for rt_ids x hypotheses.
    
    Args:
        rt_data: Dictionary of rt_id -> hypothesis -> set of iterations
        total_expected: Expected number of iterations
        title_suffix: Suffix to add to heatmap title (e.g., "Results" or "Logs")
    """
    # Filter out excluded RT_IDs
    rt_ids_filtered = [rt_id for rt_id in sorted(rt_data.keys()) if rt_id not in EXCLUDED_RT_IDS]
    
    # Build matrix: rows=rt_ids, cols=hypotheses
    z_data = []
    hover_text = []
    
    for rt_id in rt_ids_filtered:
        row = []
        hover_row = []
        for hyp in ['h1', 'h2', 'h3', 'h4']:
            completed = len(rt_data[rt_id][hyp])
            percentage = (completed / total_expected * 100) if total_expected > 0 else 0
            row.append(percentage)
            hover_row.append(f'RT: {rt_id}<br>Hyp: {hyp}<br>Completed: {completed}/{total_expected}<br>{percentage:.1f}%')
        z_data.append(row)
        hover_text.append(hover_row)
    
    excluded_count = len(EXCLUDED_RT_IDS)
    
    fig = go.Figure(data=go.Heatmap(
        z=z_data,
        x=['H1', 'H2', 'H3', 'H4'],
        y=rt_ids_filtered,
        colorscale='RdYlGn',
        text=hover_text,
        hovertemplate='%{text}<extra></extra>',
        colorbar=dict(title="% Complete")
    ))
    
    fig.update_layout(
        title=f'RT-specific {title_suffix} completion heatmap - {len(rt_ids_filtered)}/{TOTAL_EXPECTED_RT_IDS} RTs (% of iterations complete)',
        xaxis_title='Hypothesis',
        yaxis_title='RT_ID',
        height=max(800, len(rt_ids_filtered) * 12),
        yaxis=dict(
            autorange='reversed',
            tickmode='linear',
            tick0=0,
            dtick=1
        )
    )
    
    return fig


def create_progress_bars(stats):
    """Create progress bars for each hypothesis showing total iterations completed."""
    hypotheses = ['h1', 'h2', 'h3', 'h4']
    
    fig = go.Figure()
    
    for hyp in hypotheses:
        if hyp in stats['hypotheses']:
            hyp_stats = stats['hypotheses'][hyp]
            completed = hyp_stats['total_iterations']
            expected = hyp_stats['expected_total']
            percentage = (completed / expected * 100) if expected > 0 else 0
            
            fig.add_trace(go.Bar(
                name=hyp.upper(),
                x=[completed],
                y=[hyp.upper()],
                orientation='h',
                text=f"{completed:,}/{expected:,} ({percentage:.1f}%)",
                textposition='auto',
                marker_color='green' if percentage >= 100 else 'orange' if percentage > 50 else 'red'
            ))
    
    fig.update_layout(
        title='Overall progress by hypothesis (out of total number of jobs 120 RTs x 1000 iterations)',
        xaxis_title='Iterations completed',
        yaxis_title='Hypothesis',
        showlegend=False,
        height=300,
        barmode='group'
    )
    
    return fig


# Initialize Dash app
app = dash.Dash(__name__)

app.layout = html.Div([
    html.H1('Idiosyncrasy Analysis Progress Dashboard', 
            style={'textAlign': 'center', 'color': '#2c3e50'}),
    
    html.Div(id='last-updated', 
             style={'textAlign': 'center', 'color': '#7f8c8d', 'marginBottom': '20px'}),
    
    # Summary statistics cards
    html.Div([
        html.Div([
            html.H3('# of RTs with analysed hyothesis'),
            html.H2(id='total-rt-ids', style={'color': '#3498db'})
        ], className='stat-card', style={'flex': '1', 'margin': '10px', 'padding': '20px', 
                                          'backgroundColor': '#ecf0f1', 'borderRadius': '5px'}),

        html.Div([
            html.H3('Min/Max completed iterations'),
            html.H2(id='max-iterations', style={'color': '#9b59b6'})
        ], className='stat-card', style={'flex': '1', 'margin': '10px', 'padding': '20px',
                                          'backgroundColor': '#ecf0f1', 'borderRadius': '5px'}),

        html.Div([
            html.H3('# of generated result csv-files'),
            html.H2(id='total-csv-results', style={'color': '#2ecc71'})
        ], className='stat-card', style={'flex': '1', 'margin': '10px', 'padding': '20px',
                                          'backgroundColor': '#ecf0f1', 'borderRadius': '5px'}),

        html.Div([
            html.H3('# of created log-files'),
            html.H2(id='total-logfiles', style={'color': '#f39c12'})
        ], className='stat-card', style={'flex': '1', 'margin': '10px', 'padding': '20px',
                                          'backgroundColor': '#ecf0f1', 'borderRadius': '5px'}),

        html.Div([
            html.H3('Parallelization Efficiency'),
            html.Div(id='parallelization-efficiency', style={'color': '#e74c3c'})
        ], className='stat-card', style={'flex': '1', 'margin': '10px', 'padding': '20px',
                                          'backgroundColor': '#ecf0f1', 'borderRadius': '5px'}),
    ], style={'display': 'flex', 'flexWrap': 'wrap', 'justifyContent': 'space-around'}),
    
    # Progress bars
    dcc.Graph(id='progress-bars'),
    
    # CSV Results Heatmap with target selector
    html.Div([
        html.H2('Results Completion Heatmap (based on csv-files)', 
                style={'textAlign': 'center', 'marginTop': '40px', 'marginBottom': '10px'}),
        html.Div([
            html.Label('Target Iterations: ', style={'fontSize': '16px', 'marginRight': '10px', 'fontWeight': 'bold'}),
            dcc.Input(
                id='heatmap-target-csv',
                type='number',
                value=1000,
                min=1,
                max=10000,
                step=1,
                placeholder='Enter target...',
                style={'width': '120px', 'fontSize': '16px', 'padding': '5px', 'border': '2px solid #3498db'}
            ),
            html.Button('Auto (Max)', id='auto-target-btn-csv', n_clicks=0,
                       style={'marginLeft': '10px', 'padding': '5px 15px', 'fontSize': '14px',
                              'backgroundColor': '#3498db', 'color': 'white', 'border': 'none',
                              'borderRadius': '3px', 'cursor': 'pointer'}),
            html.Span(id='current-max-csv', style={'marginLeft': '15px', 'fontSize': '14px', 'color': '#7f8c8d'})
        ], style={'textAlign': 'center', 'marginBottom': '10px'}),
        html.Div('Edit the value and click Auto button or wait for auto-refresh (30s)', 
                 style={'textAlign': 'center', 'fontSize': '12px', 'color': '#95a5a6', 'fontStyle': 'italic', 'marginBottom': '20px'})
    ]),
    dcc.Graph(id='heatmap-csv'),
    
    # Log Files Heatmap with target selector
    html.Div([
        html.H2('Execution Completion Heatmap (based on log-files)', 
                style={'textAlign': 'center', 'marginTop': '40px', 'marginBottom': '10px'}),
        html.Div([
            html.Label('Target Iterations: ', style={'fontSize': '16px', 'marginRight': '10px', 'fontWeight': 'bold'}),
            dcc.Input(
                id='heatmap-target-log',
                type='number',
                value=1000,
                min=1,
                max=10000,
                step=1,
                placeholder='Enter target...',
                style={'width': '120px', 'fontSize': '16px', 'padding': '5px', 'border': '2px solid #27ae60'}
            ),
            html.Button('Auto (Max)', id='auto-target-btn-log', n_clicks=0,
                       style={'marginLeft': '10px', 'padding': '5px 15px', 'fontSize': '14px',
                              'backgroundColor': '#27ae60', 'color': 'white', 'border': 'none',
                              'borderRadius': '3px', 'cursor': 'pointer'}),
            html.Span(id='current-max-log', style={'marginLeft': '15px', 'fontSize': '14px', 'color': '#7f8c8d'})
        ], style={'textAlign': 'center', 'marginBottom': '10px'}),
        html.Div('Edit the value and click Auto button or wait for auto-refresh (30s)', 
                 style={'textAlign': 'center', 'fontSize': '12px', 'color': '#95a5a6', 'fontStyle': 'italic', 'marginBottom': '20px'})
    ]),
    dcc.Graph(id='heatmap-log'),
    
    # Hypothesis statistics table
    html.H2('Hypothesis-specific statistics', style={'textAlign': 'center', 'marginTop': '40px'}),
    html.Div(id='hypothesis-table'),
    
    # Execution time estimates
    html.H2('Execution time estimates', style={'textAlign': 'center', 'marginTop': '40px'}),
    html.Div(id='execution-time-info', style={'textAlign': 'center', 'fontSize': '14px', 'color': '#7f8c8d', 'marginBottom': '10px'}),
    html.Div(id='execution-time-table'),
    html.Div([
        html.P('NOTE: These estimates include empirical parallelization efficiency based on observed completion rates.', 
               style={'fontSize': '12px', 'color': '#95a5a6', 'fontStyle': 'italic', 'marginTop': '10px', 'textAlign': 'center'}),
        html.P('Efficiency is automatically updated from ongoing execution. Actual times may vary depending on I/O patterns, system load, and resource availability.',
               style={'fontSize': '12px', 'color': '#95a5a6', 'fontStyle': 'italic', 'textAlign': 'center'})
    ]),
    
    # Auto-refresh component
    dcc.Interval(
        id='interval-component',
        interval=REFRESH_INTERVAL,
        n_intervals=0
    )
], style={'fontFamily': 'Arial, sans-serif', 'padding': '20px'})


# Callback to update heatmap target with auto button
@app.callback(
    Output('heatmap-target-csv', 'value'),
    [Input('auto-target-btn-csv', 'n_clicks')],
    prevent_initial_call=True
)
def auto_set_target_csv(n_clicks):
    """Set CSV heatmap target to maximum observed iterations."""
    rt_data = collect_results()
    _, max_iter, _ = determine_expected_range(rt_data)
    return max_iter


@app.callback(
    Output('heatmap-target-log', 'value'),
    [Input('auto-target-btn-log', 'n_clicks')],
    prevent_initial_call=True
)
def auto_set_target_log(n_clicks):
    """Set log heatmap target to maximum observed iterations."""
    rt_data = collect_logs()
    _, max_iter, _ = determine_expected_range(rt_data)
    return max_iter


@app.callback(
    [Output('last-updated', 'children'),
     Output('total-rt-ids', 'children'),
     Output('max-iterations', 'children'),
     Output('total-csv-results', 'children'),
     Output('total-logfiles', 'children'),
     Output('parallelization-efficiency', 'children'),
     Output('progress-bars', 'figure'),
     Output('heatmap-csv', 'figure'),
     Output('heatmap-log', 'figure'),
     Output('hypothesis-table', 'children'),
     Output('current-max-csv', 'children'),
     Output('current-max-log', 'children'),
     Output('execution-time-info', 'children'),
     Output('execution-time-table', 'children')],
    [Input('interval-component', 'n_intervals'),
     Input('heatmap-target-csv', 'value'),
     Input('heatmap-target-log', 'value')],
    prevent_initial_call=False
)
def update_dashboard(n_intervals, heatmap_target_csv, heatmap_target_log):
    """Update all dashboard components."""
    # Collect data for both CSV results and logs
    rt_data_csv = collect_results()
    rt_data_log = collect_logs()
    
    if not rt_data_csv and not rt_data_log:
        empty_fig = go.Figure()
        empty_fig.update_layout(title="No data available yet")
        return (
            f"Last updated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')} | No data available",
            "0", "0 / 0", "0", "0", "N/A",
            empty_fig, empty_fig, empty_fig,
            html.Div("No data available", style={'textAlign': 'center', 'padding': '20px'}),
            "", "", "", html.Div()
        )
    
    # Determine expected range from CSV data (actual results)
    _, max_iter_actual_csv, total_expected_csv = determine_expected_range(rt_data_csv)
    # Determine expected range from log data (executed jobs)
    _, max_iter_actual_log, total_expected_log = determine_expected_range(rt_data_log)
    
    # Use user-specified targets or auto-detected for HEATMAP DISPLAY
    total_expected_for_csv = heatmap_target_csv if (heatmap_target_csv and heatmap_target_csv > 0) else total_expected_csv
    total_expected_for_log = heatmap_target_log if (heatmap_target_log and heatmap_target_log > 0) else total_expected_log
    
    # Calculate statistics based on ACTUAL determined range (for accurate completion counts)
    stats = calculate_statistics(rt_data_csv, total_expected_csv)
    
    # Count CSV result files
    csv_count = 0
    if os.path.exists(RESULTS_DIR):
        for root, dirs, files in os.walk(RESULTS_DIR):
            csv_count += sum(1 for f in files if f.endswith('.csv'))
    
    # Count log files
    log_count = 0
    if os.path.exists(LOG_DIR):
        for root, dirs, files in os.walk(LOG_DIR):
            log_count += sum(1 for f in files if f.endswith('.log') and not f.endswith('_Setup.log'))
    
    # Create visualizations
    progress_bars_fig = create_progress_bars(stats)
    heatmap_csv_fig = create_heatmap(rt_data_csv, total_expected_for_csv, "Results (csv-files)")
    heatmap_log_fig = create_heatmap(rt_data_log, total_expected_for_log, "log-file based")
    
    # Create hypothesis statistics table
    hyp_table_data = []
    for hyp in ['h1', 'h2', 'h3', 'h4']:
        if hyp in stats['hypotheses']:
            h = stats['hypotheses'][hyp]
            hyp_table_data.append({
                'Hypothesis': hyp.upper(),
                'RT_IDs Analyzed': f"{h['total_rt_ids']}/{TOTAL_EXPECTED_RT_IDS}",
                'Min Iterations': h['min'],
                'Max Iterations': h['max'],
                'Mean Iterations': f"{h['mean']:.1f}",
                'Complete RTs': f"{h['complete']}/{TOTAL_EXPECTED_RT_IDS} ({100*h['complete']/TOTAL_EXPECTED_RT_IDS:.1f}%)",
                'Incomplete RTs': h['incomplete']
            })
    
    hyp_table = dash_table.DataTable(
        data=hyp_table_data,
        columns=[{'name': col, 'id': col} for col in hyp_table_data[0].keys()] if hyp_table_data else [],
        style_cell={'textAlign': 'center', 'padding': '10px'},
        style_header={'backgroundColor': '#3498db', 'color': 'white', 'fontWeight': 'bold'},
        style_data_conditional=[
            {'if': {'row_index': 'odd'}, 'backgroundColor': '#f9f9f9'}
        ]
    )
    
    # Calculate parallelization efficiency by core count
    efficiency_by_cores = calculate_parallelization_efficiency_by_cores()
    
    # Format efficiency display with table of different core counts
    efficiency_rows = []
    for cores in sorted(efficiency_by_cores.keys()):
        eff = efficiency_by_cores[cores]
        efficiency_rows.append(
            html.Div(
                f"{cores} cores: {eff*100:.1f}%",
                style={'fontSize': '14px', 'marginBottom': '4px', 'fontFamily': 'monospace'}
            )
        )
    
    efficiency_display = html.Div(efficiency_rows, style={'textAlign': 'left'})
    
    # Calculate mean execution time and create time estimates
    mean_time = calculate_mean_execution_time()
    if mean_time > 0:
        exec_time_info = f"Based on {log_count} completed log files (mean: {mean_time:.2f} minutes per job)"
        exec_time_table = create_execution_time_table(mean_time, efficiency_by_cores)
    else:
        exec_time_info = "No execution time data available yet"
        exec_time_table = html.Div("Not enough data to estimate execution times", 
                                   style={'textAlign': 'center', 'padding': '20px', 'color': '#95a5a6'})
    
    # Format min/max iterations display per hypothesis
    return (
        f"Last updated: {stats['timestamp']} | Next refresh in 30 seconds",
        f"{stats['rt_ids_with_data']}/{stats['total_rt_ids']}",
        html.Div([
            html.Div([
                html.Span('H1: ', style={'fontWeight': 'bold'}),
                html.Span(f"min={stats['min_iterations_per_hyp']['h1']}, max={stats['max_iterations_per_hyp']['h1']}")
            ], style={'fontSize': '16px', 'marginBottom': '5px'}),
            html.Div([
                html.Span('H2: ', style={'fontWeight': 'bold'}),
                html.Span(f"min={stats['min_iterations_per_hyp']['h2']}, max={stats['max_iterations_per_hyp']['h2']}")
            ], style={'fontSize': '16px', 'marginBottom': '5px'}),
            html.Div([
                html.Span('H3: ', style={'fontWeight': 'bold'}),
                html.Span(f"min={stats['min_iterations_per_hyp']['h3']}, max={stats['max_iterations_per_hyp']['h3']}")
            ], style={'fontSize': '16px', 'marginBottom': '5px'}),
            html.Div([
                html.Span('H4: ', style={'fontWeight': 'bold'}),
                html.Span(f"min={stats['min_iterations_per_hyp']['h4']}, max={stats['max_iterations_per_hyp']['h4']}")
            ], style={'fontSize': '16px'})
        ]),
        str(csv_count),
        str(log_count),
        efficiency_display,
        progress_bars_fig,
        heatmap_csv_fig,
        heatmap_log_fig,
        hyp_table,
        f"(Current max in CSV data: {max_iter_actual_csv})",
        f"(Current max in log data: {max_iter_actual_log})",
        exec_time_info,
        exec_time_table
    )


if __name__ == '__main__':
    print("\n" + "="*60)
    print("IDIOSYNCRASY ANALYSIS PROGRESS DASHBOARD")
    print("="*60)
    print("\nStarting dashboard server...")
    print("Open your browser and navigate to: http://localhost:8051")
    print("\nDashboard will auto-refresh every 30 seconds")
    print("Press Ctrl+C to stop the server")
    print("="*60 + "\n")
    
    app.run(debug=False, host='localhost', port=8051)
