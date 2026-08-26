# ManyDaughters replication materials: idiosyncrasy analysis

## Overview

The idiosyncrasy workflow reruns selected research-team (RT) analysis do-files on repeated 5% SOEP samples. A job is one RT ID, hypothesis, and iteration combination.

The public replication package includes the individual result CSVs, the combined Stata dataset, and the generated completeness outputs. The original per-job logs are omitted because they were produced from unredacted RT do-files and may contain names that cannot be published. Accordingly, `data/derived/5_idiosyncrasy_logs/` intentionally contains only `RedactionNote.txt`, which records this omission. Re-executing the jobs is optional and requires either authorized access to SOEP v37 or a compatible set of presampled data that is not distributed with this package.

Run all commands from the `DataPreprocessing` project root.

## Included outputs

```text
data/derived/5_idiosyncrasy_results/<rt_id>/
data/derived/5_idiosyncrasy_logs/RedactionNote.txt
data/derived/idiosyncrasy_results_combined.dta
data/derived/idiosyncrasy_completeness_report.html
data/derived/idiosyncrasy_completeness_missing_iteration_diagnostics.csv
data/derived/idiosyncrasy_completeness_manual_comments.csv
```

You can rebuild the combined dataset and completeness report from the supplied result files without rerunning the confidential-data analysis.

## Requirements for executing jobs

- Python packages from `requirements.txt`
- Stata; the original workflow used StataNow 19, while RT do-files are grouped by required Stata version
- authorized access to the omitted SOEP inputs, or access to compatible presampled raw and derived data
- sufficient temporary disk space

Install the Python dependencies with:

```powershell
python -m pip install -r requirements.txt
```

If Stata is not at the configured default location, set `STATA_EXECUTABLE` in the environment or a project-root `.env` file:

```text
STATA_EXECUTABLE=C:/Program Files/StataNow19/StataMP-64.exe
```

## Stage selected RT analyses

Candidate RT do-files are archived by Stata version and work status under:

```text
data/raw/dofiles_consistency_checked/dofiles_idiosyncrasy/
```

The runner does not search that archive recursively. To select work, copy the desired hypothesis do-files into the parent staging directory:

```text
data/raw/dofiles_consistency_checked/
```

The required setup files are already present in that staging directory:

```text
5%-idiosyncrasy_Setup.do
5%-idiosyncrasy_Setup_Presampled.do
```

Analysis filenames must end in `_h1.do`, `_h2.do`, `_h3.do`, or `_h4.do`. The six characters immediately before that suffix are interpreted as the RT ID. The runner executes every eligible `.do` file directly in the staging directory, except setup files and files beginning with `0_`.

Inspect the staging directory before a long run. Do not place ordinary 5%/95% analysis files or unrelated do-files there. Remove copied hypothesis files after the selected idiosyncrasy work is complete; the archived originals remain in `dofiles_idiosyncrasy/`.

The ordinary runners `02_run_RT_analysis_05.py` and `03_run_RT_analysis_95.py` follow a different convention: they operate directly on `dofiles_analysis/` and do not use this staging procedure.

## Run the analysis

```powershell
python code/04_prepare_idiosyncrasy_index_5.py
```

Interactive startup configures:

1. the data setup method;
2. the inclusive iteration range, normally `0..1000`;
3. whether completed CSV jobs should be skipped;
4. whether results missing a corresponding log should be rerun;
5. the worker count; and
6. the delay between job submissions.

Iteration 0 reproduces the 5% sample originally available to RTs. The idiosyncrasy completeness report evaluates iterations 1 through 1000.

Set a ceiling on parallel workers in PowerShell with:

```powershell
$env:MD_MAX_WORKERS = 4
python code/04_prepare_idiosyncrasy_index_5.py
```

### Data setup variants

`5%-idiosyncrasy_Setup.do` is the standard on-demand setup. It constructs an iteration-specific 5% sample from the full confidential SOEP v37 inputs. Those inputs are omitted from this public package.

`5%-idiosyncrasy_Setup_Presampled.do` copies iteration-specific raw and derived datasets from a network or custom location. The original project's default network paths are not publicly accessible. External users must supply their own compatible paths when prompted.

Although the runner labels the presampled setup as its interactive default, it is not usable from the replication package alone.

### Runtime and storage

Runtime varies substantially with RT code, iteration range, worker count, setup method, Stata version, and storage performance. On the original project laptop, one RT with four hypotheses and 1,001 iterations took about one week with one worker and no submission stagger.

Each concurrent worker can require up to approximately 10 GB of temporary space for SOEP subsamples. Allow at least `worker count × 10 GB`, plus space for results, logs, and filesystem overhead. Test a small iteration range before scheduling a full run.

## Execution isolation and outputs

Jobs use a dynamic queue. Each job receives isolated data and result-staging directories. Successful files are moved to:

```text
data/derived/5_idiosyncrasy_results/<rt_id>/
    idiosyncrasy_<timestamp>_<rt_id>_h<hypothesis>_results_iter<iteration>.csv

data/derived/5_idiosyncrasy_logs/<rt_id>/
    idiosyncrasy_<timestamp>_<rt_id>_h<hypothesis>_iter<iteration>.log

logs/5_idiosyncrasy_execution_logs/
    execution_log_<timestamp>.txt
```

Missing CSV output is treated as a hard failure. Some recoverable failures, such as a failed log move, are requeued. Review the final failure summary; process completion does not by itself prove that every scheduled job produced a result.

## Stop execution

- Press `Ctrl+C` once to request a graceful stop after active work completes.
- Press `Ctrl+C` again to force an immediate exit.
- From another terminal, use either emergency-stop command:

```powershell
python code/auxiliary/stop_04_idiosyncrasy.py
python code/04_prepare_idiosyncrasy_index_5.py --stop
```

The emergency stop searches for the runner, its workers, and related Stata processes. Check the operating system's process manager if Stata remains active.

## Combine supplied or newly generated results

Run:

```stata
do code/08_collect_RT_results_idiosyncrasy.do
```

The collector recursively reads result CSVs, derives provenance from their filenames, validates columns, standardizes `interpretation`, reports duplicate keys, and writes:

```text
data/derived/idiosyncrasy_results_combined.dta
```

The collector uses Windows shell commands and is Windows-oriented. With the full set of supplied CSVs, this is a long-running step.

## Check completeness

Generate the HTML report and flattened missing-iteration diagnostics with:

```powershell
python code/auxiliary/generate_idiosyncrasy_completeness_report.py
```

The default outputs are:

```text
data/derived/idiosyncrasy_completeness_report.html
data/derived/idiosyncrasy_completeness_missing_iteration_diagnostics.csv
```

The report evaluates iterations 1 through 1000 for hypotheses 1 through 4, scans individual result and log filenames, classifies likely reasons for missing iterations, and incorporates `idiosyncrasy_completeness_manual_comments.csv`. Optional path overrides are `--project-root`, `--output`, and `--missing-iterations-export`.

## Optional monitoring tools

All non-numbered Python utilities are located in `code/auxiliary/`.

Command-line progress summary:

```powershell
python code/auxiliary/analyze_idiosyncrasy_progress.py
python code/auxiliary/analyze_idiosyncrasy_progress.py --verbose
python code/auxiliary/analyze_idiosyncrasy_progress.py --incomplete-only
```

Live dashboard:

```powershell
python code/auxiliary/analyze_idiosyncrasy_progress_dashboard.py
```

Then open `http://localhost:8051`.

Estimate parallelization efficiency from execution logs:

```powershell
python code/auxiliary/estimate_idiosyncrasy_parallelization_efficiency.py
```

Inspect unexpected file extensions in result and log trees:

```powershell
python code/auxiliary/cleanup_results_directories.py --dry-run
```

The cleanup tool can delete files after an explicit confirmation. Review its targets and stop all runner and Stata processes before using deletion mode.

## Troubleshooting

### No jobs are found

Confirm that selected analysis do-files are directly in `data/raw/dofiles_consistency_checked/`, end in `_h1.do` through `_h4.do`, and are not setup files.

### Expected result file not found

Inspect the corresponding Stata log and terminal output. Common causes are an RT do-file error, missing licensed inputs, an invalid setup path, or output that does not follow the expected CSV filename convention.

### A rerun skips work unexpectedly

By default, an existing matching CSV counts as complete. Disable skip mode to rerun all selected jobs in the chosen range. Enable the missing-log option only when a CSV without a log should be rerun.

### Network or directory visibility errors

Use fewer workers and retain or increase the submission stagger. Presampled jobs copy large files, and shared storage can become a bottleneck.

### Temporary data remain after interruption

Stop the runner, worker, and Stata processes before cleanup. The runner removes known job data on normal completion and scans for orphaned temporary files at startup.
