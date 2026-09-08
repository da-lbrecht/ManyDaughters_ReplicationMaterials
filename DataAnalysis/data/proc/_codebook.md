# Codebook — #ManyDaughters processed data

This codebook documents the processed data frames exported to `data/proc/` — each as `<name>.rds` (faithful column types and variable labels) and `<name>.csv` (portable, human-readable), alongside the complete `_env.RData` image. Variable labels are those assigned in `code/01_data_processing.Rmd`. This file is regenerated automatically by the data-processing pipeline.

## Data frames

- [`df.blfs`](#dfblfs) — 1016 rows × 4 variables
- [`df.cons`](#dfcons) — 127 rows × 13 variables
- [`df.evls`](#dfevls) — 5080 rows × 7 variables
- [`df.flow`](#dfflow) — 1 row × 4 variables
- [`df.proc`](#dfproc) — 127 rows × 16 variables
- [`df.rslts`](#dfrslts) — 480 rows × 22 variables
- [`df.srvy`](#dfsrvy) — 230 rows × 19 variables

## `df.blfs`

Belief elicitations — long format, one row per (team × hypothesis × stage): Stage-1 priors and Stage-2 posteriors.

*1016 rows × 4 variables*

| Variable | Label | Type | Values / range | Missing |
|---|---|---|---|---|
| `tid` | Research Team ID | character | 127 unique values | 0 |
| `hyp` | Hypothesis ID | factor | h1; h2; h3; h4 | 0 |
| `stage` | Project Stage | factor | s1; s2 | 0 |
| `belief` | Belief: Support (0-1) | numeric | 0.00–1.00 | 4 |

## `df.cons`

RT-PAP / code consistency audit (PAP §1.5 / §2.1) — one row per research team: overall verdict and per-type deviation counts.

*127 rows × 13 variables*

| Variable | Label | Type | Values / range | Missing |
|---|---|---|---|---|
| `tid` | Research Team ID | character | 127 unique values | 0 |
| `group` | Treatment Condition | factor | no data; data | 0 |
| `audit10` | In Random 10% Audit | factor | no; yes | 0 |
| `consistent` | PAP-to-Code Consistency | factor | inconsistent; consistent | 0 |
| `n_incons` | No. Inconsistencies | numeric | 0–15 | 0 |
| `inc_filter_less` | Inconsistency: Less Strict Filter | numeric | 0–4 | 0 |
| `inc_filter_more` | Inconsistency: More Strict Filter | numeric | 0–4 | 0 |
| `inc_dv` | Inconsistency: Different DV | numeric | 0–3 | 0 |
| `inc_iv` | Inconsistency: Different IV | numeric | 0–0 | 0 |
| `inc_ctrl_less` | Inconsistency: Fewer Controls | numeric | 0–4 | 0 |
| `inc_ctrl_more` | Inconsistency: More Controls | numeric | 0–4 | 0 |
| `inc_test` | Inconsistency: Different Test | numeric | 0–4 | 0 |
| `inc_spec` | Inconsistency: Different Specification | numeric | 0–4 | 0 |

## `df.evls`

Peer evaluations — long format, one row per (rated team × hypothesis × rater): reasonableness, informativeness, and the combined quality rating.

*5080 rows × 7 variables*

| Variable | Label | Type | Values / range | Missing |
|---|---|---|---|---|
| `tid` | Research Team ID | character | 127 unique values | 0 |
| `hyp` | Hypothesis ID | factor | h1; h2; h3; h4 | 0 |
| `rid` | ID of Rating Team | character | 127 unique values | 0 |
| `group` | Treatment Condition | factor | no data; data | 0 |
| `rsnbl` | Rating: Reasonableness (0-10) | numeric | 0–10 | 0 |
| `infrm` | Rating: Informativeness (0-10) | numeric | 0–10 | 0 |
| `qulty` | Rating: PAP Quality (0-10) | numeric | 0.00–10.00 | 0 |

## `df.flow`

Participant-flow counts (PAP §3) — a single-row summary of recruitment, dropout, completion, and valid-results counts.

*1 row × 4 variables*

| Variable | Label | Type | Values / range | Missing |
|---|---|---|---|---|
| `recruited` | Research Teams Recruited | numeric | 160–160 | 0 |
| `dropout` | Research Teams Dropped Out Before Completion | numeric | 33–33 | 0 |
| `completed` | Research Teams Completing All Stages | numeric | 127–127 | 0 |
| `results_valid` | Completing RTs With Reproducible/Valid Results | numeric | 120–120 | 0 |

## `df.proc`

Team-level analysis variables — one row per research team: process indicators, team composition, and aggregated experience.

*127 rows × 16 variables*

| Variable | Label | Type | Values / range | Missing |
|---|---|---|---|---|
| `tid` | Research Team ID | character | 127 unique values | 0 |
| `group` | Treatment Condition | factor | no data; data | 0 |
| `rprd_itrt` | Reprod.: No. Iterations | numeric | 1–7 | 0 |
| `effort` | Effort in Stages 1+2 (0-10) | numeric | 3–10 | 0 |
| `hours_spent` | Hours Spent in Stages 1+2 | numeric | 7–975 | 1 |
| `lit_search` | Literature Search (0-10) | numeric | 0–10 | 0 |
| `smpl_anal` | Simplest Analysis (0-10) | numeric | 0–10 | 0 |
| `mltp_reg` | Multiple Regressions (0-10) | numeric | 0–10 | 1 |
| `team_of_two` | Team of Two | factor | no; yes | 0 |
| `seniority` | (Max.) Assoc./Full Prof. | factor | junior; senior | 0 |
| `economics` | (Max.) Economist | factor | no; yes | 0 |
| `top_pub` | (Max.) AJG 4*/FT50 Pub. | factor | no; yes | 0 |
| `exp_soep` | (Max.) Experience: SOEP Data | factor | no; yes | 0 |
| `exp_anal` | (Max.) Experience: Analysis (0-10) | numeric | 5.67–10.00 | 0 |
| `exp_pap` | (Max.) Experience: PAPs (0-10) | numeric | 0–10 | 0 |
| `indx_rt` | RT Idiosyncrasy Index | numeric | 0.09–0.86 | 8 |

## `df.rslts`

Research-team results — one row per (team × hypothesis): z-values, directionality, significance, the idiosyncrasy index, and outlier flags.

*480 rows × 22 variables*

| Variable | Label | Type | Values / range | Missing |
|---|---|---|---|---|
| `tid` | Research Team ID | character | 120 unique values | 0 |
| `group` | Treatment Condition | factor | no data; data | 0 |
| `hyp` | Hypothesis ID | character | 4 unique values | 0 |
| `n_obs` | No. Observations | numeric | 211–573844 | 8 |
| `n_ind` | No. Individuals | numeric | 0–85843 | 15 |
| `b` | Coefficient Estimate | numeric | -7.10–77.23 | 0 |
| `se` | Standard Error | numeric | 0.00–43.29 | 0 |
| `fam` | Test Family | character | 5 unique values | 0 |
| `df_m` | Model DFs | numeric | 63–129 | 476 |
| `df_res` | Residual DFs | numeric | 17–573841 | 94 |
| `stat` | Test Statistic | numeric | -56.86–41.36 | 0 |
| `log_p` | log(p-Value) | numeric | -1609.26–0.00 | 0 |
| `intprt` | Interpretation | character | 2 unique values | 0 |
| `z` | z-Value | numeric | -56.66–41.32 | 0 |
| `dir` | Directionality | factor | negative; positive | 0 |
| `sig` | Significance | factor | p > 0.05; p < 0.05 | 0 |
| `ssc` | Stat. Sig. Criterion | factor | no; yes | 0 |
| `abs_dev` | Absolute Deviation | numeric | 0.00–55.86 | 0 |
| `outl_rsnbl` | Outlier: Low Reasonableness | factor | no; yes | 0 |
| `outl_fences` | Outlier: Tukey's Fences | factor | no; yes | 0 |
| `indx` | Idiosyncrasy Index | numeric | 0.00–1.00 | 1 |
| `n_plac` | Idiosyncrasy: No. Placebo Draws | numeric | 31–1000 | 1 |

## `df.srvy`

Entry/screening survey — one row per individual researcher (demographics, position, publication record, self-rated experience, and team identifiers).

*230 rows × 19 variables*

| Variable | Label | Type | Values / range | Missing |
|---|---|---|---|---|
| `tid` | Research Team ID | character | 127 unique values | 0 |
| `size` | Team Size | numeric | 1–2 | 0 |
| `group` | Treatment Condition | factor | no data; data | 0 |
| `member` | Team Member ID | character | 2 unique values | 0 |
| `pid` | Participant ID | character | 230 unique values | 0 |
| `recruit` | Recruitment Channel | character | 5 unique values | 0 |
| `lctn` | Geographic Location | character | 5 unique values | 0 |
| `gndr` | Gender | character | 3 unique values | 0 |
| `field` | Research Field | character | 7 unique values | 0 |
| `career` | Academic Position | character | 8 unique values | 0 |
| `pub_top` | Top Publication | factor | no; yes | 0 |
| `pub_emp` | Empirical Publication | factor | no; yes | 0 |
| `pub_rq` | Study on Daughters Hypothesis | factor | no; yes | 0 |
| `exp_reg` | Experience: Regression (0-10) | numeric | 5–10 | 0 |
| `exp_stata` | Experience: Stata (0-10) | numeric | 0–10 | 0 |
| `exp_data` | Experience: Large Data (0-10) | numeric | 1–10 | 0 |
| `exp_pap` | Experience: PAPs (0-10) | numeric | 0–10 | 0 |
| `exp_meta` | Experience: Meta-Science | factor | no; yes | 0 |
| `exp_soep` | Experience: SOEP | factor | no; yes | 0 |
