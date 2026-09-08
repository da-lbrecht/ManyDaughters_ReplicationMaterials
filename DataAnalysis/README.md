# ManyDaughters replication materials: data analysis

Last updated: 27 August 2026

## Overview

This directory contains the data-analysis component of the replication materials for:

> Albrecht et al. (2026), “The Effect of Data Access on Pre-Analysis Plans: Evidence from a many-analyst study on the daughters hypothesis.” Working paper; journal and DOI not yet available.

The complete author list is provided in the manuscript. 

## Data availability and provenance statements

### Statement about rights

The authors certify that they had legitimate access to and permission to use all data employed in the study. They also have permission to redistribute all data, code, RT submissions, and results included in the public replication materials. RT members gave active written consent to publish pseudonymized survey responses, RT do-files and results, and identifiable RT pre-analysis plans.

Some source data cannot be made publicly available. SOEP microdata are subject to the data-use conditions of the German Institute for Economic Research (DIW Berlin) and are omitted from this package. DIW Berlin administers and preserves the original SOEP data without a specified end date. Researchers seeking access must obtain the appropriate SOEP data-use agreement from DIW Berlin.

All materials included in this replication package are distributed under the Creative Commons Attribution 4.0 International Public License. See [LICENSE](LICENSE). This license does not grant rights to the omitted SOEP data or override third-party data-use conditions.

## References

Albrecht, David, et al. 2026. “The Effect of Data Access on Pre-Analysis Plans: Evidence from a many-analyst study on the daughters hypothesis.” Working paper.

# #ManyDaughters — Coordinators' Analysis Code

Replication package for the project coordinators' meta-analysis of **#ManyDaughters**, a multi-analyst study in which independent research teams (RTs) tested the same four hypotheses about the effects of having daughters on the same dataset, the German Socio-Economic Panel (SOEP).

Half the teams received 5% of the data *before* writing their pre-analysis plan; the other half received it only afterwards. The study therefore has two themes: what the teams found about daughters, and what early access to data does to pre-analysis plans, analysis quality, and results.

- **Pre-analysis plan:** <https://osf.io/cny7d/>
- **Project website:** <https://www.manydaughters.com>
- **Coordinators:** David Albrecht, Aurélien Baillon, Anna Dreber, Felix Holzmeister,
  Taisuke Imai, Magnus Johannesson, Levent Neyse, Séverine Toussaert

### The four RT hypotheses

Having daughters leads to …

1. stronger support for gender equality (attitudes);
2. more left-leaning political preferences;
3. more gender equality in the household (behaviors/outcomes);
4. a larger increase in support for gender equality for men than for women.

---

## Data availability

**The SOEP data are not part of this package and cannot be redistributed.** The study uses SOEP-Core release 37 (international edition), restricted to 2000–2020 and to respondents who completed the adult individual questionnaire in at least one of those years. Access is available to any researcher through the SOEP's official channels.

What this package *does* contain is the coordinators' analysis code together with the team-level derived data it operates on: RT results, survey responses, peer assessments, and the audit records. These are one row per research team (or per team × hypothesis), contain no SOEP microdata, and are what every table and figure in the reports is built from.

| Input file | Contents |
|---|---|
| `data/derived/ManyDaughters_preprocessed.dta` | Team-level master file: entry survey, belief surveys, peer assessments, per-team results from running each RT's Stata code on the 95% sample, and project-tracking fields |
| `data/derived/idiosyncrasy_results_combined.dta` | Per-(team × hypothesis) $p$-values from re-running each RT's code on the original 5% sample (`iteration == 0`) and on placebo 5% subsamples (`iteration > 0`) |
| `data/derived/PAPtoCode_ConsistencyCheck_100%sample_results.xlsx` | Coordinators' audit of each RT-PAP against the submitted Stata code |

The RT analysis codes and RT-PAPs are published separately, as described in PAP §1.9.

---

## Repository layout

```
├── run_all.R                          renders the four reports into out/
├── code/
│   ├── 01_data_processing.Rmd         builds every analysis frame
│   ├── 02_summary_stats.Rmd           descriptive statistics
│   ├── 03_preregistered_analyses.Rmd  the registered tests
│   ├── 04_belief_payments.Rmd         resolves the incentivized beliefs
│   ├── _codebook.R                    codebook generator, sourced by 01
│   └── _cstm.css                      report styling
├── data/
│   ├── derived/                       inputs (see above)
│   └── proc/                          generated: analysis frames + codebook
└── out/                               generated: rendered reports
    └── figures/                       figures for the manuscript
```

`data/derived/` also carries `PAPtoCode_ConsistencyCheck_10%sample_results.xlsx`, the record of the initial random audit under PAP §1.5. The pipeline does not read it. Both spreadsheets list all 127 teams, but the 10% file records verdicts only for the 14 audited pairs, of which 7 were judged inconsistent, above the 20% threshold that triggered the review of all pairs. The 100% file carries those same 14 verdicts unchanged, plus verdicts for the remaining teams and the breakdown by deviation type. It is kept for transparency about the escalation.

---

## Reproducing the results

### Requirements

R and Pandoc (Pandoc ships with both RStudio and Quarto). Each report installs any missing packages on first run. Developed and last verified under:

```
R 4.5.1 · rmarkdown 2.31 · knitr 1.51 · tidyverse 2.0.0 · dplyr 1.2.1
here 1.0.2 · haven 2.5.5 · labelled 2.16.0 · readxl 1.4.5 · stringr 1.6.0
gt 1.0.0 · fixest 0.14.0 · modelsummary 2.6.0
ggplot2 4.0.2 · viridis 0.6.5 · scales 1.4.0 · patchwork 1.3.2
```

`dplyr` ≥ 1.1 is required (the `relationship =` argument to `left_join()`) and `here` ≥ 1.0 (for `here::i_am()`). The code uses no native pipes or lambda shorthand, so it is not tied to a recent R release; the package versions above are the binding constraint.

### Running it

The four reports build on each other: 02, 03, and 04 each load the environment that 01 writes, so they have to run in order. The runner script at the project root renders all four into `out/`:

```bash
Rscript run_all.R
```

Each file can also be knitted individually in RStudio (again in order). The results are identical; the only difference is that the Knit button places the rendered report next to its source file in `code/` rather than in `out/`. All other outputs — the processed data, the figures, the payment list — end up in the right place either way, because every path inside the reports is anchored to the project root with `here::i_am()`.

If you script the rendering yourself, give every call its own environment (`envir = new.env()`, as `run_all.R` does). Each report begins with `rm(list = ls())`, which would otherwise wipe the calling script's variables mid-loop.

The whole pipeline takes about a minute on a current laptop. Step 03 dominates because it writes the manuscript figures at 600 dpi.

### What gets written

| Step | Writes |
|---|---|
| `01` | `data/proc/_env.RData` (the image 02–04 load), one `.rds` + `.csv` per frame, `data/proc/_codebook.md`, `out/01_data_processing.html` |
| `02` | `out/02_summary_stats.html` |
| `03` | `out/03_preregistered_analyses.html`, `out/figures/figure_{2,3,4,5}.{png,pdf}` |
| `04` | `out/04_belief_payments.html`, `out/belief_payments.csv` |

`out/figures/figure_timeline.{tex,pdf,png}` is a standalone TikZ figure, not produced by the
pipeline; rebuild it with `pdflatex figure_timeline.tex`.

Everything is deterministic: `set.seed(32844837)` fixes the figure jitter in 03 and the entire belief-payment randomization in 04, so re-running reproduces the same winners, hypotheses, price-list rows, lottery draws, and payment list.

---

## The analysis frames

`01_data_processing.Rmd` reduces the inputs to seven tidy frames, each documented variable-by-variable in the generated `data/proc/_codebook.md`.

| Frame | Unit | Contents |
|---|---|---|
| `df.srvy` | researcher | Entry survey: demographics, position, publications, self-rated experience |
| `df.blfs` | team × hypothesis × stage | Prior and posterior beliefs that the hypothesis is supported |
| `df.evls` | rated team × hypothesis × rater | Peer ratings: reasonableness, informativeness, and their mean (quality) |
| `df.cons` | team | RT-PAP-to-code consistency verdict and deviation counts |
| `df.rslts` | team × hypothesis | Signed $z$-values, significance flags, absolute deviations, outlier flags, idiosyncrasy index |
| `df.proc` | team | Process indicators, team composition, aggregated experience, RT idiosyncrasy index |
| `df.flow` | — | Participant-flow counts |

### Sample

```
160 recruited  →  33 dropped out  →  127 completed all stages  →  120 with valid results
```

The 127 completing teams are the analysis sample defined by PAP §3. Seven of them produced improper estimates (e.g., standard errors of zero) for at least one hypothesis; they are excluded from analyses that use the teams' results and retained everywhere else. That exclusion is **not** preregistered and is flagged as such in the reports.

---

## Preregistration and disclosed deviations

The pre-analysis plan was registered before recruitment opened. Every registered analysis in PAP §3.1–§3.4, together with the §1.8 effect-size conversions and the §1.5 consistency check, is implemented in `03_preregistered_analyses.Rmd`. In the rendered reports, each test opens with the registered specification set in a gray italic block, and any departure from it is called out immediately below in a box with a red rule down its left edge, so a reader can see at a glance what was planned and what was not.

`03` reports the registered analyses only. `04_belief_payments.Rmd` covers the incentive mechanism, which is a payment obligation rather than a hypothesis test.

Deviations, each documented in full where it occurs:

| # | Deviation | Where |
|---|---|---|
| 1 | **Bootstrapped standard errors replaced by conservative ones.** The registered two-stage bootstrap (1,000 iterations, resampling 41,170 households) proved infeasible: bootstrap samples cannot simultaneously preserve the SOEP identifier structure the RT codes require and de-duplicate the with-replacement repeats, and running all RT codes across all iterations would have exceeded a year of computation. Because all teams re-analyze the same 95% sample, their $z$-values are correlated rather than independent, so the mean $z$ is treated as a single standard-normal statistic ($se = 1$). This is exact under perfect dependence and conservative otherwise. | 03, Primary 1a–d and Secondary 1a–d |
| 2 | **Seven teams with improper estimates excluded** from all results-based analyses. | 03 preamble; 02 |
| 3 | **Consistency recorded as one verdict per team**, not the per-hypothesis indicator of PAP §2.1. | 02 |
| 4 | **Correlation effect size uses the inverse-Fisher form** $\tanh(\bar z \sqrt{1/(\bar n - 3)})$; the expression as printed in PAP §1.8 saturates to $\pm 1$ at these sample sizes. | 03, effect-size table |
| 5 | **Confidence intervals on the group means** in the per-team $z$ figure; §3.1.1 registered the means without intervals. | 03, primary descriptive analyses |
| 6 | **Two additional robustness tests of Secondary 5** (Mann–Whitney $U$; $t$-test excluding the two teams reporting over 600 hours). | 03, Secondary 5 |
| 7 | **Outlier-excluded MDEs** reported as context; PAP §3.4 excludes robustness-test MDEs. | 03, MDE table note |

Two further points of interpretation, resolved in favor of the plain reading of the PAP and noted in the reports:

- The two robustness exclusion criteria (low peer-rated reasonableness and Tukey's-fences outliers) are applied **jointly**, in a single "excl. outliers" sample. PAP §3.1.1 states that the outlier exclusion is "in addition to the previous exclusion criterion" and reports "an additional set" of results. Outlier status is determined on the full sample, before the reasonableness exclusion, as the plan prescribes.
- The idiosyncrasy index targets **1,000 placebo subsamples** per (team × hypothesis) cell. That target is met for 409 of the 479 cells with a non-missing index; the remaining 70 rest on fewer draws (median 970, minimum 31), because some teams' code did not execute on every subsample. The realized count is carried in `df.rslts$n_plac` and reported in 02.

---

## Contact

Questions about the project: <info@manydaughters.com>. Questions about this code: Felix Holzmeister (<felix.holzmeister@uibk.ac.at>).


