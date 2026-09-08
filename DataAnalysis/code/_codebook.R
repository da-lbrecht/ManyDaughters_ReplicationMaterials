# _codebook.R -- generate a Markdown codebook for the processed data frames.
#
# write_codebook(proc_dir) reads <proc_dir>/_env.RData and writes
# <proc_dir>/_codebook.md, documenting every df.* frame and its variables
# (names, labels assigned via set_variable_labels(), types, and factor levels
# or numeric ranges) so the portable .rds / .csv copies are self-describing.

write_codebook <- function(proc_dir) {

  e <- new.env()
  load(file.path(proc_dir, "_env.RData"), envir = e)
  frames <- sort(ls(e, pattern = "^df\\."))

  # short, human-readable description of each processed data frame
  frame.desc <- c(
    "df.srvy"  = "Entry/screening survey — one row per individual researcher (demographics, position, publication record, self-rated experience, and team identifiers).",
    "df.blfs"  = "Belief elicitations — long format, one row per (team × hypothesis × stage): Stage-1 priors and Stage-2 posteriors.",
    "df.evls"  = "Peer evaluations — long format, one row per (rated team × hypothesis × rater): reasonableness, informativeness, and the combined quality rating.",
    "df.cons"  = "RT-PAP / code consistency audit (PAP §1.5 / §2.1) — one row per research team: overall verdict and per-type deviation counts.",
    "df.rslts" = "Research-team results — one row per (team × hypothesis): z-values, directionality, significance, the idiosyncrasy index, and outlier flags.",
    "df.proc"  = "Team-level analysis variables — one row per research team: process indicators, team composition, and aggregated experience.",
    "df.flow"  = "Participant-flow counts (PAP §3) — a single-row summary of recruitment, dropout, completion, and valid-results counts."
  )

  esc    <- function(x) gsub("|", "\\|", x, fixed = TRUE)        # escape md table pipes
  anchor <- function(nm) gsub("[^a-z0-9]+", "", tolower(nm))     # GitHub-style header id
  plur   <- function(n, w) sprintf("%d %s", n, if (n == 1) w else paste0(w, "s"))

  describe <- function(v) {
    lbl <- attr(v, "label", exact = TRUE)
    lbl <- if (is.null(lbl)) "" else as.character(lbl)
    if (is.factor(v))
      return(list(type = "factor",  vals = paste(levels(v), collapse = "; "), label = lbl))
    if (is.logical(v))
      return(list(type = "logical", vals = "TRUE; FALSE", label = lbl))
    if (inherits(v, "Date"))
      return(list(type = "date",
                  vals = paste(format(range(v, na.rm = TRUE)), collapse = " to "), label = lbl))
    if (is.numeric(v)) {
      nn <- v[!is.na(v)]
      if (!length(nn)) return(list(type = "numeric", vals = "(all missing)", label = lbl))
      intg <- all(nn == round(nn))
      f <- function(z) if (intg) formatC(z, format = "d") else formatC(z, format = "f", digits = 2)
      return(list(type = "numeric",
                  vals = paste0(f(min(nn)), "–", f(max(nn))), label = lbl))
    }
    nn <- v[!is.na(v)]
    list(type = "character", vals = paste0(length(unique(nn)), " unique values"), label = lbl)
  }

  L <- c(
    "# Codebook — #ManyDaughters processed data",
    "",
    "This codebook documents the processed data frames exported to `data/proc/` — each as `<name>.rds` (faithful column types and variable labels) and `<name>.csv` (portable, human-readable), alongside the complete `_env.RData` image. Variable labels are those assigned in `code/01_data_processing.Rmd`. This file is regenerated automatically by the data-processing pipeline.",
    "",
    "## Data frames",
    ""
  )
  L <- c(L, vapply(frames, function(nm)
    sprintf("- [`%s`](#%s) — %s × %s",
            nm, anchor(nm), plur(nrow(get(nm, e)), "row"),
            plur(ncol(get(nm, e)), "variable")), character(1)))

  for (nm in frames) {
    fr <- get(nm, e)
    d  <- if (nm %in% names(frame.desc)) unname(frame.desc[nm]) else ""
    L <- c(L, "", sprintf("## `%s`", nm), "", d, "",
           sprintf("*%s × %s*", plur(nrow(fr), "row"), plur(ncol(fr), "variable")), "",
           "| Variable | Label | Type | Values / range | Missing |",
           "|---|---|---|---|---|")
    for (cn in names(fr)) {
      di <- describe(fr[[cn]])
      L <- c(L, sprintf("| `%s` | %s | %s | %s | %d |",
                        cn, esc(di$label), di$type, esc(di$vals), sum(is.na(fr[[cn]]))))
    }
  }

  writeLines(enc2utf8(L), file.path(proc_dir, "_codebook.md"), useBytes = TRUE)
  invisible(file.path(proc_dir, "_codebook.md"))
}
