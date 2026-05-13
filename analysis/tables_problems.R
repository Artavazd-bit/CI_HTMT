library(dplyr)

if (!exists("problem_estimates", inherits = FALSE)) source("analysis/prep_problems.R")

OUT_DIR <- "outputs/tables"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- problem_estimates: keep all rows with at least one failure --------------
# HTMT methods (delta/perc/bc/bca) share a single point estimate, so each rep
# produces four identical rows in problem_estimates -- collapse them to one
# row per (condition x htmt). CFA's two methods (wald_cfa / wald_cfa_robust)
# can fail independently because ML and MLR are separate fits, so keep both.
pe <- problem_estimates %>%
  mutate(method = ifelse(estimator == "htmt", NA_character_, method)) %>%
  distinct(correlation, n, dtype, estimator, method,
           n_reps, n_failed, pct_failed) %>%
  filter(n_failed > 0)

# --- problem_bounds: collapse conf_level (counts are identical across alphas) -
pb_check <- problem_bounds %>%
  group_by(correlation, n, dtype, estimator, method) %>%
  summarize(distinct_counts = n_distinct(n_bounds_failed),
            .groups = "drop")
if (any(pb_check$distinct_counts > 1L)) {
  warning("problem_bounds: n_bounds_failed differs across conf_levels in some ",
          "groups. Collapsed table will use the max.")
}

pb <- problem_bounds %>%
  group_by(correlation, n, dtype, estimator, method) %>%
  summarize(n_reps            = first(n_reps),
            n_estimate_ok     = first(n_estimate_ok),
            n_bounds_failed   = max(n_bounds_failed),
            pct_bounds_failed = max(pct_bounds_failed),
            .groups = "drop") %>%
  filter(n_bounds_failed > 0) %>%
  arrange(desc(pct_bounds_failed),
          correlation, n, dtype, estimator, method)

# --- LaTeX writer (booktabs) -------------------------------------------------
escape_latex <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x
}

write_latex_table <- function(df, file, caption, label, align, fmt) {
  stopifnot(length(align) == ncol(df), length(fmt) == ncol(df))
  body <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(seq_len(ncol(df)), function(j) {
      v <- df[[j]][i]
      if (is.na(v)) return("--")
      f <- fmt[j]
      if (f == "s") escape_latex(v) else sprintf(f, v)
    }, character(1))
    paste0(paste(cells, collapse = " & "), " \\\\")
  }, character(1))

  header <- paste(colnames(df), collapse = " & ")
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    sprintf("\\begin{tabular}{%s}", paste(align, collapse = "")),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(lines, file)
}

# --- problem_estimates table -------------------------------------------------
pe_tab <- pe %>%
  transmute(`$\\rho$`        = correlation,
            `$n$`            = n,
            Distribution     = dtype,
            Estimator        = estimator,
            Method           = ifelse(is.na(method), "--", method),
            `$N_{\\text{fail}}$` = n_failed,
            `\\% failed`     = pct_failed)

write_latex_table(
  pe_tab,
  file    = file.path(OUT_DIR, "problem_estimates.tex"),
  caption = paste("Replications in which the point estimate could not be",
                  "computed, by condition, estimator, and (for CFA) fitting",
                  "method (ML vs MLR). HTMT shares one point estimate across",
                  "all four CI methods, so its row carries `--' for Method.",
                  "Rates are over 1{,}000 replications per condition.",
                  "Conditions with no failures are omitted."),
  label   = "tab:problem-estimates",
  align   = c("r", "r", "l", "l", "l", "r", "r"),
  fmt     = c("%.2f", "%d", "s", "s", "s", "%d", "%.1f")
)

# --- problem_bounds table ----------------------------------------------------
pb_tab <- pb %>%
  transmute(`$\\rho$`        = correlation,
            `$n$`            = n,
            Distribution     = dtype,
            Estimator        = estimator,
            Method           = method,
            `$N_{\\text{est}}$`  = n_estimate_ok,
            `$N_{\\text{fail}}$` = n_bounds_failed,
            `\\% failed`     = pct_bounds_failed)

write_latex_table(
  pb_tab,
  file    = file.path(OUT_DIR, "problem_bounds.tex"),
  caption = paste("Replications with successful point estimate but missing",
                  "lower or upper confidence bound (NA or NaN), by condition,",
                  "estimator, and method. Counts and rates are identical",
                  "across the three confidence levels (0.90, 0.95, 0.99) and",
                  "are reported once per group; the rate is over",
                  "$N_{\\text{est}}$, the replications with a valid estimate.",
                  "Conditions with no failures are omitted."),
  label   = "tab:problem-bounds",
  align   = c("r", "r", "l", "l", "l", "r", "r", "r"),
  fmt     = c("%.2f", "%d", "s", "s", "s", "%d", "%d", "%.2f")
)

message(sprintf("Wrote %s (%d rows) and %s (%d rows).",
                file.path(OUT_DIR, "problem_estimates.tex"), nrow(pe_tab),
                file.path(OUT_DIR, "problem_bounds.tex"),    nrow(pb_tab)))
