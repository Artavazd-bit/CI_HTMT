library(dplyr)

# LaTeX table of conditions x methods with any estimate-or-bound failure.
# Rows where pct_problem == 0 are omitted -- the goal is to spotlight the
# problematic cells. Sorted by descending failure rate.

if (!exists("problems", inherits = FALSE)) source("analysis/prep_problems.R")

OUT_DIR <- "outputs/tables"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

method_labels <- c(wald_cfa = "CFA", wald_cfa_robust = "CFA-MLR",
                   delta = "Asymptotic", perc = "Percentile",
                   bc = "BC", bca = "BCa")

dtype_labels <- c(normal = "normal", moderate = "moderate", severe = "severe")

probtab <- problems %>%
  filter(n_problem > 0) %>%
  transmute(correlation,
            n,
            dtype  = dtype_labels[dtype],
            method = method_labels[method],
            n_reps,
            n_problem,
            pct_problem) %>%
  arrange(desc(pct_problem), correlation, n, dtype, method)

# --- LaTeX writer (booktabs) -------------------------------------------------
escape_latex <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x
}

col_headers <- c("$\\rho$", "$n$", "Distribution", "Method",
                 "$N_{\\text{reps}}$", "$N_{\\text{fail}}$", "\\% failed")
align       <- c("r", "r", "l", "l", "r", "r", "r")
fmt         <- c("%.2f", "%d", "s", "s", "%d", "%d", "%.1f")
stopifnot(length(col_headers) == ncol(probtab),
          length(align)       == ncol(probtab),
          length(fmt)         == ncol(probtab))

body <- vapply(seq_len(nrow(probtab)), function(i) {
  cells <- vapply(seq_len(ncol(probtab)), function(j) {
    v <- probtab[[j]][i]
    if (is.na(v)) return("--")
    f <- fmt[j]
    if (f == "s") escape_latex(v) else sprintf(f, v)
  }, character(1))
  paste0(paste(cells, collapse = " & "), " \\\\")
}, character(1))

caption <- paste(
  "Replications in which the point estimate or a confidence bound could not",
  "be computed, by population correlation $\\rho$, sample size $n$, indicator",
  "distribution, and CI method. For CFA / CFA-MLR a lavaan warning or error",
  "is treated as a failed estimate (matching the clean-rep filter used in",
  "coverage analyses); for HTMT only hard NA estimates or non-finite bounds",
  "count. Counts and rates are taken at $\\alpha = 0.05$ and are identical",
  "across $\\alpha \\in \\{0.10, 0.05, 0.01\\}$ in every condition.",
  "Conditions with no failures are omitted. Rows are sorted by descending",
  "failure rate."
)

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  sprintf("\\caption{%s}", caption),
  "\\label{tab:problems}",
  sprintf("\\begin{tabular}{%s}", paste(align, collapse = "")),
  "\\toprule",
  paste0(paste(col_headers, collapse = " & "), " \\\\"),
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

out_file <- file.path(OUT_DIR, "problems.tex")
writeLines(lines, out_file)
message(sprintf("Wrote %s (%d rows).", out_file, nrow(probtab)))
