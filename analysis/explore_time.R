library(dplyr)
library(tidyverse)

dfall95 <- dfall2[dfall2$conf_level == 0.95,]

time_resag <- dfall95 %>%
              group_by(n, method) %>%
              summarize(meantime = mean(time), .groups = "drop") %>%
              group_by(n) %>%
              mutate(rel_time_compared_cfa =
                       (meantime - meantime[method == "wald_cfa"]) /
                       meantime[method == "wald_cfa"] * 100) %>%
              ungroup()

time_meantime_wide <- time_resag %>%
                      select(n, method, meantime) %>%
                      pivot_wider(names_from = method, values_from = meantime)

time_relpct_wide <- time_resag %>%
                    select(n, method, rel_time_compared_cfa) %>%
                    pivot_wider(names_from = method, values_from = rel_time_compared_cfa)

# --- LaTeX table: relative time vs. wald_cfa baseline (per n) ----------------
OUT_DIR <- "outputs/tables"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

method_order <- c("delta", "perc", "bc", "bca")
relpct_tab <- time_relpct_wide %>%
              select(n, all_of(method_order)) %>%
              arrange(n)

col_headers <- c("$n$", "Asymptotic", "Percentile", "BC", "BCa")
align       <- c("r", "r", "r", "r", "r")
fmt         <- c("%d", "%.1f", "%.1f", "%.1f", "%.1f")

body <- vapply(seq_len(nrow(relpct_tab)), function(i) {
  cells <- vapply(seq_len(ncol(relpct_tab)), function(j) {
    v <- relpct_tab[[j]][i]
    if (is.na(v)) "--" else sprintf(fmt[j], v)
  }, character(1))
  paste0(paste(cells, collapse = " & "), " \\\\")
}, character(1))

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  paste("\\caption{Mean per-replication compute time of each HTMT confidence",
        "interval method relative to the Wald-CFA baseline, expressed as a",
        "percentage difference: $(t_{\\text{method}} - t_{\\text{wald\\_cfa}})",
        " / t_{\\text{wald\\_cfa}} \\times 100$. Means are taken over all",
        "replications at $\\alpha = 0.05$, grouped by sample size $n$.",
        "Positive values indicate slower than the baseline.}"),
  "\\label{tab:relative-time-vs-wald}",
  sprintf("\\begin{tabular}{%s}", paste(align, collapse = "")),
  "\\toprule",
  paste0(paste(col_headers, collapse = " & "), " \\\\"),
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

out_file <- file.path(OUT_DIR, "relative_time_vs_wald.tex")
writeLines(lines, out_file)
message(sprintf("Wrote %s (%d rows).", out_file, nrow(relpct_tab)))

