library(dplyr)

source("analysis/prep_boot_validity.R")

OUT_DIR <- "outputs/tables"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Keep only conditions where the bootstrap had any missings; rows where 100%
# of reps had zero missings are uninformative and inflate the table.
bv <- boot_validity %>% filter(.data[["0"]] < 100)

# --- LaTeX writer (booktabs, no escape on headers) ---------------------------
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

bv_tab <- bv %>%
  transmute(`$\\rho$`        = correlation,
            `$n$`            = n,
            Distribution     = dtype,
            `0`        = .data[["0"]],
            `1--25`    = .data[["1-25"]],
            `26--50`   = .data[["26-50"]],
            `51--100`   = .data[["51-100"]],
            `101--500`  = .data[["101-500"]],
            `501--750` = .data[["501-750"]],
            `751--1000` = .data[["751-1000"]])

write_latex_table(
  bv_tab,
  file    = file.path(OUT_DIR, "boot_validity.tex"),
  caption = paste("Relative frequency [in \\%] of replications by number of",
                  "bootstrap samples for which the HTMT was not computable,",
                  "across conditions. Per condition, 1{,}000 replications",
                  "each draw 1{,}000 bootstrap samples; columns bin the",
                  "per-replication miss-count. Conditions where every",
                  "replication had 0 missings are omitted; rows sum to 100\\%."),
  label   = "tab:boot-validity",
  align   = c("r", "r", "l", "r", "r", "r", "r", "r", "r", "r"),
  fmt     = c("%.2f", "%d", "s",
              "%.1f", "%.1f", "%.1f", "%.1f",
              "%.1f", "%.1f", "%.1f")
)

message(sprintf("Wrote %s (%d rows).",
                file.path(OUT_DIR, "boot_validity_v2.tex"), nrow(bv_tab)))
