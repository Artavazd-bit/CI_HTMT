library(dplyr)

if (!exists("jack_validity", inherits = FALSE)) source("analysis/prep_boot_validity.R")

OUT_DIR <- "outputs/tables"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Keep only conditions where the bootstrap had any missings; rows where 100%
# of reps had zero missings are uninformative and inflate the table.
bv <- jack_validity %>% filter(.data[["0"]] < 100)

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
  transmute(`$\\phi$`        = correlation.x,
            `$n$`            = n.x,
            `data distribution`     = dtype.x,
            `0`        = .data[["0"]],
            `0--1`    = .data[["0-1"]],
            `1--100`   = .data[["1-100"]])

jack_validity_tab <- jack_validity %>% 
  transmute(`$\\phi$`        = correlation.x,
            `$n$`            = n.x,
            `data distribution`     = dtype.x,
            `0`        = .data[["0"]],
            `0--1`    = .data[["0-1"]],
            `1--100`   = .data[["1-100"]])

write_latex_table(
  bv_tab,
  file    = file.path(OUT_DIR, "jack_validity.tex"),
  caption = paste("Relative frequency [in \\%] of replications by number of",
                  "jackknife samples for which the HTMT is not computable,",
                  "across conditions."),
  label   = "tab:boot-validity",
  align   = c("r", "r", "l", "r", "r", "r"),
  fmt     = c("%.2f", "%d", "s",
              "%.1f", "%.1f", "%.1f")
)

write_latex_table(
  jack_validity_tab,
  file    = file.path(OUT_DIR, "jack_validity_all.tex"),
  caption = paste("Relative frequency [in \\%] of replications by number of",
                  "jackknife samples for which the HTMT is not computable,",
                  "across conditions."),
  label   = "tab:boot-validity",
  align   = c("r", "r", "l", "r", "r", "r"),
  fmt     = c("%.2f", "%d", "s",
              "%.1f", "%.1f", "%.1f")
)

