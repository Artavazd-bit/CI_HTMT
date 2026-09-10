library(dplyr)

if (!exists("boot_validity", inherits = FALSE)) source("analysis/prep_boot_validity.R")

OUT_DIR <- "outputs/tables"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Keep only conditions where the bootstrap had any missings; 
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
    "\\footnotesize",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    sprintf("\\begin{tabular}{%s}", paste(align, collapse = "")),
    "\\toprule",
    # multicolumn header
    "& & & \\multicolumn{7}{c}{Number of inadmissible bootstrap estimates} \\\\",  
    paste0(header, " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\vspace{4pt}" , 
    "{\\footnotesize Note: Only conditions in which at least one bootstrap estimate was inadmissible are reported.}" ,
    "\\end{table}"
  )
  writeLines(lines, file)
}

bv_tab <- bv %>%
  transmute(`$\\phi$`        = correlation.x,
            `Data Distribution`     = dtype.x,
            `$n$`            = n.x,
            `0`        = .data[["0"]],
            `1--25`    = .data[["1-25"]],
            `26--50`   = .data[["26-50"]],
            `51--100`   = .data[["51-100"]],
            `101--500`  = .data[["101-500"]],
            `501--750` = .data[["501-750"]],
            `751--1000` = .data[["751-1000"]])

boot_validity_tab <- boot_validity %>% 
  transmute(`$\\phi$`        = correlation.x,
            `Data Distribution`     = dtype.x,
            `$n$`            = n.x,
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
  caption = paste("Relative frequency [in \\%] of simulation runs by number of",
                  "bootstrap samples for which the HTMT is not admissible,",
                  "across conditions."),
  label   = "tab:boot-validity",
  align   = c("r", "l", "r", "r", "r", "r", "r", "r", "r", "r"),
  fmt     = c("%.2f", "s", "%d",
              "%.1f", "%.1f", "%.1f", "%.1f",
              "%.1f", "%.1f", "%.1f")
)

write_latex_table(
  boot_validity_tab,
  file    = file.path(OUT_DIR, "boot_validity_all.tex"),
  caption = paste("Relative frequency [in \\%] of simulation runs by number of",
                  "bootstrap samples for which the HTMT is not admissible,",
                  "across conditions."),
  label   = "tab:boot-validity_all",
  align   = c("r", "l", "r", "r", "r", "r", "r", "r", "r", "r"),
  fmt     = c("%.2f", "s", "%d",
              "%.1f", "%.1f", "%.1f", "%.1f",
              "%.1f", "%.1f", "%.1f")
)

message(sprintf("Wrote %s (%d rows).",
                file.path(OUT_DIR, "boot_validity.tex"), nrow(bv_tab)))
