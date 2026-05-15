if (!exists("RESULTS_DIR", inherits = TRUE)) RESULTS_DIR <- "results/results_2026_05_14"
CI_DIR  <- file.path(RESULTS_DIR, "ci")

ci_files <- list.files(CI_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(ci_files) == 0L) stop("No .rds files found in CI_DIR: ", CI_DIR)

dfall  <- do.call(rbind, lapply(ci_files,  readRDS))


sp <- dfall[dfall$estimator == "htmt" & is.na(dfall$estimate) & dfall$n > 25,]

# example case for which no HTMT could be calculated: 
sp[1,]
# task id: 55, rep in batch: 71
df <- readRDS("C:/Forschung/CI_HTMT/results/results_2026_05_14/datasets/df_task_00055.rds")

data <- df[[71]]$data

cov <- as.data.frame(cov(data))

source("Rcode/HTMT.R")

htmt_val <- HTMT(data, nindicator = 3)

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

write_latex_table(
  cov,
  file    = "outputs/tables/covariance_example.tex",
  caption = paste("Covariance Matrix of a replication for which no HTMT estimate could be produced."),
  label   = "tab:examplecov",
  align   = c("r", "r", "r", "r", "r", "r"),
  fmt     = c("%.4f", "%.4f", "%.4f", "%.4f", "%.4f","%.4f")
)

