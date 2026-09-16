source("analysis/prep_problems.R")

df_cfa <- dfall2[dfall2$estimator_grp == c("cfa", "cfa_robust"), ]


df_cfa_prob <- df_cfa[, ]


unique(df_cfa_prob$warning_message)

unique(df_cfa_prob$error_message)

normalize_msg <- function(x) {
  x <- sub("^\\s*lavaan->[[:alnum:]_]+\\(\\):", "", x)   # drop the calling function
  x <- gsub("\\(= [^)]*\\)", "(= <value>)", x)           # blank the eigenvalue only
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

parts <- strsplit(df_cfa_prob$warning_message, "||", fixed = TRUE)

w <- data.frame(
  rep = rep(seq_along(parts), lengths(parts)),
  msg = normalize_msg(unlist(parts)),
  stringsAsFactors = FALSE
)

msg_tab <- as.data.frame(table(w$msg), stringsAsFactors = FALSE)
names(msg_tab) <- c("msg", "n")
msg_tab[order(-msg_tab$n), ]

labels <- data.frame(
  pattern = c("ov variances are negative",
              "covariance matrix of latent variables",
              "vcov) does not appear to be positive",
              "Could not compute standard errors",
              "not all elements of the gradient",
              "solution has NOT been found"),
  label   = c("Negative estimated error variance",
              "Latent covariance matrix not positive definite",
              "Parameter covariance matrix not positive definite",
              "Standard errors not computable",
              "Gradient not near zero at reported solution",
              "Optimizer reported no solution"),
  stringsAsFactors = FALSE
)

hits <- sapply(labels$pattern, function(p) grepl(p, msg_tab$msg, fixed = TRUE))
n_hits <- rowSums(hits)

if (any(n_hits != 1)) {
  stop("message classification is not one-to-one: ",
       sum(n_hits == 0), " unmatched, ", sum(n_hits > 1), " ambiguous")
}

msg_tab$label <- labels$label[apply(hits, 1, which)]