source("Rcode/cfa.R")
source("Rcode/HTMT.R")
source("Rcode/boot.R")

collapse_warns <- function(w) {
  if (length(w) == 0L) return(NA_character_)
  paste(unique(w), collapse = " || ")
}

ci_battery <- function(data, nboot = 1000, nindicator = 3, alpha = 0.05) {

  cfa_err   <- NA_character_
  htmt_err  <- NA_character_
  warns_cfa  <- character()
  warns_htmt <- character()

  cfa_res <- withCallingHandlers(
    tryCatch(
      cfa_one(data, alpha = alpha),
      error = function(e) { cfa_err <<- conditionMessage(e); NULL }
    ),
    warning = function(w) {
      warns_cfa <<- c(warns_cfa, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  htmt_res <- withCallingHandlers(
    tryCatch({
      htmt_dm <- HTMTDM(data, nindicator = nindicator, alpha = alpha)
      htmt_pe <- htmt_dm$htmt
      b <- myboot(data, HTMT, nindicator = nindicator, nboot = nboot)
      j <- jackknife(data, HTMT, nindicator = nindicator)
      ci_perc <- list(
        lowerbound = unname(quantile(b, alpha / 2)),
        upperbound = unname(quantile(b, 1 - alpha / 2))
      )
      ci_bc  <- bootbc (b,    alpha = alpha, estimate = htmt_pe)
      ci_bca <- bootbca(b, j, alpha = alpha, estimate = htmt_pe)
      list(htmt_pe = htmt_pe, dm = htmt_dm, perc = ci_perc, bc = ci_bc, bca = ci_bca,
           n_boot_valid = length(b), n_jack_valid = length(j))
    }, error = function(e) { htmt_err <<- conditionMessage(e); NULL }),
    warning = function(w) {
      warns_htmt <<- c(warns_htmt, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  ci <- data.frame(
    estimator  = c("cfa",      "htmt",  "htmt", "htmt", "htmt"),
    method     = c("wald_cfa", "delta", "perc", "bc",   "bca"),
    estimate   = NA_real_,
    lowerbound = NA_real_,
    upperbound = NA_real_,
    stringsAsFactors = FALSE
  )
  if (!is.null(cfa_res)) {
    ci[1, c("estimate", "lowerbound", "upperbound")] <-
      c(cfa_res$cor_est, cfa_res$lowerbound, cfa_res$upperbound)
  }
  if (!is.null(htmt_res)) {
    ci[2:5, "estimate"]   <- htmt_res$htmt_pe
    ci[2:5, "lowerbound"] <- c(htmt_res$dm$lowerbound,
                               htmt_res$perc$lowerbound,
                               htmt_res$bc$lowerbound,
                               htmt_res$bca$lowerbound)
    ci[2:5, "upperbound"] <- c(htmt_res$dm$upperbound,
                               htmt_res$perc$upperbound,
                               htmt_res$bc$upperbound,
                               htmt_res$bca$upperbound)
  }

  lrt <- data.frame(
    chisq_diff = if (!is.null(cfa_res)) cfa_res$chisq_diff else NA_real_,
    df_diff    = if (!is.null(cfa_res)) cfa_res$df_diff    else NA_real_,
    p_diff     = if (!is.null(cfa_res)) cfa_res$p_diff     else NA_real_
  )

  errors <- data.frame(
    estimator       = c("cfa", "htmt"),
    error_message   = c(cfa_err, htmt_err),
    warning_message = c(collapse_warns(warns_cfa), collapse_warns(warns_htmt)),
    n_boot_valid    = c(NA_integer_,
                        if (!is.null(htmt_res)) htmt_res$n_boot_valid else NA_integer_),
    n_jack_valid    = c(NA_integer_,
                        if (!is.null(htmt_res)) htmt_res$n_jack_valid else NA_integer_),
    stringsAsFactors = FALSE
  )

  list(ci = ci, lrt = lrt, errors = errors)
}
