source("Rcode/cfa.R")
source("Rcode/HTMT.R")
source("Rcode/boot.R")

collapse_warns <- function(w) {
  if (length(w) == 0L) return(NA_character_)
  paste(unique(w), collapse = " || ")
}

# Time a single expression; returns list(value, secs).
time_call <- function(expr) {
  t0 <- Sys.time()
  val <- force(expr)
  list(value = val, secs = as.numeric(Sys.time() - t0, units = "secs"))
}

ci_battery <- function(data, nboot = 1000, nindicator = 3,
                       conf_levels = c(0.90, 0.95, 0.99)) {

  cfa_err   <- NA_character_
  htmt_err  <- NA_character_
  warns_cfa  <- character()
  warns_htmt <- character()

  # ---- CFA block: shared across alphas (one fit per dataset) ----
  cfa_res <- withCallingHandlers(
    tryCatch(
      cfa_one(data),
      error = function(e) { cfa_err <<- conditionMessage(e); NULL }
    ),
    warning = function(w) {
      warns_cfa <<- c(warns_cfa, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # ---- HTMT block: shared across alphas ----
  # Compute delta-method core (htmt point estimate + SE), bootstrap, and
  # jackknife once per dataset; per-alpha steps reuse all of these.
  htmt_res <- withCallingHandlers(
    tryCatch({
      td   <- time_call(HTMT_delta_se(data, nindicator))
      ds   <- td$value
      tb   <- time_call(myboot(data, HTMT, nindicator = nindicator, nboot = nboot))
      b    <- tb$value
      tj   <- time_call(jackknife(data, HTMT, nindicator = nindicator))
      j    <- tj$value
      list(htmt_pe = ds$htmt, se = ds$se, b = b, j = j,
           t_delta_core = td$secs, t_boot = tb$secs, t_jack = tj$secs,
           n_boot_valid = length(b), n_jack_valid = length(j))
    }, error = function(e) { htmt_err <<- conditionMessage(e); NULL }),
    warning = function(w) {
      warns_htmt <<- c(warns_htmt, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # ---- Per-conf-level CI extraction ----
  ci_rows <- vector("list", length(conf_levels))
  for (idx in seq_along(conf_levels)) {
    cl    <- conf_levels[idx]
    alpha <- 1 - cl

    # wald_cfa
    if (!is.null(cfa_res)) {
      tw <- time_call({
        pe  <- lavaan::parameterEstimates(cfa_res$fit_uncon, level = cl)
        pe[pe$lhs == "xi_1" & pe$op == "~~" & pe$rhs == "xi_2", ]
      })
      row     <- tw$value
      cfa_est <- row$est
      cfa_lo  <- row$ci.lower
      cfa_up  <- row$ci.upper
      t_wald  <- cfa_res$t_uncon + tw$secs
    } else {
      cfa_est <- NA_real_; cfa_lo <- NA_real_; cfa_up <- NA_real_
      t_wald  <- NA_real_
    }

    # delta / perc / bc / bca all derive from htmt_res
    if (!is.null(htmt_res)) {
      htmt_pe <- htmt_res$htmt_pe

      tq_d <- time_call(HTMTDM_ci(htmt_pe, htmt_res$se, alpha))
      delta_lo <- tq_d$value$lowerbound
      delta_up <- tq_d$value$upperbound
      t_delta  <- htmt_res$t_delta_core + tq_d$secs

      tq_p <- time_call(list(
        lo = unname(quantile(htmt_res$b, alpha / 2)),
        up = unname(quantile(htmt_res$b, 1 - alpha / 2))
      ))
      perc_lo <- tq_p$value$lo
      perc_up <- tq_p$value$up
      t_perc  <- htmt_res$t_boot + tq_p$secs

      tq_bc <- time_call(bootbc(htmt_res$b, alpha = alpha, estimate = htmt_pe))
      bc_lo <- tq_bc$value$lowerbound
      bc_up <- tq_bc$value$upperbound
      t_bc  <- htmt_res$t_boot + tq_bc$secs

      tq_bca <- time_call(bootbca(htmt_res$b, htmt_res$j,
                                  alpha = alpha, estimate = htmt_pe))
      bca_lo <- tq_bca$value$lowerbound
      bca_up <- tq_bca$value$upperbound
      t_bca  <- htmt_res$t_boot + htmt_res$t_jack + tq_bca$secs
    } else {
      htmt_pe <- NA_real_
      delta_lo <- delta_up <- NA_real_; t_delta <- NA_real_
      perc_lo  <- perc_up  <- NA_real_; t_perc  <- NA_real_
      bc_lo    <- bc_up    <- NA_real_; t_bc    <- NA_real_
      bca_lo   <- bca_up   <- NA_real_; t_bca   <- NA_real_
    }

    ci_rows[[idx]] <- data.frame(
      conf_level = cl,
      estimator  = c("cfa",      "htmt",  "htmt", "htmt", "htmt"),
      method     = c("wald_cfa", "delta", "perc", "bc",   "bca"),
      estimate   = c(cfa_est, htmt_pe, htmt_pe, htmt_pe, htmt_pe),
      lowerbound = c(cfa_lo,  delta_lo, perc_lo, bc_lo,  bca_lo),
      upperbound = c(cfa_up,  delta_up, perc_up, bc_up,  bca_up),
      time       = c(t_wald,  t_delta,  t_perc,  t_bc,   t_bca),
      stringsAsFactors = FALSE
    )
  }

  ci <- do.call(rbind, ci_rows)
  rownames(ci) <- NULL

  lrt <- data.frame(
    chisq_diff = if (!is.null(cfa_res)) cfa_res$lrt$chisq_diff else NA_real_,
    df_diff    = if (!is.null(cfa_res)) cfa_res$lrt$df_diff    else NA_real_,
    p_diff     = if (!is.null(cfa_res)) cfa_res$lrt$p_diff     else NA_real_,
    time       = if (!is.null(cfa_res))
                   cfa_res$t_uncon + cfa_res$t_con + cfa_res$t_lrt
                 else NA_real_
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
