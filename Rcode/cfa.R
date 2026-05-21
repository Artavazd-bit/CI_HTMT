# provides the functions used for the cfa methods

model_unconstrained <- '
              #  latent variables
                xi_1 =~ NA*x11 + x12 + x13
                xi_2 =~ NA*x21 + x22 + x23

                xi_1 ~~ xi_2
                xi_1 ~~ 1 * xi_1
                xi_2 ~~ 1 * xi_2
              '

model_constrained <- '
              #  latent variables
                xi_1 =~ NA*x11 + x12 + x13
                xi_2 =~ NA*x21 + x22 + x23

                xi_1 ~~ 1 * xi_2
                xi_1 ~~ 1 * xi_1
                xi_2 ~~ 1 * xi_2
                '

cfa_one <- function(data, estimator = "ML"){

  run_step <- function(expr) {
    warns <- character()
    val <- withCallingHandlers(
      tryCatch(expr,
               error = function(e) structure(list(err = conditionMessage(e)),
                                             class = "step_error")),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    if (inherits(val, "step_error")) {
      list(value = NULL, err = val$err, warns = warns)
    } else {
      list(value = val, err = NA_character_, warns = warns)
    }
  }

  t0 <- Sys.time()
  s_uncon <- run_step(lavaan::cfa(model_unconstrained, data = data,
                                  estimator = estimator))
  t_uncon <- as.numeric(Sys.time() - t0, units = "secs")

  t1 <- Sys.time()
  s_con <- run_step(lavaan::cfa(model_constrained, data = data,
                                estimator = estimator))
  t_con <- as.numeric(Sys.time() - t1, units = "secs")

  lrt_res <- NULL
  err_lrt <- NA_character_
  warns_lrt <- character()
  t_lrt <- NA_real_
  if (!is.null(s_uncon$value) && !is.null(s_con$value)) {
    t2 <- Sys.time()
    s_lrt <- run_step(lavaan::lavTestLRT(s_uncon$value, s_con$value))
    t_lrt <- as.numeric(Sys.time() - t2, units = "secs")
    err_lrt   <- s_lrt$err
    warns_lrt <- s_lrt$warns
    if (!is.null(s_lrt$value)) {
      lrt_res <- list(
        chisq_diff = s_lrt$value$`Chisq diff`[2],
        df_diff    = s_lrt$value$`Df diff`[2],
        p_diff     = s_lrt$value$`Pr(>Chisq)`[2]
      )
    }
  }

  list(
    fit_uncon = s_uncon$value, t_uncon = t_uncon,
    err_uncon = s_uncon$err,   warns_uncon = s_uncon$warns,
    fit_con   = s_con$value,   t_con   = t_con,
    err_con   = s_con$err,     warns_con   = s_con$warns,
    lrt       = lrt_res,       t_lrt   = t_lrt,
    err_lrt   = err_lrt,       warns_lrt   = warns_lrt
  )
}
