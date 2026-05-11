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
  t0 <- Sys.time()
  fit_uncon <- lavaan::cfa(model_unconstrained, data = data, estimator = estimator)
  t_uncon <- as.numeric(Sys.time() - t0, units = "secs")

  t1 <- Sys.time()
  fit_con <- lavaan::cfa(model_constrained, data = data, estimator = estimator)
  t_con <- as.numeric(Sys.time() - t1, units = "secs")

  t2 <- Sys.time()
  lrt <- lavaan::lavTestLRT(fit_uncon, fit_con)
  t_lrt <- as.numeric(Sys.time() - t2, units = "secs")

  list(
    fit_uncon = fit_uncon,
    t_uncon   = t_uncon,
    t_con     = t_con,
    t_lrt     = t_lrt,
    lrt = list(
      chisq_diff = lrt$`Chisq diff`[2],
      df_diff    = lrt$`Df diff`[2],
      p_diff     = lrt$`Pr(>Chisq)`[2]
    )
  )
}
