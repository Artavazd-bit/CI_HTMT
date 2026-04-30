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
                
cfa_one <- function(data, alpha = 0.05){
  fit_uncon <- lavaan::cfa(model_unconstrained, data = data)
  fit_con <- lavaan::cfa(model_constrained, data = data)
  
  lrt <- lavaan::lavTestLRT(fit_uncon, fit_con)
  
  pe  <- lavaan::parameterEstimates(fit_uncon, level = 1 - alpha)
  row <- pe[pe$lhs == "xi_1" & pe$op == "~~" & pe$rhs == "xi_2", ]
  
  data.frame(cor_est = row$est, lowerbound = row$ci.lower, upperbound = row$ci.upper,
             chisq_diff = lrt$`Chisq diff`[2], p_diff = lrt$`Pr(>Chisq)`[2], df_diff = lrt$`Df diff`[2])
}
