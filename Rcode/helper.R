
make_population_model <- function(rho){
  paste("xi_1 =~ 1 * x11 + 1 * x12 + 1 * x13",
        "xi_2 =~ 1 * x21 + 1 * x22 + 1 * x23",
        # variances
        "xi_1 ~~ 1 * xi_1",
        "xi_2 ~~ 1 * xi_2", 
        # correlation
        sprintf("xi_1 ~~ %g * xi_2", rho),
        # errorterms
        "x11 ~~ 0.6 * x11 + 0 * x12 + 0 * x13 + 0 * x21 + 0 * x22 + 0 * x23",
        "x12 ~~ 0.5 * x12 + 0 * x13 + 0 * x21 + 0 * x22 + 0 * x23",
        "x13 ~~ 0.2 * x13 + 0 * x21 + 0 * x22 + 0 * x23",
        "x21 ~~ 0.6 * x21 + 0 * x22 + 0 * x23",
        "x22 ~~ 0.5 * x22 + 0 * x23",
        "x23 ~~ 0.2 * x23",
        # intercepts
        "x11 ~ 0 * 1",
        "x12 ~ 0 * 1",
        "x13 ~ 0 * 1",
        "x21 ~ 0 * 1", 
        "x22 ~ 0 * 1", 
        "x23 ~ 0 * 1",
        sep = " /n ")
}

