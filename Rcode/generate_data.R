generate_data <- function(n, 
                          population_model,
                          datatype = c("normal", "nonnormal"), 
                          skewness = NULL, 
                          kurtosis = NULL) {
  datatype = match.arg(datatype)
  
  if(datatype == "normal"){
    return(lavaan::simulateData(
      model = population_model,
      sample.nobs = n,
      empirical = FALSE, 
      kurtosis = NULL,
      skewness = NULL
    ))
  }
  
  fit <- sem(population_model, do.fit = FALSE)
  popcov <- lavInspect(fit, "implied")$cov
  
  res <- covsim::rPLSIM(
    N = n,
    sigma.target = popcov,
    skewness = skewness,
    excesskurtosis = kurtosis,
    numsegments = 8,
    reps = 1,
    verbose = FALSE
  )
  out <- as.data.frame(res[1][1])
  colnames(out) <- colnames(popcov)
  out
}
