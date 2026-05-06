# helper function
get_S <- function(data, use_cor = FALSE){
  if(use_cor) cor(data) else cov(data)
}

# Very simple function to calculate the HTMT
HTMT <- function(data, use_cor = FALSE, nindicator){
  S <- get_S(data, use_cor)
  i <- 1:nindicator
  m1 <- mean(S[i,i][lower.tri(S[i,i])])
  m2 <- mean(S[-i,-i][lower.tri(S[-i,-i])])
  if(m1 < 0 || m2 < 0){
    warning("Negative mean inside sqrt()")
  }
  mean(S[i,-i]) / sqrt(m1 * m2)
}

# function for analytical derivatives of the htmt function
HTMT_jacobian <- function(data, use_cor = FALSE, nindicator){
  S <- get_S(data, use_cor)
  i <- 1:nindicator
  m1 <- mean(S[i,i][lower.tri(S[i,i])])
  m2 <- mean(S[-i,-i][lower.tri(S[-i,-i])])
  htmtval <- mean(S[i,-i]) / sqrt(m1 * m2)
  deltaintra1 <- -htmtval * (nindicator * (nindicator-1) * m1)^(-1)
  deltaintra2 <- -htmtval * (nindicator * (nindicator-1) * m2)^(-1)
  deltainter <- (nindicator^-2) / sqrt(m1*m2) 
  list(grad= c(rep(deltaintra1, 2), rep(deltainter, 3), deltaintra1, 
    rep(deltainter, 6), rep(deltaintra2, 3)), htmt = htmtval) 
}

## calcovcov: Caluclates the variance covariance matrix of the off diagonal 
#              non-redundant covariances. 
# this function is used within the delta method function to derive omega for cov-
# matrices
calcovcov <- function(data) {
  n <- nrow(data)
  p <- ncol(data)
  size_n <- (p * p - p) / 2
  
  data_centered <- scale(data, center = TRUE, scale = FALSE)
  
  # Create indices for lower triangle
  indices <- which(lower.tri(matrix(0, p, p)), arr.ind = TRUE)
  
  # Initialize result matrix
  vc_r <- matrix(0, nrow = size_n, ncol = size_n)
  
  # Pre-calculate all possible products of centered variables
  products <- array(0, dim = c(n, p, p))
  for(i in 1:p) {
    for(j in i:p) {
      products[, i, j] <- data_centered[, i] * data_centered[, j] 
      products[, j, i] <- products[, i, j]
    }
  }
  
  # Calculate covariances using vectorized operations
  for(idx1 in 1:nrow(indices)) {
    x <- indices[idx1, "row"] 
    y <- indices[idx1, "col"]
    for(idx2 in idx1:nrow(indices)) {
      z <- indices[idx2, "row"] 
      t <- indices[idx2, "col"]
      # Calculate fourth-order moments using pre-computed products
      omega_xyzt <- mean(products[, x, y] * products[, z, t]) - 
        ( mean(products[, x, y]) * mean(products[, z, t]) )
      vc_r[idx1, idx2] <- omega_xyzt
      vc_r[idx2, idx1] <- omega_xyzt  
    }
  }
  return(vc_r)
}

# Shared delta-method core: jacobian + omega + standard error.
# Reused across multiple alphas without recomputation.
HTMT_delta_se <- function(data, nindicator) {
  out <- HTMT_jacobian(data, use_cor = FALSE, nindicator)
  omega <- calcovcov(data)
  se <- sqrt(t(out$grad) %*% omega %*% out$grad / nrow(data))[1]
  list(htmt = out$htmt, se = se)
}

# Per-alpha extraction of the delta-method bounds from htmt + se.
HTMTDM_ci <- function(htmt, se, alpha) {
  z <- qnorm(p = 1 - (alpha / 2), mean = 0, sd = 1)
  list(lowerbound = htmt - z * se, upperbound = htmt + z * se)
}

# Backward-compatible wrapper: full delta-method CI for one alpha.
HTMTDM <- function(data, nindicator, alpha = 0.05) {
  ds <- HTMT_delta_se(data, nindicator)
  ci <- HTMTDM_ci(ds$htmt, ds$se, alpha)
  list(htmt = ds$htmt, se = ds$se,
       lowerbound = ci$lowerbound, upperbound = ci$upperbound)
}

