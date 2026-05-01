myboot <- function(data, statisticfun, ..., nboot){
  boot <- sapply(1:nboot, function(x) statisticfun(data = dplyr::slice_sample(data, n = nrow(data), replace = TRUE), ...))
  valid_boot <- boot[!is.na(boot)]
  valid_boot
}

jackknife <- function(data,  statisticfun, ...)
{
  jack <- sapply(seq.int(from = 1, to = nrow(data)), 
                 function(x) statisticfun(data = data[-x, , drop = FALSE], ...))
  valid_jack <- jack[!is.na(jack)]
  valid_jack
}

bootbca <- function(boot, jack, alpha = 0.05, estimate){
  z0 <- qnorm(p = mean(boot < estimate), mean = 0, sd = 1)
  
  jackmean <- mean(jack)
  jacksd <- sd(jack)
  statcentered <- jackmean - jack
  statcenteredsq <- (statcentered)^2
  statcenteredthree <- (statcentered)^3
  sumthree <- sum(statcenteredthree)
  sumsq <- sum(statcenteredsq)
  accelerator <- sumthree / (6*sumsq^(3/2))
  
  zalpha <- qnorm(p = alpha/2)
  zalpha2 <- qnorm(p = 1-(alpha/2))
  a1 <- pnorm(q = (z0 + (z0 + zalpha)/(1-accelerator*(z0+zalpha))), sd = 1, mean = 0)
  a2 <- pnorm(q = (z0 + (z0 + zalpha2)/(1-accelerator*(z0+zalpha2))), sd = 1, mean = 0)
  
  lowerbound <- unname(quantile(boot, probs = a1))
  upperbound <- unname(quantile(boot, probs = a2))
  list(lowerbound = lowerbound, upperbound = upperbound)
}

bootbc <- function(boot, alpha = 0.05, estimate){
  z0 <- qnorm(p = mean(boot < estimate), mean = 0, sd = 1)
  
  zalpha <- qnorm(p = alpha/2, mean = 0, sd = 1)
  zalpha2 <- qnorm(p = 1-(alpha/2), mean = 0, sd = 1)
  
  bc1 <- pnorm(q = 2 * z0 + zalpha, sd = 1, mean = 0)
  bc2 <- pnorm(q = 2 * z0 + zalpha2, sd = 1, mean = 0)
  lowerbound <- unname(quantile(boot, probs = bc1))
  upperbound <- unname(quantile(boot, probs = bc2))
  
  list(lowerbound = lowerbound, upperbound = upperbound)
}





