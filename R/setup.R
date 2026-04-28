
generate_data <- function(n, beta, sigma = 1) {
  x <- rnorm(n)
  y <- beta * x + rnorm(n, sd = sigma)
  data.frame(x = x, y = y)
}

analyse_one <- function(n, beta, sigma = 1) {
  d   <- generate_data(n, beta, sigma)
  fit <- lm(y ~ x, data = d)
  ci  <- confint(fit, "x", level = 0.95)
  data.frame(
    n        = n,
    beta     = beta,
    bhat     = unname(coef(fit)["x"]),
    ci_lo    = ci[1],
    ci_hi    = ci[2],
    covered  = ci[1] <= beta & beta <= ci[2]
  )
}