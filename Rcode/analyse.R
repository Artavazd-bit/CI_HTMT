source("Rcode/cfa.R")
source("Rcode/HTMT.R")
source("Rcode/boot.R")

ci_battery <- function(data, nboot = 1000, nindicator = 3, alpha = 0.05){
  
  cfa_res <- cfa_one(data, alpha = alpha)
  htmt_dm <- HTMTDM(data, nindicator = nindicator, alpha = alpha)
  htmt_pe <- htmt_dm$htmt
  
  b <- myboot(data, HTMT, nindicator = nindicator, nboot = nboot)
  j <- jackknife(data, HTMT, nindicator = nindicator)
  
  ci_perc <- list(lowerbound = unname(quantile(b, alpha / 2)),
                  upperbound = unname(quantile(b, 1 - alpha / 2))
                  )
  
  ci_bc <- bootbc(b, alpha = alpha, estimate = htmt_pe)
  ci_bca <- bootbca(b, j, alpha = alpha, estimate = htmt_pe)
  
  ci <- data.frame(
    estimator = c("cfa", "htmt", "htmt", "htmt", "htmt"),
    method = c("wald_cfa", "delta", "perc", "bc", "bca"),
    estimate = c(cfa_res$cor_est, htmt_pe, htmt_pe, htmt_pe, htmt_pe),
    lowerbound = c(cfa_res$lowerbound, htmt_dm$lowerbound, ci_perc$lowerbound, ci_bc$lowerbound, ci_bca$lowerbound),
    upperbound = c(cfa_res$upperbound, htmt_dm$upperbound, ci_perc$upperbound, ci_bc$upperbound, ci_bca$upperbound),
    stringsAsFactors = FALSE
  )
  ci
}
