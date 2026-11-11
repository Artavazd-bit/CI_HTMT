# Re-run bootstrap on stored datasets to inspect skewness/bias of HTMT bootstrap distribution
setwd("/mnt/Forschung/CI_HTMT")
source("Rcode/HTMT.R")
cond <- readRDS("conditions.rds")
sel <- cond[cond$n %in% c(25, 50, 100, 400, 1600) & cond$correlation %in% c(0.7, 1.0) &
            cond$dtype %in% c("normal", "severe") & cond$rep_batch <= 3, ]   # 300 runs per condition
skew <- function(x) { m <- mean(x); s <- sd(x); mean((x - m)^3) / s^3 }
set.seed(1)
B <- 1000L
out <- list()
for (t in sel$task_id) {
  dl <- readRDS(sprintf("results/results_2026_05_14/datasets/df_task_%05d.rds", t))
  for (r in dl) {
    d <- r$data; if (is.null(d)) next
    est <- suppressWarnings(HTMT(d, nindicator = 3))
    if (!is.finite(est)) next
    n <- nrow(d)
    boots <- vapply(seq_len(B), function(b) suppressWarnings(HTMT(d[sample.int(n, n, TRUE), , drop = FALSE], nindicator = 3)), numeric(1))
    boots <- boots[is.finite(boots)]
    out[[length(out) + 1]] <- data.frame(task_id = t, n = n, phi = sel$correlation[sel$task_id == t],
      dtype = sel$dtype[sel$task_id == t], est = est, nvalid = length(boots),
      bias = mean(boots) - est, skew = skew(boots), med_minus_mean = median(boots) - mean(boots),
      p_below = mean(boots < est))
  }
}
res <- do.call(rbind, out)
saveRDS(res, "results/revision/bootskew_rev3.rds")
agg <- aggregate(cbind(skew, bias, p_below) ~ dtype + phi + n, data = res, FUN = function(x) round(median(x), 3))
agg$share_skew_gt_0.5 <- aggregate(skew ~ dtype + phi + n, data = res, FUN = function(x) round(mean(x > 0.5), 2))$skew
agg$share_bias_pos <- aggregate(bias ~ dtype + phi + n, data = res, FUN = function(x) round(mean(x > 0), 2))$bias
agg$runs <- aggregate(skew ~ dtype + phi + n, data = res, FUN = length)$skew
print(agg[order(agg$dtype, agg$phi, agg$n), ], row.names = FALSE)



library(dplyr)
library(moments)
source("analysis/prep_ci.R")

mom_data <- dfall2[, c("estimate", "correlation", "n", "dtype", )] 


skewnesstable <- dfall2 %>% 
  group_by(correlation, n, dtype, method, conf_level) %>% 
  summarize(mean = mean(estimate),
            sd = sd(estimate),
            skewness = skewness(estimate),
            kurtosis = kurtosis(estimate)
  )


table <- unique(skewnesstable[skewnesstable$conf_level == 0.9 & skewnesstable$method == "bc",])
