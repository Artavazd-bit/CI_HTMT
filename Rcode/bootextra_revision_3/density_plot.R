library(ggplot2)
library(tikzDevice)
library(dplyr)
library(moments)

dl <- readRDS("results/results_bootextra/run_2026_11_11/datasets/df_task_00001.rds")
dl[[1]]$boot                 # bootstrap HTMT values of replication 1
plot(density(dl[[1]]$boot))  # its bootstrap density

RESULTS_DIR <- "results/results_bootextra/run_2026_11_12"

data_dir <- file.path(RESULTS_DIR, "datasets")

files <- list.files(data_dir, pattern = "\\.rds$", full.names = TRUE)

boot_long <- do.call(rbind, lapply(files, function(f) {
  dl <- readRDS(f)
  do.call(rbind, lapply(dl, function(r) {
    if (length(r$boot) == 0L) return(NULL)
    data.frame(condition_id = r$condition_id,
               rep_id       = (r$rep_batch - 1L) * 100L + r$rep_in_batch, 
               htmt_boot    = r$boot)
  }))
}))

skew_stats <- skew %>%
              group_by(condition_id) %>%
              summarize(mean = mean(skewness), median = median(skewness))

skew <- boot_long %>%
  filter(condition_id %in% c(111, 121)) %>%
  group_by(condition_id, rep_id) %>%
  summarize(skewness = moments::skewness(htmt_boot), .groups = "drop") %>%
  mutate(condition_id = factor(condition_id))


skewdensplots <- ggplot(skew, aes(skewness)) +
  geom_density() +
  facet_wrap(~ condition_id, ncol = 1, scales = "free",
             labeller = as_labeller(c(`111` = "$n = 25, \\phi = 1$, Severly Non-Normal",
                                      `121` = "$n = 400, \\phi = 1$, Normal"))) +
  geom_vline(data = skew_stats, aes(xintercept = mean), linetype = "dashed") +
  geom_vline(data = skew_stats, aes(xintercept = median), linetype = "dotted") +
  labs(x = "Skewness of the bootstrap HTMT distribution", y = "Density") + 
  theme_minimal()

tikz(file = sprintf("outputs/plots/skewdensplots.tex", OUTDIR), width=8.4, height=5.2)
print(skewdensplots)
endoffile <- dev.off()




