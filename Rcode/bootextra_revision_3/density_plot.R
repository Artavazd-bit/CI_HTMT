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

plot_bootdens <- function(cid, from, to, ymax = NULL, alpha = 0.04) {
  d  <- boot_long[boot_long$condition_id == cid, ]
  sp <- split(d$htmt_boot, d$rep_id)       
   
  dens <- do.call(rbind, lapply(names(sp), function(id) {
    k <- density(sp[[id]], from = from, to = to)
    data.frame(rep_id = id, x = k$x, y = k$y)
  }))
  
  p <- ggplot(dens, aes(x = x, y = y, group = rep_id)) +   
    geom_line(alpha = alpha, linewidth = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    labs(x = "Bootstrap HTMT", y = "Density") +
    theme_minimal()
  if (!is.null(ymax)) p <- p + coord_cartesian(ylim = c(0, ymax))
  p
}
p121 <- plot_bootdens(121, from = 0.92, to = 1.08)                        # n = 400, normal
p111 <- plot_bootdens(111, from = 0.4,  to = 2.2, ymax = 10, alpha = 0.06) # n = 25, severe

ggsave("outputs/plots/bootdens_n400_normal.png", p121, width = 5, height = 3.5, dpi = 600)
ggsave("outputs/plots/bootdens_n25_severe.png",  p111, width = 5, height = 3.5, dpi = 600)

OUTDIR <- "outputs/plots/"

tikz(file = sprintf("bootdens_n400_normal.tex", OUTDIR), width=8.4, height=5.2)
print(p121)
endoffile <- dev.off()

tikz(file = sprintf("bootdens_n25_severe.tex", OUTDIR), width=8.4, height=5.2)
print(p111)
endoffile <- dev.off()


plot_bootdens1 <- function(cid, from, to, ymax = NULL) {
  d    <- boot_long[boot_long$condition_id == cid, ]
  grid <- seq(from, to, length.out = 256)
  dens <- sapply(split(d$htmt_boot, d$rep_id), function(x)
    density(x, from = from, to = to, n = length(grid))$y)
  q <- t(apply(dens, 1, quantile, c(.05, .25, .5, .75, .95)))
  band <- data.frame(x = grid, q)
  names(band) <- c("x", "q05", "q25", "q50", "q75", "q95")
  ggplot(band, aes(x)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "grey85") +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "grey60") +
    geom_line(aes(y = q50)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    coord_cartesian(ylim = if (!is.null(ymax)) c(0, ymax)) +
    labs(x = "Bootstrap HTMT", y = "Density")
}

p121 <- plot_bootdens1(121, from = 0.92, to = 1.08)                        # n = 400, normal
p111 <- plot_bootdens1(111, from = 0.4,  to = 2.2, ymax = 10) # n = 25, severe

OUTDIR <- "outputs/plots/"

tikz(file = sprintf("bootdens_n400_normal.tex", OUTDIR), width=8.4, height=5.2)
print(p121)
endoffile <- dev.off()

tikz(file = sprintf("bootdens_n25_severe.tex", OUTDIR), width=8.4, height=5.2)
print(p111)
endoffile <- dev.off()



skew <- boot_long %>%
  filter(condition_id %in% c(111, 121)) %>%
  group_by(condition_id, rep_id) %>%
  summarize(skewness = moments::skewness(htmt_boot), .groups = "drop") %>%
  mutate(condition_id = factor(condition_id))

skew_stats <- skew %>%
  group_by(condition_id) %>%
  summarize(mean = mean(skewness), median = median(skewness))

plot_skew <- ggplot(skew, aes(skewness, colour = condition_id)) +
  geom_density() +
  geom_vline(data = skew_stats, aes(xintercept = mean, colour = condition_id),
             linetype = "dashed") +
  geom_vline(data = skew_stats, aes(xintercept = median, colour = condition_id),
             linetype = "dotted") +
  labs(x = "Skewness of the bootstrap HTMT distribution", y = "Density",
       colour = "Condition")
  #coord_cartesian(ylim = c(0, 0.1))


plot_skew2<- ggplot(skew, aes(skewness)) +
  geom_density() +
  geom_vline(data = skew_stats, aes(xintercept = mean), linetype = "dashed") +
  geom_vline(data = skew_stats, aes(xintercept = median), linetype = "dotted") +
  facet_wrap(~ condition_id, ncol = 1) +
  labs(x = "Skewness of the bootstrap HTMT distribution", y = "Density")

skew1 <- skew[skew$condition_id == 111,]

ggplot(skew1, aes(skewness)) + 
  geom_density()

skew2 <- skew[skew$condition_id == 121,]
ggplot(skew2, aes(skewness)) + 
  geom_density()


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




