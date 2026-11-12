library(ggplot2)

dl <- readRDS("results/results_bootextra/datasets/df_task_00001.rds")
dl[[1]]$boot                 # bootstrap HTMT values of replication 1
plot(density(dl[[1]]$boot))  # its bootstrap density

RESULTS_DIR <- "results/results_bootextra"

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
