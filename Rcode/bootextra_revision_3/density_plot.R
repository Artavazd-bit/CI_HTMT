RESULTS_DIR <- "results/results_bootextra"

data_dir <- file.path(RESULTS_DIR, "datasets")

names <- list.files(data_dir, pattern = "\\.rds$", full.names = TRUE)

boot <- list(0)
for(ii in names){
  ls_all <- readRDS(ii)
  for(jj in length(ls_all)){
    boot[[ii]][[jj]] <- ls_all[[jj]]$boot 
  }
}

ls_all <- readRDS("results/results_bootextra/datasets/df_task_00001.rds")
length(ls_all)


dl <- readRDS("results/results_bootextra/datasets/df_task_00001.rds")
dl[[1]]$boot                 # bootstrap HTMT values of replication 1
plot(density(dl[[1]]$boot))  # its bootstrap density

All files into one long data frame

files <- list.files("results/results_bootextra/datasets",
                    pattern = "^df_task_\\d+\\.rds$", full.names = TRUE)

boot_long <- do.call(rbind, lapply(files, function(f) {
  dl <- readRDS(f)
  do.call(rbind, lapply(dl, function(r) {
    if (length(r$boot) == 0L) return(NULL)
    data.frame(condition_id = r$condition_id,
               rep_id       = (r$rep_batch - 1L) * 100L + r$rep_in_batch,  # 1..1000 per condition
               htmt_boot    = r$boot)
  }))
}))

Use condition_id to separate the two conditions: 111 is φ = 1, n = 25, severe, and 121 is φ = 1, n = 400, normal. The task_id stored in the list is only the SLURM index 1–20.

One density per replication, drawn as a faint line

library(ggplot2)

plot_bootdens <- function(cid, from, to, ymax = NULL, alpha = 0.04) {
  d  <- boot_long[boot_long$condition_id == cid, ]
  sp <- split(d$htmt_boot, d$rep_id)          # 1000 vectors, one per replication
  
  dens <- do.call(rbind, lapply(names(sp), function(id) {
    k <- density(sp[[id]], from = from, to = to, n = 256)
    data.frame(rep_id = id, x = k$x, y = k$y)
  }))
  
  p <- ggplot(dens, aes(x = x, y = y, group = rep_id)) +   # group = one curve per replication
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
