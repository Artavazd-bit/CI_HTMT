library(ggplot2)

# Expects `inadm` to be loaded already with columns:
#   dtype ("normal"/"moderate"/"severe"), n,
#   correlation (formatted as "Phi == 0.70"), n2 (formatted as "n == 25"),
#   missing (= NBOOT - n_boot_valid; integer count of failed bootstrap replications)

dir.create("plots", showWarnings = FALSE)

NBOOT         <- 1000  # matches sim.R
REPS_PER_CELL <- 1000  # 100 reps_per_task * 10 rep_batch per (corr, n, dtype)

make_inadmis_plot <- function(inadm, dtypes_keep, n_keep,
                              nboot = NBOOT, reps_per_cell = REPS_PER_CELL) {
  d <- inadm[inadm$dtype %in% dtypes_keep &
             inadm$n == n_keep &
             !is.na(inadm$missing) &
             inadm$missing > 0, ]

  ggplot(d, aes(x = missing)) +
    geom_histogram(aes(y = after_stat((count / reps_per_cell) * 100)),
                   breaks = c(1, seq(from = 100, to = nboot, by = 100))) +
    facet_grid(cols = vars(correlation), rows = vars(n2), labeller = label_parsed) +
    theme(legend.position = "bottom") +
    scale_y_continuous(name = "Relative frequency [%]") +
    scale_x_continuous(name = "Number of non-calculable bootstrap HTMTs")
}

inadmis_specs <- list(
  list(dtypes = "normal",                file = "plots/normalinadmis.png"),
  list(dtypes = c("moderate", "severe"), file = "plots/nonnormalinadmis.png")
)

for (s in inadmis_specs) {
  p <- make_inadmis_plot(inadm, s$dtypes, n_keep = 25)
  ggsave(s$file, plot = p, width = 12.375, height = 4.48)
}
