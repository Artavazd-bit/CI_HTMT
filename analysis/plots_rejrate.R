library(ggplot2)

# Expects `resag` to be loaded already with columns:
#   correlation (formatted as "Phi == 0.70"), n, dtype ("normal"/"moderate"/"severe"),
#   method2 (factor), covagoneag (coverage of phi=1 in %, 0-100)

dir.create("plots", showWarnings = FALSE)

NOMINAL_REJ_AT_ONE <- 5  # alpha * 100, nominal rejection rate when phi == 1
PRACTICAL_HLINE    <- 80 # practical-significance reference for phi < 1

y_breaks <- seq(0, 100, by = 10)

for (dt in c("moderate", "normal", "severe")) {
  d <- resag[resag$dtype == dt, ]
  d$rejrate <- 100 - d$covagoneag
  d$hline   <- ifelse(d$correlation == "Phi == 1.00", NOMINAL_REJ_AT_ONE, PRACTICAL_HLINE)

  p <- ggplot(d, aes(x = as.factor(n), y = rejrate, group = method2)) +
    geom_line(aes(linetype = method2)) +
    geom_point(aes(shape = method2)) +
    facet_grid(cols = vars(correlation), labeller = label_parsed) +
    geom_hline(data = d, aes(yintercept = hline)) +
    scale_y_continuous(breaks = y_breaks, name = "Rejection rate") +
    theme(legend.position = "bottom") +
    labs(x = "Sample size") +
    scale_linetype_discrete(name = "Type of CI:") +
    scale_shape_discrete(name = "Type of CI:")

  ggsave(paste0("plots/rejrate_", dt, ".png"),
         plot = p, width = 12.375, height = 4.48)
}
