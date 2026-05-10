library(ggplot2)

# Expects `resag` to be loaded already with columns:
#   correlation (formatted as "Phi == 0.70"), n, dtype ("normal"/"moderate"/"severe"),
#   method2 (factor), covagoneag (coverage of phi=1 in %, 0-100),
#   conf_level (numeric, e.g. 0.90, 0.95, 0.99)

dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)

PRACTICAL_HLINE <- 80 # practical-significance reference for phi < 1
y_breaks <- seq(0, 100, by = 10)

for (cl in sort(unique(resag$conf_level))) {
  resag_cl           <- resag[resag$conf_level == cl, ]
  nominal_rej_at_one <- (1 - cl) * 100
  cl_tag             <- sprintf("cl%02d", round(cl * 100))

  for (dt in c("moderate", "normal", "severe")) {
    d <- resag_cl[resag_cl$dtype == dt, ]
    d$rejrate <- 100 - d$covagoneag
    d$hline   <- ifelse(d$correlation == "Phi == 1.00",
                        nominal_rej_at_one, PRACTICAL_HLINE)

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

    ggsave(sprintf("outputs/plots/rejrate_%s_%s.png", dt, cl_tag),
           plot = p, width = 15.375, height = 4.48)
  }
}
