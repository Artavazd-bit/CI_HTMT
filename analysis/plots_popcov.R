library(ggplot2)
library(patchwork)

# Expects `resag` to be loaded already with columns:
#   correlation (formatted as "Phi == 0.70"), n, dtype ("normal"/"moderate"/"severe"),
#   method2 (factor), upperwithin, lowerwithin (both percentages 0-100)

dir.create("plots", showWarnings = FALSE)

NOMINAL <- 97.5  # one-sided nominal coverage (alpha = 0.05 fixed in ci_battery)

make_popcov_plot <- function(resag, dtype_sel, lowertick) {
  d <- resag[resag$dtype == dtype_sel, ]
  y_breaks <- c(seq(lowertick, 100, by = 5), NOMINAL)

  p_upper <- ggplot(d, aes(x = as.factor(n), y = upperwithin, group = method2)) +
    geom_line(aes(linetype = method2)) +
    geom_point(aes(shape = method2)) +
    facet_grid(cols = vars(correlation), labeller = label_parsed) +
    geom_hline(yintercept = NOMINAL) +
    scale_y_continuous(name = "Pop. correlation value below upper limit (%)",
                       breaks = y_breaks, limits = c(lowertick, 100)) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())

  p_lower <- ggplot(d, aes(x = as.factor(n), y = lowerwithin, group = method2)) +
    geom_line(aes(linetype = method2)) +
    geom_point(aes(shape = method2)) +
    facet_grid(cols = vars(correlation), labeller = label_parsed) +
    geom_hline(yintercept = NOMINAL) +
    scale_y_reverse(name = "Pop. correlation value above lower limit (%)",
                    breaks = y_breaks, limits = c(100, lowertick)) +
    theme_minimal() +
    theme(legend.position = "bottom", strip.text.x = element_blank()) +
    labs(x = "Sample size") +
    scale_linetype_discrete(name = "Type of CI:") +
    scale_shape_discrete(name = "Type of CI:")

  p_upper / p_lower
}

popcov_specs <- list(
  list(dtype = "severe",   lowertick = 60, file = "plots/popcov_severe.png"),
  list(dtype = "moderate", lowertick = 65, file = "plots/popcov_moderate.png"),
  list(dtype = "normal",   lowertick = 85, file = "plots/popcov_normal.png")
)

for (s in popcov_specs) {
  p <- make_popcov_plot(resag, s$dtype, s$lowertick)
  ggsave(s$file, plot = p, width = 15.375, height = 9.15625)
}
