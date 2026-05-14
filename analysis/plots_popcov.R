library(ggplot2)
library(patchwork)

# Expects `resag` to be loaded already with columns:
#   correlation (formatted as "Phi == 0.70"), n, dtype ("normal"/"moderate"/"severe"),
#   method2 (factor), upperwithin, lowerwithin (both percentages 0-100),
#   conf_level (numeric, e.g. 0.90, 0.95, 0.99)

dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)

# One-sided nominal coverage as a function of conf level cl:
#   nominal upper = cl + (1 - cl)/2 = (1 + cl) / 2
nominal_one_sided <- function(cl) (1 + cl) / 2 * 100

make_popcov_plot <- function(resag, dtype_sel, lowertick, nominal) {
  d <- resag[resag$dtype == dtype_sel, ]
  y_breaks <- c(seq(lowertick, 100, by = 5), nominal)

  p_upper <- ggplot(d, aes(x = as.factor(n), y = upperwithin, group = method2)) +
    geom_line(aes(linetype = method2)) +
    geom_point(aes(shape = method2)) +
    facet_grid(cols = vars(correlation), labeller = label_parsed) +
    geom_hline(yintercept = nominal) +
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
    geom_hline(yintercept = nominal) +
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
  list(dtype = "severe",   lowertick = 50, file_stem = "outputs/plots/popcov_severe"),
  list(dtype = "moderate", lowertick = 50, file_stem = "outputs/plots/popcov_moderate"),
  list(dtype = "normal",   lowertick = 50, file_stem = "outputs/plots/popcov_normal")
)

for (cl in sort(unique(resag$conf_level))) {
  resag_cl <- resag[resag$conf_level == cl, ]
  nominal  <- nominal_one_sided(cl)
  cl_tag   <- sprintf("cl%02d", round(cl * 100))
  for (s in popcov_specs) {
    p <- make_popcov_plot(resag_cl, s$dtype, s$lowertick, nominal)
    ggsave(sprintf("%s_%s.png", s$file_stem, cl_tag),
           plot = p, width = 15.375, height = 9.15625)
  }
}

