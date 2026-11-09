library(ggplot2)
library(patchwork)
library(tikzDevice)
# Expects `resag` to be loaded already with columns:
#   correlation (formatted as "Phi == 0.70"), n, dtype ("normal"/"moderate"/"severe"),
#   method2 (factor), upperwithin, lowerwithin (both percentages 0-100),
#   conf_level (numeric, e.g. 0.90, 0.95, 0.99)

dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)

size_scale_line <- 0.55
size_scale_point <- 0.8
# colour-blind-safe palette (Okabe-Ito), in factor order of method2:
# Percentile, Asymptotic, BCa, BC, CFA-ML, CFA-MLR
ci_colours <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7")
# One-sided nominal coverage as a function of conf level cl:
#   nominal upper = cl + (1 - cl)/2 = (1 + cl) / 2
nominal_one_sided <- function(cl) (1 + cl) / 2 * 100

make_popcov_plot <- function(resag, dtype_sel, lowertick, nominal) {
  d <- resag[resag$dtype == dtype_sel, ]
  # Tight, data-driven lower limit: nearest multiple of 5 below the smallest
  # value in either panel (the `lowertick` argument is only a floor for it).
  lowertick <- max(lowertick, floor(min(c(d$upperwithin, d$lowerwithin)) / 5) * 5)
  y_breaks  <- unique(c(lowertick, seq(ceiling(lowertick / 10) * 10, 90, by = 10),
                        95, nominal))

  p_upper <- ggplot(d, aes(x = as.factor(n), y = upperwithin, group = method2)) +
    geom_line(aes(linetype = method2, colour = method2), linewidth = size_scale_line) +
    geom_point(aes(shape = method2, colour = method2), size = size_scale_point) +
    facet_grid(cols = vars(correlation)) +
    geom_hline(yintercept = nominal) +
    scale_colour_manual(values = ci_colours) +
    scale_y_continuous(name = "Pop. Correlation Value\nBelow Upper Limit (\\%)",
                       breaks = y_breaks, limits = c(lowertick, 100), labels = as.character) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())

  p_lower <- ggplot(d, aes(x = as.factor(n), y = lowerwithin, group = method2)) +
    geom_line(aes(linetype = method2, colour = method2), linewidth = size_scale_line) +
    geom_point(aes(shape = method2, colour = method2), size = size_scale_point) +
    facet_grid(cols = vars(correlation)) +
    geom_hline(yintercept = nominal) +
    scale_y_reverse(name = "Pop. Correlation Aalue\nAbove Lower Limit (\\%)",
                    breaks = y_breaks, limits = c(100, lowertick), labels = as.character) +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.text.x = element_blank(),
          axis.text.x  = element_text(angle = 45, hjust = 1)) +
    labs(x = "Sample Size") +
    scale_linetype_discrete(name = "Type of CI:") +
    scale_shape_discrete(name = "Type of CI:") +
    scale_colour_manual(name = "Type of CI:", values = ci_colours) +
    guides(linetype = guide_legend(nrow = 1),
           shape    = guide_legend(nrow = 1),
           colour   = guide_legend(nrow = 1))

  p_upper / p_lower
}

# `lowertick` is a floor; the actual lower limit is derived from the data per
# figure (see make_popcov_plot), e.g. 85/70/65 for the 95% CIs.
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
    tikz(file = sprintf("%s_%s.tex", s$file_stem, cl_tag), width=8.4, height=5.2)
    p <- make_popcov_plot(resag_cl, s$dtype, s$lowertick, nominal)
    print(p)
    endoffile <- dev.off() 
    #ggsave(sprintf("%s_%s.png", s$file_stem, cl_tag),
           #plot = p, width = 8.4, height = 5.2, dpi = 300)
  }
}

