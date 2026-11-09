library(ggplot2)
library(patchwork)
library(tikzDevice)

# expects output from prep_ci

dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)

OUTDIR <- "outputs/plots/rejrate"

PRACTICAL_HLINE <- 80 # practical-significance reference for phi < 1
y_breaks <- seq(0, 100, by = 20)

size_scale_line <- 0.55
size_scale_point <- 0.8
# colour-blind-safe palette (Okabe-Ito), in factor order of method2:
# Percentile, Asymptotic, BCa, BC, CFA-ML, CFA-MLR
ci_colours <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7")

dtype_labeller <- c(normal   = "Normal",
                    moderate = "Moderately\nNon-Normal",
                    severe   = "Severely\nNon-Normal")

resag$dtype2 <- factor(resag$dtype,
                              levels = c("normal", "moderate", "severe"))

for (cl in sort(unique(resag$conf_level))) {
  resag_cl           <- resag[resag$conf_level == cl, ]
  nominal_rej_at_one <- (1 - cl) * 100
  cl_tag             <- sprintf("cl%02d", round(cl * 100))

  d <- resag_cl
  d$rejrate <- 100 - d$covagoneag
  d$hline   <- ifelse(d$correlation == "$\\phi = 1.00 $",
                      nominal_rej_at_one, PRACTICAL_HLINE)
  
  p <- ggplot(d, aes(x = as.factor(n), y = rejrate, group = method2)) +
    geom_line(aes(linetype = method2, colour = method2), linewidth = size_scale_line) +
    geom_point(aes(shape = method2, colour = method2), size = size_scale_point) +
    facet_grid(rows = vars(dtype2), cols = vars(correlation),
               labeller = labeller(dtype2 = dtype_labeller)) +
    geom_hline(data = d, aes(yintercept = hline)) +
    theme_minimal() + 
    scale_y_continuous(breaks = y_breaks, name = "Rejection Rate (\\%)") +
    theme(legend.position = "bottom", 
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Sample Size") +
    scale_linetype_discrete(name = "Type of CI:") +
    scale_shape_discrete(name = "Type of CI:") +
    scale_colour_manual(name = "Type of CI:", values = ci_colours) +
    guides(linetype = guide_legend(nrow = 1),
           shape    = guide_legend(nrow = 1),
           colour   = guide_legend(nrow = 1))
  
  tikz(file = sprintf("%s_%s.tex", OUTDIR, cl_tag), width=8.4, height=5.2)
  print(p)
  endoffile <- dev.off()
  #ggsave(sprintf("%s_%s.png", OUTDIR, cl_tag),
  #       plot = p, width = 8.4, height = 5.2, dpi = 300)
  
}

