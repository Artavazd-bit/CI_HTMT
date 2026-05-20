library(ggplot2)

# expects output from prep_ci

dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)

OUTDIR <- "outputs/plots/rejrate"

PRACTICAL_HLINE <- 80 # practical-significance reference for phi < 1
y_breaks <- seq(0, 100, by = 10)

dtype_labeller <- c(normal   = "normal",
                    moderate = "moderately non-normal",
                    severe   = "severely non-normal")

resag$dtype2 <- factor(resag$dtype,
                              levels = c("normal", "moderate", "severe"))

for (cl in sort(unique(resag$conf_level))) {
  resag_cl           <- resag[resag$conf_level == cl, ]
  nominal_rej_at_one <- (1 - cl) * 100
  cl_tag             <- sprintf("cl%02d", round(cl * 100))

  d <- resag_cl
  d$rejrate <- 100 - d$covagoneag
  d$hline   <- ifelse(d$correlation == "Phi == 1.00",
                      nominal_rej_at_one, PRACTICAL_HLINE)
  
  p <- ggplot(d, aes(x = as.factor(n), y = rejrate, group = method2)) +
    geom_line(aes(linetype = method2)) +
    geom_point(aes(shape = method2)) +
    facet_grid(rows = vars(dtype2), cols = vars(correlation),
               labeller = labeller(correlation = label_parsed,
                                   dtype2 = dtype_labeller)) +
    geom_hline(data = d, aes(yintercept = hline)) +
    scale_y_continuous(breaks = y_breaks, name = "Rejection rate (%)") +
    theme(legend.position = "bottom") +
    labs(x = "Sample size") +
    scale_linetype_discrete(name = "Type of CI:") +
    scale_shape_discrete(name = "Type of CI:") + 
    guides(linetype = guide_legend(nrow = 1),
           shape = guide_legend(nrow=1))
  
  ggsave(sprintf("%s_%s.png", OUTDIR, cl_tag),
         plot = p, width = 15.375, height = 9.15625)
}

