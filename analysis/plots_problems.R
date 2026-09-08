library(ggplot2)

if (!exists("problems", inherits = FALSE)) source("analysis/prep_problems.R")

OUT_DIR <- "outputs/plots"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

dtype_labeller <- c(normal   = "normal",
                    moderate = "moderately non-normal",
                    severe   = "severely non-normal")



problems_plot <- problems
problems_plot$dtype <- factor(problems_plot$dtype,
                              levels = c("normal", "moderate", "severe"))

# Heatmap: same facet layout, fill = failure rate. Cells with 0% stay white
make_problems_heatmap <- function(d) {
  ggplot(d, aes(x = as.factor(n), y = method2, fill = pct_problem)) +
    geom_tile(colour = "grey85") +
    geom_text(aes(label = ifelse(pct_problem > 0,
                                 sprintf("%.1f", pct_problem), "")),
              size = 2.4) +
    facet_grid(rows = vars(correlation_lbl), cols = vars(dtype),
               labeller = labeller(correlation_lbl = label_parsed,
                                   dtype = dtype_labeller)) +
    scale_fill_gradient(name = "Inadmissible rate (%)",
                        low = "white", high = "firebrick",
                        limits = c(0, NA)) +
    scale_y_discrete(limits = rev) +
    labs(x = "Sample size", y = NULL) +
    theme_minimal(base_size = 9) +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          axis.text.y = element_text(size = 7),                       # NEW
          axis.text.x = element_text(angle = 45, hjust = 1))          # NEW
}

p_heat <- make_problems_heatmap(problems_plot)
ggsave(file.path(OUT_DIR, "problems_heatmap.png"),
       plot = p_heat, width = 8.4, height = 5.2, dpi = 300)
