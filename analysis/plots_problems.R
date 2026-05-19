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
# so the eye locks onto the few problematic conditions.
make_problems_heatmap <- function(d) {
  ggplot(d, aes(x = as.factor(n), y = method2, fill = pct_problem)) +
    geom_tile(colour = "grey85") +
    geom_text(aes(label = ifelse(pct_problem > 0,
                                 sprintf("%.1f", pct_problem), "")),
              size = 2.5) +
    facet_grid(rows = vars(dtype), cols = vars(correlation_lbl),
               labeller = labeller(correlation_lbl = label_parsed,
                                   dtype = dtype_labeller)) +
    scale_fill_gradient(name = "Inadmissible rate (%)",
                        low = "white", high = "firebrick",
                        limits = c(0, NA)) +
    scale_y_discrete(limits = rev) +
    labs(x = "Sample size", y = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_blank())
}

p_heat <- make_problems_heatmap(problems_plot)
ggsave(file.path(OUT_DIR, "problems_heatmap.png"),
       plot = p_heat, width = 15.375, height = 9.15625)
