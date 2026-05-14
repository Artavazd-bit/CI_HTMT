library(ggplot2)

if (!exists("problems", inherits = FALSE)) source("analysis/prep_problems.R")

OUT_DIR <- "outputs/plots"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

dtype_labeller <- c(normal   = "normal",
                    moderate = "moderately non-normal",
                    severe   = "severely non-normal")

make_problems_plot <- function(d) {
  ggplot(d, aes(x = as.factor(n), y = pct_problem, group = method2)) +
    geom_line(aes(linetype = method2)) +
    geom_point(aes(shape = method2)) +
    facet_grid(rows = vars(dtype), cols = vars(correlation_lbl),
               labeller = labeller(correlation_lbl = label_parsed,
                                   dtype = dtype_labeller)) +
    scale_y_continuous(name = "Estimate or bound failure rate (%)") +
    labs(x = "Sample size") +
    scale_linetype_discrete(name = "Type of CI:") +
    scale_shape_discrete(name = "Type of CI:") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

problems_plot <- problems
problems_plot$dtype <- factor(problems_plot$dtype,
                              levels = c("normal", "moderate", "severe"))

for (cl in sort(unique(problems_plot$conf_level))) {
  d <- problems_plot[problems_plot$conf_level == cl, ]
  p <- make_problems_plot(d)
  cl_tag <- sprintf("cl%02d", round(cl * 100))
  ggsave(file.path(OUT_DIR, sprintf("problems_%s.png", cl_tag)),
         plot = p, width = 15.375, height = 9.15625)
}
