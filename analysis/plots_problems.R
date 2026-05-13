library(dplyr)
library(ggplot2)

if (!exists("problem_estimates", inherits = FALSE)) source("analysis/prep_problems.R")

OUT_DIR <- "outputs/plots"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Collapse HTMT's four identical method rows to one (delta/perc/bc/bca share a
# single point estimate); keep CFA's ML and MLR as separate series since they
# fail independently. Unlike `pe` in tables_problems.R we keep zero-failure
# rows so each series spans the full sample-size axis.
pe_plot <- problem_estimates %>%
  mutate(method = ifelse(estimator == "htmt", NA_character_, method)) %>%
  distinct(correlation, n, dtype, estimator, method, pct_failed) %>%
  mutate(
    series = factor(case_when(
      estimator == "htmt"         ~ "HTMT",
      method    == "wald_cfa"        ~ "CFA (ML)",
      method    == "wald_cfa_robust" ~ "CFA (MLR)"
    ), levels = c("HTMT", "CFA (ML)", "CFA (MLR)")),
    correlation = factor(sprintf("Phi == %.2f", correlation)),
    dtype       = factor(dtype, levels = c("normal", "moderate", "severe"))
  )

p <- ggplot(pe_plot, aes(x = as.factor(n), y = pct_failed, group = series)) +
  geom_line(aes(linetype = series)) +
  geom_point(aes(shape = series)) +
  facet_grid(rows = vars(dtype), cols = vars(correlation),
             labeller = labeller(
               correlation = label_parsed,
               dtype = c(normal   = "normal",
                         moderate = "moderately non-normal",
                         severe   = "severely non-normal")
             )) +
  scale_y_continuous(name = "Estimate failure rate (%)") +
  labs(x = "Sample size") +
  scale_linetype_discrete(name = "Estimator:") +
  scale_shape_discrete(name = "Estimator:") +
  theme(legend.position = "bottom")

ggsave(file.path(OUT_DIR, "problem_estimates.png"),
       plot = p, width = 15.375, height = 9.15625)
