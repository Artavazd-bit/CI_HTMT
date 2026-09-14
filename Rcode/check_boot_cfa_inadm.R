library(dplyr)
library(ggplot2)
library(patchwork)

if (!exists("RESULTS_DIR", inherits = TRUE)) RESULTS_DIR <- "results/results_2026_05_14"
CI_DIR  <- file.path(RESULTS_DIR, "ci")
ERR_DIR <- file.path(RESULTS_DIR, "errors")

ci_files <- list.files(CI_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(ci_files) == 0L) stop("No .rds files found in CI_DIR: ", CI_DIR)

err_files <- list.files(ERR_DIR, pattern = "\\.rds$", full.names = TRUE)
if (length(err_files) == 0L) stop("No .rds files found in ERR_DIR: ", ERR_DIR)

dfall  <- do.call(rbind, lapply(ci_files,  readRDS))
errall <- do.call(rbind, lapply(err_files, readRDS))

message(sprintf("Loaded %d CI rows from %d task files (%d unique task_ids).",
                nrow(dfall), length(ci_files), length(unique(dfall$task_id))))

# Map CI method -> estimator group so the join key matches errall$estimator.
ci_estimator_grp <- c(wald_cfa = "cfa", wald_cfa_robust = "cfa_robust",
                      delta = "htmt", perc = "htmt", bc = "htmt", bca = "htmt")
dfall$estimator_grp <- ci_estimator_grp[dfall$method]

# Left-join errors onto CI on (task_id, rep_in_batch, estimator_grp). One-to-many.
err_join <- errall[errall$scope == "uncon" | is.na(errall$scope), c("task_id", "rep_in_batch", "estimator",
                                                                    "error_message", "warning_message", 
                                                                    "n_boot_valid", "n_jack_valid")]
err_join <- dplyr::rename(err_join, estimator_grp = estimator)
# left join
dfall <- merge(dfall, err_join,
               by = c("task_id", "rep_in_batch", "estimator_grp"),
               all.x = TRUE, sort = FALSE)

dfall$upperwithin <- dfall$correlation < dfall$upperbound
dfall$lowerwithin <- dfall$correlation > dfall$lowerbound
dfall$coverageone <- (1 > dfall$lowerbound) & (1 < dfall$upperbound)

err_clean <- ifelse(dfall$estimator_grp == "htmt", ifelse(dfall$method == "delta", TRUE, 
                    is.na(dfall$error_message) & dfall$n_boot_valid > 900), TRUE
                    #is.na(dfall$error_message) & is.na(dfall$warning_message)
                    )
finite_ok <- is.finite(dfall$estimate) &
  is.finite(dfall$lowerbound) &
  is.finite(dfall$upperbound)
dfall2 <- dfall[err_clean & finite_ok, ]

resag <- dfall2 %>%
  group_by(correlation, n, dtype, method, conf_level) %>%
  summarize(upperwithin = mean(upperwithin) * 100,
            lowerwithin = mean(lowerwithin) * 100,
            covagoneag  = mean(coverageone) * 100,
            time_mean = mean(time),
            .groups = "drop")


method_labels <- c(perc = "Percentile", delta = "Asymptotic",
                   bca = "BCa", bc = "BC",
                   wald_cfa = "CFA-ML", wald_cfa_robust = "CFA-MLR")
resag$method2 <- factor(method_labels[resag$method], levels = method_labels)

resag$correlation <- format(resag$correlation, nsmall = 2)
resag$correlation <- paste("$\\phi =", resag$correlation, "$")


size_scale_line <- 0.4
size_scale_point <- 0.8

nominal_one_sided <- function(cl) (1 + cl) / 2 * 100

make_popcov_plot <- function(resag, dtype_sel, lowertick, nominal) {
  d <- resag[resag$dtype == dtype_sel, ]
  y_breaks <- c(seq(lowertick, 90, by = 10), nominal)
  
  p_upper <- ggplot(d, aes(x = as.factor(n), y = upperwithin, group = method2)) +
    geom_line(aes(linetype = method2), linewidth = size_scale_line) +
    geom_point(aes(shape = method2), size = size_scale_point) +
    facet_grid(cols = vars(correlation)) +
    geom_hline(yintercept = nominal) +
    scale_y_continuous(name = "Pop. correlation value\nbelow upper limit (\\%)",
                       breaks = y_breaks, limits = c(lowertick, 100), labels = as.character) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())
  
  p_lower <- ggplot(d, aes(x = as.factor(n), y = lowerwithin, group = method2)) +
    geom_line(aes(linetype = method2), linewidth = size_scale_line) +
    geom_point(aes(shape = method2), size = size_scale_point) +
    facet_grid(cols = vars(correlation)) +
    geom_hline(yintercept = nominal) +
    scale_y_reverse(name = "Pop. correlation value\nabove lower limit (\\%)",
                    breaks = y_breaks, limits = c(100, lowertick), labels = as.character) +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.text.x = element_blank(),
          axis.text.x  = element_text(angle = 45, hjust = 1)) +
    labs(x = "Sample size") +
    scale_linetype_discrete(name = "Type of CI:") +
    scale_shape_discrete(name = "Type of CI:") + 
    guides(linetype = guide_legend(nrow = 1),
           shape = guide_legend(nrow=1))
  
  p_upper / p_lower
}


popcov_specs <- list(
  list(dtype = "severe",   lowertick = 50, file_stem = "outputs/plots/popcov_severe_alt"),
  list(dtype = "moderate", lowertick = 60, file_stem = "outputs/plots/popcov_moderate_alt"),
  list(dtype = "normal",   lowertick = 80, file_stem = "outputs/plots/popcov_normal_alt")
)


for (cl in sort(unique(resag$conf_level))) {
  resag_cl <- resag[resag$conf_level == cl, ]
  nominal  <- nominal_one_sided(cl)
  cl_tag   <- sprintf("cl%02d", round(cl * 100))
  for (s in popcov_specs) {
    #tikz(file = sprintf("%s_%s.tex", s$file_stem, cl_tag), width=8.4, height=5.2)
    p <- make_popcov_plot(resag_cl, s$dtype, s$lowertick, nominal)
    #print(p)
    #endoffile <- dev.off() 
    ggsave(sprintf("%s_%s.png", s$file_stem, cl_tag),
    plot = p, width = 8.4, height = 5.2, dpi = 300)
  }
}


