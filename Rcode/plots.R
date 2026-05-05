library(dplyr)
library(ggplot2)
library(patchwork)

dfall <- readRDS("simresults/2026_04_25_arrayresults/aggregated.rds")
length(unique(dfall$source_file)) == 1350

dfall$upperwithin <- dfall$correlation < dfall$upperbound
dfall$lowerwithin <- dfall$correlation > dfall$lowerbound

dfall$correlation <- format(dfall$correlation, nsmall = 2)
dfall$correlation <- paste("Phi ==", dfall$correlation)

dfall2 <- dfall[!is.na(dfall$lowerbound),]
resag <- dfall2 %>% 
  group_by(correlation, n, datatype, alpha, method) %>%
  summarize(upperwithin = mean(upperwithin)*100,
            lowerwithin = mean(lowerwithin)*100,
            covagoneag = mean(coverageone)*100,
            covagcorrag = mean(coveragecorr)*100
  )


resag$method2[resag$method == "boot"] <- "Percentile" 
resag$method2[resag$method == "delta"] <- "Asymptotic" 
resag$method2[resag$method == "bcaboot"] <- "BCa"
resag$method2[resag$method == "bcboot"] <- "BC"
resag$method2[resag$method == "conphi"] <- "cfa"



resag$method2 <- factor(resag$method2, levels=c("Percentile", "Asymptotic", "BCa", "BC", "cfa"))

resag <- resag[!is.na(resag$method2),] 

################################################################################
## Coverage of corr
################################################################################
alpha = c(0.05)
datatype = "severenonnormal"
lowertick <- 60

y_breaks <- c(seq(lowertick, 100, by = 5), (1-alpha/2)*100)


p_upper_n <- ggplot(resag[resag$alpha == alpha & resag$datatype == datatype,], aes(x = as.factor(n), y = upperwithin, group = method)) +
  geom_line(aes(linetype = method2)) +
  geom_point(aes(shape = method2)) +
  facet_grid(cols = vars(correlation), labeller = label_parsed) + 
  geom_hline(yintercept = (1-alpha/2)*100) + 
  scale_y_continuous(name = "Pop. correlation value below upper limit (%)", breaks = y_breaks, 
                     limits = c(lowertick, 100) )  +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) 

p_lower_n <- ggplot(resag[resag$alpha == alpha & resag$datatype == datatype,], aes(x = as.factor(n), y = lowerwithin, group = method)) +
  geom_line(aes(linetype = method2)) +
  geom_point(aes(shape = method2)) +
  facet_grid(cols = vars(correlation), labeller = label_parsed, ) + 
  geom_hline(yintercept = (1-alpha/2)*100) +
  scale_y_reverse(name = "Pop. correlation value above lower limit (%)", breaks = y_breaks, 
                  limits = c(100, lowertick)) +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text.x = element_blank()) +
  labs(x = "Sample size") + 
  scale_linetype_discrete(name = "Type of CI:") +
  scale_shape_discrete(name = "Type of CI:")

popcov005severenonnormal <- p_upper_n / p_lower_n
ggsave("2026_04_25_plots/popcoverage_005_nonnormal_severe.png", plot = popcov005severenonnormal, width = 15.375, height = 9.15625)
###############################################################################alpha = c(0.05)
alpha = c(0.05)
datatype = "moderatenonnormal"
lowertick <- 65

y_breaks <- c(seq(lowertick, 100, by = 5), (1-alpha/2)*100)

decimal_labeller <- function(x) {
  sprintf("%.2f", as.numeric(x))
}

p_upper_n <- ggplot(resag[resag$alpha == alpha & resag$datatype == datatype,], aes(x = as.factor(n), y = upperwithin, group = method)) +
  geom_line(aes(linetype = method2)) +
  geom_point(aes(shape = method2)) +
  facet_grid(cols = vars(correlation), labeller = label_parsed) + 
  geom_hline(yintercept = (1-alpha/2)*100) + 
  scale_y_continuous(name = "Pop. correlation value below upper limit (%)", breaks = y_breaks, 
                     limits = c(lowertick, 100) )  +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) 

p_lower_n <- ggplot(resag[resag$alpha == alpha & resag$datatype == datatype,], aes(x = as.factor(n), y = lowerwithin, group = method)) +
  geom_line(aes(linetype = method2)) +
  geom_point(aes(shape = method2)) +
  facet_grid(cols = vars(correlation), labeller = label_parsed) + 
  geom_hline(yintercept = (1-alpha/2)*100) +
  scale_y_reverse(name = "Pop. correlation value above lower limit (%)", breaks = y_breaks, 
                  limits = c(100, lowertick)) +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text.x = element_blank()) +
  labs(x = "Sample size") + 
  scale_linetype_discrete(name = "Type of CI:") +
  scale_shape_discrete(name = "Type of CI:")

popcov005nonnormal_moderate <- p_upper_n / p_lower_n
ggsave("2026_04_25_plots/popcoverage_005_nonnormal_moderate.png", plot = popcov005nonnormal_moderate, width = 15.375, height = 9.15625)
################################################################################
alpha = c(0.05)
datatype = "normal"
lowertick <- 85

y_breaks <- c(seq(lowertick, 100, by = 5), (1-alpha/2)*100)

decimal_labeller <- function(x) {
  sprintf("%.2f", as.numeric(x))
}

p_upper_n <- ggplot(resag[resag$alpha == alpha & resag$datatype == datatype,], aes(x = as.factor(n), y = upperwithin, group = method)) +
  geom_line(aes(linetype = method2)) +
  geom_point(aes(shape = method2)) +
  facet_grid(cols = vars(correlation), labeller = label_parsed) + 
  geom_hline(yintercept = (1-alpha/2)*100) + 
  scale_y_continuous(name = "Pop. correlation value below upper limit (%)", breaks = y_breaks, 
                     limits = c(lowertick, 100) )  +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) 

p_lower_n <- ggplot(resag[resag$alpha == alpha & resag$datatype == datatype,], aes(x = as.factor(n), y = lowerwithin, group = method)) +
  geom_line(aes(linetype = method2)) +
  geom_point(aes(shape = method2)) +
  facet_grid(cols = vars(correlation), labeller = label_parsed) + 
  geom_hline(yintercept = (1-alpha/2)*100) +
  scale_y_reverse(name = "Pop. correlation value above lower limit (%)", breaks = y_breaks, 
                  limits = c(100, lowertick)) +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text.x = element_blank()) +
  labs(x = "Sample size") + 
  scale_linetype_discrete(name = "Type of CI:") +
  scale_shape_discrete(name = "Type of CI:")

popcov005normal <- p_upper_n / p_lower_n
ggsave("2026_04_25_plots/popcoverage_005_normal.png", plot = popcov005normal, width = 15.375, height = 9.15625)
###############################################################################################

################################################################################
## Coverage of one 
################################################################################
resag$covagone2 <- 100 - resag$covagoneag
resag$hline <- 80
resag$hline[resag$correlation == "Phi == 1.00"] <- (resag$alpha[resag$correlation == "Phi == 1.00"]) * 100
alphafilter <- c(0.05, 0.1, 0.01) 

y_breaks2 <- seq(0, 100, by = 10)
for(datatype in c("moderatenonnormal", "normal", "severenonnormal")){
for(alpha in alphafilter){
covagoneplot <- ggplot(resag[resag$datatype == datatype & resag$alpha == alpha,], aes(x = as.factor(n), y = covagone2, group = method)) + 
  geom_line(aes(linetype = method2)) + 
  facet_grid(cols = vars(correlation), labeller = label_parsed) + 
  geom_point(aes(shape = method2)) + 
  theme(legend.position = "bottom") +
  labs(x = "Sample size") + 
  scale_linetype_discrete(name = "Type of CI:") +
  scale_shape_discrete(name = "Type of CI:") + 
  geom_hline(data = resag[resag$datatype == datatype & resag$alpha == alpha,], aes(yintercept = hline)) + 
  scale_y_continuous(breaks = y_breaks2, name = "Rejection rate")

ggsave(paste0("covagoneplot", alpha*100, datatype, ".png"), plot = covagoneplot, width = 12.375, height = 4.48)
}
}  
################################################################################


cmpdf <- dfall %>% filter(alpha == 0.05, datatype == "nonnormal")

cmpdf$tme <- as.numeric(cmpdf$time)
cmpdfag <- cmpdf %>% group_by(method, n) %>% summarize(time  = mean(tme))


compcmpdf <- tidyr::pivot_wider(data = cmpdfag, names_from = method, values_from = time)

################################################################################
cmpdf <- dfall %>% filter(alpha == 0.05, datatype == "normal")

cmpdf$tme <- as.numeric(cmpdf$time)
cmpdfag <- cmpdf %>% group_by(method, n) %>% summarize(time  = mean(tme))


compcmpdf <- tidyr::pivot_wider(data = cmpdfag, names_from = method, values_from = time)

################################################################################
missing <- dfall[!is.na(dfall$missing) & dfall$alpha == 0.05, ]
missingfilt <- missing[,c("seed", "method", "missing", "sim_runs", "datatype", "correlation", "n")]
missingfilt$missingint[missingfilt$missing == 0] <- "None"
missingfilt$missingint[1 <= missingfilt$missing& missingfilt$missing <10] <- "[1,10)"
missingfilt$missingint[10 <= missingfilt$missing& missingfilt$missing <20] <- "[10,20)"
missingfilt$missingint[20 <= missingfilt$missing& missingfilt$missing <30] <- "[20,30)"
missingfilt$missingint[30 <= missingfilt$missing& missingfilt$missing <40] <- "[30,40)"
missingfilt$missingint[40 <= missingfilt$missing& missingfilt$missing <50] <- "[40,50)"
missingfilt$missingint[50 <= missingfilt$missing& missingfilt$missing <60] <- "[50,60)"
missingfilt$missingint[60 <= missingfilt$missing& missingfilt$missing <70] <- "[60,70)"
missingfilt$missingint[70 <= missingfilt$missing& missingfilt$missing <80] <- "[70,80)"
missingfilt$missingint[80 <= missingfilt$missing& missingfilt$missing <90] <- "[80,90)"
missingfilt$missingint[90 <= missingfilt$missing& missingfilt$missing <100] <- "[90,100)"
missingfilt$missingint[100 <= missingfilt$missing& missingfilt$missing <110] <- "[100,110)"
missingfilt$missingint[110 <= missingfilt$missing& missingfilt$missing <120] <- "[110,120)"
missingfilt$missingint[120 <= missingfilt$missing& missingfilt$missing <130] <- "[120,130)"
missingfilt$missingint[130 <= missingfilt$missing& missingfilt$missing <140] <- "[130,140)"
missingfilt$missingint[140 <= missingfilt$missing& missingfilt$missing <150] <- "[140,150)"
missingfilt$missingint[150 <= missingfilt$missing& missingfilt$missing <160] <- "[150,160)"
missingfilt$missingint[160 <= missingfilt$missing& missingfilt$missing <170] <- "[160,170)"
missingfilt$missingint[170 <= missingfilt$missing& missingfilt$missing <180] <- "[170,180)"

missingfilt$missingint <- ordered(missingfilt$missingint, levels=c("None", "[1,10)", "[10,20)", "[20,30)",  "[30,40)", "[40,50)", "[50,60)"
                                                                   , "[60,70)", "[70,80)", "[80,90)", "[90,100)", "[100,110)", "[110,120)", 
                                                                   "[120,130)", "[130,140)", "[140,150)", "[150,160)", "[160,170)", "[170,180)"))


missingfilt <- missingfilt %>% 
  filter(method == "boot") %>%
  group_by(correlation, missingint, datatype, method) %>%
  select(n) %>%
  table() 

missingfilt$n2 <- paste("n ==", missingfilt$n)


missingfilt2 <- missingfilt %>% 
  filter(missing != 0, datatype == "nonnormal") %>% 
  group_by(correlation, missingint) %>%
  select(n) %>% 
  table()

missingfilt3 <- missingfilt2 %>% 
  mutate(
    "[100+)" = "[100,110)" + "[110,120)" + "[120,130)" + "[130,140)" + 
      "[140,150)" + "[150,160)" + "[160,170)" + "[170,180)"
  ) %>% 
  select(correlation, None, "[1,10)", "[10,20)", "[20,30)", "[30,40)", 
         "[40,50)", "[50,60)", "[60,70)", "[70,80)", "[80,90)", "[90,100)", "[100+)")




normalinadmis <- ggplot(missingfilt[missingfilt$datatype == "normal" & missingfilt$n == 25 & missingfilt$method == "boot" & missingfilt$missing != 0, ], aes(x = missing)) + 
  geom_histogram(aes(y=after_stat((count/(10000)) * 100)), breaks = c(1, seq(from = 10, to = 180, by =10))) + 
  facet_grid(cols = vars(correlation), rows  = vars(n2), labeller = label_parsed) +  
  theme(legend.position = "bottom") + 
  scale_y_continuous(name = "Relative frequency [%]") + 
  scale_x_continuous(name = "Number of non-calculable bootstrap HTMTs")

ggsave("normalinadmis.png", plot = normalinadmis, width = 12.375, height = 4.48)

nonnormalinadmis <- ggplot(missingfilt[missingfilt$datatype == "nonnormal" & missingfilt$n == 25 & missingfilt$method == "boot" & missingfilt$missing != 0, ], aes(x = missing)) + 
  geom_histogram(aes(y=after_stat((count/(10000))* 100)), breaks = c(1, seq(from = 10, to = 180, by =10))) + 
  facet_grid(cols = vars(correlation), rows  = vars(n2), labeller = label_parsed) +  
  theme(legend.position = "bottom") + 
  scale_y_continuous(name = "Relative frequency [%]") + 
  scale_x_continuous(name = "Number of non-calculable bootstrap HTMTs")

ggsave("nonnormalinadmis.png", plot = nonnormalinadmis, width = 12.375, height = 4.48)