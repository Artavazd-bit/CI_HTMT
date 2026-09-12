library(cSEM)
library(lavaan)
library(semTools)

# Load one of the simulated Datasets: 

datadir <- "results/results_2026_05_14/datasets/df_task_00266.rds"

datards <- readRDS(datadir)

data <- datards[[1]]$data

# conditions: 
# popcor = 0.7
# severe non-normality
# n = 6400

# own implementation
source("Rcode/HTMT.R")
htmt_own <- HTMT(data, use_cor = TRUE, nindicator = 3)

# cSEM 
model_cSEM <- '
              #  latent variables
                xi_1 =~ x11 + x12 + x13
                xi_2 =~ x21 + x22 + x23

                xi_1 ~~ xi_2
                xi_1 ~~ xi_1
                xi_2 ~~ xi_2
              '

# no structural model so we use inner weighting scheme "SUMCORR"
res <- cSEM::csem(.data = data, .model = model_cSEM, .approach_weights = "SUMCORR")
htmtcsem <- cSEM::calculateHTMT(.object = res, .type_htmt = "htmt")
htmtcsem$htmts[2,1]

htmt_own - htmtcsem$htmts[2,1]


# semTools
model_semTools <- '
              #  latent variables
                xi_1 =~ x11 + x12 + x13
                xi_2 =~ x21 + x22 + x23

                xi_1 ~~ xi_2
                xi_1 ~~ xi_1
                xi_2 ~~ xi_2
              '
htmtsemTools <- semTools::htmt(model = model_semTools, data = data, htmt2 = FALSE)

htmtsemTools[2,1] - htmt_own



