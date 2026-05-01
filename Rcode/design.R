library(lavaan)
library(dplyr)

nindicator <- 3
coefs <- 1
corr <- c(0.7, 0.8, 0.9, 0.95, 1)
param <- expand.grid(loading1 = coefs, loading2 = coefs, correlation = corr)
simModels <- foreach(i = 1:nrow(param), .combine = "rbind") %do%
  {
    simCommonFactor <- 
      paste(
        paste("xi_1 =~ ",param$loading1[i], "*x11 + ",param$loading1[i],"*x12 + ",param$loading1[i], "*x13"),"\n"
        , paste("xi_2 =~ ",param$loading2[i], "*x21 + ",param$loading2[i],"*x22 + ",param$loading2[i], "*x23"), "\n"
        , paste("xi_1 ~~ 1*xi_1 + ", param$correlation[i], "*xi_2"),"\n"
        , "xi_2 ~~ 1*xi_2 \n"
        , paste("x11 ~~", 0.6, "*x11 + 0*x12 + 0*x13 + 0*x21 + 0*x22 + 0*x23"),"\n"
        , paste("x12 ~~", 0.5, "*x12 + 0*x13 + 0*x21 + 0*x22 + 0*x23"),"\n"
        , paste("x13 ~~", 0.2, "*x13 + 0*x21 + 0*x22 + 0*x23"),"\n"
        , paste("x21 ~~", 0.6, "*x21 + 0*x22 + 0*x23"),"\n"
        , paste("x22 ~~", 0.5, "*x22 + 0*x23"),"\n"
        , paste("x23 ~~", 0.2, "*x23"), "\n"
        , paste("x11 ~ 0*1"), "\n"
        , paste("x12 ~ 0*1"), "\n"
        , paste("x13 ~ 0*1"), "\n"
        , paste("x21 ~ 0*1"), "\n"
        , paste("x22 ~ 0*1"), "\n"
        , paste("x23 ~ 0*1"), "\n"
      )
    save <- data.frame(
      loading_1 = param$loading1[i],
      loading_2 =  param$loading2[i],
      correlation = param$correlation[i],
      model_id = i,
      model = simCommonFactor
    )
    save
    #rm(save, simCommonFactor, i)
  }
simModels
rm(coefs, corr, param, simCommonFactor, save, i)
short_models <- simModels[c("correlation", "model", "model_id")]

dt_lookup <- data.frame(datatype = c(1, 2, 3), 
                     dtype = c("normal", "moderate", "severe"),
                     skewness = I(list(NULL, rep(2, nindicator*2), rep(3, nindicator*2))),
                     kurtosis = I(list(NULL, rep(7, nindicator*2), rep(21, nindicator*2)))
)

conditions <- expand.grid(rep_batch = 1:10,
                          correlation = c(0.7, 0.8, 0.9, 0.95, 1),
                          n = c(25, 50, 100, 200, 400, 800, 1600, 3200, 6400),
                          datatype = c(1 , 2, 3)
                          )

conditions <- conditions %>% 
              left_join(dt_lookup, by = "datatype") %>%
              left_join(short_models, by = "correlation") 








