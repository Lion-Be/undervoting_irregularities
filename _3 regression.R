#' ------------------------------------------------------------------------
#' regression analysis, predicting undervoting irregularities
#' Lion Behrens
#' ------------------------------------------------------------------------

library(betareg)
library(stargazer)

# load data 
load("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas17.Rdata")
actas17 <- actas17[-which(actas17$ELECTORES_REGISTRO_pres<100),] # delete polling stations with <100 eligible voters


#' -------------------------------
# 0. data preparation ------------
#' -------------------------------

  # y: extent of undervoting
  actas17$under_pres_consulta <- abs((actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_consulta) / actas17$SUFRAGANTES_pres)
  actas17$under_pres_asam_prov <- abs((actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_pres)
  actas17$under_nac_prov <- abs((actas17$SUFRAGANTES_asam_nac - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_asam_nac)
  actas17$under_andino_prov <- abs((actas17$SUFRAGANTES_andino - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_andino)
  actas17$under_consulta_prov <- abs((actas17$SUFRAGANTES_consulta - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_consulta)

  # winner's vote share
  actas17$pw_pres <- actas17$MORENO_pres / actas17$SUFRAGANTES_pres

  # runner up vote share
  actas17$runner_pres <- actas17$LASSO_pres / actas17$SUFRAGANTES_pres
  
  # turnout
  actas17$turnout_pres <- actas17$SUFRAGANTES_pres / actas17$ELECTORES_REGISTRO_pres
  actas17$turnout_pres[actas17$turnout_pres > 1] <- 1
  actas17$turnout_asam_prov <- actas17$SUFRAGANTES_asam_prov / actas17$ELECTORES_REGISTRO_pres
  actas17$turnout_asam_prov[actas17$turnout_asam_prov > 1] <- 1
  actas17$turnout_asam_nac <- actas17$SUFRAGANTES_asam_nac / actas17$ELECTORES_REGISTRO_pres
  actas17$turnout_asam_nac[actas17$turnout_asam_nac > 1] <- 1
  actas17$turnout_andino <- actas17$SUFRAGANTES_andino / actas17$ELECTORES_REGISTRO_pres
  actas17$turnout_andino[actas17$turnout_andino > 1] <- 1
  actas17$turnout_consulta <- actas17$SUFRAGANTES_consulta / actas17$ELECTORES_REGISTRO_pres
  actas17$turnout_consulta[actas17$turnout_consulta > 1] <- 1
  
  # closeness of electoral race
  actas17$closeness_pres <- abs(actas17$MORENO_pres - actas17$LASSO_pres)

  # share of null votes
  actas17$null_pres <- actas17$VOTOS_NULOS_pres / actas17$SUFRAGANTES_pres
  
  # share of blank votes
  actas17$blank_pres <- actas17$VOTOS_EN_BLANCO_pres / actas17$SUFRAGANTES_pres
  
#' -----------------------
# 1. modeling ------------
#' -----------------------

  excluded_cases <- c(which(actas17$under_pres_asam_prov==0), which(actas17$under_pres_asam_prov>=1))
  controls <- ~. + pw_pres + runner_pres + ELECTORES_REGISTRO_pres + turnout_pres + null_pres + blank_pres
  m17_pres <- betareg(update(under_pres_asam_prov ~ closeness_pres, controls), link="logit",
                      data = actas17[-excluded_cases,]) ### n should be the same as unvervoting_n. Which ones drop out due to Inf or value greater 1? I should actually only exclude the 0s. 
  
  
  stargazer(m17_pres,
            covariate.labels = c("Closeness of Electoral Race", "Winner's Vote Share",
                                 "Runner Up's Vote Share", "Number of Eligible Voters", "Percentage Turnout", 
                                 "Percentage Null Votes", "Percentage Blank Votes"), 
            star.cutoffs = c(.05, .01, .001))
  
  
  
  
  
  
  
  
  
  
  
  
  
  