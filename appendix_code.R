library(EnvStats) 
library(fields)
library(foreign)

# load data at polling station level, General Elections 2017
load("actas17.Rdata")

# delete polling stations with <100 eligible voters
actas17 <- actas17[-which(actas17$ELECTORES_REGISTRO_pres<100),] 

# run model, presidential election 
actas17 <- actas17[-which(actas17$turnout_pres>1),] # exclude if turnout > 1
ecu17_pres <- 
  est_fraud(eligible = actas17$ELECTORES_REGISTRO_pres, 
            turnout_main = actas17$SUFRAGANTES_pres,
            turnout_baseline = actas17$SUFRAGANTES_asam_prov,
            winner_main = actas17$MORENO_pres,
            uncertainty = c("fundamental", "estimation"),
            n_iter = 100, 
            n_postdraws = 500,#
            n_burnin = 400,
            seed = 12345
            )

# run model, national parliament election 
actas17 <- actas17[-which(actas17$turnout_nac>1),] # exclude if turnout > 1
ecu17_nac <- 
  est_fraud(eligible = actas17$ELECTORES_REGISTRO_asam_nac, 
            turnout_main = actas17$SUFRAGANTES_asam_nac,
            turnout_baseline = actas17$SUFRAGANTES_asam_prov,
            winnershare_main = actas17$winnershare_asam_nac,
            uncertainty = c("fundamental", "estimation"),
            n_iter = 100, 
            n_postdraws = 500,#
            n_burnin = 400,
            seed = 12345
            )

# run model, Andean parliament election 
actas17 <- actas17[-which(actas17$turnout_andean>1),] # exclude if turnout > 1
ecu17_andean <- 
  est_fraud(eligible = actas17$ELECTORES_REGISTRO_andino, 
            turnout_main = actas17$SUFRAGANTES_andino,
            turnout_baseline = actas17$SUFRAGANTES_asam_prov,
            winnershare_main = actas17$winnershare_andino,
            uncertainty = c("fundamental", "estimation"),
            n_iter = 100, 
            n_postdraws = 500,#
            n_burnin = 400,
            seed = 12345
            )

# run model, national referendum 
actas17 <- actas17[-which(actas17$turnout_referend>1),] # exclude if turnout > 1
ecu17_referendum <- 
  est_fraud(eligible = actas17$ELECTORES_REGISTRO_consulta, 
            turnout_main = actas17$SUFRAGANTES_consulta,
            turnout_baseline = actas17$SUFRAGANTES_asam_prov,
            winner_main = actas17$Si_consulta,
            uncertainty = c("fundamental", "estimation"),
            n_iter = 100, 
            n_postdraws = 500,#
            n_burnin = 400,
            seed = 12345
            )