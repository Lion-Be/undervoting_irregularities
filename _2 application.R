library(EnvStats) # for ebeta
library(stringr)
library(fields)
library(foreign)

# load data, General Elections 2017
load("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas17.Rdata")
actas17 <- actas17[-which(actas17$ELECTORES_REGISTRO_pres<100),] # delete polling stations with <100 eligible voters

cand17 <- read.spss("U:/PhD Electoral Fraud/Data/Ecuador/CNE/2017 Elecciones Generales/candidatos 2017.sav", to.data.frame=T)
parties17 <- read.spss("U:/PhD Electoral Fraud/Data/Ecuador/CNE/2017 Elecciones Generales/organizaciones polticas 2017.sav", to.data.frame=T)
cand17$NUM_PARTIDO <- gsub(" ", "", cand17$NUM_PARTIDO)

set.seed(11111)

#' ----------------------------------------------
# 3. Ecuadorian General Elections, 2017 ---------
#' ----------------------------------------------
# baseline election: regional parliaments 

  # Presidential election 
  used_data <- actas17[which(!is.na(actas17$SUFRAGANTES_pres) & !is.na(actas17$SUFRAGANTES_asam_prov) & !is.na(actas17$MORENO_pres)),]
  ecu17_pres_asam_prov <- est_fraud(eligible = used_data$ELECTORES_REGISTRO_pres, 
                                    turnout_main = used_data$SUFRAGANTES_pres,
                                    turnout_baseline = used_data$SUFRAGANTES_asam_prov,
                                    winner_main = used_data$MORENO_pres,
                                    uncertainty = c("estimation"),
                                    n_iter = 200, 
                                    n_iter2 = 20,
                                    seed = 12345
                                    )
  
 
  # National parliament
  # winner is ALIANZA PAIS, OP_CODIGO 589
  cand17_mpais <- str_c(word(cand17[cand17$OP_CODIGO=="589" & cand17$DIGNIDAD_NOMBRE == "ASAMBLEÍSTAS NACIONALES", "CANDIDATO_NOMBRE"], 1), "_asam_nac")
  mpais_votes <- actas17[,cand17_mpais]
  all_votes <- actas17[,which(str_detect(colnames(actas17), "_asam_nac"))]
  all_votes <- all_votes[,-c(1:28, 254)]
  actas17$winnershare_asam_nac <- rowSums(mpais_votes) / rowSums(all_votes)
  
  used_data <- actas17[which(!is.na(actas17$SUFRAGANTES_asam_nac) & !is.na(actas17$SUFRAGANTES_asam_prov) & !is.na(actas17$winnershare_asam_nac)),]
  ecu17_asam_nac_asam_prov <- est_fraud(eligible = used_data$ELECTORES_REGISTRO_asam_nac, 
                                    turnout_main = used_data$SUFRAGANTES_asam_nac,
                                    turnout_baseline = used_data$SUFRAGANTES_asam_prov,
                                    winnershare_main = used_data$winnershare_asam_nac,
                                    uncertainty = c("estimation"),
                                    n_iter = 200, 
                                    n_iter2 = 20,
                                    seed = 12345
  )
  
  # Andean parliament
  # winner is alliance between ALIANZA PAIS and Ecuadorian Socialist Party, OP_CODIGO 42
  cand17_mpais <- str_c(word(cand17[cand17$OP_CODIGO=="42" & cand17$DIGNIDAD_NOMBRE == "PARLAMENTARIOS ANDINOS", "CANDIDATO_NOMBRE"], 1), "_andino")
  mpais_votes <- actas17[,cand17_mpais]
  all_votes <- actas17[,which(str_detect(colnames(actas17), "_andino"))]
  all_votes <- all_votes[,-c(1:28, 84)]
  actas17$winnershare_andino <- rowSums(mpais_votes) / rowSums(all_votes)
  
  used_data <- actas17[which(!is.na(actas17$SUFRAGANTES_andino) & !is.na(actas17$SUFRAGANTES_asam_prov) & !is.na(actas17$winnershare_andino)),]
  ecu17_andino_asam_prov <- est_fraud(eligible = used_data$ELECTORES_REGISTRO_andino, 
                                        turnout_main = used_data$SUFRAGANTES_andino,
                                        turnout_baseline = used_data$SUFRAGANTES_asam_prov,
                                        winnershare_main = used_data$winnershare_andino,
                                        uncertainty = c("estimation"),
                                        n_iter = 200, 
                                        n_iter2 = 20,
                                        seed = 12345
  )
  
  # National referendum 
  used_data <- actas17[which(!is.na(actas17$SUFRAGANTES_consulta) & !is.na(actas17$SUFRAGANTES_asam_prov) & !is.na(actas17$Si_consulta)),]
  actas17$winnershare_consulta <- actas17$Si_consulta / actas17$SUFRAGANTES_consulta
  ecu17_consulta_asam_prov <- est_fraud(eligible = used_data$ELECTORES_REGISTRO_consulta, 
                                      turnout_main = used_data$SUFRAGANTES_consulta,
                                      turnout_baseline = used_data$SUFRAGANTES_asam_prov,
                                      winner_main = used_data$Si_consulta,
                                      uncertainty = c("estimation"),
                                      n_iter = 200, 
                                      n_iter2 = 20,
                                      seed = 12345
  )
  
  
  
  
  
  
  
  
  
  
#' -----------------------------------------------------------
# 4. simulated elections, different fraud parameters ---------
#' -----------------------------------------------------------

  #' --------------------------------------------------
  # 4.1 inform simulations from empirical values ------
  #' --------------------------------------------------

    entities <- actas17$ELECTORES_REGISTRO_pres
  
    # estimate binomial success probabilities for absolute turnout
    actas17$turnout_pres <- actas17$SUFRAGANTES_pres / actas17$ELECTORES_REGISTRO_pres
    actas17$turnout_pres[actas17$turnout_pres > 1] <- 1
    beta_est <- ebeta(actas17$turnout_pres, method="mle")
    
    turnout_probs <- rbeta(nrow(actas17), 
                           shape1 = beta_est$parameters[1], 
                           shape2 = beta_est$parameters[2])
    
    # estimate binomial success probabilities for winner's absolute votes
    actas17$winnershare_pres <- actas17$MORENO_pres/actas17$SUFRAGANTES_pres
    beta_est <- ebeta(actas17$winnershare_pres, method="mle")
    
    winner_probs <- rbeta(nrow(actas17), 
                          shape1 = beta_est$parameters[1], 
                          shape2 = beta_est$parameters[2])
    
    # number of entities with undervoting
    actas17$under_pres_asam_prov <- abs((actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_pres)
    undervoting_n = length(which(actas17$under_pres_asam_prov!=0))
    
    # estimate standard deviation of undervoting discrepancies
    actas17$under_pres_asam_prov <- (actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_asam_prov) ## same number as in descriptive analysis? I deleted all polling stations with n<100 
    undervoting_sd <- sd(actas17$under_pres_asam_prov[actas17$under_pres_asam_prov!=0], na.rm=T) 
    
    
    # simulate artifical elections
    sim_elections <- list()  
    id <- 0
    for (share in c(0, 0.2, 0.4, 0.6, 0.8)) {
      id <- id+1  
      sim_elections[[id]] <- gen_data(entities = entities, 
                                      turnout_probs = turnout_probs, 
                                      winner_probs = winner_probs, 
                                      undervoting_n = undervoting_n, 
                                      undervoting_sd = undervoting_sd, 
                                      share_fraud = share
      )
    } 
    names(sim_elections) <- str_c("share_fraud = ", c(0, 0.2, 0.4, 0.6, 0.8))

    
  #' -----------------------------------
  # 4.2 estimate fraud parameters ------
  #' -----------------------------------
    
    est_results <- as.data.frame(matrix(NA, nrow=length(sim_elections), ncol=4))
    colnames(est_results) <- c("true_share", "est_share", "95_lower", "95_upper")
    est_results$true_share <- c(0, 0.2, 0.4, 0.6, 0.8)
    
    for (df in 1:length(sim_elections)) { 
      
      sim_elections[[df]]$turnout_a_share[sim_elections[[df]]$turnout_a_share > 1] <- 1
      beta_est <- ebeta(sim_elections[[df]]$turnout_a_share, method="mle")
      turnout_probs <- rbeta(nrow(sim_elections[[df]]), 
                             shape1 = beta_est$parameters[1], 
                             shape2 = beta_est$parameters[2])
      
      sim_elections[[df]]$winner_share[sim_elections[[df]]$winner_share > 1] <- 1
      beta_est <- ebeta(sim_elections[[df]]$winner_share, method="mle")
      winner_probs <- rbeta(nrow(sim_elections[[df]]), 
                             shape1 = beta_est$parameters[1], 
                             shape2 = beta_est$parameters[2])
      
      
      est_results[df,2:4] <- est_fraud(entities = sim_elections[[df]]$entities, 
                                       turnout_probs = turnout_probs,
                                       winner_probs = winner_probs,
                                       undervoting_n = length(which(sim_elections[[df]]$under!=0)),
                                       undervoting_sd = sd(sim_elections[[df]]$under),
                                       underperc_emp = sim_elections[[df]]$under_perc, 
                                       pw_emp = sim_elections[[df]]$winner_share,
                                       n_iter = 1)
                                       
      save(est_results, file="est_results.RData")
    }
      
  
  
#' -----------------------------------------------------------------
# 5. simulated elections, different extents of undervoting ---------
#' -----------------------------------------------------------------

  est_results <- expand.grid(seq(0, 0.8, 0.2), c(500,100,1500,2000,2500,5000))
  colnames(est_results) <- c("true_share", "undervoting_n")
  est_results$est_share <- est_results$`95_lower` <- est_results$`95_higher` <- NA
  
  for (row in 1:nrow(est_results)) { 
    
    sim_election <- gen_data(entities = entities, 
                             turnout_probs = turnout_probs, 
                             winner_probs = winner_probs, 
                             undervoting_n = est_results$undervoting_n[row], 
                             undervoting_sd = undervoting_sd, 
                             share_fraud = est_results$true_share[row]
    )
    
    est_results[df,3:5] <- est_fraud(eligible = entities, 
                                     turnout_main = sim_election$turnout_a,
                                     turnout_baseline = sim_election$turnout_b,
                                     winner_main = sim_election$winner,
                                     uncertainty = c("fundamental", "estimation"),
                                     n_iter = 100, 
                                     n_iter2 = 1,
                                     seed = 12345
    )
    save(est_results, file="est_results_simstudy.RData")
  
  }
  
  

  

  
  
 


