library(EnvStats) # for ebeta
library(stringr)

# load data, General Elections 2017
load("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas17.Rdata")
actas17 <- actas17[-which(actas17$ELECTORES_REGISTRO_pres<100),] # delete polling stations with <100 eligible voters


#' ---------------------------------------------------
# estimate parameters for Ecuador 2017 elections -----
# election_a = presidential election
# election_b = provincial/regional parliaments
#' ---------------------------------------------------

  # define entities (polling stations)
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


#' -----------------------------------------------------------------------
# simulate artifical Ecuadorian elections with different share_fraud -----
# estimate share_fraud 
#' -----------------------------------------------------------------------

  # simulate
  sim_elections <- list()  
  id <- 0
  for(share in c(0, 0.2, 0.4, 0.6, 0.8)) {
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

  # estimate
  est_results <- as.data.frame(matrix(NA, nrow=length(sim_elections), ncol=4))
  colnames(est_results) <- c("true_share", "est_share", "95_lower", "95_upper")
  est_results$true_share <- c(0, 0.2, 0.4, 0.6, 0.8)
  
  for (df in 1:length(sim_elections)) { 
    est_results[df,2:4] <- est_fraud(entities = entities, 
                                     turnout_probs = turnout_probs, 
                                     winner_probs = winner_probs, 
                                     undervoting_n = undervoting_n, 
                                     undervoting_sd = undervoting_sd, 
                                     underperc_emp = sim_elections[[df]]$under_perc[which(sim_elections[[df]]$under_perc!=0)],
                                     pw_emp = sim_elections[[df]]$winner_share[which(sim_elections[[df]]$under_perc!=0)],
                                     n_iter = 10
                                     )
    print(df)
    
  }
    
 
  
  
  
  
  










#' -----------------------------
# compare to empirical data ----
#' -----------------------------

# empirical 
par(mfrow=c(1,1))
plot(actas17$under_pres_asam_prov[actas17$under_pres_asam_prov!=0], 
     actas17$pw_pres[actas17$under_pres_asam_prov!=0], 
     xlim=c(0,1), col="darkgrey",  bty="n", pch=20
)
lw1 <- loess(pw_pres ~ under_pres_asam_prov, data=actas17[actas17$under_pres_asam_prov!=0,])
j <- order(actas17[actas17$under_pres_asam_prov!=0,]$under_pres_asam_prov)
lines(actas17[actas17$under_pres_asam_prov!=0,]$under_pres_asam_prov[j],lw1$fitted[j],col="lightblue")

# simulated clean 
df <- df_list[[10]]
df$under_share <- abs((df$turnout_a - df$turnout_b) / df$turnout_a)
plot(df$under_share[df$under!=0], 
     df$winner_share[df$under!=0], 
     xlim=c(0,1), col="darkgrey",  bty="n", pch=20
)
lw1 <- loess(winner_share ~ under_share, data=df[df$under!=0,])
j <- order(df[df$under!=0,]$under_share)
lines(df[df$under!=0,]$under_share[j],lw1$fitted[j],col="lightblue")

