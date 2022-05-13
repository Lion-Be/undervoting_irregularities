# define function
gen_data <- function(entities,         # vector with eligible voters
                     turnout_probs,    # binomial success probs, turnout
                     winner_probs,     # binomial success probs, votes for winner
                     undervoting_n,    # n of entities with undervoting
                     undervoting_sd,   # sd of undervoting distribution 
                     share_fraud       # share of polling stations with undervoting at which probabilistic fraud is happening
                     ) {
  
  # model absolute baseline turnout and absolute votes for winner
  turnout_b <- rbinom(length(entities), entities, turnout_probs)
  winner <- rbinom(length(entities), turnout_b, winner_probs)
  others <- turnout_b - winner

  # model undervoting discrepancies
  under <- rnorm(undervoting_n, 0, undervoting_sd) #### does my modeled undervoting look like empirical undervoting?????
  under[under>-1 & under < 0] <- -1
  under[under<1 & under > 0] <- 1
  under <- as.integer(under)
 
  # favor_winner <- rbinom(undervoting_n, abs(under), winner_share[ids] + fraud_para) 
  ##### so: I first have undervoting_n polling stations with undervoting
  ##### define a *share* (that I can iterate over)
  ##### from this share of polling stations, define the vast majority of under_votes as favor winner (like below with 0.9)
  ##### from the rest, define theta as winner_share, there is no fraud happening
  ##### *share* is hence the share of polling stations with undervoting where probabilsitic fraud is happening
  ##### => this is the quantity that I aim to reverse-engineer
  ##### in the estimation function, estimate a non-parametric (!) distance metric
  ##### in my current understanding, in the code below this share is 1, so probabilistic fraud happening
  ##### at all polling stations with undervoting. 
 
  # sample clean and frauded polling stations among those with undervoting discrepancies
  ids <- sample(1:length(entities), undervoting_n)
  ids_fraud <- sample(ids, undervoting_n * share_fraud)
  ids_clean <- ids[-which(is.element(ids, ids_fraud))]
  if (share_fraud == 0) ids_clean <- ids               
   
  # add/remove votes as probabilistic fraud
  if (share_fraud > 0) {
    favor_winner <- rbinom(length(ids_fraud), abs(under[1:length(ids_fraud)]), 0.9) 
    favor_others <- abs(under[1:length(ids_fraud)]) - favor_winner 
      
    ids_adding <- sample(ids_fraud, length(which(under[1:length(ids_fraud)]>0)))
    winner[ids_adding] <- winner[ids_adding] + favor_winner[under[1:length(ids_fraud)] > 0]
    others[ids_adding] <- others[ids_adding] + favor_others[under[1:length(ids_fraud)] > 0]
    
    ids_remove <- ids_fraud[-which(is.element(ids_fraud, ids_adding))]
    winner[ids_remove] <- winner[ids_remove] - favor_others[under[1:length(ids_fraud)] < 0]
    others[ids_remove] <- others[ids_remove] - favor_winner[under[1:length(ids_fraud)] < 0] 
  } # end if
  
  # add/remove votes proportional to winner's vote share at clean polling stations
  winner_votes <- rbinom(length(ids_clean), abs(under[(length(ids_fraud)+1):length(under)]), winner_share[ids_clean]) 
  others_votes <- abs(under[(length(ids_fraud)+1):length(under)]) - winner_votes
  
  ids_adding <- sample(ids_clean, length(which(under[(length(ids_fraud)+1):length(under)]>0)))
  winner[ids_adding] <- winner[ids_adding] + winner_votes[under[(length(ids_fraud)+1):length(under)] > 0]
  others[ids_adding] <- others[ids_adding] + others_votes[under[(length(ids_fraud)+1):length(under)] > 0]
  
  ids_remove <- ids_fraud[-which(is.element(ids_fraud, ids_adding))]
  winner[ids_remove] <- winner[ids_remove] - winner_votes[under[(length(ids_fraud)+1):length(under)] < 0]
  others[ids_remove] <- others[ids_remove] - others_votes[under[(length(ids_fraud)+1):length(under)] < 0] 
  
  # redefine variables
  turnout_a <- winner + others
  turnout_a_share <- turnout_a / entities
  under_perc <- abs((turnout_a - turnout_b) / turnout_a)
  winner_share <- winner / turnout_a
  others_share <- others / turnout_a
  
  # construct data 
  under <- turnout_a - turnout_b
  df <- as.data.frame(cbind(entities, turnout_a, turnout_b, under, under_perc, turnout_a_share, 
                      winner, others, winner_share, others_share))
  return(df)
  
}



#' -----------------------------------
# simulate data for Ecuador 2017 -----
#' -----------------------------------
  library(EnvStats) # for ebeta

  #' ---------------------------------
  # preparation ----------------------
  #' ---------------------------------

    load("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas17.Rdata")
    actas17 <- actas17[-which(actas17$ELECTORES_REGISTRO_pres<100),] # delete polling stations with <100 eligible voters
    ## don't I need to model undervoting_sd as empirical sd * 1/5?
    
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
    
    # estimate standard deviation of undervoting discrepancies
    actas17$under_pres_asam_prov <- (actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_asam_prov) ## same number as in descriptive analysis? I deleted all polling stations with n<100 
    undervoting_sd <- sd(actas17$under_pres_asam_prov[actas17$under_pres_asam_prov!=0], na.rm=T) 
    
    
  #' -----------------------------------
  # simulate clean and frauded data ----
  #' -----------------------------------
  
    df_list <- list()  
    id <- 0
    for(share_fraud in seq(0, 0.9, 0.1)) {
      id <- id+1  
      df_list[[id]] <- gen_data(entities = actas17$ELECTORES_REGISTRO_pres, 
                                turnout_probs = turnout_probs, 
                                winner_probs = winner_probs, 
                                undervoting_n = length(which(actas17$under_pres_asam_prov!=0)), 
                                undervoting_sd = undervoting_sd, 
                                share_fraud = share_fraud
                               )
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
    
    
  
    
    
    
    
    
    
    
    
    
    
    
    



