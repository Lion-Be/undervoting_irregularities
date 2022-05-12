# define function
gen_data <- function(entities,         # vector with eligible voters
                     turnout_probs,    # binomial success probs, turnout
                     winner_probs,     # binomial success probs, votes for winner
                     undervoting_n,    # n of entities with undervoting
                     undervoting_sd,   # sd of undervoting distribution 
                     fraud_para        # fraud parameter to be estimated
                     ) {
  
  # model absolute baseline turnout and absolute votes for winner
  turnout_b <- rbinom(length(entities), entities, turnout_probs)
  winner <- rbinom(length(entities), turnout_b, winner_probs)
  others <- turnout_b - winner
  winner_share <- winner / turnout_b
  winner_share[winner_share < 0] <- 0 
  winner_share[winner_share > 1] <- 1 
  
  # model undervoting discrepancies
  under <- rnorm(undervoting_n, 0, undervoting_sd) #### does my modeled undervoting look like empirical undervoting?????
  under[under>-1 & under < 0] <- -1
  under[under<1 & under > 0] <- 1
  under <- as.integer(under)
  ids <- sample(1:length(entities), undervoting_n)
  
  # introduce fraud 
  favor_winner <- rbinom(undervoting_n, abs(under), winner_share[ids] + fraud_para) 
  favor_others <- abs(under) - favor_winner 
  
  ids_adding <- sample(ids, length(which(under>0)))
  winner[ids_adding] <- winner[ids_adding] + favor_winner[under > 0]
  others[ids_adding] <- others[ids_adding] + favor_others[under > 0]
  
  ids_remove <- ids[-which(is.element(ids, ids_adding))]
  winner[ids_remove] <- winner[ids_remove] - favor_others[under < 0]
  others[ids_remove] <- others[ids_remove] - favor_winner[under < 0] 
 
  # redefine variables
  turnout_a <- winner + others
  turnout_a_share <- turnout_a / entities
  winner_share <- winner / turnout_a
  others_share <- others / turnout_a
  
  # construct data 
  under <- turnout_a - turnout_b
  df <- as.data.frame(cbind(entities, turnout_a, turnout_b, under, turnout_a_share, 
                      winner, others, winner_share, others_share))
  return(df)
  
}

# once I simulated data, check if share of places where winner (simulated) won
# is same as share where Moreno (empirical) won. 
# or repeat it 1000 times and see whether it converges. 
# then fraud and see whether difference is unbiased estimate of fraud parameter. 


#' -----------------------------------
# simulate data for Ecuador 2017 -----
#' -----------------------------------
  library(EnvStats) # for ebeta

  #' ---------------------------------
  # preparation ----------------------
  #' ---------------------------------

    load("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas17.Rdata")
    actas17 <- actas17[-which(actas17$ELECTORES_REGISTRO_pres<100),] # delette polling stations with <100 eligible voters
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
    actas17$under_pres_consulta <- (actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_consulta) ## same number as in descriptive analysis? I deleted all polling stations with n<100 
    undervoting_sd <- sd(actas17$under_pres_consulta[actas17$under_pres_consulta!=0], na.rm=T) 
    
    
  #' -----------------------------------
  # simulate clean and frauded data ----
  #' -----------------------------------
  
    df_list <- list()  
    id <- 0
    for(par in seq(0, 0.9, 0.01)) {
      id <- id+1  
      df_list[[id]] <- gen_data(entities = actas17$ELECTORES_REGISTRO_pres, 
                                turnout_probs = turnout_probs, 
                                winner_probs = winner_probs, 
                                undervoting_n = length(which(actas17$under_pres_consulta>0)), 
                                undervoting_sd = undervoting_sd, 
                                fraud_para = 0
                               )
    } 

 
  #' -----------------------------
  # compare to empirical data ----
  #' -----------------------------

    # empirical 
    par(mfrow=c(1,1))
    plot(actas17$under_pres_consulta[actas17$under_pres_consulta>0], 
         actas17$pw_pres[actas17$under_pres_consulta>0], 
         xlim=c(0,1), col="darkgrey",  bty="n", pch=20
    )
    lw1 <- loess(pw_pres ~ under_pres_consulta, data=actas17[actas17$under_pres_consulta>0,])
    j <- order(actas17[actas17$under_pres_consulta>0,]$under_pres_consulta)
    lines(actas17[actas17$under_pres_consulta>0,]$under_pres_consulta[j],lw1$fitted[j],col="lightblue")
    
    # simulated clean 
    df <- df_list[[2]]
    df$under_share <- (df$turnout_a - df$turnout_b) / df$turnout_a
    plot(df$under_share[df$under!=0], 
         df$winner_share[df$under!=0], 
         xlim=c(0,1), col="darkgrey",  bty="n", pch=20
    )
    lw1 <- loess(winner_share ~ under_share, data=df[df$under!=0,])
    j <- order(df[df$under!=0,]$under_share)
    lines(df[df$under!=0,]$under_share[j],lw1$fitted[j],col="lightblue")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    



