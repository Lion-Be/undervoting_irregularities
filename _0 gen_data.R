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
  winner <- rbinom(length(entities), entities, winner_probs)
  winner_share <- winner / turnout_b
  winner_share[winner_share < 0] <- 0 
  winner_share[winner_share > 1] <- 1 
  
  # model undervoting discrepancies
  under <- as.integer(rnorm(undervoting_n, 0, undervoting_sd)) #### does my modeled undervoting look like empirical undervoting?????
  turnout_a <- turnout_b
  ids <- sample(1:length(entities), undervoting_n)
  turnout_a[ids] <- turnout_a[ids] + under
  turnout_share <- turnout_a / entities ### some exceed 1
  
  # introduce fraud [open]
  favor_winner <- rbinom(undervoting_n, abs(under), winner_share[ids] + fraud_para)
  ids_adding <- ids[1:length(which(under>0))]
  ids_remove <- ids[(length(which(under>0))+1):length(ids)]
  winner[ids_adding] <- winner[ids_adding] + favor_winner[ids_adding]
  winner_share <- winner / turnout_a
  
  # construct data 
  df <- as.data.frame(entities, turnout_a, turnout_share, under, 
                      winner, winner_share)
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

  ### preparation
  load("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas17.Rdata")
  actas17 <- actas17[-which(actas17$ELECTORES_REGISTRO_pres<100),] # delette polling stations with <100 eligible voters
  ## don't I need to model undervoting_sd as empirical sd * 1/5?
  
  # estimate binomial success probabilities for absolute turnout
  actas17$turnout_pres <- actas17$SUFRAGANTES_pres / actas17$ELECTORES_REGISTRO_pres
  actas17$turnout_pres[actas17$turnout_pres > 1] <- 1
  beta_est <- ebeta(actas17$turnout_pres, method="mle")
  
  turnout_probs <- rbeta(length(actas17$ELECTORES_REGISTRO_pres), 
                         shape1 = beta_est$parameters[1], 
                         shape2 = beta_est$parameters[2])
  
  # estimate binomial success probabilities for winner's absolute votes
  actas17$winnershare_pres <- actas17$MORENO_pres/actas17$SUFRAGANTES_pres
  beta_est <- ebeta(actas17$winnershare_pres, method="mle")
  
  winner_probs <- rbeta(length(actas17$ELECTORES_REGISTRO_pres), 
                         shape1 = beta_est$parameters[1], 
                         shape2 = beta_est$parameters[2])
  
  # estimate standard deviation of undervoting discrepancies
  actas17$under_pres_consulta <- (actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_consulta) ## same number as in descriptive analysis? I deleted all polling stations with n<100 
  undervoting_sd <- sd(actas17$under_pres_consulta[actas17$under_pres_consulta!=0], na.rm=T) 
  
  
  ### simulate
  df <- gen_data(entities = actas17$ELECTORES_REGISTRO_pres, 
                 turnout_probs = turnout_probs, 
                 winner_probs = winner_probs, 
                 undervoting_n = length(which(actas17$under_pres_consulta>0)), 
                 undervoting_sd = undervoting_sd, 
                 fraud_para = 0
                 )

  ### compare to empirical data






