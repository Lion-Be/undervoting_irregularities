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
  winner_share[turnout_b == 0] <- 0 ### couldn't I just delete all polling stations with less than 50 voters
  
  # model undervoting discrepancies
  under <- as.integer(rnorm(undervoting_n, 0, undervoting_sd)) #### does my modeled undervoting look like empirical undervoting?????
  turnout_a <- turnout_b
  ids <- sample(1:length(entities), undervoting_n)
  turnout_a[ids] <- turnout_a[ids] + under
  turnout_share <- turnout_a / entities ### many exceed 1
  
  # introduce fraud
  winner <- winner + rbinom(undervoting_n, under, winner_share + fraud_para)
  winner_share <- winner / turnout_a
  
  # construct data 
  df <- as.data.frame(entities, turnout_a, turnout_share, under, 
                      winner, winner_share)
  return(df)
  
}

#' -----------------------------------
# simulate data for Ecuador 2017 -----
#' -----------------------------------

  ### preparation
  load("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas17.Rdata")
  ## I could delete polling stations with less than 50 eligible voters and with turnout=1
  ## don't I need to model undervoting_sd as empirical sd * 1/5?
  
  turnout_probs <- rnorm(length(actas17$ELECTORES_REGISTRO_pres), 
                         mean(actas17$SUFRAGANTES_pres/actas17$ELECTORES_REGISTRO_pres, na.rm=T), 
                         sd(actas17$SUFRAGANTES_pres/actas17$ELECTORES_REGISTRO_pres, na.rm=T)
                         )
  turnout_probs[turnout_probs > 1] <- 1
  
  winner_probs <- rnorm(length(actas17$ELECTORES_REGISTRO_pres), 
                        mean(actas17$MORENO_pres/actas17$SUFRAGANTES_pres, na.rm=T), 
                        sd(actas17$MORENO_pres/actas17$SUFRAGANTES_pres, na.rm=T)
  )                            
  winner_probs[winner_probs < 0] <- 0
  
  
  undervoting_sd <- sd(actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_consulta, na.rm=T) 
  
  ### simulate
  df <- gen_data(entities = actas17$ELECTORES_REGISTRO_pres, 
                 turnout_probs = turnout_probs, 
                 winner_probs = winner_probs, 
                 undervoting_n = length(which(actas17$under_pres_consulta>0)), 
                 undervoting_sd = undervoting_sd, 
                 fraud_para = 0
                 )

  ### compare to empirical data






