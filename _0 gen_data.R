# define function
gen_data <- function(entities,          # vector with eligible voters
                     turnout_probs,     # binomial success probs, turnout
                     winner_probs,      # binomial success probs, votes for winner
                     undervoting_n,     # n of entities with undervoting
                     undervoting_sd,    # sd of undervoting distribution 
                     share_fraud,       # share of polling stations with undervoting at which probabilistic fraud is happening
                     theta_fraud = 0.8  # theta parameter for constructing favor_winner
                     ) {
  
  # model absolute baseline turnout and absolute votes for winner
  turnout_b <- rbinom(length(entities), entities, turnout_probs)
  winner <- rbinom(length(entities), turnout_b, winner_probs)
  others <- turnout_b - winner
  winner_share <- winner/turnout_b
  
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
    favor_winner <- rbinom(length(ids_fraud), 
                           abs(under[1:length(ids_fraud)]), 
                           theta_fraud
                           ) 
    favor_others <- abs(under[1:length(ids_fraud)]) - favor_winner 
      
    ids_adding <- sample(ids_fraud, 
                         length(which(under[1:length(ids_fraud)]>0))
                         )
    winner[ids_adding] <- winner[ids_adding] + favor_winner[under[1:length(ids_fraud)] > 0]
    others[ids_adding] <- others[ids_adding] + favor_others[under[1:length(ids_fraud)] > 0]
    
    ids_remove <- ids_fraud[-which(is.element(ids_fraud, ids_adding))]
    winner[ids_remove] <- winner[ids_remove] - favor_others[under[1:length(ids_fraud)] < 0]
    others[ids_remove] <- others[ids_remove] - favor_winner[under[1:length(ids_fraud)] < 0] 
  } # end if
  
  # add/remove votes proportional to winner's vote share at clean polling stations
  # polling stations with undervoting have same properties (winner vote shares) than polling stations without undervoting
  winner_votes <- rbinom(length(ids_clean), 
                         abs(under[(length(ids_fraud)+1):length(under)]), 
                         winner_share[ids_clean]
                         ) 
  others_votes <- abs(under[(length(ids_fraud)+1):length(under)]) - winner_votes
  
  ids_adding <- sample(ids_clean, 
                       length(which(under[(length(ids_fraud)+1):length(under)]>0))
                       )
  winner[ids_adding] <- winner[ids_adding] + winner_votes[under[(length(ids_fraud)+1):length(under)] > 0]
  others[ids_adding] <- others[ids_adding] + others_votes[under[(length(ids_fraud)+1):length(under)] > 0]
  
  ids_remove <- ids_clean[-which(is.element(ids_clean, ids_adding))]
  winner[ids_remove] <- winner[ids_remove] - winner_votes[under[(length(ids_fraud)+1):length(under)] < 0]
  others[ids_remove] <- others[ids_remove] - others_votes[under[(length(ids_fraud)+1):length(under)] < 0] 
  
  # redefine variables
  winner[which(winner<0)] <- 0
  others[which(others<0)] <- 0
  
  turnout_a <- winner + others
  turnout_a_share <- turnout_a / entities
  turnout_b_share <- turnout_b / entities
  under_perc <- abs((turnout_a - turnout_b) / turnout_a)
  winner_share <- winner / turnout_a
  others_share <- others / turnout_a
  frauded_id <- rep(0, length(entities))
  frauded_id[ids_fraud] <- 1
  
  # construct data 
  under <- turnout_a - turnout_b
  df <- as.data.frame(cbind(entities, turnout_a, turnout_b, under, under_perc, turnout_a_share, turnout_b_share, 
                      winner, others, winner_share, others_share, frauded_id))
  return(df)
  
}
