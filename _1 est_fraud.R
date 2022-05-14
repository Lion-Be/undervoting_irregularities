# define function 
est_fraud <- function(entities,         # vector with eligible voters
                      turnout_probs,    # binomial success probs, turnout
                      winner_probs,     # binomial success probs, votes for winner
                      undervoting_n,    # n of entities with undervoting
                      undervoting_sd,   # sd of undervoting distribution 
                      underperc_emp,    # undervoting extent empirical of length(entities)
                      pw_emp,           # winner's vote share empirical of length(entities)
                      n_iter = 1000     # number of iterations to construct uncertainty interval
                      ) {
  
  dat_emp <- as.data.frame(cbind(underperc_emp, pw_emp))
  dat_emp <- dat_emp[order(dat_emp$underperc_emp),]
  colnames(dat_emp) <- c("under_perc", "winner_share")
  
  share_estimate <- rep(NA, n_iter)
  for (iter in 1:n_iter) {
    
    # simulate data under different share_fraud parameters
    df_list <- list()
    euc_dist <- rep(NA, length(seq(0, 0.99,0.01)))
    id <- 0
    for (share in seq(0, 0.99,0.01)) {
      id <- id+1
      df_list[[id]] <- gen_data(entities = entities, 
                                turnout_probs = turnout_probs, 
                                winner_probs = winner_probs, 
                                undervoting_n = undervoting_n, 
                                undervoting_sd = undervoting_sd, 
                                share_fraud = share)
      
      # calculate distance metric between p(under_share, winner_share) and 
      # p(underperc_emp, pw_emp) for every data pair 
      dat_sim <- as.data.frame(cbind(df_list[[id]]$under_perc, df_list[[id]]$winner_share))
      colnames(dat_sim) <- c("under_perc", "winner_share")
      dat_sim <- dat_sim[dat_sim$under_perc!=0,]
      dat_sim <- dat_sim[order(dat_sim$under_perc),]
      #### calculate Euclidean distance between each row in dat_emp and dat_sim, sum up
      euc_dist_ind <- rep(NA, nrow(dat_sim))
      if(nrow(dat_emp) != nrow(dat_sim)) {
        print("nrow(dat_emp) != nrow(dat_sim)")
        break
      } # end if
      
      for (row in 1:nrow(dat_sim)) 
        euc_dist_ind[row] <- dist(rbind(dat_emp[row,], dat_sim[row,]), method="euclidean")
      
      euc_dist[id] <- sum(euc_dist_ind)
    } # end for share
 
    # identify fraud parameter that minimizes distance metric
    share_estimate[iter] <- seq(0, 0.99,0.01)[which.min(euc_dist)]
    
  } # end for iter
  
   return(c(mean(share_estimate), 
           quantile(sort(share_estimate), 0.025),
           quantile(sort(share_estimate), 0.975)
           ))
  
}
