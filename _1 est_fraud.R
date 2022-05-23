# define function 
est_fraud <- function(entities,         # vector with eligible voters
                      turnout_probs,    # binomial success probs, turnout
                      winner_probs,     # binomial success probs, votes for winner
                      undervoting_n,    # n of entities with undervoting
                      undervoting_sd,   # sd of undervoting distribution 
                      underperc_emp,    # undervoting extent empirical of length(entities)
                      pw_emp,           # winner's vote share empirical of length(entities)
                      n_iter = 1,       # outer iterations
                      k = 100,           # inner iterations
                      under = NA,       # vector of undervoting discrepancies (empirical)
                      ids = NA          # vector of ids with undervoting discrepancies (empirical)
                      ) {
  
  dat_emp <- as.data.frame(cbind(underperc_emp, pw_emp))
  dat_emp <- dat_emp[order(dat_emp$pw_emp),]
  colnames(dat_emp) <- c("under_perc", "winner_share")
  
  euc_estimate <- mahalanobis_estimate <-
    rep(NA, n_iter)
  
  for (iter in 1:n_iter) {
    
    # simulate data under different share_fraud parameters
    euc_dist <- rep(NA, length(seq(0, 0.99,0.01)))
    mahalanobis_dist <- rep(NA, length(seq(0, 0.99,0.01)))
    id <- 0    
    
    for (share in seq(0, 0.99,0.01)) {
      euc_dist_k <- rep(NA, k)
      mahalanobis_dist_k <- rep(NA, k)
      id <- id+1
      id_k <- 0
        
      for (k_iter in 1:k) {
        id_k <- id_k+1
        df <- gen_data(entities = entities, 
                       turnout_probs = turnout_probs, 
                       winner_probs = winner_probs, 
                       undervoting_n = undervoting_n, 
                       undervoting_sd = undervoting_sd, 
                       share_fraud = share, 
                       under = under, 
                       ids = ids)
      
        # calculate distance metric between p(under_share, winner_share) and 
        # p(underperc_emp, pw_emp) for every data pair 
        dat_sim <- as.data.frame(cbind(df$under_perc, df$winner_share))
        colnames(dat_sim) <- c("under_perc", "winner_share")
        dat_sim <- dat_sim[dat_sim$under_perc!=0,]
        dat_sim <- dat_sim[order(dat_sim$winner_share),]
        
        # calculate Euclidean distance between each row in dat_emp and dat_sim, sum up
        euc_dist_k[id_k] <- sum(diag(fields::rdist(dat_emp, dat_sim)))
        mahalanobis_dist_k[id_k] <- sum(mahalanobis(dat_emp, colMeans(dat_sim), cov(dat_sim)))
      } # end for k_iter
      
      euc_dist[id] <- mean(euc_dist_k)
      mahalanobis_dist[id] <- mean(mahalanobis_dist_k)
      print(id)
      
    } # end for share
 
    # identify fraud parameter that minimizes distance metric
    euc_estimate[iter] <- seq(0, 0.99, 0.01)[which.min(euc_dist)]
    mahalanobis_estimate[iter] <- seq(0, 0.99, 0.01)[which.min(mahalanobis_dist)]
    print(iter)
    
  } # end for iter
  
  out_list <- list()
  out_list[["euclidean"]] <- c(mean(euc_estimate), 
                               quantile(sort(euc_estimate), 0.025),
                               quantile(sort(euc_estimate), 0.975)
                                )
  out_list[["mahalanobis"]] <- c(mean(mahalanobis_estimate), 
                                 quantile(sort(mahalanobis_estimate), 0.025),
                                 quantile(sort(mahalanobis_estimate), 0.975)
                                 )
  return(out_list)
 
}
