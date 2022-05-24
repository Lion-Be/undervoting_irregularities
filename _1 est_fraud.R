# define function 
est_fraud <- function(entities,         # vector with eligible voters
                      turnout,
                      # turnout_probs,    # binomial success probs, turnout
                      winnershare,
                      # winner_probs,     # binomial success probs, votes for winner
                      undervoting,      # vector with raw undervoting discrepancies
                      underperc_emp,    # undervoting extent empirical of length(entities)
                      pw_emp,           # winner's vote share empirical of length(entities)
                      n_iter = 50,     # number of iterations for mean estimation (estimation uncertainty)
                      n_postdraws = 500,# number of posterior draws for parameters (fundamental uncertainty)
                      n_burnin = 499,   # burn-in period
                      seed = 12345
                      ) {
  
  #' ----------------------------------
  # set up empirical data -------------
  #' ----------------------------------
  dat_emp <- as.data.frame(cbind(underperc_emp, pw_emp))
  dat_emp <- dat_emp[order(dat_emp$pw_emp),]
  colnames(dat_emp) <- c("under_perc", "winner_share")
  
  
  #' ------------------------------------------------------------
  # model fundamental uncertainty around parameters -------------
  #' ------------------------------------------------------------
  
    # rstan setup
    if (!is.element("rstan", (.packages()))) library(rstan)
    rstan_options(auto_write = TRUE) # to avoid recompilation of unchanged Stan programs
    options(mc.cores = parallel::detectCores())
    Sys.setenv(LOCAL_CPP = "-mtune = native")
  
    # get posterior samples for undervoting_sd in rnorm(undervoting_n, 0, undervoting_sd)
    model_undervoting <- as.character("
    data {
      int<lower = 1> N;  // Total number of trials
      vector[N] undervoting;  // Score in each trial
    }
    
    parameters {
      real mu;
      real<lower = 0> sigma;
    }
    
    model {
      // Priors:
      target += normal_lpdf(mu | 0, 1000);
      target += inv_gamma_lpdf(sigma | 0.001, 0.001);
      // Likelihood:
      for(i in 1:N)
        target += normal_lpdf(undervoting[i] | mu, sigma);
    }
    
    ")
    write(x=model_undervoting, file="model_undervoting.stan", append=FALSE)
    undervoting_data <- list(undervoting = undervoting, N = length(undervoting))
    m_undervoting <- stan(
      file = "model_undervoting.stan",
      data = undervoting_data, 
      chains = 1, 
      iter = n_postdraws, 
      warmup = n_burnin
    )
    undervoting_sigma <- as.matrix(m_undervoting)[,"sigma"]
  
    # get posterior samples for alpha, beta used for turnout_probs 
    model_turnout_probs <- as.character("
    data {
      int<lower = 0> N;  // Total number of trials
      real turnout[N];  // Score in each trial
    }
    
    parameters {
     real<lower = 0> alpha;
     real<lower = 0> beta;
    }
    
    model {
     alpha ~ normal(0, 1000);
     beta ~ normal(0, 1000);
     target += beta_lpdf(turnout | alpha, beta); // same as below
    //x ~ beta(alpha, beta);
    }
    
    ")
    write(x=model_turnout_probs, file="model_turnout_probs.stan", append=FALSE)
    turnout <- turnout[-which(is.na(turnout) | turnout == 0 | turnout ==1)]
    turnout_data <- list(turnout = turnout, N = length(turnout))
    m_turnout <- stan(
      file = "model_turnout_probs.stan",
      data = turnout_data, 
      chains = 1, 
      iter = n_postdraws, 
      warmup = n_burnin
    )
    turnout_alpha <- as.matrix(m_turnout)[,"alpha"] 
    turnout_beta <- as.matrix(m_turnout)[,"beta"]
    
    # get posterior samples for alpha, beta used for winner_probs
    model_winnershare_probs <- as.character("
    data {
      int<lower = 0> N;  // Total number of trials
      real winnershare[N];  // Score in each trial
    }
    
    parameters {
     real<lower = 0> alpha;
     real<lower = 0> beta;
    }
    
    model {
     alpha ~ normal(0, 1000);
     beta ~ normal(0, 1000);
     target += beta_lpdf(winnershare | alpha, beta); // same as below
    //x ~ beta(alpha, beta);
    }
    
    ")
    write(x=model_winnershare_probs, file="model_winnershare_probs.stan", append=FALSE)
    winnershare <- winnershare[-which(is.na(winnershare) | winnershare == 0 | winnershare ==1)]
    winnershare_data <- list(winnershare = winnershare, N = length(winnershare))
    m_winnershare <- stan(
      file = "model_winnershare_probs.stan",
      data = winnershare_data, 
      chains = 1, 
      iter = n_postdraws, 
      warmup = n_burnin
    )
    winnershare_alpha <- as.matrix(m_winnershare)[,"alpha"] 
    winnershare_beta <- as.matrix(m_winnershare)[,"beta"]
    
    
  
  #' ---------------------------
  # estimate fraud -------------
  #' --------------------------- 
    
    euc_estimate_postdraw <- rep(NA, n_postdraws)
    for (post_draw in 1:n_postdraws) {
      
      # simulate data under different share_fraud parameters
      euc_dist <- rep(NA, length(seq(0, 0.99,0.02)))
      id_share <- 0    
      
      for (share in seq(0, 0.99,0.02)) {
        euc_dist_iter <- rep(NA, n_iter)
        id_share <- id_share+1
        
        for (iter in 1:n_iter) {
          df <- gen_data(entities = entities, 
                         turnout_probs = rbeta(length(entities), turnout_alpha[post_draw], turnout_beta[post_draw]), 
                         winner_probs = rbeta(length(entities), winnershare_alpha[post_draw], winnershare_beta[post_draw]),  
                         undervoting_n = length(undervoting), 
                         undervoting_sd = undervoting_sigma[post_draw], 
                         share_fraud = share
                         )
        
          # calculate distance metric between p(under_share, winner_share) and 
          # p(underperc_emp, pw_emp) for every data pair 
          dat_sim <- as.data.frame(cbind(df$under_perc, df$winner_share))
          colnames(dat_sim) <- c("under_perc", "winner_share")
          dat_sim <- dat_sim[dat_sim$under_perc!=0,]
          dat_sim <- dat_sim[order(dat_sim$winner_share),]
          
          # calculate Euclidean distance between each row in dat_emp and dat_sim, sum up
          euc_dist_iter[iter] <- sum(diag(fields::rdist(dat_emp, dat_sim)))
        } # end for iter
        
        euc_dist[id_share] <- mean(euc_dist_iter[!euc_dist_iter==Inf])
        print(id_share)
        
      } # end for share
    
      # identify fraud parameter that minimizes distance metric
      euc_estimate_postdraw <- seq(0, 0.99, 0.02)[which.min(euc_dist)]
      print(str_c("post_draw = ", post_draw))
      
    } # end for post_draw
    
    out <- c(mean(euc_estimate_postdraw), 
             quantile(sort(euc_estimate_postdraw), 0.025),
             quantile(sort(euc_estimate_postdraw), 0.975)
             )
   
    return(out)
   
}
