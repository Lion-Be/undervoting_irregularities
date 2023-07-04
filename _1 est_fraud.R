# define function 
est_fraud <- function(eligible, # vector with eligible voters
                      turnout_main, # vector with absolute turnout across entities in main election, n=length(eligible)
                      turnout_baseline, # vector with absolute turnout across entities in baseline election, n=length(eligible)
                      winner_main = NA, # vector with absolute number of votes for winner in main election
                      winnershare_main = NA, # vector with share of votes for winner in main election
                      uncertainty = c("fundamental", "estimation"), # which types of uncertainty should be incorporated 
                      q = 50,      # number of iterations underlying mean estimation (estimation uncertainty)
                      n_iter2 = 100, # number of times mean is estimated (estimation uncertainty)
                      n_postdraws = 500,# number of posterior draws for parameters (fundamental uncertainty)
                      n_burnin = 400,   # burn-in period (fundamental uncertainty)
                      seed = 12345 # reproduction
                      ) {
  
  #' -------------------
  # set up -------------
  #' -------------------
  set.seed(seed)
  
  if (!is.element("estimation", uncertainty))
    stop("is.element('estimation', uncertainty) == FALSE. Model cannot be fit without incorporating estimation uncertainty.")
  
  turnoutshare_baseline <- turnout_baseline / eligible
  turnoutshare_baseline[turnoutshare_baseline > 1] <- 1
  if (is.na(winnershare_main)[1])
    winnershare_main <- winner_main / turnout_main
  winnershare_main[winnershare_main < 0] <- 0
  winnershare_main[winnershare_main > 1] <- 1
  undervoting <- turnout_main - turnout_baseline
  undervoting <- undervoting[-which(is.na(undervoting) | undervoting == 0)]
  underperc_emp <- abs((turnout_main - turnout_baseline) / turnout_main)
  underperc_emp <- underperc_emp[which(turnout_main != turnout_baseline)]
  
  #' ---------------------------------------------
  # construct data frame (empirical) -------------
  #' ---------------------------------------------
  pw_emp <- winnershare_main[which(turnout_main != turnout_baseline)]
  dat_emp <- as.data.frame(cbind(underperc_emp, pw_emp))
  dat_emp <- dat_emp[order(dat_emp$pw_emp),]
  colnames(dat_emp) <- c("under_perc", "winner_share")
  
  
  #' ----------------------------------------------------------
  # estimate parameters from data (point estimates, MLE) ------
  #' ----------------------------------------------------------
  if (!is.element("fundamental", uncertainty)) {
  
    # estimate standard deviation of undervoting distribution (only non-zero entries)
    undervoting_sigma <- sd(undervoting) 
    
    # estimate binomial success probabilities for absolute turnout
    turnout_beta_est <- ebeta(turnoutshare_baseline, method="mle")
    turnout_alpha <- turnout_beta_est$parameters[1]
    turnout_beta <- turnout_beta_est$parameters[2]
    
    # estimate binomial success probabilities for winner's absolute votes
    winner_beta_est <- ebeta(winnershare_main, method="mle")
    winnershare_alpha <- winner_beta_est$parameters[1]
    winnershare_beta <- winner_beta_est$parameters[2]
    
  } # end if
  
  
  #' ---------------------------------------------------------------------------------------
  # model fundamental uncertainty around parameters (posteriors, Bayesian estimation) ------
  #' ---------------------------------------------------------------------------------------
  if (is.element("fundamental", uncertainty)) {
    
    # rstan setup
    if (!is.element("rstan", (.packages()))) library(rstan)
    #rstan_options(auto_write = TRUE) # to avoid recompilation of unchanged Stan programs
    #options(mc.cores = parallel::detectCores())
    #Sys.setenv(LOCAL_CPP = "-mtune = native")
    
    
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
    if (length(which(turnoutshare_baseline == 0 | turnoutshare_baseline ==1)) > 0)
      turnoutshare_baseline <- turnoutshare_baseline[-which(turnoutshare_baseline == 0 | turnoutshare_baseline ==1)]
    turnout_data <- list(turnout = turnoutshare_baseline, N = length(turnoutshare_baseline))
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
    if (length(which(winnershare_main == 0 | winnershare_main ==1)) > 0)
      winnershare_main <- winnershare_main[-which(winnershare_main == 0 | winnershare_main ==1)]
    winnershare_data <- list(winnershare = winnershare_main, N = length(winnershare_main))
    m_winnershare <- stan(
      file = "model_winnershare_probs.stan",
      data = winnershare_data, 
      chains = 1, 
      iter = n_postdraws, 
      warmup = n_burnin
    )
    winnershare_alpha <- as.matrix(m_winnershare)[,"alpha"] 
    winnershare_beta <- as.matrix(m_winnershare)[,"beta"]
    
  } # end if
  
  
  #' ---------------------------
  # estimate fraud -------------
  #' --------------------------- 
  
    if (!is.element("fundamental", uncertainty)) {
      # simply iterate over fraud estimation n_iter2 times, always with 
      # same parameter values estimated by MLE
      n_postdraws <- n_iter2
      turnout_alpha <- rep(turnout_alpha, n_postdraws)
      turnout_beta <- rep(turnout_beta, n_postdraws)
      winnershare_alpha <- rep(winnershare_alpha, n_postdraws)
      winnershare_beta <- rep(winnershare_beta, n_postdraws)
      undervoting_sigma <- rep(undervoting_sigma, n_postdraws)
    }
  
    # define number of iterations of Steps 3-5
    iterations <- ifelse(is.element("fundamental", uncertainty),
                         n_postdraws - n_burnin, 
                         n_iter2)
    
    euc_estimate_postdraw <- rep(NA, iterations)
    for (post_draw in 1:iterations) {
      
      # simulate data under different share_fraud parameters
      euc_dist <- rep(NA, length(seq(0, 0.99,0.02)))
      id_share <- 0    
      
      for (share in seq(0, 0.99,0.02)) {
        euc_dist_iter <- rep(NA, q)
        id_share <- id_share+1
        
        for (iter in 1:q) {
          df <- gen_data(entities = eligible, 
                         turnout_probs = rbeta(length(eligible), turnout_alpha[post_draw], turnout_beta[post_draw]), 
                         winner_probs = rbeta(length(eligible), winnershare_alpha[post_draw], winnershare_beta[post_draw]),  
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
          x <- diag(fields::rdist(dat_emp, dat_sim))[which(!is.nan(diag(fields::rdist(dat_emp, dat_sim))))]
          euc_dist_iter[iter] <- sum(x)
        } # end for iter
        
        euc_dist[id_share] <- mean(euc_dist_iter[!euc_dist_iter==Inf])
        print(str_c("+++Simulating election results, share_fraud = ", share, "+++"))
        
      } # end for share
    
      # identify fraud parameter that minimizes distance metric
      if (length(seq(0, 0.99, 0.02)[which.min(euc_dist)]) > 0)
        euc_estimate_postdraw[post_draw] <- seq(0, 0.99, 0.02)[which.min(euc_dist)]
      
      print(str_c("Simulations finished for ", post_draw, " out of ", iterations, " iterations of Steps 3-5."))
      
    } # end for post_draw
    
    out <- c(mean(euc_estimate_postdraw, na.rm = T), 
             quantile(sort(euc_estimate_postdraw), 0.025, na.rm = T),
             quantile(sort(euc_estimate_postdraw), 0.975, na.rm = T)
             )
   
    return(out)
   
}
