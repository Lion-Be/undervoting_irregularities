library(EnvStats) # for ebeta
library(stringr)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(ggpubr)
library(fields)

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

  # simulate artifical elections
  sim_elections <- list()  
  id <- 0
  for (share in c(0, 0.2, 0.4, 0.6, 0.8)) {
    id <- id+1  
    sim_elections[[id]] <- gen_data(entities = entities, 
                                    turnout_probs = turnout_probs, 
                                    winner_probs = winner_probs, 
                                    undervoting_n = undervoting_n, 
                                    undervoting_sd = undervoting_sd, 
                                    share_fraud = share, 
                                    under = NA, 
                                    ids = NA
    )
  } 
  names(sim_elections) <- str_c("share_fraud = ", c(0, 0.2, 0.4, 0.6, 0.8))

  # estimate
  est_results_euclid <- as.data.frame(matrix(NA, nrow=length(sim_elections), ncol=4))
  colnames(est_results_euclid) <- c("true_share", "est_share", "95_lower", "95_upper")
  est_results_euclid$true_share <- c(0, 0.2, 0.4, 0.6, 0.8)
  est_results_mahalanobis <- est_results_euclid
  
  for (df in 1:length(sim_elections)) { 
    results <- est_fraud(entities = entities, 
                         turnout_probs = turnout_probs, 
                         winner_probs = winner_probs, 
                         undervoting_n = undervoting_n, 
                         undervoting_sd = undervoting_sd, 
                         underperc_emp = sim_elections[[df]]$under_perc[which(sim_elections[[df]]$under_perc!=0)],
                         pw_emp = sim_elections[[df]]$winner_share[which(sim_elections[[df]]$under_perc!=0)],
                         under = sim_elections[[df]]$under[which(sim_elections[[df]]$under!=0)],
                         ids = which(sim_elections[[df]]$under!=0),
                         n_iter = 1,
                         k = 100
    )
    
    est_results_euclid[df,2:4] <- results[["euclidean"]]
    est_results_mahalanobis[df,2:4] <- results[["mahalanobis"]]
    
    print(df)
    save(est_results_euclid, file="est_results_euclid.RData")
    save(est_results_mahalanobis, file="est_results_mahalanobis.RData")
  }
    

  
  
#' -------------------------------------------------------
# graphically explore effect of fraud on scatterplot -----
# using parameters from Ecuador 2017 
#' -------------------------------------------------------

  # play around with parameters to make plots more informative/more extreme
  # e.g. just higher undervoting_n
  # these don't need to mimic parameters from Ecuador, just orientate on them
  
  ###### fraud needs to become more extreme in order to mirror in scatterplots
  ###### or does something go wrong with binding the data?
  ###### currently: all plots basically look the same
  ###### also: play around with stat_smooth or geom_smooth?
  
  sim_data <- bind_rows(sim_elections)
  sim_data$true_share <- as.factor(c(rep(0, nrow(sim_data)/5),
                           rep(0.2, nrow(sim_data)/5),
                           rep(0.4, nrow(sim_data)/5),
                           rep(0.6, nrow(sim_data)/5),
                           rep(0.8, nrow(sim_data)/5))
  )
  
  theme_set(theme_bw())
  
  fig3.1 <- sim_data[sim_data$true_share==0 & sim_data$under !=0,] %>%
    ggplot(aes(x=under_perc, winner_share)) + 
    xlim(0,1) + ylim(0,1) + 
    xlab("") + ylab("Winner's vote share") + 
    ggtitle("Percentage of frauded polling stations = 0") +
    geom_point(alpha=0.8, color="grey", shape=1, size=3) +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  fig3.1 <- fig3.1 + geom_smooth(method="loess", se=F, col="orange")
  
  fig3.2 <- sim_data[sim_data$true_share==0.2 & sim_data$under !=0,] %>%
    ggplot(aes(x=under_perc, winner_share)) + 
    xlim(0,1) + ylim(0,1) + 
    xlab("") + ylab("") + 
    ggtitle("Percentage of frauded polling stations = 0.2") +
    geom_point(alpha=0.8, color="grey", shape=1, size=3) +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  fig3.2 <- fig3.2 + geom_smooth(method="loess", se=F, col="orange")
  
  fig3.3 <- sim_data[sim_data$true_share==0.4 & sim_data$under !=0,] %>%
    ggplot(aes(x=under_perc, winner_share)) + 
    xlim(0,1) + ylim(0,1) + 
    xlab("Percentage of discrepant votes") + ylab("Winner's vote share") + 
    ggtitle("Percentage of frauded polling stations = 0.4") +
    geom_point(alpha=0.8, color="grey", shape=1, size=3) +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  fig3.3 <- fig3.3 + geom_smooth(method="loess", se=F, col="orange")
  
  fig3.4 <- sim_data[sim_data$true_share==0.6 & sim_data$under !=0,] %>%
    ggplot(aes(x=under_perc, winner_share)) + 
    xlim(0,1) + ylim(0,1) + 
    xlab("Percentage of discrepant votes") + ylab("") + 
    ggtitle("Percentage of frauded polling stations = 0.6") +
    geom_point(alpha=0.8, color="grey", shape=1, size=3) +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  fig3.4 <- fig3.4 + geom_smooth(method="loess", se=F, col="orange")
  
  ggarrange(fig3.1, fig3.2, fig3.3, fig3.4, 
            ncol=2, nrow=2)
  
  
  
 







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

