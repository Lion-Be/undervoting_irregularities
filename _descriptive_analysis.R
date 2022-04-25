#' ------------------------------------------------------------------------
#' descriptive analysis of undervoting irregularities
#' Local elections 2019
#' Lion Behrens
#' ------------------------------------------------------------------------#

library(scales) # for alpha function in plotting
library(VGAM) # for dlaplace
load("U:/PhD Electoral Fraud/Papers/Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas17.Rdata")

#' ------------------------------
# 1. data preparation -----------
#' ------------------------------

  # turnout
  actas17$turnout_pres <- actas17$SUFRAGANTES_pres / actas17$ELECTORES_REGISTRO_pres
  actas17$turnout_pres[actas17$turnout_pres > 1] <- 1
  actas17$turnout_asam_prov <- actas17$SUFRAGANTES_asam_prov / actas17$ELECTORES_REGISTRO_pres
  actas17$turnout_asam_prov[actas17$turnout_asam_prov > 1] <- 1
  actas17$turnout_asam_nac <- actas17$SUFRAGANTES_asam_nac / actas17$ELECTORES_REGISTRO_pres
  actas17$turnout_asam_nac[actas17$turnout_asam_nac > 1] <- 1
  actas17$turnout_andino <- actas17$SUFRAGANTES_andino / actas17$ELECTORES_REGISTRO_pres
  actas17$turnout_andino[actas17$turnout_andino > 1] <- 1
  actas17$turnout_consulta <- actas17$SUFRAGANTES_consulta / actas17$ELECTORES_REGISTRO_pres
  actas17$turnout_consulta[actas17$turnout_consulta > 1] <- 1
  
  # extent of undervoting
  actas17$under_pres_consulta <- abs((actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_consulta) / actas17$SUFRAGANTES_pres)
  actas17$under_pres_asam_prov <- abs((actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_pres)
  actas17$under_nac_prov <- abs((actas17$SUFRAGANTES_asam_nac - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_asam_nac)
  actas17$under_andino_prov <- abs((actas17$SUFRAGANTES_andino - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_andino)
  actas17$under_consulta_prov <- abs((actas17$SUFRAGANTES_consulta - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_consulta)
  
  
  
  
  
  # winner's vote share
  actas17$pw_pres <- actas17$MORENO_pres / actas17$SUFRAGANTES_pres
  
  
  
  
#' ---------------------------
# 2. visualization -----------
#' --------------------------- 

  # univariate distribution of undervoting irregularities 
  hist(actas17$SUFRAGANTES_pres[actas17$under_pres_asam_prov!=0] - actas17$SUFRAGANTES_asam_prov[actas17$under_pres_asam_prov!=0], 
       breaks=100, xlim=c(-100,100), col="darkgrey", prob=T, main="", xlab="Untervoting Irregularities")
  hist(actas17$SUFRAGANTES_consulta[actas17$under_consulta_prov!=0] - actas17$SUFRAGANTES_asam_prov[actas17$under_consulta_prov!=0], 
       breaks=100, xlim=c(-100,100), add=T, col=alpha("deeppink", 0.5), prob=T)
  hist(actas17$SUFRAGANTES_asam_nac[actas17$under_nac_prov!=0] - actas17$SUFRAGANTES_asam_prov[actas17$under_nac_prov!=0], 
       breaks=100, xlim=c(-100,100), add=T, col=alpha("lightblue", 0.5), prob=T)
  hist(actas17$SUFRAGANTES_andino[actas17$under_andino_prov!=0] - actas17$SUFRAGANTES_asam_prov[actas17$under_andino_prov!=0], 
       breaks=100, xlim=c(-100,100), add=T, col=alpha("chartreuse", 0.5), prob=T)
  
  ## the undervoting discrepancies are not distributed normal, double exponential (laplace)
  ## distribution describes them way besser, heavier (?) tails
  ## -----> also needs to be changed in formula section of manuscript, I don't assume the 
  ## errors to be normal. 
  ## in formula section: T_i^e itself needs to be distributed as Laplace(mu, b)
  ## MLEs of the double exponential (Laplace) distribution 
  x <- actas17$SUFRAGANTES_andino[actas17$under_andino_prov!=0] - actas17$SUFRAGANTES_asam_prov[actas17$under_andino_prov!=0]
  x_scale <- seq(min(x[!is.na(x)]),max(x[!is.na(x)]),1) 
  location_par <- median(x, na.rm = T)
  scale_par <- sum(abs(x[!is.na(x)]-median(x[!is.na(x)])))/length(x[!is.na(x)])
  
  n_est <- dlaplace(x_scale, 
                    location=location_par, 
                    scale=scale_par)
  lines(x_scale, n_est, col="black", lwd=2)
  ### add text where mu and b is stated at top right corner
  ### add 2017 to top left corner
  ### replicate figure for 2019 data, plot side by side
  
  
  
  # chains sorted by turnout
  actas17 <- actas17[order(actas17$SUFRAGANTES_asam_prov),] 
  actas17$id <- 1:nrow(actas17)
  
  par(mfrow=c(4,1))
  par(mar = c(0, 4.5, 4, 1), xpd=T)
  plot(actas17$id[actas17$SUFRAGANTES_pres<351], actas17$SUFRAGANTES_pres[actas17$SUFRAGANTES_pres<351], 
       type="l", col="darkgrey", 
       bty="n", xaxt="n", yaxt="n", xlab="", ylab="", main="")
  lines(actas17$id[actas17$SUFRAGANTES_asam_prov<351], actas17$SUFRAGANTES_asam_prov[actas17$SUFRAGANTES_asam_prov<351], col="chartreuse3", lwd=2)
  text(9000, 350, labels="Presidential Election", cex=1.2)
  segments(-200, 350, 1000, 350, col="darkgrey", lwd=2)
  text(-3000, 420, labels="2017", cex=1.5, font=2)
  
  par(mar = c(0, 4.5, 0.5, 1))
  plot(actas17$id[actas17$SUFRAGANTES_asam_nac<351], actas17$SUFRAGANTES_asam_nac[actas17$SUFRAGANTES_asam_nac<351], 
       type="l", col="darkgrey", 
       bty="n", xaxt="n", yaxt="n", xlab="", ylab="", main="")
  lines(actas17$id[actas17$SUFRAGANTES_asam_prov<351], actas17$SUFRAGANTES_asam_prov[actas17$SUFRAGANTES_asam_prov<351], col="chartreuse3", lwd=2)
  text(8500, 350, labels="National Parliament", cex=1.2)
  segments(-200, 350, 1000, 350, col="darkgrey", lwd=2)
  
  plot(actas17$id[actas17$SUFRAGANTES_andino<351], actas17$SUFRAGANTES_andino[actas17$SUFRAGANTES_andino<351], 
       type="l", col="darkgrey", 
       bty="n", xaxt="n", yaxt="n", xlab="", ylab="", main="")
  lines(actas17$id[actas17$SUFRAGANTES_asam_prov<351], actas17$SUFRAGANTES_asam_prov[actas17$SUFRAGANTES_asam_prov<351], col="chartreuse3", lwd=2)
  text(8500, 350, labels="Andean Parliament", cex=1.2)
  segments(-200, 350, 1000, 350, col="darkgrey", lwd=2)
  
  par(mar = c(4.5, 4.5, 1, 1)) 
  plot(actas17$id[actas17$SUFRAGANTES_consulta<351], actas17$SUFRAGANTES_consulta[actas17$SUFRAGANTES_consulta<351], 
       type="l", col="darkgrey", ylim=c(0,380), 
       bty="n", xlab="Polling Station ID", ylab="Absolute Turnout", main="", cex.lab=1.2)
  lines(actas17$id[actas17$SUFRAGANTES_asam_prov<351], actas17$SUFRAGANTES_asam_prov[actas17$SUFRAGANTES_asam_prov<351], col="chartreuse3", lwd=2)
  text(9500, 400, labels="National Referendum", cex=1.2)
  text(9500, 340, labels="Regional Parliaments", cex=1.2)
  segments(-200, 400, 1000, 400, col="darkgrey", lwd=2)
  segments(-200, 340, 1000, 340, col="chartreuse", lwd=2)
  
  # saved with width=450, height=700
  
  #### alternative figure: green line plots vote share winner
  #### grey lines plot extent of undervoting. but this is already substantially informative
  
  
  
  
  
  
  
  
  
  # extent of undervoting vs. winner's vote share
  actas17 <- actas17[order(actas17$pw_pres),]
  par(mfrow=c(1,1))
  plot(actas17$pw_pres[actas17$under_pres_consulta<1.01], actas17$under_pres_consulta[actas17$under_pres_consulta<1.01], 
       type="l", col="darkgrey", 
       bty="n", xlab="Moreno Vote Share", ylab="Undervoting Presidential vs. Consulta", main="")
  lines(actas17$pw_pres[actas17$under_pres_asam_prov<1.01], actas17$under_pres_asam_prov[actas17$under_pres_asam_prov<1.01],
        col="lightblue")
  
  
  
  # scatterplot and loess
  par(mfrow=c(1,1))
  plot(actas17$under_pres_consulta[actas17$under_pres_consulta>0], 
       actas17$pw_pres[actas17$under_pres_consulta>0], 
       xlim=c(0,1), col="darkgrey",  bty="n", pch=20
       )
  lw1 <- loess(pw_pres ~ under_pres_consulta, data=actas17[actas17$under_pres_consulta>0,])
  j <- order(actas17[actas17$under_pres_consulta>0,]$under_pres_consulta)
  lines(actas17[actas17$under_pres_consulta>0,]$under_pres_consulta[j],lw1$fitted[j],col="lightblue")
  
  #### => plot scatterplots from simulated elections with different degrees of fraud over each other
  
  
  
  # histograms of winner's vote share if undervoting is present vs. not
  hist(actas17$pw_pres[actas17$under_pres_consulta==0], breaks=100, 
       main="", xlab="Moreno Vote Share", ylab="Frequency")
  abline(v=mean(actas17$pw_pres[actas17$under_pres_consulta==0], na.rm = T))
  hist(actas17$pw_pres[actas17$under_pres_consulta>0], breaks=100, col="red", add=T)
  abline(v=mean(actas17$pw_pres[actas17$under_pres_consulta>0], na.rm = T), col="red")
  
  
  
  
  
  # chains sorted by vote share
  actas17 <- actas17[order(actas17$pw_pres),] 
  actas17$id <- 1:nrow(actas17)
  plot(actas17$id, actas17$turnout_pres, type="l", col="darkgrey")
  lines(actas17$id, actas17$turnout_consulta, col="green")






