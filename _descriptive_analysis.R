#' ------------------------------------------------------------------------
#' descriptive analysis of undervoting irregularities
#' Local elections 2019
#' Lion Behrens
#' ------------------------------------------------------------------------#

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
  
  # winner's vote share
  actas17$pw_pres <- actas17$MORENO_pres / actas17$SUFRAGANTES_pres
  
  
  
  
#' ---------------------------
# 2. visualization -----------
#' --------------------------- 

  # chains sorted by turnout
  actas17 <- actas17[order(actas17$SUFRAGANTES_consulta),] 
  actas17$id <- 1:nrow(actas17)
  
  par(mfrow=c(2,2))
  plot(actas17$id, actas17$SUFRAGANTES_pres, 
       type="l", col="darkgrey", 
       bty="n", xlab="", ylab="Number of Turned Out Voters", main="Presidential Election")
  lines(actas17$id, actas17$SUFRAGANTES_consulta, col="green", lwd=2)
  
  plot(actas17$id, actas17$SUFRAGANTES_asam_nac, 
       type="l", col="darkgrey", 
       bty="n", xlab="", ylab="", main="National Parliament")
  lines(actas17$id, actas17$SUFRAGANTES_consulta, col="green", lwd=2)
  
  plot(actas17$id, actas17$SUFRAGANTES_asam_prov, 
       type="l", col="darkgrey", 
       bty="n", xlab="Polling Station ID", ylab="Number of Turned Out Voters", main="Regional Parlaments")
  lines(actas17$id, actas17$SUFRAGANTES_consulta, col="green", lwd=2)
  
  plot(actas17$id, actas17$SUFRAGANTES_andino, 
       type="l", col="darkgrey", 
       bty="n", xlab="Polling Station ID", ylab="", main="Andean Parliament")
  lines(actas17$id, actas17$SUFRAGANTES_consulta, col="green", lwd=2)
  
    ### maybe change the scale to absolute levels?
  
  
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
       xlim=c(0,1)
       )
  lw1 <- loess(pw_pres ~ under_pres_consulta, data=actas17[actas17$under_pres_consulta>0,])
  j <- order(actas17[actas17$under_pres_consulta>0,]$under_pres_consulta)
  lines(actas17[actas17$under_pres_consulta>0,]$under_pres_consulta[j],lw1$fitted[j],col="lightblue")
  
  
  
  
  
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






