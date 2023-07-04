#' ------------------------------------------------------------------------
#' descriptive analysis of undervoting irregularities
#' Ecuadorian elections 2017, 2019
#' Lion Behrens
#' ------------------------------------------------------------------------#

library(scales) # for alpha function in plotting
library(VGAM) # for dlaplace
load("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas17.Rdata")
actas17 <- actas17[-which(actas17$ELECTORES_REGISTRO_pres<100),] # delete polling stations with <100 eligible voters

load("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas19.Rdata")
actas19_alcal_conc_rural <- actas19_alcal_conc_rural[-which(actas19_alcal_conc_rural$ELECTORES_REGISTRO_alcal<100),] # delete polling stations with <100 eligible voters
actas19_alcal_conc_urban <- actas19_alcal_conc_urban[-which(actas19_alcal_conc_urban$ELECTORES_REGISTRO_alcal<100),] # delete polling stations with <100 eligible voters
actas19_alcal_pref <- actas19_alcal_pref[-which(actas19_alcal_pref$ELECTORES_REGISTRO_alcal<100),] # delete polling stations with <100 eligible voters
actas19_alcal_vocales <- actas19_alcal_vocales[-which(actas19_alcal_vocales$ELECTORES_REGISTRO_alcal<100),] # delete polling stations with <100 eligible voters


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
  actas19_alcal_conc_rural$under <- abs(actas19_alcal_conc_rural$SUFRAGANTES_alcal - actas19_alcal_conc_rural$SUFRAGANTES_conc_rural) / actas19_alcal_conc_rural$SUFRAGANTES_alcal
  actas19_alcal_conc_urban$under <- abs(actas19_alcal_conc_urban$SUFRAGANTES_alcal - actas19_alcal_conc_urban$SUFRAGANTES_conc_urban) / actas19_alcal_conc_urban$SUFRAGANTES_alcal
  actas19_alcal_pref$under <- abs(actas19_alcal_pref$SUFRAGANTES_alcal - actas19_alcal_pref$SUFRAGANTES_pref) / actas19_alcal_pref$SUFRAGANTES_alcal
  actas19_alcal_vocales$under <- abs(actas19_alcal_vocales$SUFRAGANTES_alcal - actas19_alcal_vocales$SUFRAGANTES_vocales) / actas19_alcal_vocales$SUFRAGANTES_alcal
  
  # winner's vote share
  actas17$pw_pres <- actas17$MORENO_pres / actas17$SUFRAGANTES_pres
  
  
  
  
#' ---------------------------
# 2. visualization -----------
#' --------------------------- 

  #' ---------------------------------------------------------------------
  # 2.1 univariate distribution of undervoting irregularities  -----------
  #' --------------------------------------------------------------------- 
  
    ### 2017 election 
    par(mfrow=c(1,1))
    hist(actas17$SUFRAGANTES_pres[actas17$under_pres_asam_prov!=0] - actas17$SUFRAGANTES_asam_prov[actas17$under_pres_asam_prov!=0], 
         breaks=100, xlim=c(-100,100), col="darkgrey", prob=T, main="", xlab="Untervoting Irregularities", cex.lab=1.2)
    par(xpd=T)
    text(-100, 0.09, labels="2017", cex=1.5, font=2)
    par(xpd=F)
    hist(actas17$SUFRAGANTES_consulta[actas17$under_consulta_prov!=0] - actas17$SUFRAGANTES_asam_prov[actas17$under_consulta_prov!=0], 
         breaks=100, xlim=c(-100,100), add=T, col=alpha("deeppink", 0.5), prob=T)
    hist(actas17$SUFRAGANTES_asam_nac[actas17$under_nac_prov!=0] - actas17$SUFRAGANTES_asam_prov[actas17$under_nac_prov!=0], 
         breaks=100, xlim=c(-100,100), add=T, col=alpha("lightblue", 0.5), prob=T)
    hist(actas17$SUFRAGANTES_andino[actas17$under_andino_prov!=0] - actas17$SUFRAGANTES_asam_prov[actas17$under_andino_prov!=0], 
         breaks=100, xlim=c(-100,100), add=T, col=alpha("chartreuse", 0.5), prob=T)
   
    
    # fit normal with MLEs 
    x_scale <- seq(-100,100,1)
    x <- actas17$SUFRAGANTES_andino[actas17$under_andino_prov!=0] - actas17$SUFRAGANTES_asam_prov[actas17$under_andino_prov!=0]
    n_est <- dnorm(x_scale, 
                   mean=mean(x, na.rm=T),
                   sd=sd(x, na.rm=T))
    lines(x_scale, n_est, col="black", lwd=3, lty=3)
    
    # fit normal with adjusted MLEs 
    n_est <- dnorm(x_scale, 
                   mean=mean(x, na.rm=T),
                   sd=(1/2) * sd(x, na.rm=T))
    lines(x_scale, n_est, col="black", lwd=3, lty=2)
    
    n_est <- dnorm(x_scale, 
                   mean=mean(x, na.rm=T),
                   sd=(1/5) * sd(x, na.rm=T))
    lines(x_scale, n_est, col="black", lwd=3, lty=1)
    
    # add legends
    legend(25, 0.08, legend=c(expression(paste(mu,"= 0, ",sigma," = 1/5 * sd(x)")), 
                              expression(paste(mu,"= 0, ",sigma," = 1/2 * sd(x)")),
                              expression(paste(mu,"= 0, ",sigma," = sd(x) (MLE)"))),
           lty=c(1,2,3), lwd=c(3,3,3), bty = "n", cex=1.2)
    
    legend(-100, 0.08, legend=c("Presidential Election",
                               "National Parliament",
                               "Andean Parliament",
                               "National Referendum"),
           col=c("darkgrey", "deeppink", "lightblue", "chartreuse"), 
           lwd=3, bty = "n", cex=1.2)
    ## saved with width=1000, height=850
    
    
    ### 2019 election 
    par(mfrow=c(1,1))
    hist(actas19_alcal_conc_rural$SUFRAGANTES_alcal[actas19_alcal_conc_rural$under!=0] - actas19_alcal_conc_rural$SUFRAGANTES_conc_rural[actas19_alcal_conc_rural$under!=0], 
         breaks=100, xlim=c(-100,100), ylim=c(0,0.2), col="darkgrey", prob=T, main="", xlab="Untervoting Irregularities", cex.lab=1.2)
    par(xpd=T)
    text(-100, 0.23, labels="2019", cex=1.5, font=2)
    par(xpd=F)
    hist(actas19_alcal_conc_urban$SUFRAGANTES_alcal[actas19_alcal_conc_urban$under!=0] - actas19_alcal_conc_urban$SUFRAGANTES_conc_urban[actas19_alcal_conc_urban$under!=0], 
         breaks=100, xlim=c(-100,100), add=T, col=alpha("deeppink", 0.5), prob=T)
    hist(actas19_alcal_pref$SUFRAGANTES_alcal[actas19_alcal_pref$under!=0] - actas19_alcal_pref$SUFRAGANTES_pref[actas19_alcal_pref$under!=0], 
         breaks=100, xlim=c(-100,100), add=T, col=alpha("lightblue", 0.5), prob=T)
    hist(actas19_alcal_vocales$SUFRAGANTES_alcal[actas19_alcal_vocales$under!=0] - actas19_alcal_vocales$SUFRAGANTES_vocales[actas19_alcal_vocales$under!=0], 
         breaks=100, xlim=c(-100,100), add=T, col=alpha("chartreuse", 0.5), prob=T)
    
    
    # fit normal with MLEs 
    x_scale <- seq(-100,100,1)
    x <- actas19_alcal_vocales$SUFRAGANTES_alcal[actas19_alcal_vocales$under!=0] - actas19_alcal_vocales$SUFRAGANTES_vocales[actas19_alcal_vocales$under!=0]
    n_est <- dnorm(x_scale, 
                   mean=mean(x, na.rm=T),
                   sd=sd(x, na.rm=T))
    lines(x_scale, n_est, col="black", lwd=3, lty=3)
    
    # fit normal with adjusted MLEs 
    n_est <- dnorm(x_scale, 
                   mean=mean(x, na.rm=T),
                   sd=(1/2) * sd(x, na.rm=T))
    lines(x_scale, n_est, col="black", lwd=3, lty=2)
    
    n_est <- dnorm(x_scale, 
                   mean=mean(x, na.rm=T),
                   sd=(1/5) * sd(x, na.rm=T))
    lines(x_scale, n_est, col="black", lwd=3, lty=1)
    
    # add legends
    legend(25, 0.2, legend=c(expression(paste(mu,"= 0, ",sigma," = 1/5 * sd(x)")), 
                              expression(paste(mu,"= 0, ",sigma," = 1/2 * sd(x)")),
                              expression(paste(mu,"= 0, ",sigma," = sd(x) (MLE)"))),
           lty=c(1,2,3), lwd=c(3,3,3), bty = "n", cex=1.2)
    
    legend(-100, 0.2, legend=c("Rural Councilors",
                               "Urban Councilors",
                               "Provincial Prefects",
                               "Members of Parish Boards"),
           col=c("darkgrey", "deeppink", "lightblue", "chartreuse"), 
           lwd=3, bty = "n", cex=1.2)
    ## saved with width=1000, height=850
    

  #' ------------------------------------------------------------
  # 2.2 undervoting irregularities sorted by turnout  -----------
  #' ------------------------------------------------------------ 
  
    ### 2017 election
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
    text(36500, 20, labels="n=39,319", cex=1.2)
    
    par(mar = c(0, 4.5, 0.5, 1))
    plot(actas17$id[actas17$SUFRAGANTES_asam_nac<351], actas17$SUFRAGANTES_asam_nac[actas17$SUFRAGANTES_asam_nac<351], 
         type="l", col="darkgrey", 
         bty="n", xaxt="n", yaxt="n", xlab="", ylab="", main="")
    lines(actas17$id[actas17$SUFRAGANTES_asam_prov<351], actas17$SUFRAGANTES_asam_prov[actas17$SUFRAGANTES_asam_prov<351], col="chartreuse3", lwd=2)
    text(8500, 350, labels="National Parliament", cex=1.2)
    segments(-200, 350, 1000, 350, col="darkgrey", lwd=2)
    text(36500, 20, labels="n=39,319", cex=1.2)
    
    plot(actas17$id[actas17$SUFRAGANTES_andino<351], actas17$SUFRAGANTES_andino[actas17$SUFRAGANTES_andino<351], 
         type="l", col="darkgrey", 
         bty="n", xaxt="n", yaxt="n", xlab="", ylab="", main="")
    lines(actas17$id[actas17$SUFRAGANTES_asam_prov<351], actas17$SUFRAGANTES_asam_prov[actas17$SUFRAGANTES_asam_prov<351], col="chartreuse3", lwd=2)
    text(8500, 350, labels="Andean Parliament", cex=1.2)
    segments(-200, 350, 1000, 350, col="darkgrey", lwd=2)
    text(36500, 20, labels="n=39,319", cex=1.2)
    
    par(mar = c(4.5, 4.5, 1, 1)) 
    plot(actas17$id[actas17$SUFRAGANTES_consulta<351], actas17$SUFRAGANTES_consulta[actas17$SUFRAGANTES_consulta<351], 
         type="l", col="darkgrey", ylim=c(0,380), 
         bty="n", xlab="Polling Station ID", ylab="Absolute Turnout", main="", cex.lab=1.2)
    lines(actas17$id[actas17$SUFRAGANTES_asam_prov<351], actas17$SUFRAGANTES_asam_prov[actas17$SUFRAGANTES_asam_prov<351], col="chartreuse3", lwd=2)
    text(9500, 400, labels="National Referendum", cex=1.2)
    text(9500, 340, labels="Regional Parliaments", cex=1.2)
    segments(-200, 400, 1000, 400, col="darkgrey", lwd=2)
    segments(-200, 340, 1000, 340, col="chartreuse", lwd=2)
    text(36500, 20, labels="n=39,319", cex=1.2)
    # saved with width=450, height=700
  
    ### 2019 election
    actas19_alcal_vocales <- actas19_alcal_vocales[order(actas19_alcal_vocales$SUFRAGANTES_alcal),] 
    actas19_alcal_vocales$id <- 1:nrow(actas19_alcal_vocales)
    par(mfrow=c(4,1))
    par(mar = c(0, 4.5, 4, 1), xpd=T)
    plot(actas19_alcal_vocales$id, actas19_alcal_vocales$SUFRAGANTES_vocales, 
         type="l", col="darkgrey", 
         bty="n", xaxt="n", yaxt="n", xlab="", ylab="", main="")
    lines(actas19_alcal_vocales$id, actas19_alcal_vocales$SUFRAGANTES_alcal, col="chartreuse3", lwd=2)
    text(-300, 440, labels="2019", cex=1.5, font=2)
    text(1450, 350, labels="Members of Parish Boards", cex=1.2)
    segments(-200, 350, 0.02436528*nrow(actas19_alcal_vocales), 350, col="darkgrey", lwd=2)
    text(5450, 20, labels="n=5,746", cex=1.2)
    
    actas19_alcal_conc_rural <- actas19_alcal_conc_rural[order(actas19_alcal_conc_rural$SUFRAGANTES_alcal),] 
    actas19_alcal_conc_rural$id <- 1:nrow(actas19_alcal_conc_rural)
    plot(actas19_alcal_conc_rural$id, actas19_alcal_conc_rural$SUFRAGANTES_conc_rural, 
         type="l", col="darkgrey", 
         bty="n", xaxt="n", yaxt="n", xlab="", ylab="", main="")
    lines(actas19_alcal_conc_rural$id, actas19_alcal_conc_rural$SUFRAGANTES_alcal, col="chartreuse3", lwd=2)
    text(1100, 350, labels="Rural Councilors", cex=1.2)
    segments(-200, 350, 0.02436528*nrow(actas19_alcal_conc_rural), 350, col="darkgrey", lwd=2)
    text(6000, 20, labels="n=6,359", cex=1.2)
    
    par(mar = c(0, 4.5, 0.5, 1))
    actas19_alcal_conc_urban <- actas19_alcal_conc_urban[order(actas19_alcal_conc_urban$SUFRAGANTES_alcal),] 
    actas19_alcal_conc_urban$id <- 1:nrow(actas19_alcal_conc_urban)
    plot(actas19_alcal_conc_urban$id, actas19_alcal_conc_urban$SUFRAGANTES_conc_urban, 
         type="l", col="darkgrey", 
         bty="n", xaxt="n", yaxt="n", xlab="", ylab="", main="")
    lines(actas19_alcal_conc_urban$id, actas19_alcal_conc_urban$SUFRAGANTES_alcal, col="chartreuse3", lwd=2)
    text(2000, 350, labels="Urban Councilors", cex=1.2)
    segments(-200, 350, 0.02436528*nrow(actas19_alcal_conc_urban), 350, col="darkgrey", lwd=2)
    text(10600, 20, labels="n=11,375", cex=1.2)
    
    par(mar = c(4.5, 4.5, 1, 1)) 
    actas19_alcal_pref <- actas19_alcal_pref[order(actas19_alcal_pref$SUFRAGANTES_alcal),] 
    actas19_alcal_pref$id <- 1:nrow(actas19_alcal_pref)
    plot(actas19_alcal_pref$id, actas19_alcal_pref$SUFRAGANTES_pref, 
         type="l", col="darkgrey", ylim=c(0,380), 
         bty="n", xlab="Polling Station ID", ylab="Absolute Turnout", main="", cex.lab=1.2)
    lines(actas19_alcal_pref$id, actas19_alcal_pref$SUFRAGANTES_alcal, col="chartreuse3", lwd=2)
    text(3050, 400, labels="Provincial Prefects", cex=1.2)
    text(2200, 340, labels="City Mayors", cex=1.2)
    segments(-200, 400, 0.02436528*nrow(actas19_alcal_pref), 400, col="darkgrey", lwd=2)
    segments(-200, 340, 0.02436528*nrow(actas19_alcal_pref), 340, col="chartreuse", lwd=2)
    text(14800, 20, labels="n=15,857", cex=1.2)
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
  
  
  

  
  
  
  
  # chains sorted by vote share
  actas17 <- actas17[order(actas17$pw_pres),] 
  actas17$id <- 1:nrow(actas17)
  plot(actas17$id, actas17$turnout_pres, type="l", col="darkgrey")
  lines(actas17$id, actas17$turnout_consulta, col="green")






