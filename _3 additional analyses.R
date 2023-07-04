#' ------------------------------------------------------------------------
#' regression analysis, predicting undervoting irregularities
#' Lion Behrens
#' ------------------------------------------------------------------------

library(betareg)
library(stargazer)
library(sf)
library(tmap)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# load data 
load("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas17.Rdata")
actas17 <- actas17[-which(actas17$ELECTORES_REGISTRO_pres<100),] # delete polling stations with <100 eligible voters

load("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/actas19.Rdata")
actas19_alcal_conc_rural <- actas19_alcal_conc_rural[-which(actas19_alcal_conc_rural$ELECTORES_REGISTRO_alcal<100),] # delete polling stations with <100 eligible voters
actas19_alcal_conc_urban <- actas19_alcal_conc_urban[-which(actas19_alcal_conc_urban$ELECTORES_REGISTRO_alcal<100),] # delete polling stations with <100 eligible voters
actas19_alcal_pref <- actas19_alcal_pref[-which(actas19_alcal_pref$ELECTORES_REGISTRO_alcal<100),] # delete polling stations with <100 eligible voters
actas19_alcal_vocales <- actas19_alcal_vocales[-which(actas19_alcal_vocales$ELECTORES_REGISTRO_alcal<100),] # delete polling stations with <100 eligible voters



#' -------------------------------
# 0. data preparation ------------
#' -------------------------------

  # y: extent of undervoting
  actas17$under_pres_consulta <- abs((actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_consulta) / actas17$SUFRAGANTES_pres)
  actas17$under_pres_asam_prov <- abs((actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_pres)
  actas17$under_nac_prov <- abs((actas17$SUFRAGANTES_asam_nac - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_asam_nac)
  actas17$under_andino_prov <- abs((actas17$SUFRAGANTES_andino - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_andino)
  actas17$under_consulta_prov <- abs((actas17$SUFRAGANTES_consulta - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_consulta)

  # winner's vote share
  actas17$pw_pres <- actas17$MORENO_pres / actas17$SUFRAGANTES_pres

  # runner up vote share
  actas17$runner_pres <- actas17$LASSO_pres / actas17$SUFRAGANTES_pres
  
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
  
  # closeness of electoral race
  actas17$closeness_pres <- abs(actas17$MORENO_pres - actas17$LASSO_pres)

  # share of null votes
  actas17$null_pres <- actas17$VOTOS_NULOS_pres / actas17$SUFRAGANTES_pres
  
  # share of blank votes
  actas17$blank_pres <- actas17$VOTOS_EN_BLANCO_pres / actas17$SUFRAGANTES_pres
  
  
#' ---------------------------------------------------------
# 1. visualize dependent variable of regression ------------
#' ---------------------------------------------------------

  actas17$under_pres_asam_prov_alt <- (actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_pres
  hist(actas17$under_pres_asam_prov_alt[which(actas17$under_pres_asam_prov_alt != 0)], 
       breaks=200, xlim=c(-0.5,0.5), yaxt = "n", ylab="", main="", 
       xlab="Share of Discrepant Votes Among All Votes", cex.lab=1.75, cex.axis=1.5)
  
  
#' ------------------------------------
# 2. Ecuadorian map -------------------
#' ------------------------------------
  
  #' ----------------------------------
  # extent of undervoting 
  #' ----------------------------------
  
    # for visualization, exclude foreign territories
    actas17 = actas17[-which(actas17$TERRITORIO_NOMBRE_pres == "EXTERIOR"),]
    actas17 <- actas17[-which(actas17$PROVINCIA_NOMBRE_pres=="GALÁPAGOS"),]
    ecu <- read_sf("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/ECU_shapefile/ECU_adm2.shp")
    
    # make locality names compatible
    actas17$canton_title <- str_to_lower(actas17$CANTON_NOMBRE_pres)
    actas17$canton_title <- gsub("á", "a", actas17$canton_title)
    actas17$canton_title <- gsub("é", "e", actas17$canton_title)
    actas17$canton_title <- gsub("í", "i", actas17$canton_title)
    actas17$canton_title <- gsub("ó", "o", actas17$canton_title)
    actas17$canton_title <- gsub("ú", "u", actas17$canton_title)
    
    ecu$canton_title <- str_to_lower(ecu$NAME_2)
    ecu$canton_title <- gsub("á", "a", ecu$canton_title)
    ecu$canton_title <- gsub("é", "e", ecu$canton_title)
    ecu$canton_title <- gsub("í", "i", ecu$canton_title)
    ecu$canton_title <- gsub("ó", "o", ecu$canton_title)
    ecu$canton_title <- gsub("ú", "u", ecu$canton_title)
    ecu$canton_title[which(ecu$canton_title == "alfredo baquerizo moreno")] <- "a.baquerizo moreno"
    ecu$canton_title[which(ecu$canton_title == "coronel marcelino maridueña")] <- "crnl. marcelino maridueñas"
    ecu$canton_title[which(ecu$canton_title == "general antonio elizalde")] <- "gral. a. elizalde"
    ecu$canton_title[which(ecu$canton_title == "nobol")] <- "nobol / piedrahita"
    ecu$canton_title[which(ecu$canton_title == "orellana")] <- "fco.de orellana"
    ecu$canton_title[which(ecu$canton_title == "san jacinto de yaguac")] <- "yaguachi"
    ecu$canton_title[which(ecu$canton_title == "san miguel de urcuqui")] <- "urcuqui"
    ecu$canton_title[which(ecu$canton_title == "san pedro de pelileo")] <- "pelileo"
    ecu$canton_title[which(ecu$canton_title == "santiago de pillaro")] <- "pillaro"
    ecu$canton_title[which(ecu$canton_title == "santo domingo de los colorados")] <- "santo domingo"
    ecu$canton_title[which(ecu$canton_title == "veinticuatro de mayo")] <- "24 de mayo"
    ecu$canton_title[which(ecu$NAME_1=="Cañar" & ecu$NAME_2=="No Delimitado")] <- "suscal"
    ecu$canton_title[which(ecu$NAME_1=="Manabi" & ecu$NAME_2=="No Delimitado")] <- "jama"
    
    # compute mean extent of undervoting per canton
    actas17 <- actas17[-which(is.na(actas17$under_pres_asam_prov) | actas17$under_pres_asam_prov==Inf),] 
    actas17_canton <- actas17[,c("canton_title", "under_pres_asam_prov")]
    actas17_canton <- actas17_canton %>% 
      group_by(canton_title) %>% 
      summarise(under_perc_mean = mean(under_pres_asam_prov)) 
     
    # merge datasets
    ecu <- merge(ecu, actas17_canton, by="canton_title")
    ecu$under_perc_mean <- ecu$under_perc_mean * 100
    colnames(ecu)[13] <- "Share of Discrepant Votes Among All Votes"
    
    # plot
    tm_shape(ecu) +
      tm_polygons(col = "Share of Discrepant Votes Among All Votes", 
                  palette=c("grey90", "grey70", "grey50", "grey30", "grey10")) +
      tm_layout(frame=F, legend.show=F) + 
      tm_add_legend(type="fill", 
                    labels=c("0-1%", "1-2%", "2-3%", "3-4%", "4-5%"), 
                    col=c("grey90", "grey70", "grey50", "grey30", "grey10"),
                    title="Share of Discrepant Votes Among All Votes"
      )
    
    tm_shape(ecu) +
      tm_polygons(col = "Share of Discrepant Votes Among All Votes", 
                  palette=c("grey90", "grey70", "grey50", "grey30", "grey10")) +
      tm_layout(frame=F, legend.only = T, legend.title.size=2, legend.text.size=2) + 
      tm_add_legend(type="fill", 
      labels=c("0-1%", "1-2%", "2-3%", "3-4%", "4-5%"), 
      col=c("grey90", "grey70", "grey50", "grey30", "grey10"),
      title="Share of Discrepant Votes Among All Votes"
      )
  
    
  #' ----------------------------------
  # correlations within cantons
  #' ----------------------------------

    # compute correlations per canton
    actas17$cor_under_pw_pres <- NA
    actas17$sig_under_pw_pres <- NA
    for (canton in unique(actas17$canton_title)) {
      
      used_data <- actas17[which(actas17$canton_title==canton),]
      used_data <- used_data[which(!is.na(used_data$under_pres_asam_prov) & !is.na(used_data$pw_pres)),]
      moa <- abs(cor(used_data$under_pres_asam_prov, used_data$pw_pres))
      if(is.na(moa)) moa <- 0
      sig <- cor.test(used_data$under_pres_asam_prov, used_data$pw_pres, method="pearson")$p.value < 0.05
      if(is.na(sig)) sig <- F
      
      actas17$cor_under_pw_pres[which(actas17$canton_title==canton)] <- moa
      actas17$sig_under_pw_pres[which(actas17$canton_title==canton)] <- sig
      
    }
  
    # merge datasets
    actas17_canton <- actas17[,c("canton_title", "cor_under_pw_pres", "sig_under_pw_pres")]
    actas17_canton <- actas17_canton %>% 
      group_by(canton_title) %>% 
      summarise(cor_under_pw_pres = first(cor_under_pw_pres), 
                sig_under_pw_pres = first(sig_under_pw_pres)) 
    
    ecu <- merge(ecu, actas17_canton, by="canton_title")
    colnames(ecu)[14] <- "Correlation Between Undervoting and Winner's Vote Share"
    
    # plot
    p1 <- tm_shape(ecu) +
      tm_polygons(col = "Correlation Between Undervoting and Winner's Vote Share", 
                  palette=c("grey90", "grey70", "grey50", "grey30", "grey10")) +
      tm_layout(frame=F, legend.show=F) 
    ecu <- ecu[which(ecu$sig_under_pw_pres==T),]
    p1 + tm_shape(ecu) +
      tm_dots(size=1, col="red")
      
    
    tm_shape(ecu) +
      tm_polygons(col = "Correlation Between Undervoting and Winner's Vote Share", 
                  palette=c("grey90", "grey70", "grey50", "grey30", "grey10")) +
      tm_layout(frame=F, legend.only = T, legend.title.size=2, legend.text.size=2) + 
      tm_add_legend(type="fill", 
                    labels=c("0.0-0.2", "0.2-0.4", "0.4-0.6", "0.6-0.8", "0.8-1.0"), 
                    col=c("grey90", "grey70", "grey50", "grey53", "grey10"),
                    title="Correlation Between Undervoting and Winner's Vote Share"
      )
    
    tm_shape(ecu) +
      tm_polygons(col = "Correlation Between Undervoting and Winner's Vote Share", 
                  palette=c("grey90", "grey70", "grey50", "grey30", "grey10")) +
      tm_layout(frame=F, legend.only = T, legend.title.size=2, legend.text.size=2) + 
      tm_add_legend(type="symbol", 
                    labels=c("significant"), 
                    col=c("red"),
                    title="Hypothesis Test"
      )
    
    # store cantons with significant correlations
    ecu_sig <- ecu
    
  #' ----------------------------------------------------------
  # significant correlations vs. Lasso's vote shares in 2013
  #' ----------------------------------------------------------
  
    
    ecu <- read_sf("U:/PhD Electoral Fraud/Papers/02_Detecting Unbalanced Fraud Approaches From Undervoting Irregularities/undervoting_irregularities/ECU_shapefile/ECU_adm2.shp")
    
    # make locality names compatible
    ecu$canton_title <- str_to_lower(ecu$NAME_2)
    ecu$canton_title <- gsub("á", "a", ecu$canton_title)
    ecu$canton_title <- gsub("é", "e", ecu$canton_title)
    ecu$canton_title <- gsub("í", "i", ecu$canton_title)
    ecu$canton_title <- gsub("ó", "o", ecu$canton_title)
    ecu$canton_title <- gsub("ú", "u", ecu$canton_title)
    ecu$canton_title[which(ecu$canton_title == "alfredo baquerizo moreno")] <- "a.baquerizo moreno"
    ecu$canton_title[which(ecu$canton_title == "coronel marcelino maridueña")] <- "crnl. marcelino maridueñas"
    ecu$canton_title[which(ecu$canton_title == "general antonio elizalde")] <- "gral. a. elizalde"
    ecu$canton_title[which(ecu$canton_title == "nobol")] <- "nobol / piedrahita"
    ecu$canton_title[which(ecu$canton_title == "orellana")] <- "fco.de orellana"
    ecu$canton_title[which(ecu$canton_title == "san jacinto de yaguac")] <- "yaguachi"
    ecu$canton_title[which(ecu$canton_title == "san miguel de urcuqui")] <- "urcuqui"
    ecu$canton_title[which(ecu$canton_title == "san pedro de pelileo")] <- "pelileo"
    ecu$canton_title[which(ecu$canton_title == "santiago de pillaro")] <- "pillaro"
    ecu$canton_title[which(ecu$canton_title == "santo domingo de los colorados")] <- "santo domingo"
    ecu$canton_title[which(ecu$canton_title == "veinticuatro de mayo")] <- "24 de mayo"
    ecu$canton_title[which(ecu$NAME_1=="Cañar" & ecu$NAME_2=="No Delimitado")] <- "suscal"
    ecu$canton_title[which(ecu$NAME_1=="Manabi" & ecu$NAME_2=="No Delimitado")] <- "jama"
    
    load("pres13A.RData")
    pres13A = pres13A[-which(pres13A$TERRITORIO_NOMBRE == "EXTERIOR"),]
    pres13A <- pres13A[-which(pres13A$PROVINCIA_NOMBRE=="GALÁPAGOS"),]
    
    pres13A$canton_title <- str_to_lower(pres13A$CANTON_NOMBRE)
    pres13A$canton_title <- gsub("á", "a", pres13A$canton_title)
    pres13A$canton_title <- gsub("é", "e", pres13A$canton_title)
    pres13A$canton_title <- gsub("í", "i", pres13A$canton_title)
    pres13A$canton_title <- gsub("ó", "o", pres13A$canton_title)
    pres13A$canton_title <- gsub("ú", "u", pres13A$canton_title)
    
    
    # construct Lasso's vote share per canton
    pres13A$lasso_share <- pres13A$LASSO / pres13A$SUFRAGANTES
    pres13A_canton <- pres13A %>% 
      group_by(canton_title) %>% 
      summarise(lasso_share = mean(lasso_share, na.rm=T)) 
    
    ecu <- merge(ecu, pres13A_canton, by="canton_title")
    colnames(ecu)[13] <- "Guillermo Lasso Vote Share 2013"
    
    # plot
    p1 <- tm_shape(ecu) +
      tm_polygons(col = "Guillermo Lasso Vote Share 2013", 
                  palette=c("grey90", "grey70", "grey50", "grey30", "grey10")) +
      tm_layout(frame=F, legend.show=T) 
    p1 + tm_shape(ecu_sig) +
      tm_dots(size=1, col="red")
    
    
    
    
#' ----------------------------
# 3. boxplots 2017 ------------
#' ----------------------------

    par(mfrow=c(2,2))
    
    # presidential election vs. regional parliament
    actas17$underperc_pres_asam_prov <- abs((actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_pres) 
    actas17$ucat_pres_asam_prov <- NA
    actas17$ucat_pres_asam_prov[actas17$underperc_pres_asam_prov>0.9] <- NA
    actas17$ucat_pres_asam_prov[actas17$underperc_pres_asam_prov<0.9] <- 10
    actas17$ucat_pres_asam_prov[actas17$underperc_pres_asam_prov<0.8] <- 9
    actas17$ucat_pres_asam_prov[actas17$underperc_pres_asam_prov<0.7] <- 8
    actas17$ucat_pres_asam_prov[actas17$underperc_pres_asam_prov<0.6] <- 7
    actas17$ucat_pres_asam_prov[actas17$underperc_pres_asam_prov<0.5] <- 6
    actas17$ucat_pres_asam_prov[actas17$underperc_pres_asam_prov<0.4] <- 5
    actas17$ucat_pres_asam_prov[actas17$underperc_pres_asam_prov<0.3] <- 4
    actas17$ucat_pres_asam_prov[actas17$underperc_pres_asam_prov<0.2] <- 3
    actas17$ucat_pres_asam_prov[actas17$underperc_pres_asam_prov<0.1] <- 2
    actas17$ucat_pres_asam_prov[actas17$underperc_pres_asam_prov==0] <- 1
    
    par(mar = c(2, 4.5, 4.5, 1))
    boxplot(actas17$pw_pres ~ actas17$ucat_pres_asam_prov, xaxt="n",
            xlab="", ylab="Winner's Vote Share", 
            main="", frame.plot = FALSE, ylim=c(0,1), cex.lab=1.2)      
    abline(h=mean(actas17$pw_pres[which(actas17$underperc_pres_asam_prov!=0)], na.rm=T), lty=2, lwd=3, xpd=F, col="lightblue")
    par(xpd=T)
    text(2.3, 1.05, labels="Presidential Election", cex=1.2, font=2)
    
    rect(7.5, 0, 10, 1, density = 50, angle = 45,
         col = "lightgrey", border=NA)
    
    # national parliament vs. regional parliament
    actas17$underperc_asam_nac_asam_prov <- abs((actas17$SUFRAGANTES_asam_nac - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_asam_nac) 
    actas17$ucat_asam_nac_asam_prov <- NA
    actas17$ucat_asam_nac_asam_prov[actas17$underperc_asam_nac_asam_prov>0.9] <- NA
    actas17$ucat_asam_nac_asam_prov[actas17$underperc_asam_nac_asam_prov<0.9] <- 10
    actas17$ucat_asam_nac_asam_prov[actas17$underperc_asam_nac_asam_prov<0.8] <- 9
    actas17$ucat_asam_nac_asam_prov[actas17$underperc_asam_nac_asam_prov<0.7] <- 8
    actas17$ucat_asam_nac_asam_prov[actas17$underperc_asam_nac_asam_prov<0.6] <- 7
    actas17$ucat_asam_nac_asam_prov[actas17$underperc_asam_nac_asam_prov<0.5] <- 6
    actas17$ucat_asam_nac_asam_prov[actas17$underperc_asam_nac_asam_prov<0.4] <- 5
    actas17$ucat_asam_nac_asam_prov[actas17$underperc_asam_nac_asam_prov<0.3] <- 4
    actas17$ucat_asam_nac_asam_prov[actas17$underperc_asam_nac_asam_prov<0.2] <- 3
    actas17$ucat_asam_nac_asam_prov[actas17$underperc_asam_nac_asam_prov<0.1] <- 2
    actas17$ucat_asam_nac_asam_prov[actas17$underperc_asam_nac_asam_prov==0] <- 1

    par(mar = c(2, 3, 4.5, 0))
    boxplot(actas17$winnershare_asam_nac ~ actas17$ucat_asam_nac_asam_prov, xaxt="n",
            xlab="", ylab="Winner's Vote Share", 
            main="", frame.plot = FALSE, ylim=c(0,1), cex.lab=1.2)      
    abline(h=mean(actas17$winnershare_asam_nac[which(actas17$underperc_asam_nac_asam_prov!=0)], na.rm=T), lty=2, lwd=3, xpd=F, col="lightblue")
    par(xpd=T)
    text(2.3, 1.05, labels="National Parliament", cex=1.2, font=2)
    
    rect(6.5, 0, 10.5, 1, density = 50, angle = 45,
         col = "lightgrey", border=NA)
    
    # Andean parliament vs. regional parliament
    actas17$underperc_andino_asam_prov <- abs((actas17$SUFRAGANTES_andino - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_andino) 
    actas17$ucat_andino_asam_prov <- NA
    actas17$ucat_andino_asam_prov[actas17$underperc_andino_asam_prov>0.9] <- NA
    actas17$ucat_andino_asam_prov[actas17$underperc_andino_asam_prov<0.9] <- 10
    actas17$ucat_andino_asam_prov[actas17$underperc_andino_asam_prov<0.8] <- 9
    actas17$ucat_andino_asam_prov[actas17$underperc_andino_asam_prov<0.7] <- 8
    actas17$ucat_andino_asam_prov[actas17$underperc_andino_asam_prov<0.6] <- 7
    actas17$ucat_andino_asam_prov[actas17$underperc_andino_asam_prov<0.5] <- 6
    actas17$ucat_andino_asam_prov[actas17$underperc_andino_asam_prov<0.4] <- 5
    actas17$ucat_andino_asam_prov[actas17$underperc_andino_asam_prov<0.3] <- 4
    actas17$ucat_andino_asam_prov[actas17$underperc_andino_asam_prov<0.2] <- 3
    actas17$ucat_andino_asam_prov[actas17$underperc_andino_asam_prov<0.1] <- 2
    actas17$ucat_andino_asam_prov[actas17$underperc_andino_asam_prov==0] <- 1
    
    par(mar = c(4.5, 4.5, 2, 0))
    boxplot(actas17$winnershare_andino ~ actas17$ucat_andino_asam_prov, xaxt="n",
            xlab="Share of Discrepant Votes Among All Votes", ylab="Winner's Vote Share", 
            main="", frame.plot = FALSE, ylim=c(0,1), cex.lab=1.2)      
    abline(h=mean(actas17$winnershare_andino[which(actas17$underperc_andino_asam_prov!=0)], na.rm=T), lty=2, lwd=3, xpd=F, col="lightblue")
    text(1:10, y=0, srt = 60, adj = 1, xpd = TRUE,
         labels=c("0%", "0-10%", "10-20%", "20-30%", 
                  "30-40%", "40-50%", "50-60%", "60-70%", 
                  "70-80%", "80-90%"))
    par(xpd=T)
    text(2.3, 1.05, labels="Andean Parliament", cex=1.2, font=2)
    
    rect(7.5, 0, 11, 1, density = 50, angle = 45,
         col = "lightgrey", border=NA)
    
    # National referendum vs. regional parliament
    actas17$underperc_consulta_asam_prov <- abs((actas17$SUFRAGANTES_consulta - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_consulta) 
    actas17$ucat_consulta_asam_prov <- NA
    actas17$ucat_consulta_asam_prov[actas17$underperc_consulta_asam_prov>0.9] <- NA
    actas17$ucat_consulta_asam_prov[actas17$underperc_consulta_asam_prov<0.9] <- 10
    actas17$ucat_consulta_asam_prov[actas17$underperc_consulta_asam_prov<0.8] <- 9
    actas17$ucat_consulta_asam_prov[actas17$underperc_consulta_asam_prov<0.7] <- 8
    actas17$ucat_consulta_asam_prov[actas17$underperc_consulta_asam_prov<0.6] <- 7
    actas17$ucat_consulta_asam_prov[actas17$underperc_consulta_asam_prov<0.5] <- 6
    actas17$ucat_consulta_asam_prov[actas17$underperc_consulta_asam_prov<0.4] <- 5
    actas17$ucat_consulta_asam_prov[actas17$underperc_consulta_asam_prov<0.3] <- 4
    actas17$ucat_consulta_asam_prov[actas17$underperc_consulta_asam_prov<0.2] <- 3
    actas17$ucat_consulta_asam_prov[actas17$underperc_consulta_asam_prov<0.1] <- 2
    actas17$ucat_consulta_asam_prov[actas17$underperc_consulta_asam_prov==0] <- 1
    
    par(mar = c(4.5, 3, 2, 0))
    boxplot(actas17$winnershare_consulta ~ actas17$ucat_consulta_asam_prov, xaxt="n",
            xlab="Share of Discrepant Votes Among All Votes", ylab="Winner's Vote Share", 
            main="", frame.plot = FALSE, ylim=c(0,1), cex.lab=1.2)      
    abline(h=mean(actas17$winnershare_consulta[which(actas17$underperc_consulta_asam_prov!=0)], na.rm=T), lty=2, lwd=3, xpd=F, col="lightblue")
    text(1:10, y=0, srt = 60, adj = 1, xpd = TRUE,
         labels=c("0%", "0-10%", "10-20%", "20-30%", 
                  "30-40%", "40-50%", "50-60%", "60-70%", 
                  "70-80%", "80-90%"))
    par(xpd=T)
    text(2.4, 1.05, labels="National Referendum", cex=1.2, font=2)
    
    rect(5.5, 0, 10.5, 1, density = 50, angle = 45,
         col = "lightgrey", border=NA)
    
    
#' ----------------------------
# 3. boxplots 2019 ------------
#' ----------------------------  
    
    par(mfrow=c(2,2))
    
    
    ### provincial prefects vs. city mayors
    # categorize level of undervoting
    actas19_alcal_pref$underperc <- abs(actas19_alcal_pref$SUFRAGANTES_alcal - actas19_alcal_pref$SUFRAGANTES_pref) / actas19_alcal_pref$SUFRAGANTES_alcal
    actas19_alcal_pref$ucat <- NA
    actas19_alcal_pref$ucat[actas19_alcal_pref$underperc>0.9] <- NA
    actas19_alcal_pref$ucat[actas19_alcal_pref$underperc<0.9] <- 10
    actas19_alcal_pref$ucat[actas19_alcal_pref$underperc<0.8] <- 9
    actas19_alcal_pref$ucat[actas19_alcal_pref$underperc<0.7] <- 8
    actas19_alcal_pref$ucat[actas19_alcal_pref$underperc<0.6] <- 7
    actas19_alcal_pref$ucat[actas19_alcal_pref$underperc<0.5] <- 6
    actas19_alcal_pref$ucat[actas19_alcal_pref$underperc<0.4] <- 5
    actas19_alcal_pref$ucat[actas19_alcal_pref$underperc<0.3] <- 4
    actas19_alcal_pref$ucat[actas19_alcal_pref$underperc<0.2] <- 3
    actas19_alcal_pref$ucat[actas19_alcal_pref$underperc<0.1] <- 2
    actas19_alcal_pref$ucat[actas19_alcal_pref$underperc==0] <- 1
    
    # construct winners' vote shares
    x <- actas19_alcal_pref[,which(str_detect(colnames(actas19_alcal_pref), "pref"))]
    x <- x[,25:ncol(x)]
    x <- x[,sapply(x, is.numeric)]
    winner_votes <- apply(x, MARGIN = 1, FUN=max, na.rm=T)
    actas19_alcal_pref$pw <- winner_votes / actas19_alcal_pref$SUFRAGANTES_pref
    
    par(mar = c(2, 4.5, 4.5, 1))
    
    boxplot(actas19_alcal_pref$pw ~ actas19_alcal_pref$ucat, xaxt="n",
            xlab="Share of Discrepant Votes Among All Votes", ylab="Winner's Vote Share", 
            main="", frame.plot = FALSE, ylim=c(0,1), cex.lab=1.2)      
    abline(h=mean(actas19_alcal_pref$pw[which(actas19_alcal_pref$underperc!=0)], na.rm=T), lty=2, lwd=3, xpd=F, col="lightblue")
    par(xpd=T)
    text(2.3, 1.05, labels="Provincial Prefects", cex=1.2, font=2)
    
    rect(7.5, 0, 10, 1, density = 50, angle = 45,
         col = "lightgrey", border=NA)
    
    ### urban councilors vs. city mayors
    # categorize level of undervoting
    actas19_alcal_conc_urban$underperc <- abs(actas19_alcal_conc_urban$SUFRAGANTES_alcal - actas19_alcal_conc_urban$SUFRAGANTES_conc_urban) / actas19_alcal_conc_urban$SUFRAGANTES_alcal
    actas19_alcal_conc_urban$ucat <- NA
    actas19_alcal_conc_urban$ucat[actas19_alcal_conc_urban$underperc>0.9] <- NA
    actas19_alcal_conc_urban$ucat[actas19_alcal_conc_urban$underperc<0.9] <- 10
    actas19_alcal_conc_urban$ucat[actas19_alcal_conc_urban$underperc<0.8] <- 9
    actas19_alcal_conc_urban$ucat[actas19_alcal_conc_urban$underperc<0.7] <- 8
    actas19_alcal_conc_urban$ucat[actas19_alcal_conc_urban$underperc<0.6] <- 7
    actas19_alcal_conc_urban$ucat[actas19_alcal_conc_urban$underperc<0.5] <- 6
    actas19_alcal_conc_urban$ucat[actas19_alcal_conc_urban$underperc<0.4] <- 5
    actas19_alcal_conc_urban$ucat[actas19_alcal_conc_urban$underperc<0.3] <- 4
    actas19_alcal_conc_urban$ucat[actas19_alcal_conc_urban$underperc<0.2] <- 3
    actas19_alcal_conc_urban$ucat[actas19_alcal_conc_urban$underperc<0.1] <- 2
    actas19_alcal_conc_urban$ucat[actas19_alcal_conc_urban$underperc==0] <- 1
    
    # construct winners' vote shares
    x <- actas19_alcal_conc_urban[,which(str_detect(colnames(actas19_alcal_conc_urban), "conc_urban"))]
    x <- x[,25:ncol(x)]
    x <- x[,sapply(x, is.numeric)]
    winner_votes <- apply(x, MARGIN = 1, FUN=max, na.rm=T)
    actas19_alcal_conc_urban$pw <- winner_votes / actas19_alcal_conc_urban$SUFRAGANTES_conc_urban
    
    par(mar = c(2, 3, 4.5, 0))
    
    boxplot(actas19_alcal_conc_urban$pw ~ actas19_alcal_conc_urban$ucat, xaxt="n",
            xlab="", ylab="Winner's Vote Share", 
            main="", frame.plot = FALSE, ylim=c(0,1), cex.lab=1.2)      
    abline(h=mean(actas19_alcal_conc_urban$pw[which(actas19_alcal_conc_urban$underperc!=0)], na.rm=T), lty=2, lwd=3, xpd=F, col="lightblue")
    par(xpd=T)
    text(2.3, 1.05, labels="Urban Councilors", cex=1.2, font=2)
    rect(7.5, 0, 10, 1, density = 50, angle = 45,
         col = "lightgrey", border=NA)
    
    
    
    ### rural councilors vs. city mayors
    # categorize level of undervoting
    actas19_alcal_conc_rural$underperc <- abs(actas19_alcal_conc_rural$SUFRAGANTES_alcal - actas19_alcal_conc_rural$SUFRAGANTES_conc_rural) / actas19_alcal_conc_rural$SUFRAGANTES_alcal 
    actas19_alcal_conc_rural$ucat <- NA
    actas19_alcal_conc_rural$ucat[actas19_alcal_conc_rural$underperc>0.9] <- NA
    actas19_alcal_conc_rural$ucat[actas19_alcal_conc_rural$underperc<0.9] <- 10
    actas19_alcal_conc_rural$ucat[actas19_alcal_conc_rural$underperc<0.8] <- 9
    actas19_alcal_conc_rural$ucat[actas19_alcal_conc_rural$underperc<0.7] <- 8
    actas19_alcal_conc_rural$ucat[actas19_alcal_conc_rural$underperc<0.6] <- 7
    actas19_alcal_conc_rural$ucat[actas19_alcal_conc_rural$underperc<0.5] <- 6
    actas19_alcal_conc_rural$ucat[actas19_alcal_conc_rural$underperc<0.4] <- 5
    actas19_alcal_conc_rural$ucat[actas19_alcal_conc_rural$underperc<0.3] <- 4
    actas19_alcal_conc_rural$ucat[actas19_alcal_conc_rural$underperc<0.2] <- 3
    actas19_alcal_conc_rural$ucat[actas19_alcal_conc_rural$underperc<0.1] <- 2
    actas19_alcal_conc_rural$ucat[actas19_alcal_conc_rural$underperc==0] <- 1
    
    # construct winners' vote shares
    x <- actas19_alcal_conc_rural[,which(str_detect(colnames(actas19_alcal_conc_rural), "conc_rural"))]
    x <- x[,25:ncol(x)]
    x <- x[,sapply(x, is.numeric)]
    winner_votes <- apply(x, MARGIN = 1, FUN=max, na.rm=T)
    actas19_alcal_conc_rural$pw <- winner_votes / actas19_alcal_conc_rural$SUFRAGANTES_conc_rural
    
    par(mar = c(4.5, 4.5, 2, 0))
    boxplot(actas19_alcal_conc_rural$pw ~ actas19_alcal_conc_rural$ucat, xaxt="n",
            xlab="Share of Discrepant Votes Among All Votes", ylab="Winner's Vote Share", 
            main="", frame.plot = FALSE, ylim=c(0,1), cex.lab=1.2)      
    abline(h=mean(actas19_alcal_conc_rural$pw[which(actas19_alcal_conc_rural$underperc!=0)], na.rm=T), lty=2, lwd=3, xpd=F, col="lightblue")
    text(1:6, y=0, srt = 60, adj = 1, xpd = TRUE,
         labels=c("0%", "0-10%", "10-20%", "20-30%", 
                  "30-40%", "40-50%"))
    par(xpd=T)
    text(2.3, 1.05, labels="Rural Councilors", cex=1.2, font=2)
    
    rect(7.5, 0, 10, 1, density = 50, angle = 45,
         col = "lightgrey", border=NA)
    
    
    ### members of parish boards vs. city mayors
    # categorize level of undervoting
    actas19_alcal_vocales$underperc <- abs(actas19_alcal_vocales$SUFRAGANTES_alcal - actas19_alcal_vocales$SUFRAGANTES_vocales) / actas19_alcal_vocales$SUFRAGANTES_alcal
    actas19_alcal_vocales$ucat <- NA
    actas19_alcal_vocales$ucat[actas19_alcal_vocales$underperc>0.9] <- NA
    actas19_alcal_vocales$ucat[actas19_alcal_vocales$underperc<0.9] <- 10
    actas19_alcal_vocales$ucat[actas19_alcal_vocales$underperc<0.8] <- 9
    actas19_alcal_vocales$ucat[actas19_alcal_vocales$underperc<0.7] <- 8
    actas19_alcal_vocales$ucat[actas19_alcal_vocales$underperc<0.6] <- 7
    actas19_alcal_vocales$ucat[actas19_alcal_vocales$underperc<0.5] <- 6
    actas19_alcal_vocales$ucat[actas19_alcal_vocales$underperc<0.4] <- 5
    actas19_alcal_vocales$ucat[actas19_alcal_vocales$underperc<0.3] <- 4
    actas19_alcal_vocales$ucat[actas19_alcal_vocales$underperc<0.2] <- 3
    actas19_alcal_vocales$ucat[actas19_alcal_vocales$underperc<0.1] <- 2
    actas19_alcal_vocales$ucat[actas19_alcal_vocales$underperc==0] <- 1
    
    # construct winners' vote shares
    x <- actas19_alcal_vocales[,which(str_detect(colnames(actas19_alcal_vocales), "vocales"))]
    x <- x[,25:ncol(x)]
    x <- x[,sapply(x, is.numeric)]
    winner_votes <- apply(x, MARGIN = 1, FUN=max, na.rm=T)
    actas19_alcal_vocales$pw <- winner_votes / actas19_alcal_vocales$SUFRAGANTES_vocales
    
    par(mar = c(4.5, 3, 2, 0))
    boxplot(actas19_alcal_vocales$pw ~ actas19_alcal_vocales$ucat, xaxt="n",
            xlab="Share of Discrepant Votes Among All Votes", ylab="Winner's Vote Share", 
            main="", frame.plot = FALSE, ylim=c(0,1), cex.lab=1.2)      
    abline(h=mean(actas19_alcal_vocales$pw[which(actas19_alcal_vocales$underperc!=0)], na.rm=T), lty=2, lwd=3, xpd=F, col="lightblue")
    par(xpd=T)
    text(1:6, y=0, srt = 60, adj = 1, xpd = TRUE,
         labels=c("0%", "0-10%", "10-20%", "20-30%", 
                  "30-40%", "40-50%"))
    
    text(2.3, 1.05, labels="Members of Parish Boards", cex=1.2, font=2)
    
    rect(7.5, 0, 10, 1, density = 50, angle = 45,
         col = "lightgrey", border=NA)
    
    
#' ------------------------------------------
# 4. histograms of winner's vote shares -----
#' ------------------------------------------

    ## which horizontal lines to draw?
    ## one line: simulated with fraud_share=0
    ## another line: simulated with estimated fraud_share
    
    par(mfrow=c(1,1))
    
    # -----------------------------
    # presidential election
    # -----------------------------
    
      # empirical data
      hist(actas17$pw_pres[actas17$under_pres_asam_prov==0], breaks=100, 
           main="", xlab="Lenin Moreno Vote Share", ylab="Frequency", 
           xlim=c(0,0.7))
      abline(v=mean(actas17$pw_pres[actas17$under_pres_asam_prov==0], lwd=3, na.rm = T))
      hist(actas17$pw_pres[actas17$under_pres_asam_prov>0], breaks=100, col="red", add=T)
      abline(v=mean(actas17$pw_pres[actas17$under_pres_asam_prov>0], na.rm = T), lwd=3, col="red")
      
      
      # simulated data
      entities <- actas17$ELECTORES_REGISTRO_pres
    
      actas17$turnout_pres <- actas17$SUFRAGANTES_pres / actas17$ELECTORES_REGISTRO_pres
      actas17$turnout_pres[actas17$turnout_pres > 1] <- 1
      beta_est <- ebeta(actas17$turnout_pres, method="mle")
      turnout_probs <- rbeta(nrow(actas17), 
                             shape1 = beta_est$parameters[1], 
                             shape2 = beta_est$parameters[2])
      
      actas17$winnershare_pres <- actas17$MORENO_pres/actas17$SUFRAGANTES_pres
      beta_est <- ebeta(actas17$winnershare_pres, method="mle")
      winner_probs <- rbeta(nrow(actas17), 
                            shape1 = beta_est$parameters[1], 
                            shape2 = beta_est$parameters[2])
    
      actas17$under_pres_asam_prov <- abs((actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_asam_prov) / actas17$SUFRAGANTES_pres)
      undervoting_n = length(which(actas17$under_pres_asam_prov!=0))
      
      actas17$under_pres_asam_prov <- (actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_asam_prov) 
      undervoting_sd <- sd(actas17$under_pres_asam_prov[actas17$under_pres_asam_prov!=0], na.rm=T) 
    
      sim_elections <- list()  
      id <- 0
      for (share in c(0, 0.2, 0.4, 0.6, 0.8)) {
        id <- id+1  
        sim_elections[[id]] <- gen_data(entities = entities, 
                                        turnout_probs = turnout_probs, 
                                        winner_probs = winner_probs, 
                                        undervoting_n = undervoting_n, 
                                        undervoting_sd = undervoting_sd, 
                                        share_fraud = share
        )
      } 
      names(sim_elections) <- str_c("share_fraud = ", c(0, 0.2, 0.4, 0.6, 0.8))
    
      abline(v=mean(sim_elections[["share_fraud = 0"]]$winner_share[sim_elections[["share_fraud = 0"]]$under_perc>0], na.rm = T), 
             lty=2, lwd=3, col="purple")
      abline(v=mean(sim_elections[["share_fraud = 0.6"]]$winner_share[sim_elections[["share_fraud = 0.6"]]$under_perc>0], na.rm = T), 
             lty=2, lwd=3, col="purple")
    
    
    
    # national parliament 
    hist(actas17$winnershare_asam_nac[actas17$underperc_asam_nac_asam_prov==0], breaks=100, 
         main="", xlab="Alianza Pais Vote Share", ylab="Frequency")
    abline(v=mean(actas17$winnershare_asam_nac[actas17$underperc_asam_nac_asam_prov==0], na.rm = T))
    hist(actas17$winnershare_asam_nac[actas17$underperc_asam_nac_asam_prov>0], breaks=100, col="red", add=T)
    abline(v=mean(actas17$winnershare_asam_nac[actas17$underperc_asam_nac_asam_prov>0], na.rm = T), col="red")
    
    # Andean parliament 
    hist(actas17$winnershare_andino[actas17$under_andino_prov==0], breaks=100, 
         main="", xlab="Alianza Pais Vote Share", ylab="Frequency")
    abline(v=mean(actas17$winnershare_andino[actas17$under_andino_prov==0], na.rm = T))
    hist(actas17$winnershare_andino[actas17$under_andino_prov>0], breaks=100, col="red", add=T)
    abline(v=mean(actas17$winnershare_andino[actas17$under_andino_prov>0], na.rm = T), col="red")
    
    # National referendum
    hist(actas17$winnershare_consulta[actas17$under_consulta_prov==0], breaks=100, 
         main="", xlab="Vote Share for Government-Endorsed Option", ylab="Frequency")
    abline(v=mean(actas17$winnershare_consulta[actas17$under_consulta_prov==0], na.rm = T))
    hist(actas17$winnershare_consulta[actas17$under_consulta_prov>0], breaks=100, col="red", add=T)
    abline(v=mean(actas17$winnershare_consulta[actas17$under_consulta_prov>0], na.rm = T), col="red")
    
    
    
#' ----------------------------------
# 5. within-canton scatterplots -----
#' ----------------------------------
  
    cantons <- ecu$canton_title[which(ecu$`Correlation Between Undervoting and Winner's Vote Share`>0.3)]
    par(mfrow=c(3,3))
    
    for (canton in unique(cantons)) {
      
      used_data <- actas17[which(actas17$canton_title==canton),]
      used_data <- used_data[which(!is.na(used_data$under_pres_asam_prov) & !is.na(used_data$pw_pres) & used_data$under_pres_asam_prov != 0),]
      plot(used_data$under_pres_asam_prov, used_data$pw_pres)
      print(nrow(used_data))
      
    }
  
#' -----------------------
# 6. modeling ------------
#' -----------------------

  library(lme4)
  library(performance)
  library(MuMIn)
  library(MASS)
  
  controls <- ~. + closeness_pres + ELECTORES_REGISTRO_pres + turnout_pres + null_pres + blank_pres + (1 | CANTON_NOMBRE_pres)
    
  #' ------------------------------
  # 6.1 presidential election -----
  #' ------------------------------
  
    # presidential election
    # excluded_cases <- which(actas17$pw_pres==1 | actas17$pw_pres==0)
    m_pres1 <- lmer(pw_pres ~ underperc_pres_asam_prov + (1 | CANTON_NOMBRE_pres), data = actas17)
    m_pres2 <- lmer(update(pw_pres ~ underperc_pres_asam_prov, controls), 
                        data = actas17) ### n should be the same as unvervoting_n. Which ones drop out due to Inf or value greater 1? I should actually only exclude the 0s. 
    
    performance::icc(m_pres1, by_group=T)
    r.squaredGLMM(m_pres1)
    
    performance::icc(m_pres2, by_group=T)
    r.squaredGLMM(m_pres2)
    
    # get coefficients and variances
    coef <- coef(m_pres2)
    coef <- colMeans(coef$CANTON_NOMBRE_pres)
    vhat <- vcov(m_pres2)
    sigma <- sqrt(sum(residuals(m_pres2)^2) / (nrow(actas17) - length(coef)))  
    draws <- mvrnorm(10000, coef, vhat)
    
    # set covariate values, rest to means, field to 'Education', vary over bill importance
    mus <- matrix(NA, 
                  nrow=10000, 
                  ncol=length(unique(actas17$underperc_pres_asam_prov))
                  )
    
    for (i in 1:length(unique(actas17$underperc_pres_asam_prov))) {
      
      mean_setting <- c(1, sort(unique(actas17$underperc_pres_asam_prov))[i], 
                        mean(actas17$closeness_pres),
                        mean(actas17$ELECTORES_REGISTRO_pres),
                        mean(actas17$turnout_pres, na.rm=T),
                        mean(actas17$null_pres, na.rm=T),
                        mean(actas17$blank_pres, na.rm=T))
      
      mus[,i] <- draws %*% as.matrix(mean_setting)
      
    }
    
    # visualize expected values
    ses <- rep(NA, length(unique(actas17$underperc_pres_asam_prov)))
    for (i in 1:ncol(mus)) 
      ses[i] <- sd(mus[,i])
     
    exp_values <- as.data.frame(cbind(colMeans(mus),
                                      sort(unique(actas17$underperc_pres_asam_prov)),
                                      ses)
                                )
    colnames(exp_values) <- c("exp", "under", "se")
    exp_values$exp <- as.numeric(as.character(exp_values$exp))
    exp_values$imp <- as.numeric(as.character(exp_values$under))
    exp_values$se <- as.numeric(as.character(exp_values$se))
    
    p1 <- ggplot(exp_values, aes(x=under, y=exp)) +
      geom_smooth(method=lm, fill="black", color="black") +
      geom_ribbon(aes(ymin=exp-se*2, 
                      ymax=exp+se*2), alpha=0.1) +
     # geom_hline(yintercept=mean(actas17$pw_pres, na.rm=T), size=1.1, linetype="dashed", color = "black") +
      geom_rug(sides="b") + 
      scale_colour_manual(values = c("darkgrey", "black"), name = "", labels = c("", "")) +
      scale_fill_manual(values = c("darkgrey", "black"), name = "", labels = c("", "")) + 
      guides(color=F, fill=F, fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) + 
      xlim(0,1) + ylim(0.33, 0.4) +
      #scale_x_continuous(breaks=seq(0, 45, 5), limits=c(0,45),) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"),  panel.border = element_blank(), 
            axis.text=element_text(size=25), axis.title=element_text(size=25),
            legend.text=element_text(size=25), legend.title=element_text(size=25)) +
      xlab("Share of Discrepant Votes Among All Votes") + ylab("Winner's Vote Share")
    p1 <- p1 + annotation_custom(grobTree(textGrob("Presidential Election", x=0.04,  y=0.95, hjust=0,
                        gp=gpar(col="black", fontsize=20, fontface="bold"))))
  
    
  #' ----------------------------
  # 6.2 national parliament -----
  #' ----------------------------
    
    # national parliament
    # excluded_cases <- which(actas17$winnershare_asam_nac ==1 | actas17$winnershare_asam_nac==0)
    m_nac1 <- lmer(winnershare_asam_nac ~ underperc_asam_nac_asam_prov + (1 | CANTON_NOMBRE_pres), data = actas17)
    m_nac2 <- lmer(update(winnershare_asam_nac ~ underperc_asam_nac_asam_prov, controls), 
                       data = actas17) ### n should be the same as unvervoting_n. Which ones drop out due to Inf or value greater 1? I should actually only exclude the 0s. 
    
    performance::icc(m_nac1, by_group=T)
    r.squaredGLMM(m_nac1)
    
    performance::icc(m_nac2, by_group=T)
    r.squaredGLMM(m_nac2)
    
    # get coefficients and variances
    coef <- coef(m_nac2)
    coef <- colMeans(coef$CANTON_NOMBRE_pres)
    vhat <- vcov(m_nac2)
    sigma <- sqrt(sum(residuals(m_nac2)^2) / (nrow(actas17) - length(coef)))  
    draws <- mvrnorm(10000, coef, vhat)
    
    # set covariate values, rest to means, field to 'Education', vary over bill importance
    mus <- matrix(NA, 
                  nrow=10000, 
                  ncol=length(unique(actas17$underperc_asam_nac_asam_prov))
    )
    
    for (i in 1:length(unique(actas17$underperc_asam_nac_asam_prov))) {
      
      mean_setting <- c(1, sort(unique(actas17$underperc_asam_nac_asam_prov))[i], 
                        mean(actas17$closeness_pres),
                        mean(actas17$ELECTORES_REGISTRO_pres),
                        mean(actas17$turnout_pres, na.rm=T),
                        mean(actas17$null_pres, na.rm=T),
                        mean(actas17$blank_pres, na.rm=T))
      
      mus[,i] <- draws %*% as.matrix(mean_setting)
      
    }
    
    # visualize expected values
    ses <- rep(NA, length(unique(actas17$underperc_asam_nac_asam_prov)))
    for (i in 1:ncol(mus)) 
      ses[i] <- sd(mus[,i])
    
    exp_values <- as.data.frame(cbind(colMeans(mus),
                                      sort(unique(actas17$underperc_asam_nac_asam_prov)),
                                      ses)
    )
    colnames(exp_values) <- c("exp", "under", "se")
    exp_values$exp <- as.numeric(as.character(exp_values$exp))
    exp_values$imp <- as.numeric(as.character(exp_values$under))
    exp_values$se <- as.numeric(as.character(exp_values$se))
    
    ggplot(exp_values, aes(x=under, y=exp)) +
      geom_smooth(method=lm, fill="black") +
      geom_ribbon(aes(ymin=exp-se*2, 
                      ymax=exp+se*2), alpha=0.1) +
      #geom_hline(yintercept=0, size=1.1, linetype="dashed", color = "black") +
      geom_rug(sides="bl") + 
      scale_colour_manual(values = c("darkgrey", "black"), name = "", labels = c("", "")) +
      scale_fill_manual(values = c("darkgrey", "black"), name = "", labels = c("", "")) + 
      guides(color=F, fill=F, fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) + 
      xlim(0,1) +
      #scale_x_continuous(breaks=seq(0, 45, 5), limits=c(0,45),) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"),  panel.border = element_blank(), 
            axis.text=element_text(size=25), axis.title=element_text(size=25),
            legend.text=element_text(size=25), legend.title=element_text(size=25)) +
      xlab("Share of Discrepant Votes Among All Votes") + ylab("Winner's Vote Share")
    
    
  #' --------------------------
  # 6.3 Andean parliament -----
  #' --------------------------
    
    # Andean parliament
    # excluded_cases <- which(actas17$winnershare_andino ==1 | actas17$winnershare_andino==0)
    m_andino1 <- lmer(winnershare_andino ~ underperc_andino_asam_prov + (1 | CANTON_NOMBRE_pres), data = actas17)
    m_andino2 <- lmer(update(winnershare_andino ~ underperc_andino_asam_prov, controls), 
                      data = actas17) ### n should be the same as unvervoting_n. Which ones drop out due to Inf or value greater 1? I should actually only exclude the 0s. 
    
    performance::icc(m_andino1, by_group=T)
    r.squaredGLMM(m_andino1)
    
    performance::icc(m_andino2, by_group=T)
    r.squaredGLMM(m_andino2)
    
    # get coefficients and variances
    coef <- coef(m_andino2)
    coef <- colMeans(coef$CANTON_NOMBRE_pres)
    vhat <- vcov(m_andino2)
    sigma <- sqrt(sum(residuals(m_andino2)^2) / (nrow(actas17) - length(coef)))  
    draws <- mvrnorm(10000, coef, vhat)
    
    # set covariate values, rest to means, field to 'Education', vary over bill importance
    mus <- matrix(NA, 
                  nrow=10000, 
                  ncol=length(unique(actas17$underperc_andino_asam_prov))
    )
    
    for (i in 1:length(unique(actas17$underperc_andino_asam_prov))) {
      
      mean_setting <- c(1, sort(unique(actas17$underperc_andino_asam_prov))[i], 
                        mean(actas17$closeness_pres),
                        mean(actas17$ELECTORES_REGISTRO_pres),
                        mean(actas17$turnout_pres, na.rm=T),
                        mean(actas17$null_pres, na.rm=T),
                        mean(actas17$blank_pres, na.rm=T))
      
      mus[,i] <- draws %*% as.matrix(mean_setting)
      
    }
    
    # visualize expected values
    ses <- rep(NA, length(unique(actas17$underperc_andino_asam_prov)))
    for (i in 1:ncol(mus)) 
      ses[i] <- sd(mus[,i])
    
    exp_values <- as.data.frame(cbind(colMeans(mus),
                                      sort(unique(actas17$underperc_andino_asam_prov)),
                                      ses)
    )
    colnames(exp_values) <- c("exp", "under", "se")
    exp_values$exp <- as.numeric(as.character(exp_values$exp))
    exp_values$imp <- as.numeric(as.character(exp_values$under))
    exp_values$se <- as.numeric(as.character(exp_values$se))
    
    ggplot(exp_values, aes(x=under, y=exp)) +
      geom_smooth(method=lm, fill="black") +
      geom_ribbon(aes(ymin=exp-se*2, 
                      ymax=exp+se*2), alpha=0.1) +
      #geom_hline(yintercept=0, size=1.1, linetype="dashed", color = "black") +
      geom_rug(sides="bl") + 
      scale_colour_manual(values = c("darkgrey", "black"), name = "", labels = c("", "")) +
      scale_fill_manual(values = c("darkgrey", "black"), name = "", labels = c("", "")) + 
      guides(color=F, fill=F, fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) + 
      xlim(0,1) +
      #scale_x_continuous(breaks=seq(0, 45, 5), limits=c(0,45),) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"),  panel.border = element_blank(), 
            axis.text=element_text(size=25), axis.title=element_text(size=25),
            legend.text=element_text(size=25), legend.title=element_text(size=25)) +
      xlab("Share of Discrepant Votes Among All Votes") + ylab("Winner's Vote Share") 
    
    
  #' ----------------------------
  # 6.4 National referendum -----
  #' ----------------------------
    
    # National referendum 
    # excluded_cases <- which(actas17$winnershare_consulta ==1 | actas17$winnershare_consulta==0)
    m_consulta1 <- lmer(winnershare_consulta ~ underperc_consulta_asam_prov + (1 | CANTON_NOMBRE_pres), data = actas17)
    m_consulta2 <- lmer(update(winnershare_consulta ~ underperc_consulta_asam_prov, controls), 
                         data = actas17) ### n should be the same as unvervoting_n. Which ones drop out due to Inf or value greater 1? I should actually only exclude the 0s. 
    
    performance::icc(m_consulta1, by_group=T)
    r.squaredGLMM(m_consulta1)
    
    performance::icc(m_consulta2, by_group=T)
    r.squaredGLMM(m_consulta2)
    
    # get coefficients and variances
    coef <- coef(m_consulta2)
    coef <- colMeans(coef$CANTON_NOMBRE_pres)
    vhat <- vcov(m_consulta2)
    sigma <- sqrt(sum(residuals(m_consulta2)^2) / (nrow(actas17) - length(coef)))  
    draws <- mvrnorm(10000, coef, vhat)
    
    # set covariate values, rest to means, field to 'Education', vary over bill importance
    mus <- matrix(NA, 
                  nrow=10000, 
                  ncol=length(unique(actas17$underperc_consulta_asam_prov))
    )
    
    for (i in 1:length(unique(actas17$underperc_consulta_asam_prov))) {
      
      mean_setting <- c(1, sort(unique(actas17$underperc_consulta_asam_prov))[i], 
                        mean(actas17$closeness_pres),
                        mean(actas17$ELECTORES_REGISTRO_pres),
                        mean(actas17$turnout_pres, na.rm=T),
                        mean(actas17$null_pres, na.rm=T),
                        mean(actas17$blank_pres, na.rm=T))
      
      mus[,i] <- draws %*% as.matrix(mean_setting)
      
    }
    
    # visualize expected values
    ses <- rep(NA, length(unique(actas17$underperc_consulta_asam_prov)))
    for (i in 1:ncol(mus)) 
      ses[i] <- sd(mus[,i])
    
    exp_values <- as.data.frame(cbind(colMeans(mus),
                                      sort(unique(actas17$underperc_consulta_asam_prov)),
                                      ses)
    )
    colnames(exp_values) <- c("exp", "under", "se")
    exp_values$exp <- as.numeric(as.character(exp_values$exp))
    exp_values$imp <- as.numeric(as.character(exp_values$under))
    exp_values$se <- as.numeric(as.character(exp_values$se))
    
    p2 <- ggplot(exp_values, aes(x=under, y=exp)) +
      geom_smooth(method=lm, fill="black", color="black") +
      geom_ribbon(aes(ymin=exp-se*2, 
                      ymax=exp+se*2), alpha=0.1) +
      #geom_hline(yintercept=0, size=1.1, linetype="dashed", color = "black") +
      geom_rug(sides="b") + 
      scale_colour_manual(values = c("darkgrey", "black"), name = "", labels = c("", "")) +
      scale_fill_manual(values = c("darkgrey", "black"), name = "", labels = c("", "")) + 
      guides(color=F, fill=F, fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) + 
      xlim(0,1) + ylim(0.45,0.53) +
      #scale_x_continuous(breaks=seq(0, 45, 5), limits=c(0,45),) +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"),  panel.border = element_blank(), 
            axis.text=element_text(size=25), axis.title=element_text(size=25),
            legend.text=element_text(size=25), legend.title=element_text(size=25)) +
      xlab("Share of Discrepant Votes Among All Votes") + ylab("")
    p2 <- p2 + annotation_custom(grobTree(textGrob("National Referendum", x=0.04,  y=0.95, hjust=0,
                                                   gp=gpar(col="black", fontsize=20, fontface="bold"))))
    
    
    grid.arrange(p1, p2, ncol=2)
  
    stargazer(m_pres1, m_pres2, m_nac1, m_nac2, m_andino1, m_andino2, m_consulta1, m_consulta2,
              star.cutoffs = c(.05, .01, .001))
    
    
    
  #' ----------------------------------
  # 6.2 y = extent of undervoting -----
  #' ----------------------------------
    
    
  excluded_cases <- c(which(actas17$under_pres_asam_prov==0), which(actas17$under_pres_asam_prov>=1))
  controls <- ~. + pw_pres + runner_pres + ELECTORES_REGISTRO_pres + turnout_pres + null_pres + blank_pres
  m17_pres <- betareg(update(under_pres_asam_prov ~ closeness_pres, controls), link="logit",
                      data = actas17[-excluded_cases,]) ### n should be the same as unvervoting_n. Which ones drop out due to Inf or value greater 1? I should actually only exclude the 0s. 
  
  
  stargazer(m17_pres,
            covariate.labels = c("Closeness of Electoral Race", "Winner's Vote Share",
                                 "Runner Up's Vote Share", "Number of Eligible Voters", "Percentage Turnout", 
                                 "Percentage Null Votes", "Percentage Blank Votes"), 
            star.cutoffs = c(.05, .01, .001))
  
  
  
#' ------------------------------------------------------
# 7. geographical patterns in irregularities ------------
# urban/rural divide
# size of parroquia
#' ------------------------------------------------------

  actas17$under_pres_consulta <- actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_consulta 
  actas17$under_pres_asam_prov <- actas17$SUFRAGANTES_pres - actas17$SUFRAGANTES_asam_prov
  actas17$under_nac_prov <- actas17$SUFRAGANTES_asam_nac - actas17$SUFRAGANTES_asam_prov
  actas17$under_andino_prov <- actas17$SUFRAGANTES_andino - actas17$SUFRAGANTES_asam_prov
  actas17$under_consulta_prov <- actas17$SUFRAGANTES_consulta - actas17$SUFRAGANTES_asam_prov
  actas19_alcal_conc_rural$under <- actas19_alcal_conc_rural$SUFRAGANTES_alcal - actas19_alcal_conc_rural$SUFRAGANTES_conc_rural
  actas19_alcal_conc_urban$under <- actas19_alcal_conc_urban$SUFRAGANTES_alcal - actas19_alcal_conc_urban$SUFRAGANTES_conc_urban
  actas19_alcal_pref$under <- actas19_alcal_pref$SUFRAGANTES_alcal - actas19_alcal_pref$SUFRAGANTES_pref
  actas19_alcal_vocales$under <- actas19_alcal_vocales$SUFRAGANTES_alcal - actas19_alcal_vocales$SUFRAGANTES_vocales
  
  ## general idea: split plots in 7.1 and 7.2 up 4x times and just plot irregularities 
  ## for urban/rural based on one race
  
  
  #' -----------------------
  # 7.1 scatterplots -------
  #' -----------------------
  
    library(ggpubr)
    library(ggplot2)
    library(dplyr)
    library(patchwork)
  
    actas17_urban <- actas17[actas17$PARROQUIA_ESTADO_pres=="URBANA",]
    actas17_rural <- actas17[actas17$PARROQUIA_ESTADO_pres=="RURAL",]
  
    # presidential election vs. provincial parliament
    actas17_plot1 <- actas17[,c("SUFRAGANTES_asam_prov", "SUFRAGANTES_pres", "PARROQUIA_ESTADO_pres", "under_pres_asam_prov")]
    actas17_plot1 <- actas17_plot1 %>% 
      filter(PARROQUIA_ESTADO_pres == "URBANA" | PARROQUIA_ESTADO_pres == "RURAL", 
             under_pres_asam_prov != 0)
    
    plot1 <- ggplot(actas17_plot1, aes(x = SUFRAGANTES_asam_prov, y = SUFRAGANTES_pres, color = PARROQUIA_ESTADO_pres)) + 
      geom_point(aes(color = PARROQUIA_ESTADO_pres), size = 3, alpha=0.4) + 
      scale_color_discrete(name = "Location", labels = c("Rural", "Urban")) + 
      scale_y_continuous(name = "N ballots, presidential election", limits = c(0,350)) + 
      scale_x_continuous(name = "N ballots, regional parliament", limits = c(0,350)) + 
      geom_abline(intercept = 0, slope = 1, lwd=1.5, lty = 2, col = "darkgrey") + 
      geom_abline(intercept = 35, slope = 1, lwd=1, lty = 2, col = "darkgrey") + 
      geom_abline(intercept = -35, slope = 1, lwd=1, lty = 2, col = "darkgrey") + 
      geom_curve(
        aes(x = 100, y = 43, xend = 50, yend = 43),
        arrow = arrow(length = unit(0.03, "npc")), 
        col = "darkgrey",
        angle = 0, 
        curvature = -0.5
      ) +
      geom_curve(
        aes(x = 100, y = 25, xend = 65, yend = 25),
        arrow = arrow(length = unit(0.03, "npc")), 
        col = "darkgrey",
        angle = 0, 
        curvature = -0.5
      ) +
      annotate(geom="text", x=197, y=43, label="No undervoting irregularities", col="darkgrey", cex=6) + 
      annotate(geom="text", x=215, y=25, label="Undervoting, 10% of eligible voters", col="darkgrey", cex=6) + 
      geom_rug() + 
      # theme_pubr() +
      theme_minimal(base_size=20) +
      theme(legend.position = "none")
    
    
     
      
    
    # national parliament vs. provincial parliament
    actas17_plot2 <- actas17[,c("SUFRAGANTES_asam_prov", "SUFRAGANTES_asam_nac", "PARROQUIA_ESTADO_pres", "under_nac_prov")]
    actas17_plot2 <- actas17_plot2 %>% 
      filter(PARROQUIA_ESTADO_pres == "URBANA" | PARROQUIA_ESTADO_pres == "RURAL", 
             under_nac_prov != 0)
    
    plot2 <- ggplot(actas17_plot2, aes(x = SUFRAGANTES_asam_prov, y = SUFRAGANTES_asam_nac, color = PARROQUIA_ESTADO_pres)) + 
      geom_point(aes(color = PARROQUIA_ESTADO_pres), size = 3, alpha=0.4) + 
      scale_color_discrete(name = "Location", labels = c("Rural", "Urban")) + 
      scale_y_continuous(name = "N ballots, national parliament", limits = c(0,350)) + 
      scale_x_continuous(name = "N ballots, regional parliament", limits = c(0,350)) +
      geom_abline(intercept = 0, slope = 1, lwd=1.5, lty = 2, col = "darkgrey") + 
      geom_abline(intercept = 35, slope = 1, lwd=1, lty = 2, col = "darkgrey") + 
      geom_abline(intercept = -35, slope = 1, lwd=1, lty = 2, col = "darkgrey") + 
      geom_curve(
        aes(x = 100, y = 43, xend = 50, yend = 43),
        arrow = arrow(length = unit(0.03, "npc")), 
        col = "darkgrey",
        angle = 0, 
        curvature = -0.5
      ) +
      geom_curve(
        aes(x = 100, y = 25, xend = 65, yend = 25),
        arrow = arrow(length = unit(0.03, "npc")), 
        col = "darkgrey",
        angle = 0, 
        curvature = -0.5
      ) +
      annotate(geom="text", x=197, y=43, label="No undervoting irregularities", col="darkgrey", cex=6) + 
      annotate(geom="text", x=215, y=25, label="Undervoting, 10% of eligible voters", col="darkgrey", cex=6) + 
      geom_rug() + 
      # theme_pubr() +
      theme_minimal(base_size=20) +
      theme(legend.position = "none")
    
    
    # Andean parliament vs. provincial parliament
    actas17_plot3 <- actas17[,c("SUFRAGANTES_asam_prov", "SUFRAGANTES_andino", "PARROQUIA_ESTADO_pres", "under_andino_prov")]
    actas17_plot3 <- actas17_plot3 %>% 
      filter(PARROQUIA_ESTADO_pres == "URBANA" | PARROQUIA_ESTADO_pres == "RURAL", 
             under_andino_prov != 0)
    
    plot3 <- ggplot(actas17_plot3, aes(x = SUFRAGANTES_asam_prov, y = SUFRAGANTES_andino, color = PARROQUIA_ESTADO_pres)) + 
      geom_point(aes(color = PARROQUIA_ESTADO_pres), size = 3, alpha=0.4) + 
      scale_color_discrete(name = "Location", labels = c("Rural", "Urban")) + 
      scale_y_continuous(name = "N ballots, Andean parliament", limits = c(0,350)) + 
      scale_x_continuous(name = "N ballots, regional parliament", limits = c(0,350)) + 
      geom_abline(intercept = 0, slope = 1, lwd=1.5, lty = 2, col = "darkgrey") + 
      geom_abline(intercept = 35, slope = 1, lwd=1, lty = 2, col = "darkgrey") + 
      geom_abline(intercept = -35, slope = 1, lwd=1, lty = 2, col = "darkgrey") + 
      geom_curve(
        aes(x = 100, y = 43, xend = 50, yend = 43),
        arrow = arrow(length = unit(0.03, "npc")), 
        col = "darkgrey",
        angle = 0, 
        curvature = -0.5
      ) +
      geom_curve(
        aes(x = 100, y = 25, xend = 65, yend = 25),
        arrow = arrow(length = unit(0.03, "npc")), 
        col = "darkgrey",
        angle = 0, 
        curvature = -0.5
      ) +
      annotate(geom="text", x=197, y=43, label="No undervoting irregularities", col="darkgrey", cex=6) + 
      annotate(geom="text", x=215, y=25, label="Undervoting, 10% of eligible voters", col="darkgrey", cex=6) + 
      geom_rug() + 
      # theme_pubr() +
      theme_minimal(base_size=20) +
      theme(legend.position = "none")
    
    # National referendum vs. provincial parliament
    actas17_plot4 <- actas17[,c("SUFRAGANTES_asam_prov", "SUFRAGANTES_consulta", "PARROQUIA_ESTADO_pres", "under_consulta_prov")]
    actas17_plot4 <- actas17_plot4 %>% 
      filter(PARROQUIA_ESTADO_pres == "URBANA" | PARROQUIA_ESTADO_pres == "RURAL", 
             under_consulta_prov != 0)
    
    plot4 <- ggplot(actas17_plot4, aes(x = SUFRAGANTES_asam_prov, y = SUFRAGANTES_consulta, color = PARROQUIA_ESTADO_pres)) + 
      geom_point(aes(color = PARROQUIA_ESTADO_pres), size = 3, alpha=0.4) + 
      scale_color_discrete(name = "Location", labels = c("Rural", "Urban")) + 
      scale_y_continuous(name = "N ballots, national referendum", limits = c(0,350)) + 
      scale_x_continuous(name = "N ballots, regional parliament", limits = c(0,350)) + 
      geom_abline(intercept = 0, slope = 1, lwd=1.5, lty = 2, col = "darkgrey") + 
      geom_abline(intercept = 35, slope = 1, lwd=1, lty = 2, col = "darkgrey") + 
      geom_abline(intercept = -35, slope = 1, lwd=1, lty = 2, col = "darkgrey") + 
      geom_curve(
        aes(x = 100, y = 43, xend = 50, yend = 43),
        arrow = arrow(length = unit(0.03, "npc")), 
        col = "darkgrey",
        angle = 0, 
        curvature = -0.5
      ) +
      geom_curve(
        aes(x = 100, y = 25, xend = 65, yend = 25),
        arrow = arrow(length = unit(0.03, "npc")), 
        col = "darkgrey",
        angle = 0, 
        curvature = -0.5
      ) +
      annotate(geom="text", x=197, y=43, label="No undervoting irregularities", col="darkgrey", cex=6) + 
      annotate(geom="text", x=215, y=25, label="Undervoting, 10% of eligible voters", col="darkgrey", cex=6) + 
      geom_rug() + 
      # theme_pubr() +
      theme_minimal(base_size=20) +
      theme(legend.position = "none")
    
  
    
    # heat maps? visualizing bivariate distributions with ellipses?
    
    
  #' ---------------------
  # 7.2 histograms -------
  #' ---------------------
    
    actas17_histos <- actas17[,c("PARROQUIA_ESTADO_pres", "under_pres_asam_prov", "under_nac_prov", "under_andino_prov", "under_consulta_prov")]
    
    # presidential election vs. provincial parliament
    actas17_histo1 <- actas17_histos %>% 
      filter(PARROQUIA_ESTADO_pres == "URBANA" | PARROQUIA_ESTADO_pres == "RURAL", 
             under_pres_asam_prov != 0)
    
    plot5 <- ggplot(actas17_histo1, aes(x = under_pres_asam_prov, fill = PARROQUIA_ESTADO_pres, color = PARROQUIA_ESTADO_pres)) +                       # Draw overlaying histogram
      geom_histogram(alpha = 0.4, bins = 100) +
      scale_fill_discrete(name = "Location", labels = c("Rural", "Urban")) +
      scale_x_continuous(name = "Number of Discrepant Ballots", limits = c(-150,150)) + 
      scale_y_continuous(name = "Frequency") + 
      xlab("Discrepant Number of Ballots") + 
      geom_vline(xintercept = 0, lwd=1.5, lty = 2, col = "darkgrey") + 
      geom_vline(xintercept = 35, lwd=1, lty = 2, col = "darkgrey") + 
      geom_vline(xintercept = -35, lwd=1, lty = 2, col = "darkgrey") + 
      #geom_curve(
      #  aes(x = 65, y = 150, xend = 2, yend = 150),
      #  arrow = arrow(length = unit(0.03, "npc")), 
      #  col = "darkgrey",
      #  angle = 0, 
      #  curvature = -0.5
      #) +
      #annotate(geom="text", x=110, y=150, label="No undervoting irregularities", col="darkgrey", cex=6) + 
      #annotate(geom="text", x=62, y=50, label= "10% of eligible voters", col="darkgrey", cex=6) + 
      geom_rug() +
      #theme_pubr() +
      guides(color = "none") +
      theme_minimal(base_size=20)
    
    
    # national parliament vs. provincial parliament
    actas17_histo2 <- actas17_histos %>% 
      filter(PARROQUIA_ESTADO_pres == "URBANA" | PARROQUIA_ESTADO_pres == "RURAL", 
             under_nac_prov != 0)
    
    plot6 <- ggplot(actas17_histo2, aes(x = under_nac_prov, fill = PARROQUIA_ESTADO_pres, color = PARROQUIA_ESTADO_pres)) +                       # Draw overlaying histogram
      geom_histogram(alpha = 0.4, bins = 100) +
      scale_fill_discrete(name = "Location", labels = c("Rural", "Urban")) +
      scale_x_continuous(name = "Number of Discrepant Ballots", limits = c(-150,150)) + 
      scale_y_continuous(name = "Frequency") + 
      xlab("Discrepant Number of Ballots") + 
      geom_vline(xintercept = 0, lwd=1.5, lty = 2, col = "darkgrey") + 
      geom_vline(xintercept = 35, lwd=1, lty = 2, col = "darkgrey") + 
      geom_vline(xintercept = -35, lwd=1, lty = 2, col = "darkgrey") + 
      # geom_curve(
      #   aes(x = 50, y = 95, xend = 2, yend = 95),
      #   arrow = arrow(length = unit(0.03, "npc")), 
      #   col = "darkgrey",
      #   angle = 0, 
      #   curvature = -0.5
      # ) +
      # geom_curve(
      #   aes(x = 50, y = 75, xend = 37, yend = 75),
      #   arrow = arrow(length = unit(0.03, "npc")), 
      #   col = "darkgrey",
      #   angle = 0, 
      #   curvature = -0.5
      # ) +
      # annotate(geom="text", x=105, y=95, label="No undervoting irregularities", col="darkgrey") + 
      # annotate(geom="text", x=75, y=75, label="Undervoting,", col="darkgrey") + 
      # annotate(geom="text", x=91, y=60, label="10% of eligible voters", col="darkgrey") + 
      geom_rug() +
      #theme_pubr() +
      guides(color = "none") +
      theme_minimal(base_size=20)
    
   
    # Andean parliament vs. provincial parliament
    actas17_histo3 <- actas17_histos %>% 
      filter(PARROQUIA_ESTADO_pres == "URBANA" | PARROQUIA_ESTADO_pres == "RURAL", 
             under_andino_prov != 0)
    
    plot7 <- ggplot(actas17_histo3, aes(x = under_nac_prov, fill = PARROQUIA_ESTADO_pres, color = PARROQUIA_ESTADO_pres)) +                       # Draw overlaying histogram
      geom_histogram(alpha = 0.4, bins = 100) +
      scale_fill_discrete(name = "Location", labels = c("Rural", "Urban")) +
      scale_x_continuous(name = "Number of Discrepant Ballots", limits = c(-150,150)) + 
      scale_y_continuous(name = "Frequency") + 
      xlab("Discrepant Number of Ballots") + 
      geom_vline(xintercept = 0, lwd=1.5, lty = 2, col = "darkgrey") + 
      geom_vline(xintercept = 35, lwd=1, lty = 2, col = "darkgrey") + 
      geom_vline(xintercept = -35, lwd=1, lty = 2, col = "darkgrey") + 
      geom_rug() +
      #theme_pubr() +
      guides(color = "none") +
      theme_minimal(base_size=20)
   
    
    # National referendum vs. provincial parliament
    actas17_histo4 <- actas17_histos %>% 
      filter(PARROQUIA_ESTADO_pres == "URBANA" | PARROQUIA_ESTADO_pres == "RURAL", 
             under_consulta_prov != 0)
    
    plot8 <- ggplot(actas17_histo4, aes(x = under_consulta_prov, fill = PARROQUIA_ESTADO_pres, color = PARROQUIA_ESTADO_pres)) +                       # Draw overlaying histogram
      geom_histogram(alpha = 0.4, bins = 100) +
      scale_fill_discrete(name = "Location", labels = c("Rural", "Urban")) +
      scale_x_continuous(name = "Number of Discrepant Ballots", limits = c(-150,150)) + 
      scale_y_continuous(name = "Frequency") + 
      xlab("Discrepant Number of Ballots") + 
      geom_vline(xintercept = 0, lwd=1.5, lty = 2, col = "darkgrey") + 
      geom_vline(xintercept = 35, lwd=1, lty = 2, col = "darkgrey") + 
      geom_vline(xintercept = -35, lwd=1, lty = 2, col = "darkgrey") + 
      geom_rug() +
      #theme_pubr() +
      guides(color = "none") +
      theme_minimal(base_size=20)
    
    
    grid.arrange(plot5, plot6, plot7, plot8)
    
    grid.arrange(plot1, plot5, ncol = 2)
    
    #' ---------------------------------
    # 7.3 descriptive statistics -------
    #' ---------------------------------
    
    library(stargazer)
    actas17_histosURBAN <- actas17_histos[actas17_histos$PARROQUIA_ESTADO_pres == "URBANA",]
    actas17_histosRURAL <- actas17_histos[actas17_histos$PARROQUIA_ESTADO_pres == "RURAL",]
    stargazer(actas17_histosURBAN, type="latex")    
    stargazer(actas17_histosRURAL, type="latex") # given range (min/max), differences are negligible
    
    #' ------------------
    # 7.4 t-tests -------
    #' ------------------
    
    pres13A = pres13A[-which(pres13A$TERRITORIO_NOMBRE == "EXTERIOR"),]
    pres13A <- pres13A[-which(pres13A$PROVINCIA_NOMBRE=="GALÁPAGOS"),]
    t.test(actas17$under_pres_asam_prov[actas17$PARROQUIA_ESTADO_pres=="URBANA"], 
           actas17$under_pres_asam_prov[actas17$PARROQUIA_ESTADO_pres=="RURAL"])
    t.test(actas17$under_nac_prov[actas17$PARROQUIA_ESTADO_pres=="URBANA"], 
           actas17$under_nac_prov[actas17$PARROQUIA_ESTADO_pres=="RURAL"])
    t.test(actas17$under_andino_prov[actas17$PARROQUIA_ESTADO_pres=="URBANA"], 
           actas17$under_andino_prov[actas17$PARROQUIA_ESTADO_pres=="RURAL"])
    t.test(actas17$under_consulta_prov[actas17$PARROQUIA_ESTADO_pres=="URBANA"], 
           actas17$under_consulta_prov[actas17$PARROQUIA_ESTADO_pres=="RURAL"])
    
    #' -----------------------------
    # 7.5 size of parroquia -------
    #' -----------------------------
    
    # just using number of eligible voters per parroquia
    actas17_size <- actas17[,c("PARROQUIA_NOMBRE_pres", "PARROQUIA_ESTADO_pres", "ELECTORES_REGISTRO_pres", 
                               "under_pres_asam_prov", "under_nac_prov", "under_andino_prov", "under_consulta_prov")]
    
    actas17_size <- actas17_size %>% 
      group_by(PARROQUIA_NOMBRE_pres) %>% 
      summarize(electores = sum(ELECTORES_REGISTRO_pres), 
                under_pres_asam_prov = sum(abs(under_pres_asam_prov)),
                under_nac_prov = sum(abs(under_nac_prov)),
                under_andino_prov = sum(abs(under_andino_prov)),
                under_consulta_prov = sum(abs(under_consulta_prov)),
                )
    
    actas17_size_pres <- actas17_size %>% 
      filter(under_pres_asam_prov != 0, 
             electores < 200000)
    plot9 <- ggplot(actas17_size_pres, aes(x = electores, y = under_pres_asam_prov)) + 
      geom_point(size = 3, alpha=0.4) + 
      geom_smooth(method='lm', formula= y~x) + 
      geom_smooth(method='loess', formula= y~x) + 
      geom_rug() + 
      theme_minimal() +
      theme(legend.position = "none")
    
    
    
    # merge with population density data from INEC
    library(readxl)
    ecu_dens <- read_excel("densidad_parroquia.xlsx", skip=8)
    actas17_dens <- actas17[,c("PARROQUIA_NOMBRE_pres", "PARROQUIA_ESTADO_pres", "under_pres_asam_prov", "under_nac_prov", "under_andino_prov", "under_consulta_prov")]
    colnames(actas17_dens)[1] <- "Nombre de parroquia"
    under_dens <- merge(actas17_dens, ecu_dens, by="Nombre de parroquia")
    
    
    