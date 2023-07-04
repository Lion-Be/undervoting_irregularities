#' ------------------------------------------------------------------------
#' preparing CNE data, electoral results at level of vote tallies (actas)
#' General elections 2017
#' Lion Behrens
#' ------------------------------------------------------------------------

library(foreign)
library(stringr)

# load in raw datasets
primera17 <- read.spss("U:/PhD Electoral Fraud/Data/Ecuador/CNE/2017 Elecciones Generales/primera vuelta.sav", to.data.frame=T)
cand17 <- read.spss("U:/PhD Electoral Fraud/Data/Ecuador/CNE/2017 Elecciones Generales/candidatos 2017.sav", to.data.frame=T)
parties17 <- read.spss("U:/PhD Electoral Fraud/Data/Ecuador/CNE/2017 Elecciones Generales/organizaciones polticas 2017.sav", to.data.frame=T)
segunda17 <- read.spss("U:/PhD Electoral Fraud/Data/Ecuador/CNE/2017 Elecciones Generales/segunda vuelta.sav", to.data.frame=T)
actas17_prim <- read.spss("U:/PhD Electoral Fraud/Data/Ecuador/CNE/2017 Elecciones Generales/RESULTADOS_ACTAS_2017_1V.sav", to.data.frame=T)
actas17_seg <- read.spss("U:/PhD Electoral Fraud/Data/Ecuador/CNE/2017 Elecciones Generales/RESULTADOS_ACTAS_2017_2V.sav", to.data.frame=T)

# isolate 
pres_prim17 <- primera17[primera17$COD_DIGNIDAD=="PRESIDENTE",]
pres_prim17A <- actas17_prim[actas17_prim$DIGNIDAD_NOMBRE=="PRESIDENTE",]
consulta17 <- primera17[primera17$COD_DIGNIDAD=="CONSULTA POPULAR",]
consulta17A <- actas17_prim[actas17_prim$DIGNIDAD_NOMBRE=="CONSULTA POPULAR",]
asam_prov17 <- primera17[primera17$COD_DIGNIDAD=="ASAMBLEÍSTAS PROVINCIALES",]
asam_prov17A <- actas17_prim[actas17_prim$DIGNIDAD_NOMBRE=="ASAMBLEISTAS PROVINCIALES" | actas17_prim$DIGNIDAD_NOMBRE=="ASAMBLEISTAS POR CIRCUNSCRIPCION",]
asam_nac17 <- primera17[primera17$COD_DIGNIDAD=="ASAMBLEÍSTAS NACIONALES",]
asam_nac17A <- actas17_prim[actas17_prim$DIGNIDAD_NOMBRE=="ASAMBLEISTAS NACIONALES",]
andino17 <- primera17[primera17$COD_DIGNIDAD=="PARLAMENTARIOS ANDINOS",]
andino17A <- actas17_prim[actas17_prim$DIGNIDAD_NOMBRE=="PARLAMENTARIOS ANDINOS",]

# reshape those needed
pres_prim17A <- reshape(pres_prim17A, v.names="CANDIDATO_VOTOS", timevar="CANDIDATO_CODIGO",
                        idvar=c("ACTA_CODIGO", "JUNTA_SEXO"), direction="wide")
asam_prov17A <- reshape(asam_prov17A, v.names="CANDIDATO_VOTOS", timevar="CANDIDATO_CODIGO",
                        idvar=c("ACTA_CODIGO", "JUNTA_SEXO"), direction="wide")
asam_nac17A <- reshape(asam_nac17A, v.names="CANDIDATO_VOTOS", timevar="CANDIDATO_CODIGO",
                       idvar=c("ACTA_CODIGO", "JUNTA_SEXO"), direction="wide")
andino17A <- reshape(andino17A, v.names="CANDIDATO_VOTOS", timevar="CANDIDATO_CODIGO",
                     idvar=c("ACTA_CODIGO", "JUNTA_SEXO"), direction="wide")
consulta17A <- reshape(consulta17A, v.names="CANDIDATO_VOTOS", timevar="CANDIDATO_CODIGO",
                       idvar=c("ACTA_CODIGO", "JUNTA_SEXO"), direction="wide")

# save reshapes as copies
pres_prim17Acopy <- pres_prim17A
asam_prov17Acopy <- asam_prov17A
asam_nac17Acopy <- asam_nac17A
andino17Acopy <- andino17A
consulta17Acopy <- consulta17A

# rename 
col_order <- str_c("CANDIDATO_VOTOS.", c(16, 493, 741, 692, 1541, 143, 502, 621))
pres_prim17A[,29:36] <- pres_prim17A[,col_order]
colnames(pres_prim17A)[29:36] <-word(cand17[cand17$DIGNIDAD_CODIGO==1,]$CANDIDATO_NOMBRE, 1)
rownames(pres_prim17A) <- 1:nrow(pres_prim17A)
pres_prim17A$id <- str_c(pres_prim17A$TERRITORIO_CODIGO, pres_prim17A$TERRITORIO_NOMBRE, 
                         pres_prim17A$PROVINCIA_CODIGO, pres_prim17A$PROVINCIA_NOMBRE, 
                         pres_prim17A$CIRCUNSCRIPCION_CODIGO, pres_prim17A$CIRCUNSCRIPCION_NOMBRE, 
                         pres_prim17A$CANTON_CODIGO, pres_prim17A$CANTON_NOMBRE, 
                         pres_prim17A$PARROQUIA_CODIGO, pres_prim17A$PARROQUIA_NOMBRE, pres_prim17A$PARROQUIA_ESTADO, 
                         pres_prim17A$ZONA_NOMBRE, pres_prim17A$RECINTO_CODIGO, pres_prim17A$RECINTO_NOMBRE, 
                         pres_prim17A$JUNTA_CODIGO, pres_prim17A$JUNTA_SEXO)
pres_prim17A <- pres_prim17A[order(pres_prim17A$id),]  

col_order <- str_c("CANDIDATO_VOTOS.", cand17[cand17$DIGNIDAD_NOMBRE=="ASAMBLEÍSTAS PROVINCIALES Y DEL EXTERIOR",]$CANDIDATO_CODIGO)
asam_prov17A[,29:1575] <-  asam_prov17A[,col_order]
colnames(asam_prov17A)[29:1575] <- word(cand17[cand17$DIGNIDAD_NOMBRE=="ASAMBLEÍSTAS PROVINCIALES Y DEL EXTERIOR",]$CANDIDATO_NOMBRE, 1)
rownames(asam_prov17A) <- 1:nrow(asam_prov17A)
asam_prov17A$id <- str_c(asam_prov17A$TERRITORIO_CODIGO, asam_prov17A$TERRITORIO_NOMBRE, 
                         asam_prov17A$PROVINCIA_CODIGO, asam_prov17A$PROVINCIA_NOMBRE, 
                         asam_prov17A$CIRCUNSCRIPCION_CODIGO, asam_prov17A$CIRCUNSCRIPCION_NOMBRE, 
                         asam_prov17A$CANTON_CODIGO, asam_prov17A$CANTON_NOMBRE, 
                         asam_prov17A$PARROQUIA_CODIGO, asam_prov17A$PARROQUIA_NOMBRE, asam_prov17A$PARROQUIA_ESTADO, 
                         asam_prov17A$ZONA_NOMBRE, asam_prov17A$RECINTO_CODIGO, asam_prov17A$RECINTO_NOMBRE, 
                         asam_prov17A$JUNTA_CODIGO, asam_prov17A$JUNTA_SEXO)
asam_prov17A <- asam_prov17A[order(asam_prov17A$id),] 

col_order <- str_c("CANDIDATO_VOTOS.", cand17[cand17$DIGNIDAD_NOMBRE=="ASAMBLEÍSTAS NACIONALES",]$CANDIDATO_CODIGO)
asam_nac17A[,29:253] <-  asam_nac17A[,col_order]
colnames(asam_nac17A)[29:253] <- word(cand17[cand17$DIGNIDAD_NOMBRE=="ASAMBLEÍSTAS NACIONALES",]$CANDIDATO_NOMBRE, 1)
rownames(asam_nac17A) <- 1:nrow(asam_nac17A)
asam_nac17A$id <- str_c(asam_nac17A$TERRITORIO_CODIGO, asam_nac17A$TERRITORIO_NOMBRE, 
                        asam_nac17A$PROVINCIA_CODIGO, asam_nac17A$PROVINCIA_NOMBRE, 
                        asam_nac17A$CIRCUNSCRIPCION_CODIGO, asam_nac17A$CIRCUNSCRIPCION_NOMBRE, 
                        asam_nac17A$CANTON_CODIGO, asam_nac17A$CANTON_NOMBRE, 
                        asam_nac17A$PARROQUIA_CODIGO, asam_nac17A$PARROQUIA_NOMBRE, asam_nac17A$PARROQUIA_ESTADO, 
                        asam_nac17A$ZONA_NOMBRE, asam_nac17A$RECINTO_CODIGO, asam_nac17A$RECINTO_NOMBRE, 
                        asam_nac17A$JUNTA_CODIGO, asam_nac17A$JUNTA_SEXO)
asam_nac17A <- asam_nac17A[order(asam_nac17A$id),] 

col_order <- str_c("CANDIDATO_VOTOS.", cand17[cand17$DIGNIDAD_NOMBRE=="PARLAMENTARIOS ANDINOS",]$CANDIDATO_CODIGO)
andino17A[,29:83] <-  andino17A[,col_order]
colnames(andino17A)[29:83] <- word(cand17[cand17$DIGNIDAD_NOMBRE=="PARLAMENTARIOS ANDINOS",]$CANDIDATO_NOMBRE, 1)
rownames(andino17A) <- 1:nrow(andino17A)
andino17A$id <- str_c(andino17A$TERRITORIO_CODIGO, andino17A$TERRITORIO_NOMBRE, 
                      andino17A$PROVINCIA_CODIGO, andino17A$PROVINCIA_NOMBRE, 
                      andino17A$CIRCUNSCRIPCION_CODIGO, andino17A$CIRCUNSCRIPCION_NOMBRE, 
                      andino17A$CANTON_CODIGO, andino17A$CANTON_NOMBRE, 
                      andino17A$PARROQUIA_CODIGO, andino17A$PARROQUIA_NOMBRE, andino17A$PARROQUIA_ESTADO, 
                      andino17A$ZONA_NOMBRE, andino17A$RECINTO_CODIGO, andino17A$RECINTO_NOMBRE, 
                      andino17A$JUNTA_CODIGO, andino17A$JUNTA_SEXO)
andino17A <- andino17A[order(andino17A$id),] 

colnames(consulta17A)[c(29,30)] <- c("Si", "No")
consulta17A$id <- str_c(consulta17A$TERRITORIO_CODIGO, consulta17A$TERRITORIO_NOMBRE, 
                        consulta17A$PROVINCIA_CODIGO, consulta17A$PROVINCIA_NOMBRE, 
                        consulta17A$CIRCUNSCRIPCION_CODIGO, consulta17A$CIRCUNSCRIPCION_NOMBRE, 
                        consulta17A$CANTON_CODIGO, consulta17A$CANTON_NOMBRE, 
                        consulta17A$PARROQUIA_CODIGO, consulta17A$PARROQUIA_NOMBRE, consulta17A$PARROQUIA_ESTADO, 
                        consulta17A$ZONA_NOMBRE, consulta17A$RECINTO_CODIGO, consulta17A$RECINTO_NOMBRE, 
                        consulta17A$JUNTA_CODIGO, consulta17A$JUNTA_SEXO)
consulta17A <- consulta17A[order(consulta17A$id),] 

# check if any entry is out of order
which(!(pres_prim17A$id == asam_prov17A$id & 
          pres_prim17A$id == asam_nac17A$id & 
          pres_prim17A$id == andino17A$id & 
          pres_prim17A$id == consulta17A$id)
) 

# merge 
colnames(pres_prim17A) <- str_c(colnames(pres_prim17A), "_pres")
colnames(asam_prov17A) <- str_c(colnames(asam_prov17A), "_asam_prov")
colnames(asam_nac17A) <- str_c(colnames(asam_nac17A), "_asam_nac")
colnames(andino17A) <- str_c(colnames(andino17A), "_andino")
colnames(consulta17A) <- str_c(colnames(consulta17A), "_consulta")
actas17 <- cbind(pres_prim17A, asam_prov17A, asam_nac17A, andino17A, consulta17A)

# check if it worked correctly 
which(!(actas17$id_pres == actas17$id_asam_prov & 
          actas17$id_pres == actas17$id_asam_nac &
          actas17$id_pres == actas17$id_andino &
          actas17$id_pres == actas17$id_consulta)
)

which(!(actas17$ELECTORES_REGISTRO_pres == actas17$ELECTORES_REGISTRO_asam_prov & 
          actas17$ELECTORES_REGISTRO_pres == actas17$ELECTORES_REGISTRO_asam_nac &
          actas17$ELECTORES_REGISTRO_pres == actas17$ELECTORES_REGISTRO_andino &
          actas17$ELECTORES_REGISTRO_pres == actas17$ELECTORES_REGISTRO_consulta) 
)

rm(list=setdiff(ls(), "actas17"))
save(actas17, file="actas17.Rdata")

