#' ------------------------------------------------------------------------
#' preparing CNE data, electoral results at level of vote tallies (actas)
#' Local elections 2019
#' Lion Behrens
#' ------------------------------------------------------------------------

library(foreign)
library(stringr)

# load data
secc19 <- read.spss("U:/PhD Electoral Fraud/Data/Ecuador/CNE/2019 Elecciones Seccionales/RESULTADOS_ACTAS_2019.sav", to.data.frame=T)
cand19 <- read.spss("U:/PhD Electoral Fraud/Data/Ecuador/CNE/2019 Elecciones Seccionales/candidatos 2019.sav", to.data.frame=T)

# isolate 
pref19A <- secc19[secc19$DIGNIDAD_NOMBRE=="PREFECTO Y VICEPREFECTO",]
conc_rural19A <- secc19[secc19$DIGNIDAD_NOMBRE=="CONCEJALES RURALES",]
alcal19A <- secc19[secc19$DIGNIDAD_NOMBRE=="ALCALDE",]
conc_urban19A <- secc19[secc19$DIGNIDAD_NOMBRE=="CONCEJALES URBANOS",]
vocales19A <- secc19[secc19$DIGNIDAD_NOMBRE=="VOCALES DE JUNTAS PARROQUIALES",]

# reshape 
pref19A <- reshape(pref19A, v.names="VOTOS", timevar="CANDIDATO_CODIGO",
                   idvar=c("ACTA_CODIGO", "JUNTA_SEXO"), direction="wide")
conc_rural19A <- reshape(conc_rural19A, v.names="VOTOS", timevar="CANDIDATO_CODIGO",
                         idvar=c("ACTA_CODIGO", "JUNTA_SEXO"), direction="wide")
alcal19A <- reshape(alcal19A, v.names="VOTOS", timevar="CANDIDATO_CODIGO",
                    idvar=c("ACTA_CODIGO", "JUNTA_SEXO"), direction="wide")
conc_urban19A <- reshape(conc_urban19A, v.names="VOTOS", timevar="CANDIDATO_CODIGO",
                         idvar=c("ACTA_CODIGO", "JUNTA_SEXO"), direction="wide")
vocales19A <- reshape(vocales19A, v.names="VOTOS", timevar="CANDIDATO_CODIGO",
                      idvar=c("ACTA_CODIGO", "JUNTA_SEXO"), direction="wide")

# save reshapes as copies
pref19Acopy <- pref19A
conc_rural19Acopy <- conc_rural19A
alcal19Acopy <- alcal19A
conc_urban19Acopy <- conc_urban19A
vocales19Acopy <- vocales19A

# rename 
cand19$DIGNIDAD_NOMBRE <- gsub("  ", "", cand19$DIGNIDAD_NOMBRE)
col_order <- str_c("VOTOS.", cand19[cand19$DIGNIDAD_NOMBRE=="PREFECTO Y VICEPREFECTO ",]$CANDIDATO_CODIGO)
pref19A[,25:247] <-  pref19A[,col_order]
colnames(pref19A)[25:247] <- word(cand19[cand19$DIGNIDAD_NOMBRE=="PREFECTO Y VICEPREFECTO ",]$CANDIDATO_NOMBRE, 1)
rownames(pref19A) <- 1:nrow(pref19A)
pref19A$id <- str_c(pref19A$PROVINCIA_CODIGO, pref19A$PROVINCIA_NOMBRE, 
                    pref19A$CANTON_CODIGO, pref19A$CANTON_NOMBRE, 
                    pref19A$CIRCUNSCRIPCION_CODIGO, pref19A$CICUNSCRIPCION_NOMBRE,
                    pref19A$PARROQUIA_CODIGO, pref19A$PARROQUIA_NOMBRE, 
                    pref19A$ZONA_NOMBRE, pref19A$RECINTO_CODIGO, pref19A$RECINTO_NOMBRE, 
                    pref19A$JUNTA_CODIGO, pref19A$JUNTA_SEXO)
pref19A <- pref19A[order(pref19A$id),] 

col_order <- str_c("VOTOS.", cand19[cand19$DIGNIDAD_NOMBRE=="CONCEJALES RURALES",]$CANDIDATO_CODIGO)
conc_rural19A[,25:3712] <-  conc_rural19A[,col_order]
colnames(conc_rural19A)[25:3712] <- word(cand19[cand19$DIGNIDAD_NOMBRE=="CONCEJALES RURALES",]$CANDIDATO_NOMBRE, 1)
rownames(conc_rural19A) <- 1:nrow(conc_rural19A)
conc_rural19A$id <- str_c(conc_rural19A$PROVINCIA_CODIGO, conc_rural19A$PROVINCIA_NOMBRE, 
                          conc_rural19A$CANTON_CODIGO, conc_rural19A$CANTON_NOMBRE, 
                          conc_rural19A$CIRCUNSCRIPCION_CODIGO, conc_rural19A$CICUNSCRIPCION_NOMBRE,
                          conc_rural19A$PARROQUIA_CODIGO, conc_rural19A$PARROQUIA_NOMBRE, 
                          conc_rural19A$ZONA_NOMBRE, conc_rural19A$RECINTO_CODIGO, conc_rural19A$RECINTO_NOMBRE, 
                          conc_rural19A$JUNTA_CODIGO, conc_rural19A$JUNTA_SEXO)
conc_rural19A <- conc_rural19A[order(conc_rural19A$id),] 

col_order <- str_c("VOTOS.", cand19[cand19$DIGNIDAD_NOMBRE=="ALCALDES MUNICIPALES",]$CANDIDATO_CODIGO)
alcal19A[,25:1899] <-  alcal19A[,col_order]
colnames(alcal19A)[25:1899] <- word(cand19[cand19$DIGNIDAD_NOMBRE=="ALCALDES MUNICIPALES",]$CANDIDATO_NOMBRE, 1)
rownames(alcal19A) <- 1:nrow(alcal19A)
alcal19A$id <- str_c(alcal19A$PROVINCIA_CODIGO, alcal19A$PROVINCIA_NOMBRE, 
                     alcal19A$CANTON_CODIGO, alcal19A$CANTON_NOMBRE, 
                     alcal19A$CIRCUNSCRIPCION_CODIGO, alcal19A$CICUNSCRIPCION_NOMBRE,
                     alcal19A$PARROQUIA_CODIGO, alcal19A$PARROQUIA_NOMBRE, 
                     alcal19A$ZONA_NOMBRE, alcal19A$RECINTO_CODIGO, alcal19A$RECINTO_NOMBRE, 
                     alcal19A$JUNTA_CODIGO, alcal19A$JUNTA_SEXO)
alcal19A <- alcal19A[order(alcal19A$id),] 

col_order <- str_c("VOTOS.", cand19[cand19$DIGNIDAD_NOMBRE=="CONCEJALES URBANOS",]$CANDIDATO_CODIGO)
conc_urban19A[,25:8815] <-  conc_urban19A[,col_order]
colnames(conc_urban19A)[25:8815] <- word(cand19[cand19$DIGNIDAD_NOMBRE=="CONCEJALES URBANOS",]$CANDIDATO_NOMBRE, 1)
rownames(conc_urban19A) <- 1:nrow(conc_urban19A)
conc_urban19A$id <- str_c(conc_urban19A$PROVINCIA_CODIGO, conc_urban19A$PROVINCIA_NOMBRE, 
                     conc_urban19A$CANTON_CODIGO, conc_urban19A$CANTON_NOMBRE, 
                     conc_urban19A$CIRCUNSCRIPCION_CODIGO, conc_urban19A$CICUNSCRIPCION_NOMBRE,
                     conc_urban19A$PARROQUIA_CODIGO, conc_urban19A$PARROQUIA_NOMBRE, 
                     conc_urban19A$ZONA_NOMBRE, conc_urban19A$RECINTO_CODIGO, conc_urban19A$RECINTO_NOMBRE, 
                     conc_urban19A$JUNTA_CODIGO, conc_urban19A$JUNTA_SEXO)
conc_urban19A <- conc_urban19A[order(conc_urban19A$id),] 

col_order <- str_c("VOTOS.", cand19[cand19$DIGNIDAD_NOMBRE=="VOCALES DE JUNTAS PARROQUIALES",]$CANDIDATO_CODIGO)
vocales19A[,25:26959] <-  vocales19A[,col_order]
colnames(vocales19A)[25:26959] <- word(cand19[cand19$DIGNIDAD_NOMBRE=="VOCALES DE JUNTAS PARROQUIALES",]$CANDIDATO_NOMBRE, 1)
rownames(vocales19A) <- 1:nrow(vocales19A)
vocales19A$id <- str_c(vocales19A$PROVINCIA_CODIGO, vocales19A$PROVINCIA_NOMBRE, 
                       vocales19A$CANTON_CODIGO, vocales19A$CANTON_NOMBRE, 
                       vocales19A$CIRCUNSCRIPCION_CODIGO, vocales19A$CICUNSCRIPCION_NOMBRE,
                       vocales19A$PARROQUIA_CODIGO, vocales19A$PARROQUIA_NOMBRE, 
                       vocales19A$ZONA_NOMBRE, vocales19A$RECINTO_CODIGO, vocales19A$RECINTO_NOMBRE, 
                       vocales19A$JUNTA_CODIGO, vocales19A$JUNTA_SEXO)
vocales19A <- vocales19A[order(vocales19A$id),] 

# merge 
colnames(pref19A) <- str_c(colnames(pref19A), "_pref")
colnames(conc_rural19A) <- str_c(colnames(conc_rural19A), "_conc_rural")
colnames(alcal19A) <- str_c(colnames(alcal19A), "_alcal")
colnames(conc_urban19A) <- str_c(colnames(conc_urban19A), "_conc_urban")
colnames(vocales19A) <- str_c(colnames(vocales19A), "_vocales")

# not all races took place at the same localities, so selectively merge 




























