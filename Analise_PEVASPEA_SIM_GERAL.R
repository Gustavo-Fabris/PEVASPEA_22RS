rm(list = ls())

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

#########################################################################################
RS <- 22   #####  Colocar AQUI a Regional

####  libraries a serem utilizadas  ###

library(patchwork)
library(foreign)
library (dplyr)
library (ggplot2)
library(stringr)
library(lubridate)
library(ggspatial)
library(sf)
library(tidyr)
library(gt)

####  Importando as bases de dados para formulação do Informe Epidemiológico      ####

BASE_IBGE<-read.table(file="Base_de_Dados/Auxiliares/Planilha_Base_IBGE.csv", 
                      header=TRUE, 
                      sep=",")

BASE_IBGE_BRASIL <- read.csv (file = "Base_de_Dados/Auxiliares/Planilha_Base_IBGE_BRASIL.csv",
                              header = TRUE,
                              sep = ",")

######   Criando objeto ID_REG. Será utilizado para selecionar
######   RS no DBF do SINAN ONLINE.

ID_REG <- as.data.frame(BASE_IBGE[which(BASE_IBGE$RS == RS), 6])

ID_REG <- as.numeric(ID_REG[1,1])

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

##############################################################
##################   2016  ###################################
##############################################################

DOPR2016 <- read.dbf(file = "Base_de_Dados/DBF/DOPR2016.dbf", 
                     as.is = FALSE)

DPR2016$DTNASC <- str_pad(DNPR2016$DTNASC, width = 8, side = "left", pad = "0")

for (i in BASE_IBGE[, 2]){
  DNPR2016[which(DNPR2016$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
  
}

colnames(DNPR2016)[107] <- "Regional"

AUX <- DOPR2016 %>% filter(IDANOMAL == 1,
                           Regional == RS)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2016"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2016.csv"), 
           row.names = FALSE)

AUX <- DNPR2016 %>% filter(Regional == RS)

write.csv (assign(paste0("RS", RS, "_SINASC_2016"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_2016.csv"), 
           row.names = FALSE)


