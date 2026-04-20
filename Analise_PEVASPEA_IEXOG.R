rm(list = ls())

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

#########################################################################################
RS <- 22   #####  Colocar AQUI a Regional

####  libraries a serem utilizadas  ###

library(patchwork)
library(foreign)
library (dplyr)
library (googlesheets4)
library (ggplot2)
library (httpuv)
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

IEXOGNET2016 <- read.dbf(file = "Base_de_Dados/DBF/IEXOGNET2016.dbf", 
                     as.is = FALSE)

SINAN_IEXOGNET_RS <- IEXOGNET2016 %>% 
  filter(ID_REGIONA == ID_REG | 
           ID_RG_RESI == ID_REG)

assign(paste0("RS", RS, "_2025_SINAN"), 
       SINAN_IEXOGNET_RS) 

#################################################################################################################
###     Construindo um for loop para realizar a tabela de notificados por semana epidemiológica               ###
#################################################################################################################

AUX <- matrix(data = NA, 
              nrow = nrow, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 201601

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
    
    AUX[which(AUX == i), O] <- as.integer(SINAN_IEXOGNET_RS %>%
                                            filter(ID_MN_RESI == i,
                                                   SEM_PRI == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}

AUX[,1] <- BASE_IBGE[which(BASE_IBGE$RS == RS), 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_2016_SE_Notificados"), AUX)

assign("RS_2025_SE_Notificados", AUX)

#######   Pessoa

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Menos_1_ano <- NA

AUX$Um_a_Cinco_Anos <- NA

AUX$Cinco_a_Doze_Anos <- NA

AUX$Doze_a_Dezoito_Anos <- NA

AUX$Dezoito_a_Cinq_Nove <- NA

AUX$Maior_Sessenta <- NA

AUX$Area_Urbana <- NA

AUX$Area_Rural <- NA

AUX$Sexo_Feminino <- NA

AUX$Sexo_Masculino <- NA

AUX$Analfabeto <- NA

AUX$Fundamental_Incompleto <- NA

AUX$Fundamental <- NA

AUX$Ens_Medio_Incompleto <- NA

AUX$Ens_Medio<- NA

AUX$Ens_Superior_Incompleto<- NA

AUX$Ens_Superior<- NA

AUX$Escolaridade_Ignorada<- NA

AUX$Gest_1_Tri <- NA

AUX$Gest_2_Tri <- NA

AUX$Gest_3_Tri <- NA

AUX$Gest_Idade_gest_Ign <- NA

AUX$Gest_Não <- NA

AUX$Gest_N_Aplic <- NA

AUX$Gest_Ign <- NA

AUX$Raca_Branca <- NA

AUX$Raca_Preta <- NA

AUX$Raca_Amarela <- NA

AUX$Raca_Parda <- NA

AUX$Raca_Indigena <- NA

AUX$Raca_Ignorado <- NA

###      For Loop para geração da tabela RS22_Extra       ###

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i,  
                                                          NU_IDADE_N <=3012) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i,  
                                                          NU_IDADE_N > 4000 
                                                          & 
                                                            NU_IDADE_N <=4005) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i,
                                                          NU_IDADE_N > 4005 
                                                          & 
                                                            NU_IDADE_N <=4012) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4012 
                                                          & 
                                                            NU_IDADE_N <=4018) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4018 
                                                          & 
                                                            NU_IDADE_N <= 4059) %>%
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          NU_IDADE_N > 4059 ) %>%
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ZONA == 1) %>% 
                                                    count() 
  )
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                    filter(ID_MN_RESI == i,
                                                           CS_ZONA == 2) %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 13]  <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                     filter(ID_MN_RESI == i, 
                                                            CS_SEXO == "F") %>% 
                                                     count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_SEXO == "M") %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 15]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_ESCOL_N == "00") %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 16] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == "01" 
                                                           | 
                                                             CS_ESCOL_N == "02" 
                                                           | 
                                                             CS_ESCOL_N == "03") %>%
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 17] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == "04") %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 18] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == "05") %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 19] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == "06") %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 20] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == "07") %>% 
                                                    count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 21] <- as.integer(SINAN_IEXOGNET_RS %>% 
                                                    filter(ID_MN_RESI == i, 
                                                           CS_ESCOL_N == "08") %>% 
                                                    count() 
  )
  
  
  AUX[which(AUX$COD_IBGE == i), 22]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_ESCOL_N == "09") %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 23]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_GESTANT == 1) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 24]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_GESTANT == 2) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 25]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_GESTANT == 3) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 26]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_GESTANT == 4) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 27]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_GESTANT == 5) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 28]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_GESTANT == 6) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 29]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_GESTANT == 9) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 30]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_RACA == 1) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 31]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_RACA == 2) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 32]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_RACA == 3) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 33]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_RACA == 4) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 34]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_RACA == 5) %>% 
                                                   count() 
  )
  
  AUX[which(AUX$COD_IBGE == i), 35]<- as.integer(SINAN_IEXOGNET_RS %>% 
                                                   filter(ID_MN_RESI == i, 
                                                          CS_RACA == 9) %>% 
                                                   count() 
  )
}                                             

assign(paste0("RS", RS, "_2016_PESSOA"), AUX)
