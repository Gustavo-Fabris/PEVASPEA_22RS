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

DNPR2016 <- read.dbf(file = "Base_de_Dados/DBF/DNPR2016.dbf", 
                     as.is = FALSE)

DNPR2016$DTNASC <- str_pad(DNPR2016$DTNASC, width = 8, side = "left", pad = "0")

for (i in BASE_IBGE[, 2]){
  DNPR2016[which(DNPR2016$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
  
}

colnames(DNPR2016)[107] <- "Regional"

AUX <- DNPR2016 %>% filter(IDANOMAL == 1,
                           Regional == RS)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2016"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2016.csv"), 
           row.names = FALSE)

AUX <- DNPR2016 %>% filter(Regional == RS)

write.csv (assign(paste0("RS", RS, "_SINASC_2016"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_2016.csv"), 
           row.names = FALSE)

####  Criando tabela de nascimentos por SE

AUX <- dmy(DNPR2016$DTNASC)

DNPR2016$SE <- epiweek(AUX)

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2016 %>%
                                            filter(CODMUNRES == i,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}

AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_NASC_SE_2016", AUX)

####  Criando tabela de anomalias por SE

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2016 %>%
                                            filter(CODMUNRES == i,
                                                   IDANOMAL == 1,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_ANOMAL_SE_2016", AUX)

#######

############################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(RS22_SINASC_2016 %>% 
                                                   filter(CODMUNRES == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(RS22_SINASC_2016 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(RS22_SINASC_2016 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q02") |
                                                            str_detect(CODANOMAL, "Q05") |
                                                            str_detect(CODANOMAL, "Q2")  |
                                                            str_detect(CODANOMAL, "Q35") |
                                                            str_detect(CODANOMAL, "Q36") |
                                                            str_detect(CODANOMAL, "Q37") |
                                                            str_detect(CODANOMAL, "Q54") |
                                                            str_detect(CODANOMAL, "Q56") |
                                                            str_detect(CODANOMAL, "Q66") |
                                                            str_detect(CODANOMAL, "Q69") |
                                                            str_detect(CODANOMAL, "Q71") |
                                                            str_detect(CODANOMAL, "Q72") |
                                                            str_detect(CODANOMAL, "Q73") |
                                                            str_detect(CODANOMAL, "Q743") |
                                                            str_detect(CODANOMAL, "Q792") |
                                                            str_detect(CODANOMAL, "Q793") |
                                                            str_detect(CODANOMAL, "Q90") 
                                                          ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(RS22_SINASC_2016 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q05")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(RS22_SINASC_2016 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q02")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(RS22_SINASC_2016 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q2")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(RS22_SINASC_2016 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                            str_detect(CODANOMAL, "Q35") |
                                                            str_detect(CODANOMAL, "Q36") |
                                                            str_detect(CODANOMAL, "Q37") 
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(RS22_SINASC_2016 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                            str_detect(CODANOMAL, "Q54") |
                                                            str_detect(CODANOMAL, "Q56")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(RS22_SINASC_2016 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                            str_detect(CODANOMAL, "Q66") |
                                                            str_detect(CODANOMAL, "Q69") |
                                                            str_detect(CODANOMAL, "Q71") |
                                                            str_detect(CODANOMAL, "Q72") |
                                                            str_detect(CODANOMAL, "Q73") |
                                                            str_detect(CODANOMAL, "Q743")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(RS22_SINASC_2016 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                            str_detect(CODANOMAL, "Q792") |
                                                            str_detect(CODANOMAL, "Q793") 
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(RS22_SINASC_2016 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q90")  
                                                    ) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_2016"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_2016.csv"), 
           row.names = FALSE)

#############################################################################
###   PARANÁ

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
  
    AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2016 %>% 
                                                         filter(CODMUNRES == i) %>%   
                                                         count()
    )  
    
    AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DNPR2016 %>% 
                                                     filter(CODMUNRES == i,
                                                            IDANOMAL == 1) %>%   
                                                     count()
    )
    
    AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2016 %>% 
                                                     filter(CODMUNRES == i,
                                                            IDANOMAL == 1,
                                                            str_detect(CODANOMAL, "Q00") |
                                                              str_detect(CODANOMAL, "Q01") |
                                                              str_detect(CODANOMAL, "Q02") |
                                                              str_detect(CODANOMAL, "Q05") |
                                                              str_detect(CODANOMAL, "Q2")  |
                                                              str_detect(CODANOMAL, "Q35") |
                                                              str_detect(CODANOMAL, "Q36") |
                                                              str_detect(CODANOMAL, "Q37") |
                                                              str_detect(CODANOMAL, "Q54") |
                                                              str_detect(CODANOMAL, "Q56") |
                                                              str_detect(CODANOMAL, "Q66") |
                                                              str_detect(CODANOMAL, "Q69") |
                                                              str_detect(CODANOMAL, "Q71") |
                                                              str_detect(CODANOMAL, "Q72") |
                                                              str_detect(CODANOMAL, "Q73") |
                                                              str_detect(CODANOMAL, "Q743") |
                                                              str_detect(CODANOMAL, "Q792") |
                                                              str_detect(CODANOMAL, "Q793") |
                                                              str_detect(CODANOMAL, "Q90") 
                                                     ) %>%   
                                                     count()
    )
    
    AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DNPR2016 %>% 
                                                     filter(CODMUNRES == i,
                                                            IDANOMAL == 1,
                                                            str_detect(CODANOMAL, "Q00") |
                                                              str_detect(CODANOMAL, "Q01") |
                                                              str_detect(CODANOMAL, "Q05")  
                                                     ) %>%   
                                                     count()
    )
    
    AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DNPR2016 %>% 
                                                     filter(CODMUNRES == i,
                                                            IDANOMAL == 1,
                                                            str_detect(CODANOMAL, "Q02")  
                                                     ) %>%   
                                                     count()
    )
    
    AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DNPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q2")  
                                                      ) %>%   
                                                      count()
    )
    
    AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DNPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q35") |
                                                               str_detect(CODANOMAL, "Q36") |
                                                               str_detect(CODANOMAL, "Q37") 
                                                      ) %>%   
                                                      count()
    )
    
    AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DNPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q54") |
                                                               str_detect(CODANOMAL, "Q56")  
                                                      ) %>%   
                                                      count()
    )
    
    AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DNPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q66") |
                                                               str_detect(CODANOMAL, "Q69") |
                                                               str_detect(CODANOMAL, "Q71") |
                                                               str_detect(CODANOMAL, "Q72") |
                                                               str_detect(CODANOMAL, "Q73") |
                                                               str_detect(CODANOMAL, "Q743")  
                                                      ) %>%   
                                                      count()
    )
    
    AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DNPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q792") |
                                                               str_detect(CODANOMAL, "Q793") 
                                                      ) %>%   
                                                      count()
    )
    
    AUX[which(AUX$Código_IBGE == i), 15] <- as.integer(DNPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q90")  
                                                      ) %>%   
                                                      count()
    )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_2016"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2016.csv"), 
           row.names = FALSE)

##### Criando o objeto com os dados de nascidos mortos anteriores

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$QTDFILMORT_GERAL <- NA

AUX$QTDFILMORT_ANOMALIA <- NA

DNPR2016$QTDFILMORT <- as.numeric(as.character(DNPR2016$QTDFILMORT))

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2016 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )
  AUX[which(AUX$Código_IBGE == i), 6] <-  as.integer(DNPR2016 %>% 
                                                       filter(CODMUNRES == i,
                                                              !is.na(QTDFILMORT),
                                                              QTDFILMORT < 90,
                                                              QTDFILMORT >= 1) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             QTDFILMORT >= 1) %>%   
                                                      count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_QTDFILMORT_2016"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_QTDFILMORT_2016.csv"), 
           row.names = FALSE)

rm(DNPR2016)

##############################################################
##################   2017  ###################################
##############################################################

DNPR2017 <- read.dbf(file = "Base_de_Dados/DBF/DNPR2017.dbf", 
                     as.is = FALSE)

DNPR2017$DTNASC <- str_pad(DNPR2017$DTNASC, width = 8, side = "left", pad = "0")

for (i in BASE_IBGE[, 2]){
  DNPR2017[which(DNPR2017$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
  
}

colnames(DNPR2017)[107] <- "Regional"

AUX <- DNPR2017 %>% filter(IDANOMAL == 1,
                           Regional == RS)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2017"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2017.csv"), 
           row.names = FALSE)

AUX <- DNPR2017 %>% filter(Regional == RS)

write.csv (assign(paste0("RS", RS, "_SINASC_2017"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_2017.csv"), 
           row.names = FALSE)

####  Criando tabela de nascimentos por SE

AUX <- dmy(DNPR2017$DTNASC)

DNPR2017$SE <- epiweek(AUX)

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2017 %>%
                                            filter(CODMUNRES == i,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_NASC_SE_2017", AUX)

####  Criando tabela de anomalias por SE

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2017 %>%
                                            filter(CODMUNRES == i,
                                                   IDANOMAL == 1,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_ANOMAL_SE_2017", AUX)

############################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(RS22_SINASC_2017 %>% 
                                                   filter(CODMUNRES == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(RS22_SINASC_2017 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(RS22_SINASC_2017 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q02") |
                                                            str_detect(CODANOMAL, "Q05") |
                                                            str_detect(CODANOMAL, "Q2")  |
                                                            str_detect(CODANOMAL, "Q35") |
                                                            str_detect(CODANOMAL, "Q36") |
                                                            str_detect(CODANOMAL, "Q37") |
                                                            str_detect(CODANOMAL, "Q54") |
                                                            str_detect(CODANOMAL, "Q56") |
                                                            str_detect(CODANOMAL, "Q66") |
                                                            str_detect(CODANOMAL, "Q69") |
                                                            str_detect(CODANOMAL, "Q71") |
                                                            str_detect(CODANOMAL, "Q72") |
                                                            str_detect(CODANOMAL, "Q73") |
                                                            str_detect(CODANOMAL, "Q743") |
                                                            str_detect(CODANOMAL, "Q792") |
                                                            str_detect(CODANOMAL, "Q793") |
                                                            str_detect(CODANOMAL, "Q90") 
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(RS22_SINASC_2017 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q05")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(RS22_SINASC_2017 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q02")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(RS22_SINASC_2017 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q2")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(RS22_SINASC_2017 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q35") |
                                                             str_detect(CODANOMAL, "Q36") |
                                                             str_detect(CODANOMAL, "Q37") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(RS22_SINASC_2017 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q54") |
                                                             str_detect(CODANOMAL, "Q56")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(RS22_SINASC_2017 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q66") |
                                                             str_detect(CODANOMAL, "Q69") |
                                                             str_detect(CODANOMAL, "Q71") |
                                                             str_detect(CODANOMAL, "Q72") |
                                                             str_detect(CODANOMAL, "Q73") |
                                                             str_detect(CODANOMAL, "Q743")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(RS22_SINASC_2017 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q792") |
                                                             str_detect(CODANOMAL, "Q793") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(RS22_SINASC_2017 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q90")  
                                                    ) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_2017"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_2017.csv"), 
           row.names = FALSE)

#############################################################################
###   PARANÁ

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2017 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DNPR2017 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2017 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q02") |
                                                               str_detect(CODANOMAL, "Q05") |
                                                               str_detect(CODANOMAL, "Q2")  |
                                                               str_detect(CODANOMAL, "Q35") |
                                                               str_detect(CODANOMAL, "Q36") |
                                                               str_detect(CODANOMAL, "Q37") |
                                                               str_detect(CODANOMAL, "Q54") |
                                                               str_detect(CODANOMAL, "Q56") |
                                                               str_detect(CODANOMAL, "Q66") |
                                                               str_detect(CODANOMAL, "Q69") |
                                                               str_detect(CODANOMAL, "Q71") |
                                                               str_detect(CODANOMAL, "Q72") |
                                                               str_detect(CODANOMAL, "Q73") |
                                                               str_detect(CODANOMAL, "Q743") |
                                                               str_detect(CODANOMAL, "Q792") |
                                                               str_detect(CODANOMAL, "Q793") |
                                                               str_detect(CODANOMAL, "Q90") 
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DNPR2017 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q05")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DNPR2017 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q02")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DNPR2017 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q2")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DNPR2017 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q35") |
                                                                str_detect(CODANOMAL, "Q36") |
                                                                str_detect(CODANOMAL, "Q37") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DNPR2017 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q54") |
                                                                str_detect(CODANOMAL, "Q56")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DNPR2017 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q66") |
                                                                str_detect(CODANOMAL, "Q69") |
                                                                str_detect(CODANOMAL, "Q71") |
                                                                str_detect(CODANOMAL, "Q72") |
                                                                str_detect(CODANOMAL, "Q73") |
                                                                str_detect(CODANOMAL, "Q743")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DNPR2017 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q792") |
                                                                str_detect(CODANOMAL, "Q793") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 15] <- as.integer(DNPR2017 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q90")  
                                                       ) %>%   
                                                       count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_2017"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2017.csv"), 
           row.names = FALSE)

##### Criando o objeto com os dados de nascidos mortos anteriores

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$QTDFILMORT_GERAL <- NA

AUX$QTDFILMORT_ANOMALIA <- NA

DNPR2017$QTDFILMORT <- as.numeric(as.character(DNPR2017$QTDFILMORT))

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2017 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )
  AUX[which(AUX$Código_IBGE == i), 6] <-  as.integer(DNPR2017 %>% 
                                                       filter(CODMUNRES == i,
                                                              !is.na(QTDFILMORT),
                                                              QTDFILMORT < 90,
                                                              QTDFILMORT >= 1) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2017 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             QTDFILMORT >= 1) %>%   
                                                      count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_QTDFILMORT_2017"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_QTDFILMORT_2017.csv"), 
           row.names = FALSE)

rm(DNPR2017) 

##############################################################
##################   2018  ###################################
##############################################################

DNPR2018 <- read.dbf(file = "Base_de_Dados/DBF/DNPR2018.dbf", 
                     as.is = FALSE)

DNPR2018$DTNASC <- str_pad(DNPR2018$DTNASC, width = 8, side = "left", pad = "0")

for (i in BASE_IBGE[, 2]){
  DNPR2018[which(DNPR2018$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
  
}

colnames(DNPR2018)[107] <- "Regional"

AUX <- DNPR2018 %>% filter(IDANOMAL == 1,
                           Regional == RS)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2018"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2018.csv"), 
           row.names = FALSE)

AUX <- DNPR2018 %>% filter(Regional == RS)

write.csv (assign(paste0("RS", RS, "_SINASC_2018"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_2018.csv"), 
           row.names = FALSE)

####  Criando tabela de nascimentos por SE

AUX <- dmy(DNPR2018$DTNASC)

DNPR2018$SE <- epiweek(AUX)

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2018 %>%
                                            filter(CODMUNRES == i,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_NASC_SE_2018", AUX)

####  Criando tabela de anomalias por SE

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2018 %>%
                                            filter(CODMUNRES == i,
                                                   IDANOMAL == 1,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_ANOMAL_SE_2018", AUX)


############################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(RS22_SINASC_2018 %>% 
                                                   filter(CODMUNRES == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(RS22_SINASC_2018 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(RS22_SINASC_2018 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q02") |
                                                            str_detect(CODANOMAL, "Q05") |
                                                            str_detect(CODANOMAL, "Q2")  |
                                                            str_detect(CODANOMAL, "Q35") |
                                                            str_detect(CODANOMAL, "Q36") |
                                                            str_detect(CODANOMAL, "Q37") |
                                                            str_detect(CODANOMAL, "Q54") |
                                                            str_detect(CODANOMAL, "Q56") |
                                                            str_detect(CODANOMAL, "Q66") |
                                                            str_detect(CODANOMAL, "Q69") |
                                                            str_detect(CODANOMAL, "Q71") |
                                                            str_detect(CODANOMAL, "Q72") |
                                                            str_detect(CODANOMAL, "Q73") |
                                                            str_detect(CODANOMAL, "Q743") |
                                                            str_detect(CODANOMAL, "Q792") |
                                                            str_detect(CODANOMAL, "Q793") |
                                                            str_detect(CODANOMAL, "Q90") 
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(RS22_SINASC_2018 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q05")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(RS22_SINASC_2018 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q02")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(RS22_SINASC_2018 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q2")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(RS22_SINASC_2018 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q35") |
                                                             str_detect(CODANOMAL, "Q36") |
                                                             str_detect(CODANOMAL, "Q37") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(RS22_SINASC_2018 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q54") |
                                                             str_detect(CODANOMAL, "Q56")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(RS22_SINASC_2018 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q66") |
                                                             str_detect(CODANOMAL, "Q69") |
                                                             str_detect(CODANOMAL, "Q71") |
                                                             str_detect(CODANOMAL, "Q72") |
                                                             str_detect(CODANOMAL, "Q73") |
                                                             str_detect(CODANOMAL, "Q743")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(RS22_SINASC_2018 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q792") |
                                                             str_detect(CODANOMAL, "Q793") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(RS22_SINASC_2018 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q90")  
                                                    ) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_2018"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_2018.csv"), 
           row.names = FALSE)

#############################################################################
###   PARANÁ

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2018 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DNPR2018 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2018 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q02") |
                                                               str_detect(CODANOMAL, "Q05") |
                                                               str_detect(CODANOMAL, "Q2")  |
                                                               str_detect(CODANOMAL, "Q35") |
                                                               str_detect(CODANOMAL, "Q36") |
                                                               str_detect(CODANOMAL, "Q37") |
                                                               str_detect(CODANOMAL, "Q54") |
                                                               str_detect(CODANOMAL, "Q56") |
                                                               str_detect(CODANOMAL, "Q66") |
                                                               str_detect(CODANOMAL, "Q69") |
                                                               str_detect(CODANOMAL, "Q71") |
                                                               str_detect(CODANOMAL, "Q72") |
                                                               str_detect(CODANOMAL, "Q73") |
                                                               str_detect(CODANOMAL, "Q743") |
                                                               str_detect(CODANOMAL, "Q792") |
                                                               str_detect(CODANOMAL, "Q793") |
                                                               str_detect(CODANOMAL, "Q90") 
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DNPR2018 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q05")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DNPR2018 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q02")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DNPR2018 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q2")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DNPR2018 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q35") |
                                                                str_detect(CODANOMAL, "Q36") |
                                                                str_detect(CODANOMAL, "Q37") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DNPR2018 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q54") |
                                                                str_detect(CODANOMAL, "Q56")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DNPR2018 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q66") |
                                                                str_detect(CODANOMAL, "Q69") |
                                                                str_detect(CODANOMAL, "Q71") |
                                                                str_detect(CODANOMAL, "Q72") |
                                                                str_detect(CODANOMAL, "Q73") |
                                                                str_detect(CODANOMAL, "Q743")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DNPR2018 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q792") |
                                                                str_detect(CODANOMAL, "Q793") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 15] <- as.integer(DNPR2018 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q90")  
                                                       ) %>%   
                                                       count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_2018"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2018.csv"), 
           row.names = FALSE)

##### Criando o objeto com os dados de nascidos mortos anteriores

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$QTDFILMORT_GERAL <- NA

AUX$QTDFILMORT_ANOMALIA <- NA

DNPR2018$QTDFILMORT <- as.numeric(as.character(DNPR2018$QTDFILMORT))

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2018 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )
  AUX[which(AUX$Código_IBGE == i), 6] <-  as.integer(DNPR2018 %>% 
                                                       filter(CODMUNRES == i,
                                                              !is.na(QTDFILMORT),
                                                              QTDFILMORT < 90,
                                                              QTDFILMORT >= 1) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2018 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             QTDFILMORT >= 1) %>%   
                                                      count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_QTDFILMORT_2018"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_QTDFILMORT_2018.csv"), 
           row.names = FALSE)

rm(DNPR2018)

##############################################################
##################   2019  ###################################
##############################################################

DNPR2019 <- read.dbf(file = "Base_de_Dados/DBF/DNPR2019.dbf", 
                     as.is = FALSE)

DNPR2019$DTNASC <- str_pad(DNPR2019$DTNASC, width = 8, 
                           side = "left", 
                           pad = "0")

for (i in BASE_IBGE[, 2]){
  DNPR2019[which(DNPR2019$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
  
}

colnames(DNPR2019)[107] <- "Regional"

AUX <- DNPR2019 %>% filter(IDANOMAL == 1,
                           Regional == RS)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2019"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2019.csv"), 
           row.names = FALSE)

AUX <- DNPR2019 %>% filter(Regional == RS)

write.csv (assign(paste0("RS", RS, "_SINASC_2019"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_2019.csv"), 
           row.names = FALSE)

####  Criando tabela de nascimentos por SE

AUX <- dmy(DNPR2019$DTNASC)

DNPR2019$SE <- epiweek(AUX)

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2019 %>%
                                            filter(CODMUNRES == i,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_NASC_SE_2019", AUX)

####  Criando tabela de anomalias por SE

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2019 %>%
                                            filter(CODMUNRES == i,
                                                   IDANOMAL == 1,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_ANOMAL_SE_2019", AUX)

############################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(RS22_SINASC_2019 %>% 
                                                   filter(CODMUNRES == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(RS22_SINASC_2019 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(RS22_SINASC_2019 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q02") |
                                                            str_detect(CODANOMAL, "Q05") |
                                                            str_detect(CODANOMAL, "Q2")  |
                                                            str_detect(CODANOMAL, "Q35") |
                                                            str_detect(CODANOMAL, "Q36") |
                                                            str_detect(CODANOMAL, "Q37") |
                                                            str_detect(CODANOMAL, "Q54") |
                                                            str_detect(CODANOMAL, "Q56") |
                                                            str_detect(CODANOMAL, "Q66") |
                                                            str_detect(CODANOMAL, "Q69") |
                                                            str_detect(CODANOMAL, "Q71") |
                                                            str_detect(CODANOMAL, "Q72") |
                                                            str_detect(CODANOMAL, "Q73") |
                                                            str_detect(CODANOMAL, "Q743") |
                                                            str_detect(CODANOMAL, "Q792") |
                                                            str_detect(CODANOMAL, "Q793") |
                                                            str_detect(CODANOMAL, "Q90") 
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(RS22_SINASC_2019 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q05")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(RS22_SINASC_2019 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q02")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(RS22_SINASC_2019 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q2")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(RS22_SINASC_2019 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q35") |
                                                             str_detect(CODANOMAL, "Q36") |
                                                             str_detect(CODANOMAL, "Q37") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(RS22_SINASC_2019 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q54") |
                                                             str_detect(CODANOMAL, "Q56")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(RS22_SINASC_2019 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q66") |
                                                             str_detect(CODANOMAL, "Q69") |
                                                             str_detect(CODANOMAL, "Q71") |
                                                             str_detect(CODANOMAL, "Q72") |
                                                             str_detect(CODANOMAL, "Q73") |
                                                             str_detect(CODANOMAL, "Q743")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(RS22_SINASC_2019 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q792") |
                                                             str_detect(CODANOMAL, "Q793") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(RS22_SINASC_2019 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q90")  
                                                    ) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_2019"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_2019.csv"), 
           row.names = FALSE)

#############################################################################
###   PARANÁ

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2019 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DNPR2019 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2019 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q02") |
                                                               str_detect(CODANOMAL, "Q05") |
                                                               str_detect(CODANOMAL, "Q2")  |
                                                               str_detect(CODANOMAL, "Q35") |
                                                               str_detect(CODANOMAL, "Q36") |
                                                               str_detect(CODANOMAL, "Q37") |
                                                               str_detect(CODANOMAL, "Q54") |
                                                               str_detect(CODANOMAL, "Q56") |
                                                               str_detect(CODANOMAL, "Q66") |
                                                               str_detect(CODANOMAL, "Q69") |
                                                               str_detect(CODANOMAL, "Q71") |
                                                               str_detect(CODANOMAL, "Q72") |
                                                               str_detect(CODANOMAL, "Q73") |
                                                               str_detect(CODANOMAL, "Q743") |
                                                               str_detect(CODANOMAL, "Q792") |
                                                               str_detect(CODANOMAL, "Q793") |
                                                               str_detect(CODANOMAL, "Q90") 
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DNPR2019 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q05")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DNPR2019 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q02")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DNPR2019 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q2")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DNPR2019 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q35") |
                                                                str_detect(CODANOMAL, "Q36") |
                                                                str_detect(CODANOMAL, "Q37") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DNPR2019 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q54") |
                                                                str_detect(CODANOMAL, "Q56")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DNPR2019 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q66") |
                                                                str_detect(CODANOMAL, "Q69") |
                                                                str_detect(CODANOMAL, "Q71") |
                                                                str_detect(CODANOMAL, "Q72") |
                                                                str_detect(CODANOMAL, "Q73") |
                                                                str_detect(CODANOMAL, "Q743")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DNPR2019 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q792") |
                                                                str_detect(CODANOMAL, "Q793") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 15] <- as.integer(DNPR2019 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q90")  
                                                       ) %>%   
                                                       count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_2019"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2019.csv"), 
           row.names = FALSE)

##### Criando o objeto com os dados de nascidos mortos anteriores

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$QTDFILMORT_GERAL <- NA

AUX$QTDFILMORT_ANOMALIA <- NA

DNPR2019$QTDFILMORT <- as.numeric(as.character(DNPR2019$QTDFILMORT))

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2019 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )
  AUX[which(AUX$Código_IBGE == i), 6] <-  as.integer(DNPR2019 %>% 
                                                       filter(CODMUNRES == i,
                                                              !is.na(QTDFILMORT),
                                                              QTDFILMORT < 90,
                                                              QTDFILMORT >= 1) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2019 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             QTDFILMORT >= 1) %>%   
                                                      count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_QTDFILMORT_2019"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_QTDFILMORT_2019.csv"), 
           row.names = FALSE)

rm(DNPR2019)

##############################################################
##################   2020  ###################################
##############################################################

DNPR2020 <- read.dbf(file = "Base_de_Dados/DBF/DNPR2020.dbf", 
                     as.is = FALSE)

DNPR2020$DTNASC <- str_pad(DNPR2020$DTNASC, width = 8, side = "left", pad = "0")

for (i in BASE_IBGE[, 2]){
  DNPR2020[which(DNPR2020$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
  
}

colnames(DNPR2020)[107] <- "Regional"

AUX <- DNPR2020 %>% filter(IDANOMAL == 1,
                           Regional == RS)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2020"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2020.csv"), 
           row.names = FALSE)

AUX <- DNPR2020 %>% filter(Regional == RS)

write.csv (assign(paste0("RS", RS, "_SINASC_2020"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_2020.csv"), 
           row.names = FALSE)

####  Criando tabela de nascimentos por SE

AUX <- dmy(DNPR2020$DTNASC)

DNPR2020$SE <- epiweek(AUX)

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2020 %>%
                                            filter(CODMUNRES == i,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_NASC_SE_2020", AUX)

####  Criando tabela de anomalias por SE

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2020 %>%
                                            filter(CODMUNRES == i,
                                                   IDANOMAL == 1,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_ANOMAL_SE_2020", AUX)

############################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(RS22_SINASC_2020 %>% 
                                                   filter(CODMUNRES == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(RS22_SINASC_2020 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(RS22_SINASC_2020 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q02") |
                                                            str_detect(CODANOMAL, "Q05") |
                                                            str_detect(CODANOMAL, "Q2")  |
                                                            str_detect(CODANOMAL, "Q35") |
                                                            str_detect(CODANOMAL, "Q36") |
                                                            str_detect(CODANOMAL, "Q37") |
                                                            str_detect(CODANOMAL, "Q54") |
                                                            str_detect(CODANOMAL, "Q56") |
                                                            str_detect(CODANOMAL, "Q66") |
                                                            str_detect(CODANOMAL, "Q69") |
                                                            str_detect(CODANOMAL, "Q71") |
                                                            str_detect(CODANOMAL, "Q72") |
                                                            str_detect(CODANOMAL, "Q73") |
                                                            str_detect(CODANOMAL, "Q743") |
                                                            str_detect(CODANOMAL, "Q792") |
                                                            str_detect(CODANOMAL, "Q793") |
                                                            str_detect(CODANOMAL, "Q90") 
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(RS22_SINASC_2020 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q05")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(RS22_SINASC_2020 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q02")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(RS22_SINASC_2020 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q2")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(RS22_SINASC_2020 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q35") |
                                                             str_detect(CODANOMAL, "Q36") |
                                                             str_detect(CODANOMAL, "Q37") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(RS22_SINASC_2020 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q54") |
                                                             str_detect(CODANOMAL, "Q56")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(RS22_SINASC_2020 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q66") |
                                                             str_detect(CODANOMAL, "Q69") |
                                                             str_detect(CODANOMAL, "Q71") |
                                                             str_detect(CODANOMAL, "Q72") |
                                                             str_detect(CODANOMAL, "Q73") |
                                                             str_detect(CODANOMAL, "Q743")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(RS22_SINASC_2020 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q792") |
                                                             str_detect(CODANOMAL, "Q793") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(RS22_SINASC_2020 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q90")  
                                                    ) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_2020"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_2020.csv"), 
           row.names = FALSE)

#############################################################################
###   PARANÁ

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2020 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DNPR2020 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2020 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q02") |
                                                               str_detect(CODANOMAL, "Q05") |
                                                               str_detect(CODANOMAL, "Q2")  |
                                                               str_detect(CODANOMAL, "Q35") |
                                                               str_detect(CODANOMAL, "Q36") |
                                                               str_detect(CODANOMAL, "Q37") |
                                                               str_detect(CODANOMAL, "Q54") |
                                                               str_detect(CODANOMAL, "Q56") |
                                                               str_detect(CODANOMAL, "Q66") |
                                                               str_detect(CODANOMAL, "Q69") |
                                                               str_detect(CODANOMAL, "Q71") |
                                                               str_detect(CODANOMAL, "Q72") |
                                                               str_detect(CODANOMAL, "Q73") |
                                                               str_detect(CODANOMAL, "Q743") |
                                                               str_detect(CODANOMAL, "Q792") |
                                                               str_detect(CODANOMAL, "Q793") |
                                                               str_detect(CODANOMAL, "Q90") 
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DNPR2020 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q05")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DNPR2020 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q02")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DNPR2020 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q2")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DNPR2020 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q35") |
                                                                str_detect(CODANOMAL, "Q36") |
                                                                str_detect(CODANOMAL, "Q37") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DNPR2020 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q54") |
                                                                str_detect(CODANOMAL, "Q56")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DNPR2020 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q66") |
                                                                str_detect(CODANOMAL, "Q69") |
                                                                str_detect(CODANOMAL, "Q71") |
                                                                str_detect(CODANOMAL, "Q72") |
                                                                str_detect(CODANOMAL, "Q73") |
                                                                str_detect(CODANOMAL, "Q743")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DNPR2020 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q792") |
                                                                str_detect(CODANOMAL, "Q793") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 15] <- as.integer(DNPR2020 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q90")  
                                                       ) %>%   
                                                       count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_2020"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2020.csv"), 
           row.names = FALSE)

##### Criando o objeto com os dados de nascidos mortos anteriores

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$QTDFILMORT_GERAL <- NA

AUX$QTDFILMORT_ANOMALIA <- NA

DNPR2020$QTDFILMORT <- as.numeric(as.character(DNPR2020$QTDFILMORT))

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2020 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )
  AUX[which(AUX$Código_IBGE == i), 6] <-  as.integer(DNPR2020 %>% 
                                                       filter(CODMUNRES == i,
                                                              !is.na(QTDFILMORT),
                                                              QTDFILMORT < 90,
                                                              QTDFILMORT >= 1) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2020 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             QTDFILMORT >= 1) %>%   
                                                      count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_QTDFILMORT_2020"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_QTDFILMORT_2020.csv"), 
           row.names = FALSE)

rm(DNPR2020)

##############################################################
##################   2021  ###################################
##############################################################

DNPR2021 <- read.dbf(file = "Base_de_Dados/DBF/DNPR2021.dbf", 
                     as.is = FALSE)

DNPR2021$DTNASC <- str_pad(DNPR2021$DTNASC, width = 8, side = "left", pad = "0")

for (i in BASE_IBGE[, 2]){
  DNPR2021[which(DNPR2021$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
  
}

colnames(DNPR2021)[107] <- "Regional"

AUX <- DNPR2021 %>% filter(IDANOMAL == 1,
                           Regional == RS)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2021"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2021.csv"), 
           row.names = FALSE)

AUX <- DNPR2021 %>% filter(Regional == RS)

write.csv (assign(paste0("RS", RS, "_SINASC_2021"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_2021.csv"), 
           row.names = FALSE)

####  Criando tabela de nascimentos por SE

AUX <- dmy(DNPR2021$DTNASC)

DNPR2021$SE <- epiweek(AUX)

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2021 %>%
                                            filter(CODMUNRES == i,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_NASC_SE_2021", AUX)

####  Criando tabela de anomalias por SE

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2021 %>%
                                            filter(CODMUNRES == i,
                                                   IDANOMAL == 1,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_ANOMAL_SE_2021", AUX)

############################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(RS22_SINASC_2021 %>% 
                                                   filter(CODMUNRES == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(RS22_SINASC_2021 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(RS22_SINASC_2021 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q02") |
                                                            str_detect(CODANOMAL, "Q05") |
                                                            str_detect(CODANOMAL, "Q2")  |
                                                            str_detect(CODANOMAL, "Q35") |
                                                            str_detect(CODANOMAL, "Q36") |
                                                            str_detect(CODANOMAL, "Q37") |
                                                            str_detect(CODANOMAL, "Q54") |
                                                            str_detect(CODANOMAL, "Q56") |
                                                            str_detect(CODANOMAL, "Q66") |
                                                            str_detect(CODANOMAL, "Q69") |
                                                            str_detect(CODANOMAL, "Q71") |
                                                            str_detect(CODANOMAL, "Q72") |
                                                            str_detect(CODANOMAL, "Q73") |
                                                            str_detect(CODANOMAL, "Q743") |
                                                            str_detect(CODANOMAL, "Q792") |
                                                            str_detect(CODANOMAL, "Q793") |
                                                            str_detect(CODANOMAL, "Q90") 
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(RS22_SINASC_2021 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q05")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(RS22_SINASC_2021 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q02")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(RS22_SINASC_2021 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q2")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(RS22_SINASC_2021 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q35") |
                                                             str_detect(CODANOMAL, "Q36") |
                                                             str_detect(CODANOMAL, "Q37") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(RS22_SINASC_2021 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q54") |
                                                             str_detect(CODANOMAL, "Q56")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(RS22_SINASC_2021 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q66") |
                                                             str_detect(CODANOMAL, "Q69") |
                                                             str_detect(CODANOMAL, "Q71") |
                                                             str_detect(CODANOMAL, "Q72") |
                                                             str_detect(CODANOMAL, "Q73") |
                                                             str_detect(CODANOMAL, "Q743")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(RS22_SINASC_2021 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q792") |
                                                             str_detect(CODANOMAL, "Q793") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(RS22_SINASC_2021 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q90")  
                                                    ) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_2021"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_2021.csv"), 
           row.names = FALSE)

#############################################################################
###   PARANÁ

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2021 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DNPR2021 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2021 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q02") |
                                                               str_detect(CODANOMAL, "Q05") |
                                                               str_detect(CODANOMAL, "Q2")  |
                                                               str_detect(CODANOMAL, "Q35") |
                                                               str_detect(CODANOMAL, "Q36") |
                                                               str_detect(CODANOMAL, "Q37") |
                                                               str_detect(CODANOMAL, "Q54") |
                                                               str_detect(CODANOMAL, "Q56") |
                                                               str_detect(CODANOMAL, "Q66") |
                                                               str_detect(CODANOMAL, "Q69") |
                                                               str_detect(CODANOMAL, "Q71") |
                                                               str_detect(CODANOMAL, "Q72") |
                                                               str_detect(CODANOMAL, "Q73") |
                                                               str_detect(CODANOMAL, "Q743") |
                                                               str_detect(CODANOMAL, "Q792") |
                                                               str_detect(CODANOMAL, "Q793") |
                                                               str_detect(CODANOMAL, "Q90") 
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DNPR2021 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q05")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DNPR2021 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q02")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DNPR2021 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q2")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DNPR2021 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q35") |
                                                                str_detect(CODANOMAL, "Q36") |
                                                                str_detect(CODANOMAL, "Q37") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DNPR2021 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q54") |
                                                                str_detect(CODANOMAL, "Q56")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DNPR2021 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q66") |
                                                                str_detect(CODANOMAL, "Q69") |
                                                                str_detect(CODANOMAL, "Q71") |
                                                                str_detect(CODANOMAL, "Q72") |
                                                                str_detect(CODANOMAL, "Q73") |
                                                                str_detect(CODANOMAL, "Q743")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DNPR2021 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q792") |
                                                                str_detect(CODANOMAL, "Q793") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 15] <- as.integer(DNPR2021 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q90")  
                                                       ) %>%   
                                                       count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_2021"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2021.csv"), 
           row.names = FALSE)

##### Criando o objeto com os dados de nascidos mortos anteriores

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$QTDFILMORT_GERAL <- NA

AUX$QTDFILMORT_ANOMALIA <- NA

DNPR2021$QTDFILMORT <- as.numeric(as.character(DNPR2021$QTDFILMORT))

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2021 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )
  AUX[which(AUX$Código_IBGE == i), 6] <-  as.integer(DNPR2021 %>% 
                                                       filter(CODMUNRES == i,
                                                              !is.na(QTDFILMORT),
                                                              QTDFILMORT < 90,
                                                              QTDFILMORT >= 1) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2021 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             QTDFILMORT >= 1) %>%   
                                                      count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_QTDFILMORT_2021"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_QTDFILMORT_2021.csv"), 
           row.names = FALSE)

rm(DNPR2021)

##############################################################
##################   2022  ###################################
##############################################################

DNPR2022 <- read.dbf(file = "Base_de_Dados/DBF/DNPR2022.dbf", 
                     as.is = FALSE)

DNPR2022$DTNASC <- str_pad(DNPR2022$DTNASC, width = 8, side = "left", pad = "0")

for (i in BASE_IBGE[, 2]){
  DNPR2022[which(DNPR2022$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
  
}

colnames(DNPR2022)[107] <- "Regional"

AUX <- DNPR2022 %>% filter(IDANOMAL == 1,
                           Regional == RS)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2022"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2022.csv"), 
           row.names = FALSE)

AUX <- DNPR2022 %>% filter(Regional == RS)

write.csv (assign(paste0("RS", RS, "_SINASC_2022"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_2022.csv"), 
           row.names = FALSE)

####  Criando tabela de nascimentos por SE

AUX <- dmy(DNPR2022$DTNASC)

DNPR2022$SE <- epiweek(AUX)

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2022 %>%
                                            filter(CODMUNRES == i,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_NASC_SE_2022", AUX)

####  Criando tabela de anomalias por SE

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2022 %>%
                                            filter(CODMUNRES == i,
                                                   IDANOMAL == 1,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_ANOMAL_SE_2022", AUX)

############################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(RS22_SINASC_2022 %>% 
                                                   filter(CODMUNRES == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(RS22_SINASC_2022 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(RS22_SINASC_2022 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q02") |
                                                            str_detect(CODANOMAL, "Q05") |
                                                            str_detect(CODANOMAL, "Q2")  |
                                                            str_detect(CODANOMAL, "Q35") |
                                                            str_detect(CODANOMAL, "Q36") |
                                                            str_detect(CODANOMAL, "Q37") |
                                                            str_detect(CODANOMAL, "Q54") |
                                                            str_detect(CODANOMAL, "Q56") |
                                                            str_detect(CODANOMAL, "Q66") |
                                                            str_detect(CODANOMAL, "Q69") |
                                                            str_detect(CODANOMAL, "Q71") |
                                                            str_detect(CODANOMAL, "Q72") |
                                                            str_detect(CODANOMAL, "Q73") |
                                                            str_detect(CODANOMAL, "Q743") |
                                                            str_detect(CODANOMAL, "Q792") |
                                                            str_detect(CODANOMAL, "Q793") |
                                                            str_detect(CODANOMAL, "Q90") 
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(RS22_SINASC_2022 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q05")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(RS22_SINASC_2022 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q02")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(RS22_SINASC_2022 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q2")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(RS22_SINASC_2022 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q35") |
                                                             str_detect(CODANOMAL, "Q36") |
                                                             str_detect(CODANOMAL, "Q37") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(RS22_SINASC_2022 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q54") |
                                                             str_detect(CODANOMAL, "Q56")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(RS22_SINASC_2022 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q66") |
                                                             str_detect(CODANOMAL, "Q69") |
                                                             str_detect(CODANOMAL, "Q71") |
                                                             str_detect(CODANOMAL, "Q72") |
                                                             str_detect(CODANOMAL, "Q73") |
                                                             str_detect(CODANOMAL, "Q743")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(RS22_SINASC_2022 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q792") |
                                                             str_detect(CODANOMAL, "Q793") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(RS22_SINASC_2022 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q90")  
                                                    ) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_2022"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_2022.csv"), 
           row.names = FALSE)

#############################################################################
###   PARANÁ

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2022 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DNPR2022 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2022 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q02") |
                                                               str_detect(CODANOMAL, "Q05") |
                                                               str_detect(CODANOMAL, "Q2")  |
                                                               str_detect(CODANOMAL, "Q35") |
                                                               str_detect(CODANOMAL, "Q36") |
                                                               str_detect(CODANOMAL, "Q37") |
                                                               str_detect(CODANOMAL, "Q54") |
                                                               str_detect(CODANOMAL, "Q56") |
                                                               str_detect(CODANOMAL, "Q66") |
                                                               str_detect(CODANOMAL, "Q69") |
                                                               str_detect(CODANOMAL, "Q71") |
                                                               str_detect(CODANOMAL, "Q72") |
                                                               str_detect(CODANOMAL, "Q73") |
                                                               str_detect(CODANOMAL, "Q743") |
                                                               str_detect(CODANOMAL, "Q792") |
                                                               str_detect(CODANOMAL, "Q793") |
                                                               str_detect(CODANOMAL, "Q90") 
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DNPR2022 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q05")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DNPR2022 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q02")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DNPR2022 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q2")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DNPR2022 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q35") |
                                                                str_detect(CODANOMAL, "Q36") |
                                                                str_detect(CODANOMAL, "Q37") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DNPR2022 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q54") |
                                                                str_detect(CODANOMAL, "Q56")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DNPR2022 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q66") |
                                                                str_detect(CODANOMAL, "Q69") |
                                                                str_detect(CODANOMAL, "Q71") |
                                                                str_detect(CODANOMAL, "Q72") |
                                                                str_detect(CODANOMAL, "Q73") |
                                                                str_detect(CODANOMAL, "Q743")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DNPR2022 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q792") |
                                                                str_detect(CODANOMAL, "Q793") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 15] <- as.integer(DNPR2022 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q90")  
                                                       ) %>%   
                                                       count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_2022"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2022.csv"), 
           row.names = FALSE)

##### Criando o objeto com os dados de nascidos mortos anteriores

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$QTDFILMORT_GERAL <- NA

AUX$QTDFILMORT_ANOMALIA <- NA

DNPR2022$QTDFILMORT <- as.numeric(as.character(DNPR2022$QTDFILMORT))

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2022 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )
  AUX[which(AUX$Código_IBGE == i), 6] <-  as.integer(DNPR2022 %>% 
                                                       filter(CODMUNRES == i,
                                                              !is.na(QTDFILMORT),
                                                              QTDFILMORT < 90,
                                                              QTDFILMORT >= 1) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2022 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             QTDFILMORT >= 1) %>%   
                                                      count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_QTDFILMORT_2022"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_QTDFILMORT_2022.csv"), 
           row.names = FALSE)

rm(DNPR2022)

##############################################################
##################   2023  ###################################
##############################################################

DNPR2023 <- read.dbf(file = "Base_de_Dados/DBF/DNPR2023.dbf", 
                     as.is = FALSE)

DNPR2023$DTNASC <- str_pad(DNPR2023$DTNASC, width = 8, side = "left", pad = "0")

for (i in BASE_IBGE[, 2]){
  DNPR2023[which(DNPR2023$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
  
}

colnames(DNPR2023)[107] <- "Regional"

AUX <- DNPR2023 %>% filter(IDANOMAL == 1,
                           Regional == RS)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2023"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2023.csv"), 
           row.names = FALSE)

AUX <- DNPR2023 %>% filter(Regional == RS)

write.csv (assign(paste0("RS", RS, "_SINASC_2023"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_2023.csv"), 
           row.names = FALSE)

####  Criando tabela de nascimentos por SE

AUX <- dmy(DNPR2023$DTNASC)

DNPR2023$SE <- epiweek(AUX)

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2023 %>%
                                            filter(CODMUNRES == i,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_NASC_SE_2023", AUX)

####  Criando tabela de anomalias por SE

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2023 %>%
                                            filter(CODMUNRES == i,
                                                   IDANOMAL == 1,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_ANOMAL_SE_2023", AUX)

############################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(RS22_SINASC_2023 %>% 
                                                   filter(CODMUNRES == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(RS22_SINASC_2023 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(RS22_SINASC_2023 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q02") |
                                                            str_detect(CODANOMAL, "Q05") |
                                                            str_detect(CODANOMAL, "Q2")  |
                                                            str_detect(CODANOMAL, "Q35") |
                                                            str_detect(CODANOMAL, "Q36") |
                                                            str_detect(CODANOMAL, "Q37") |
                                                            str_detect(CODANOMAL, "Q54") |
                                                            str_detect(CODANOMAL, "Q56") |
                                                            str_detect(CODANOMAL, "Q66") |
                                                            str_detect(CODANOMAL, "Q69") |
                                                            str_detect(CODANOMAL, "Q71") |
                                                            str_detect(CODANOMAL, "Q72") |
                                                            str_detect(CODANOMAL, "Q73") |
                                                            str_detect(CODANOMAL, "Q743") |
                                                            str_detect(CODANOMAL, "Q792") |
                                                            str_detect(CODANOMAL, "Q793") |
                                                            str_detect(CODANOMAL, "Q90") 
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(RS22_SINASC_2023 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q05")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(RS22_SINASC_2023 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q02")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(RS22_SINASC_2023 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q2")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(RS22_SINASC_2023 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q35") |
                                                             str_detect(CODANOMAL, "Q36") |
                                                             str_detect(CODANOMAL, "Q37") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(RS22_SINASC_2023 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q54") |
                                                             str_detect(CODANOMAL, "Q56")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(RS22_SINASC_2023 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q66") |
                                                             str_detect(CODANOMAL, "Q69") |
                                                             str_detect(CODANOMAL, "Q71") |
                                                             str_detect(CODANOMAL, "Q72") |
                                                             str_detect(CODANOMAL, "Q73") |
                                                             str_detect(CODANOMAL, "Q743")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(RS22_SINASC_2023 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q792") |
                                                             str_detect(CODANOMAL, "Q793") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(RS22_SINASC_2023 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q90")  
                                                    ) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_2023"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_2023.csv"), 
           row.names = FALSE)

#############################################################################
###   PARANÁ

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2023 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DNPR2023 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2023 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q02") |
                                                               str_detect(CODANOMAL, "Q05") |
                                                               str_detect(CODANOMAL, "Q2")  |
                                                               str_detect(CODANOMAL, "Q35") |
                                                               str_detect(CODANOMAL, "Q36") |
                                                               str_detect(CODANOMAL, "Q37") |
                                                               str_detect(CODANOMAL, "Q54") |
                                                               str_detect(CODANOMAL, "Q56") |
                                                               str_detect(CODANOMAL, "Q66") |
                                                               str_detect(CODANOMAL, "Q69") |
                                                               str_detect(CODANOMAL, "Q71") |
                                                               str_detect(CODANOMAL, "Q72") |
                                                               str_detect(CODANOMAL, "Q73") |
                                                               str_detect(CODANOMAL, "Q743") |
                                                               str_detect(CODANOMAL, "Q792") |
                                                               str_detect(CODANOMAL, "Q793") |
                                                               str_detect(CODANOMAL, "Q90") 
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DNPR2023 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q05")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DNPR2023 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q02")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DNPR2023 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q2")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DNPR2023 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q35") |
                                                                str_detect(CODANOMAL, "Q36") |
                                                                str_detect(CODANOMAL, "Q37") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DNPR2023 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q54") |
                                                                str_detect(CODANOMAL, "Q56")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DNPR2023 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q66") |
                                                                str_detect(CODANOMAL, "Q69") |
                                                                str_detect(CODANOMAL, "Q71") |
                                                                str_detect(CODANOMAL, "Q72") |
                                                                str_detect(CODANOMAL, "Q73") |
                                                                str_detect(CODANOMAL, "Q743")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DNPR2023 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q792") |
                                                                str_detect(CODANOMAL, "Q793") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 15] <- as.integer(DNPR2023 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q90")  
                                                       ) %>%   
                                                       count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_2023"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2023.csv"), 
           row.names = FALSE)

##### Criando o objeto com os dados de nascidos mortos anteriores

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$QTDFILMORT_GERAL <- NA

AUX$QTDFILMORT_ANOMALIA <- NA

DNPR2023$QTDFILMORT <- as.numeric(as.character(DNPR2023$QTDFILMORT))

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2023 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )
  AUX[which(AUX$Código_IBGE == i), 6] <-  as.integer(DNPR2023 %>% 
                                                       filter(CODMUNRES == i,
                                                              !is.na(QTDFILMORT),
                                                              QTDFILMORT < 90,
                                                              QTDFILMORT >= 1) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2023 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             QTDFILMORT >= 1) %>%   
                                                      count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_QTDFILMORT_2023"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_QTDFILMORT_2023.csv"), 
           row.names = FALSE)

rm(DNPR2023)

##############################################################
##################   2024  ###################################
##############################################################

DNPR2024 <- read.dbf(file = "Base_de_Dados/DBF/DNPR2024.dbf", 
                     as.is = FALSE)

DNPR2024$DTNASC <- str_pad(DNPR2024$DTNASC, width = 8, side = "left", pad = "0")

for (i in BASE_IBGE[, 2]){
  DNPR2024[which(DNPR2024$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
  
}

colnames(DNPR2024)[107] <- "Regional"

AUX <- DNPR2024 %>% filter(IDANOMAL == 1,
                           Regional == RS)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2024"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2024.csv"), 
           row.names = FALSE)

AUX <- DNPR2024 %>% filter(Regional == RS)

write.csv (assign(paste0("RS", RS, "_SINASC_2024"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_2024.csv"), 
           row.names = FALSE)

####  Criando tabela de nascimentos por SE

AUX <- dmy(DNPR2024$DTNASC)

DNPR2024$SE <- epiweek(AUX)

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2024 %>%
                                            filter(CODMUNRES == i,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_NASC_SE_2024", AUX)

####  Criando tabela de anomalias por SE

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2024 %>%
                                            filter(CODMUNRES == i,
                                                   IDANOMAL == 1,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_ANOMAL_SE_2024", AUX)

############################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(RS22_SINASC_2024 %>% 
                                                   filter(CODMUNRES == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(RS22_SINASC_2024 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(RS22_SINASC_2024 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q02") |
                                                            str_detect(CODANOMAL, "Q05") |
                                                            str_detect(CODANOMAL, "Q2")  |
                                                            str_detect(CODANOMAL, "Q35") |
                                                            str_detect(CODANOMAL, "Q36") |
                                                            str_detect(CODANOMAL, "Q37") |
                                                            str_detect(CODANOMAL, "Q54") |
                                                            str_detect(CODANOMAL, "Q56") |
                                                            str_detect(CODANOMAL, "Q66") |
                                                            str_detect(CODANOMAL, "Q69") |
                                                            str_detect(CODANOMAL, "Q71") |
                                                            str_detect(CODANOMAL, "Q72") |
                                                            str_detect(CODANOMAL, "Q73") |
                                                            str_detect(CODANOMAL, "Q743") |
                                                            str_detect(CODANOMAL, "Q792") |
                                                            str_detect(CODANOMAL, "Q793") |
                                                            str_detect(CODANOMAL, "Q90") 
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(RS22_SINASC_2024 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q05")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(RS22_SINASC_2024 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q02")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(RS22_SINASC_2024 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q2")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(RS22_SINASC_2024 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q35") |
                                                             str_detect(CODANOMAL, "Q36") |
                                                             str_detect(CODANOMAL, "Q37") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(RS22_SINASC_2024 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q54") |
                                                             str_detect(CODANOMAL, "Q56")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(RS22_SINASC_2024 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q66") |
                                                             str_detect(CODANOMAL, "Q69") |
                                                             str_detect(CODANOMAL, "Q71") |
                                                             str_detect(CODANOMAL, "Q72") |
                                                             str_detect(CODANOMAL, "Q73") |
                                                             str_detect(CODANOMAL, "Q743")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(RS22_SINASC_2024 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q792") |
                                                             str_detect(CODANOMAL, "Q793") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(RS22_SINASC_2024 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q90")  
                                                    ) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_2024"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_2024.csv"), 
           row.names = FALSE)

#############################################################################
###   PARANÁ

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2024 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DNPR2024 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2024 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q02") |
                                                               str_detect(CODANOMAL, "Q05") |
                                                               str_detect(CODANOMAL, "Q2")  |
                                                               str_detect(CODANOMAL, "Q35") |
                                                               str_detect(CODANOMAL, "Q36") |
                                                               str_detect(CODANOMAL, "Q37") |
                                                               str_detect(CODANOMAL, "Q54") |
                                                               str_detect(CODANOMAL, "Q56") |
                                                               str_detect(CODANOMAL, "Q66") |
                                                               str_detect(CODANOMAL, "Q69") |
                                                               str_detect(CODANOMAL, "Q71") |
                                                               str_detect(CODANOMAL, "Q72") |
                                                               str_detect(CODANOMAL, "Q73") |
                                                               str_detect(CODANOMAL, "Q743") |
                                                               str_detect(CODANOMAL, "Q792") |
                                                               str_detect(CODANOMAL, "Q793") |
                                                               str_detect(CODANOMAL, "Q90") 
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DNPR2024 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q05")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DNPR2024 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q02")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DNPR2024 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q2")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DNPR2024 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q35") |
                                                                str_detect(CODANOMAL, "Q36") |
                                                                str_detect(CODANOMAL, "Q37") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DNPR2024 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q54") |
                                                                str_detect(CODANOMAL, "Q56")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DNPR2024 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q66") |
                                                                str_detect(CODANOMAL, "Q69") |
                                                                str_detect(CODANOMAL, "Q71") |
                                                                str_detect(CODANOMAL, "Q72") |
                                                                str_detect(CODANOMAL, "Q73") |
                                                                str_detect(CODANOMAL, "Q743")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DNPR2024 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q792") |
                                                                str_detect(CODANOMAL, "Q793") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 15] <- as.integer(DNPR2024 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q90")  
                                                       ) %>%   
                                                       count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_2024"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2024.csv"), 
           row.names = FALSE)

##### Criando o objeto com os dados de nascidos mortos anteriores

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$QTDFILMORT_GERAL <- NA

AUX$QTDFILMORT_ANOMALIA <- NA

DNPR2024$QTDFILMORT <- as.numeric(as.character(DNPR2024$QTDFILMORT))

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2024 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )
  AUX[which(AUX$Código_IBGE == i), 6] <-  as.integer(DNPR2024 %>% 
                                                       filter(CODMUNRES == i,
                                                              !is.na(QTDFILMORT),
                                                              QTDFILMORT < 90,
                                                              QTDFILMORT >= 1) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2024 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             QTDFILMORT >= 1) %>%   
                                                      count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_QTDFILMORT_2024"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_QTDFILMORT_2024.csv"), 
           row.names = FALSE)

rm(DNPR2024)

##############################################################
##################   2025  ###################################
##############################################################

DNPR2025 <- read.dbf(file = "Base_de_Dados/DBF/DNPR2025.dbf", 
                     as.is = FALSE)

DNPR2025$DTNASC <- str_pad(DNPR2025$DTNASC, width = 8, side = "left", pad = "0")

for (i in BASE_IBGE[, 2]){
  DNPR2025[which(DNPR2025$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
  
}

colnames(DNPR2025)[107] <- "Regional"

AUX <- DNPR2025 %>% filter(IDANOMAL == 1,
                           Regional == RS)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2025"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2025.csv"), 
           row.names = FALSE)

AUX <- DNPR2025 %>% filter(Regional == RS)

write.csv (assign(paste0("RS", RS, "_SINASC_2025"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_2025.csv"), 
           row.names = FALSE)

####  Criando tabela de nascimentos por SE

AUX <- dmy(DNPR2025$DTNASC)

DNPR2025$SE <- epiweek(AUX)

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2025 %>%
                                            filter(CODMUNRES == i,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_NASC_SE_2025", AUX)

####  Criando tabela de anomalias por SE

AUX <- matrix(data = NA, 
              nrow = 399, 
              ncol = 54)

AUX <- as.data.frame(AUX)

colnames(AUX)[1] <- "Município" 

AUX[,1] <- BASE_IBGE[, 2]

colnames (AUX)[2:54] <- c(1:53)

N <- 1

O <- 2

for (j in 1:53){
  for (i in BASE_IBGE[, 2]){
    
    AUX[which(AUX == i), O] <- as.integer(DNPR2025 %>%
                                            filter(CODMUNRES == i,
                                                   IDANOMAL == 1,
                                                   SE == N)%>%
                                            count()
                                          
    )
  }
  N <- N +1
  O <- O +1
}


AUX[,1] <- BASE_IBGE[, 3]

AUX[(nrow(AUX)+ 1),2:54] <- apply(AUX[,2:54], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SINASC_ANOMAL_SE_2025", AUX)

############################################################################

AUX <- data.frame(Município = BASE_IBGE[which(BASE_IBGE$RS == RS), 3])

AUX$COD_IBGE <- BASE_IBGE[which(BASE_IBGE$RS == RS), 2]

AUX$Populacao <- BASE_IBGE[which(BASE_IBGE$RS == RS), 5]

AUX$RS <- BASE_IBGE[which(BASE_IBGE$RS == RS), 1]

AUX <- AUX[,c(4, 1, 2, 3)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[(which(BASE_IBGE$RS == RS)), 2]){
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(RS22_SINASC_2025 %>% 
                                                   filter(CODMUNRES == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(RS22_SINASC_2025 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(RS22_SINASC_2025 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q02") |
                                                            str_detect(CODANOMAL, "Q05") |
                                                            str_detect(CODANOMAL, "Q2")  |
                                                            str_detect(CODANOMAL, "Q35") |
                                                            str_detect(CODANOMAL, "Q36") |
                                                            str_detect(CODANOMAL, "Q37") |
                                                            str_detect(CODANOMAL, "Q54") |
                                                            str_detect(CODANOMAL, "Q56") |
                                                            str_detect(CODANOMAL, "Q66") |
                                                            str_detect(CODANOMAL, "Q69") |
                                                            str_detect(CODANOMAL, "Q71") |
                                                            str_detect(CODANOMAL, "Q72") |
                                                            str_detect(CODANOMAL, "Q73") |
                                                            str_detect(CODANOMAL, "Q743") |
                                                            str_detect(CODANOMAL, "Q792") |
                                                            str_detect(CODANOMAL, "Q793") |
                                                            str_detect(CODANOMAL, "Q90") 
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(RS22_SINASC_2025 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q05")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(RS22_SINASC_2025 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q02")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(RS22_SINASC_2025 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q2")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(RS22_SINASC_2025 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q35") |
                                                             str_detect(CODANOMAL, "Q36") |
                                                             str_detect(CODANOMAL, "Q37") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(RS22_SINASC_2025 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q54") |
                                                             str_detect(CODANOMAL, "Q56")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(RS22_SINASC_2025 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q66") |
                                                             str_detect(CODANOMAL, "Q69") |
                                                             str_detect(CODANOMAL, "Q71") |
                                                             str_detect(CODANOMAL, "Q72") |
                                                             str_detect(CODANOMAL, "Q73") |
                                                             str_detect(CODANOMAL, "Q743")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(RS22_SINASC_2025 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q792") |
                                                             str_detect(CODANOMAL, "Q793") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(RS22_SINASC_2025 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q90")  
                                                    ) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_2025"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_2025.csv"), 
           row.names = FALSE)

#############################################################################
###   PARANÁ

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$Anomalia_Detectada <- NA

AUX$Anomalia_Prioritaria_Vig_Nasc <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2025 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DNPR2025 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2025 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q02") |
                                                               str_detect(CODANOMAL, "Q05") |
                                                               str_detect(CODANOMAL, "Q2")  |
                                                               str_detect(CODANOMAL, "Q35") |
                                                               str_detect(CODANOMAL, "Q36") |
                                                               str_detect(CODANOMAL, "Q37") |
                                                               str_detect(CODANOMAL, "Q54") |
                                                               str_detect(CODANOMAL, "Q56") |
                                                               str_detect(CODANOMAL, "Q66") |
                                                               str_detect(CODANOMAL, "Q69") |
                                                               str_detect(CODANOMAL, "Q71") |
                                                               str_detect(CODANOMAL, "Q72") |
                                                               str_detect(CODANOMAL, "Q73") |
                                                               str_detect(CODANOMAL, "Q743") |
                                                               str_detect(CODANOMAL, "Q792") |
                                                               str_detect(CODANOMAL, "Q793") |
                                                               str_detect(CODANOMAL, "Q90") 
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DNPR2025 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q05")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DNPR2025 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q02")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DNPR2025 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q2")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DNPR2025 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q35") |
                                                                str_detect(CODANOMAL, "Q36") |
                                                                str_detect(CODANOMAL, "Q37") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DNPR2025 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q54") |
                                                                str_detect(CODANOMAL, "Q56")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DNPR2025 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q66") |
                                                                str_detect(CODANOMAL, "Q69") |
                                                                str_detect(CODANOMAL, "Q71") |
                                                                str_detect(CODANOMAL, "Q72") |
                                                                str_detect(CODANOMAL, "Q73") |
                                                                str_detect(CODANOMAL, "Q743")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DNPR2025 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q792") |
                                                                str_detect(CODANOMAL, "Q793") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 15] <- as.integer(DNPR2025 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q90")  
                                                       ) %>%   
                                                       count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_2025"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2025.csv"), 
           row.names = FALSE)

##### Criando o objeto com os dados de nascidos mortos anteriores

AUX <- BASE_IBGE[,-c(4,6)]

AUX$Nascidos <- NA

AUX$QTDFILMORT_GERAL <- NA

AUX$QTDFILMORT_ANOMALIA <- NA

DNPR2025$QTDFILMORT <- as.numeric(as.character(DNPR2025$QTDFILMORT))

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2025 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )
  AUX[which(AUX$Código_IBGE == i), 6] <-  as.integer(DNPR2025 %>% 
                                                       filter(CODMUNRES == i,
                                                              !is.na(QTDFILMORT),
                                                              QTDFILMORT < 90,
                                                              QTDFILMORT >= 1) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2025 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             QTDFILMORT >= 1) %>%   
                                                      count()
  )
}

AUX[(nrow(AUX) +1), 4:7] <- apply(AUX[, 4:7], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

write.csv (assign(paste0("PR_PEVASPEA_SINASC_QTDFILMORT_2025"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_QTDFILMORT_2025.csv"), 
           row.names = FALSE)

rm(DNPR2025)

################################################################################
######  Séries Históricas

### Paraná

AUX <- PR_PEVASPEA_SINASC_2016[400, ]

AUX[1, 1] <- "2016"

AUX[nrow(AUX) + 1, ] <- PR_PEVASPEA_SINASC_2017[400, ]

AUX[2, 1] <- "2017"

AUX[nrow(AUX) + 1, ] <- PR_PEVASPEA_SINASC_2018[400, ]

AUX[3, 1] <- "2018"

AUX[nrow(AUX) + 1, ] <- PR_PEVASPEA_SINASC_2019[400, ]

AUX[4, 1] <- "2019"

AUX[nrow(AUX) + 1, ] <- PR_PEVASPEA_SINASC_2020[400, ]

AUX[5, 1] <- "2020"

AUX[nrow(AUX) + 1, ] <- PR_PEVASPEA_SINASC_2021[400, ]

AUX[6, 1] <- "2021"

AUX[nrow(AUX) + 1, ] <- PR_PEVASPEA_SINASC_2022[400, ]

AUX[7, 1] <- "2022"

AUX[nrow(AUX) + 1, ] <- PR_PEVASPEA_SINASC_2023[400, ]

AUX[8, 1] <- "2023"

AUX[nrow(AUX) + 1, ] <- PR_PEVASPEA_SINASC_2024[400, ]

AUX[9, 1] <- "2024"

AUX[nrow(AUX) + 1, ] <- PR_PEVASPEA_SINASC_2025[400, ]

AUX[10, 1] <- "2025"
  
AUX <- AUX[, -c(2, 3)]

write.csv (assign(paste0("PR_PEVASPEA_SINASC_Serie_Historica"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_Serie_Historica.csv"), 
           row.names = FALSE)

### Regional

AUX <- RS22_PEVASPEA_SINASC_2016[nrow(RS22_PEVASPEA_SINASC_2016), ]

AUX[1, 1] <- "2016"

AUX[nrow(AUX) + 1, ] <- RS22_PEVASPEA_SINASC_2017[nrow(RS22_PEVASPEA_SINASC_2016), ]

AUX[2, 1] <- "2017"

AUX[nrow(AUX) + 1, ] <- RS22_PEVASPEA_SINASC_2018[nrow(RS22_PEVASPEA_SINASC_2016), ]

AUX[3, 1] <- "2018"

AUX[nrow(AUX) + 1, ] <- RS22_PEVASPEA_SINASC_2019[nrow(RS22_PEVASPEA_SINASC_2016), ]

AUX[4, 1] <- "2019"

AUX[nrow(AUX) + 1, ] <- RS22_PEVASPEA_SINASC_2020[nrow(RS22_PEVASPEA_SINASC_2016), ]

AUX[5, 1] <- "2020"

AUX[nrow(AUX) + 1, ] <- RS22_PEVASPEA_SINASC_2021[nrow(RS22_PEVASPEA_SINASC_2016), ]

AUX[6, 1] <- "2021"

AUX[nrow(AUX) + 1, ] <- RS22_PEVASPEA_SINASC_2022[nrow(RS22_PEVASPEA_SINASC_2016), ]

AUX[7, 1] <- "2022"

AUX[nrow(AUX) + 1, ] <- RS22_PEVASPEA_SINASC_2023[nrow(RS22_PEVASPEA_SINASC_2016), ]

AUX[8, 1] <- "2023"

AUX[nrow(AUX) + 1, ] <- RS22_PEVASPEA_SINASC_2024[nrow(RS22_PEVASPEA_SINASC_2016), ]

AUX[9, 1] <- "2024"

AUX[nrow(AUX) + 1, ] <- RS22_PEVASPEA_SINASC_2025[nrow(RS22_PEVASPEA_SINASC_2016), ]

AUX[10, 1] <- "2025"

AUX <- AUX[, -c(2, 3)]

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_Serie_Historica"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_Serie_Historica.csv"), 
           row.names = FALSE)

######  Rbind Fichas

AUX <- rbind(RS22_ANOMALIAS_SINASC_2016,
             RS22_ANOMALIAS_SINASC_2017,
             RS22_ANOMALIAS_SINASC_2018,
             RS22_ANOMALIAS_SINASC_2019,
             RS22_ANOMALIAS_SINASC_2020,
             RS22_ANOMALIAS_SINASC_2021,
             RS22_ANOMALIAS_SINASC_2022,
             RS22_ANOMALIAS_SINASC_2023,
             RS22_ANOMALIAS_SINASC_2024,
             RS22_ANOMALIAS_SINASC_2025)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_CONSOLIDADO"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_CONSOLIDADO.csv"), 
           row.names = FALSE)

AUX <- rbind(RS22_SINASC_2016,
             RS22_SINASC_2017,
             RS22_SINASC_2018,
             RS22_SINASC_2019,
             RS22_SINASC_2020,
             RS22_SINASC_2021,
             RS22_SINASC_2022,
             RS22_SINASC_2023,
             RS22_SINASC_2024,
             RS22_SINASC_2025)

AUX01 <- data.frame(COD = AUX[,33], 
                  Municipio = NA)

for (i in AUX[,33]){
  AUX01[which(AUX01$COD == i), 2] <- BASE_IBGE_BRASIL[which(BASE_IBGE_BRASIL$Código.Município.Completo == i),13]
  
}

AUX[,33] <- AUX01[, 2]

write.csv (assign(paste0("RS", RS, "_SINASC_CONSOLIDADO"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_SINASC_CONSOLIDADO.csv"), 
           row.names = FALSE)

AUX <- data.frame(RS22_PEVASPEA_SINASC_2025[, c(1:4)],
                  "Nascidos_2016" = RS22_PEVASPEA_SINASC_2016[, 5], 
                  "N_2016" = RS22_PEVASPEA_SINASC_2016[, 6], 
                  'Taxa_2016' = (RS22_PEVASPEA_SINASC_2016[, 6]/RS22_PEVASPEA_SINASC_2016[, 5]) *1000,
                  "Nascidos_2017" = RS22_PEVASPEA_SINASC_2017[, 5],
                  "N_2017" = RS22_PEVASPEA_SINASC_2017[, 6],
                  "Taxa_2017" = (RS22_PEVASPEA_SINASC_2017[, 6]/RS22_PEVASPEA_SINASC_2017[, 5]) *1000,
                  "Nascidos_2018" = RS22_PEVASPEA_SINASC_2018[, 5],
                  "N_2018" = RS22_PEVASPEA_SINASC_2018[, 6],
                  "Taxa_2018" = (RS22_PEVASPEA_SINASC_2018[, 6]/RS22_PEVASPEA_SINASC_2018[, 5]) *1000,
                  "Nascidos_2019" = RS22_PEVASPEA_SINASC_2019[, 5],
                  "N_2019" = RS22_PEVASPEA_SINASC_2019[, 6],
                  "Taxa_2019" = (RS22_PEVASPEA_SINASC_2019[, 6]/RS22_PEVASPEA_SINASC_2019[, 5]) *1000,
                  "Nascidos_2020" = RS22_PEVASPEA_SINASC_2020[, 5],
                  "N_2020" = RS22_PEVASPEA_SINASC_2020[, 6],
                  "Taxa_2020" = (RS22_PEVASPEA_SINASC_2020[, 6]/RS22_PEVASPEA_SINASC_2020[, 5]) *1000,
                  "Nascidos_2021" = RS22_PEVASPEA_SINASC_2021[, 5],
                  "N_2021" = RS22_PEVASPEA_SINASC_2021[, 6],
                  "Taxa_2021" = (RS22_PEVASPEA_SINASC_2021[, 6]/RS22_PEVASPEA_SINASC_2021[, 5]) *1000,
                  "Nascidos_2022" = RS22_PEVASPEA_SINASC_2022[, 5],
                  "N_2022" = RS22_PEVASPEA_SINASC_2022[, 6],
                  'Taxa_2022' = (RS22_PEVASPEA_SINASC_2022[, 6]/RS22_PEVASPEA_SINASC_2022[, 5]) *1000,
                  "Nascidos_2023" = RS22_PEVASPEA_SINASC_2023[, 5],
                  "N_2023" = RS22_PEVASPEA_SINASC_2023[, 6],
                  'Taxa_2023' = (RS22_PEVASPEA_SINASC_2023[, 6]/RS22_PEVASPEA_SINASC_2023[, 5]) *1000,
                  "Nascidos_2024" = RS22_PEVASPEA_SINASC_2024[, 5],
                  "N_2024" = RS22_PEVASPEA_SINASC_2024[, 6],
                  'Taxa_2024' = (RS22_PEVASPEA_SINASC_2024[, 6]/RS22_PEVASPEA_SINASC_2024[, 5]) *1000,
                  "Nascidos_2025" = RS22_PEVASPEA_SINASC_2025[, 5],
                  "N_2025" = RS22_PEVASPEA_SINASC_2025[, 6],
                  'Taxa_2025' = (RS22_PEVASPEA_SINASC_2025[, 6]/RS22_PEVASPEA_SINASC_2025[, 5]) *1000)

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_Serie_historica_Mun"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_Serie_historica_Mun.csv"), 
           row.names = FALSE)

AUX <- data.frame(PR_PEVASPEA_SINASC_2025[, c(1:4)],
                  "Nascidos_2016" = PR_PEVASPEA_SINASC_2016[, 5], 
                  "N_2016" = PR_PEVASPEA_SINASC_2016[, 6], 
                  'Taxa_2016' = (PR_PEVASPEA_SINASC_2016[, 6]/PR_PEVASPEA_SINASC_2016[, 5]) *1000,
                  "Nascidos_2017" = PR_PEVASPEA_SINASC_2017[, 5],
                  "N_2017" = PR_PEVASPEA_SINASC_2017[, 6],
                  "Taxa_2017" = (PR_PEVASPEA_SINASC_2017[, 6]/PR_PEVASPEA_SINASC_2017[, 5]) *1000,
                  "Nascidos_2018" = PR_PEVASPEA_SINASC_2018[, 5],
                  "N_2018" = PR_PEVASPEA_SINASC_2018[, 6],
                  "Taxa_2018" = (PR_PEVASPEA_SINASC_2018[, 6]/PR_PEVASPEA_SINASC_2018[, 5]) *1000,
                  "Nascidos_2019" = PR_PEVASPEA_SINASC_2019[, 5],
                  "N_2019" = PR_PEVASPEA_SINASC_2019[, 6],
                  "Taxa_2019" = (PR_PEVASPEA_SINASC_2019[, 6]/PR_PEVASPEA_SINASC_2019[, 5]) *1000,
                  "Nascidos_2020" = PR_PEVASPEA_SINASC_2020[, 5],
                  "N_2020" = PR_PEVASPEA_SINASC_2020[, 6],
                  "Taxa_2020" = (PR_PEVASPEA_SINASC_2020[, 6]/PR_PEVASPEA_SINASC_2020[, 5]) *1000,
                  "Nascidos_2021" = PR_PEVASPEA_SINASC_2021[, 5],
                  "N_2021" = PR_PEVASPEA_SINASC_2021[, 6],
                  "Taxa_2021" = (PR_PEVASPEA_SINASC_2021[, 6]/PR_PEVASPEA_SINASC_2021[, 5]) *1000,
                  "Nascidos_2022" = PR_PEVASPEA_SINASC_2022[, 5],
                  "N_2022" = PR_PEVASPEA_SINASC_2022[, 6],
                  'Taxa_2022' = (PR_PEVASPEA_SINASC_2022[, 6]/PR_PEVASPEA_SINASC_2022[, 5]) *1000,
                  "Nascidos_2023" = PR_PEVASPEA_SINASC_2023[, 5],
                  "N_2023" = PR_PEVASPEA_SINASC_2023[, 6],
                  'Taxa_2023' = (PR_PEVASPEA_SINASC_2023[, 6]/PR_PEVASPEA_SINASC_2023[, 5]) *1000,
                  "Nascidos_2024" = PR_PEVASPEA_SINASC_2024[, 5],
                  "N_2024" = PR_PEVASPEA_SINASC_2024[, 6],
                  'Taxa_2024' = (PR_PEVASPEA_SINASC_2024[, 6]/PR_PEVASPEA_SINASC_2024[, 5]) *1000,
                  "Nascidos_2025" = PR_PEVASPEA_SINASC_2025[, 5],
                  "N_2025" = PR_PEVASPEA_SINASC_2025[, 6],
                  'Taxa_2025' = (PR_PEVASPEA_SINASC_2025[, 6]/PR_PEVASPEA_SINASC_2025[, 5]) *1000)

write.csv (assign(paste0("PR_PEVASPEA_SINASC_Serie_historica_Mun"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_Serie_historica_Mun.csv"), 
           row.names = FALSE)

############   Dados de Regionais

AUX <- PR_PEVASPEA_SINASC_2016 %>% 
  group_by(RS) %>% 
  summarise(sum(Nascidos),
            sum(Anomalia_Detectada),
            sum(Anomalia_Prioritaria_Vig_Nasc))

colnames(AUX)[2] <- "Nascidos_2016"
colnames(AUX)[3] <- "Anomalias_2016"
colnames(AUX)[4] <- "Anomalias_Pri_2016"

AUX$Taxa_Anomalias_2016 <- (AUX$Anomalias_2016/AUX$Nascidos_2016) * 1000

AUX01 <- AUX

AUX <- PR_PEVASPEA_SINASC_2017 %>% 
  group_by(RS) %>% 
  summarise(sum(Nascidos),
            sum(Anomalia_Detectada),
            sum(Anomalia_Prioritaria_Vig_Nasc))

colnames(AUX)[2] <- "Nascidos_2017"
colnames(AUX)[3] <- "Anomalias_2017"
colnames(AUX)[4] <- "Anomalias_Pri_2017"

AUX$Taxa_Anomalias_2017 <- (AUX$Anomalias_2017/AUX$Nascidos_2017) * 1000

AUX01[, 6:9] <- AUX[, 2:5]

AUX <- PR_PEVASPEA_SINASC_2018 %>% 
  group_by(RS) %>% 
  summarise(sum(Nascidos),
            sum(Anomalia_Detectada),
            sum(Anomalia_Prioritaria_Vig_Nasc))

colnames(AUX)[2] <- "Nascidos_2018"
colnames(AUX)[3] <- "Anomalias_2018"
colnames(AUX)[4] <- "Anomalias_Pri_2018"

AUX$Taxa_Anomalias_2018 <- (AUX$Anomalias_2018/AUX$Nascidos_2018) * 1000

AUX01[, 10:13] <- AUX[, 2:5]

AUX <- PR_PEVASPEA_SINASC_2019 %>% 
  group_by(RS) %>% 
  summarise(sum(Nascidos),
            sum(Anomalia_Detectada),
            sum(Anomalia_Prioritaria_Vig_Nasc))

colnames(AUX)[2] <- "Nascidos_2019"
colnames(AUX)[3] <- "Anomalias_2019"
colnames(AUX)[4] <- "Anomalias_Pri_2019"

AUX$Taxa_Anomalias_2019 <- (AUX$Anomalias_2019/AUX$Nascidos_2019) * 1000

AUX01[, 14:17] <- AUX[, 2:5]

AUX <- PR_PEVASPEA_SINASC_2020 %>% 
  group_by(RS) %>% 
  summarise(sum(Nascidos),
            sum(Anomalia_Detectada),
            sum(Anomalia_Prioritaria_Vig_Nasc))

colnames(AUX)[2] <- "Nascidos_2020"
colnames(AUX)[3] <- "Anomalias_2020"
colnames(AUX)[4] <- "Anomalias_Pri_2020"

AUX$Taxa_Anomalias_2020 <- (AUX$Anomalias_2020/AUX$Nascidos_2020) * 1000

AUX01[, 18:21] <- AUX[, 2:5]

AUX <- PR_PEVASPEA_SINASC_2021 %>% 
  group_by(RS) %>% 
  summarise(sum(Nascidos),
            sum(Anomalia_Detectada),
            sum(Anomalia_Prioritaria_Vig_Nasc))

colnames(AUX)[2] <- "Nascidos_2021"
colnames(AUX)[3] <- "Anomalias_2021"
colnames(AUX)[4] <- "Anomalias_Pri_2021"

AUX$Taxa_Anomalias_2021 <- (AUX$Anomalias_2021/AUX$Nascidos_2021) * 1000

AUX01[, 22:25] <- AUX[, 2:5]

AUX <- PR_PEVASPEA_SINASC_2022 %>% 
  group_by(RS) %>% 
  summarise(sum(Nascidos),
            sum(Anomalia_Detectada),
            sum(Anomalia_Prioritaria_Vig_Nasc))

colnames(AUX)[2] <- "Nascidos_2022"
colnames(AUX)[3] <- "Anomalias_2022"
colnames(AUX)[4] <- "Anomalias_Pri_2022"

AUX$Taxa_Anomalias_2022 <- (AUX$Anomalias_2022/AUX$Nascidos_2022) * 1000

AUX01[, 26:29] <- AUX[, 2:5]

AUX <- PR_PEVASPEA_SINASC_2023 %>% 
  group_by(RS) %>% 
  summarise(sum(Nascidos),
            sum(Anomalia_Detectada),
            sum(Anomalia_Prioritaria_Vig_Nasc))

colnames(AUX)[2] <- "Nascidos_2023"
colnames(AUX)[3] <- "Anomalias_2023"
colnames(AUX)[4] <- "Anomalias_Pri_2023"

AUX$Taxa_Anomalias_2023 <- (AUX$Anomalias_2023/AUX$Nascidos_2023) * 1000

AUX01[, 30:33] <- AUX[, 2:5]

AUX <- PR_PEVASPEA_SINASC_2024 %>% 
  group_by(RS) %>% 
  summarise(sum(Nascidos),
            sum(Anomalia_Detectada),
            sum(Anomalia_Prioritaria_Vig_Nasc))

colnames(AUX)[2] <- "Nascidos_2024"
colnames(AUX)[3] <- "Anomalias_2024"
colnames(AUX)[4] <- "Anomalias_Pri_2024"

AUX$Taxa_Anomalias_2024 <- (AUX$Anomalias_2024/AUX$Nascidos_2024) * 1000

AUX01[, 34:37] <- AUX[, 2:5]

AUX <- PR_PEVASPEA_SINASC_2025 %>% 
  group_by(RS) %>% 
  summarise(sum(Nascidos),
            sum(Anomalia_Detectada),
            sum(Anomalia_Prioritaria_Vig_Nasc))

colnames(AUX)[2] <- "Nascidos_2025"
colnames(AUX)[3] <- "Anomalias_2025"
colnames(AUX)[4] <- "Anomalias_Pri_2025"

AUX$Taxa_Anomalias_2025 <- (AUX$Anomalias_2025/AUX$Nascidos_2025) * 1000

AUX01[, 38:41] <- AUX[, 2:5]

PR_PEVASPEA_SINASC_RS_Serie_Historica <- AUX01

write.csv (PR_PEVASPEA_SINASC_RS_Serie_Historica, 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_RS_Serie_Historica.csv"), 
           row.names = FALSE)

#### Histórico de nascimentos PR p/SE

PR_PEVASPEA_SINASC_NASC_SE_GERAL <- PR_PEVASPEA_SINASC_NASC_SE_2016[nrow(PR_PEVASPEA_SINASC_NASC_SE_2016),]
PR_PEVASPEA_SINASC_NASC_SE_GERAL[1,1] <- "2016"
PR_PEVASPEA_SINASC_NASC_SE_GERAL[nrow(PR_PEVASPEA_SINASC_NASC_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_NASC_SE_2017[nrow(PR_PEVASPEA_SINASC_NASC_SE_2017),]
PR_PEVASPEA_SINASC_NASC_SE_GERAL[2,1] <- "2017"
PR_PEVASPEA_SINASC_NASC_SE_GERAL[nrow(PR_PEVASPEA_SINASC_NASC_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_NASC_SE_2018[nrow(PR_PEVASPEA_SINASC_NASC_SE_2018),]
PR_PEVASPEA_SINASC_NASC_SE_GERAL[3,1] <- "2018"
PR_PEVASPEA_SINASC_NASC_SE_GERAL[nrow(PR_PEVASPEA_SINASC_NASC_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_NASC_SE_2019[nrow(PR_PEVASPEA_SINASC_NASC_SE_2019),]
PR_PEVASPEA_SINASC_NASC_SE_GERAL[4,1] <- "2019"
PR_PEVASPEA_SINASC_NASC_SE_GERAL[nrow(PR_PEVASPEA_SINASC_NASC_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_NASC_SE_2020[nrow(PR_PEVASPEA_SINASC_NASC_SE_2020),]
PR_PEVASPEA_SINASC_NASC_SE_GERAL[5,1] <- "2020"
PR_PEVASPEA_SINASC_NASC_SE_GERAL[nrow(PR_PEVASPEA_SINASC_NASC_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_NASC_SE_2021[nrow(PR_PEVASPEA_SINASC_NASC_SE_2021),]
PR_PEVASPEA_SINASC_NASC_SE_GERAL[6,1] <- "2021"
PR_PEVASPEA_SINASC_NASC_SE_GERAL[nrow(PR_PEVASPEA_SINASC_NASC_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_NASC_SE_2022[nrow(PR_PEVASPEA_SINASC_NASC_SE_2022),]
PR_PEVASPEA_SINASC_NASC_SE_GERAL[7,1] <- "2022"
PR_PEVASPEA_SINASC_NASC_SE_GERAL[nrow(PR_PEVASPEA_SINASC_NASC_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_NASC_SE_2023[nrow(PR_PEVASPEA_SINASC_NASC_SE_2023),]
PR_PEVASPEA_SINASC_NASC_SE_GERAL[8,1] <- "2023"
PR_PEVASPEA_SINASC_NASC_SE_GERAL[nrow(PR_PEVASPEA_SINASC_NASC_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_NASC_SE_2024[nrow(PR_PEVASPEA_SINASC_NASC_SE_2024),]
PR_PEVASPEA_SINASC_NASC_SE_GERAL[9,1] <- "2024"
PR_PEVASPEA_SINASC_NASC_SE_GERAL[nrow(PR_PEVASPEA_SINASC_NASC_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_NASC_SE_2025[nrow(PR_PEVASPEA_SINASC_NASC_SE_2025),]
PR_PEVASPEA_SINASC_NASC_SE_GERAL[10,1] <- "2025"

#### Histórico de anomalias PR p/SE

PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL <- PR_PEVASPEA_SINASC_ANOMAL_SE_2016[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_2016),]
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[1,1] <- "2016"
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_ANOMAL_SE_2017[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_2017),]
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[2,1] <- "2017"
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_ANOMAL_SE_2018[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_2018),]
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[3,1] <- "2018"
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_ANOMAL_SE_2019[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_2019),]
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[4,1] <- "2019"
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_ANOMAL_SE_2020[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_2020),]
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[5,1] <- "2020"
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_ANOMAL_SE_2021[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_2021),]
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[6,1] <- "2021"
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_ANOMAL_SE_2022[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_2022),]
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[7,1] <- "2022"
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_ANOMAL_SE_2023[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_2023),]
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[8,1] <- "2023"
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_ANOMAL_SE_2024[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_2024),]
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[9,1] <- "2024"
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL) +1,] <- PR_PEVASPEA_SINASC_ANOMAL_SE_2025[nrow(PR_PEVASPEA_SINASC_ANOMAL_SE_2025),]
PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[10,1] <- "2025"

#### Salvando arquivos que ficaram para trás

write.csv (PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_NASC_SE_GERAL, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_GERAL.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_NASC_SE_2016, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_2016.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_NASC_SE_2017, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_2017.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_NASC_SE_2018, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_2018.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_NASC_SE_2019, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_2019.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_NASC_SE_2020, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_2020.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_NASC_SE_2021, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_2021.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_NASC_SE_2022, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_2022.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_NASC_SE_2023, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_2023.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_NASC_SE_2024, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_2024.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_NASC_SE_2025, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_2025.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_ANOMAL_SE_2016, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_2016.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_ANOMAL_SE_2017, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_2017.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_ANOMAL_SE_2018, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_2018.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_ANOMAL_SE_2019, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_2019.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_ANOMAL_SE_2020, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_2020.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_ANOMAL_SE_2021, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_2021.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_ANOMAL_SE_2022, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_2022.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_ANOMAL_SE_2023, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_2023.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_ANOMAL_SE_2024, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_2024.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SINASC_ANOMAL_SE_2025, 
           "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_2025.csv", 
           row.names = FALSE)


rm(AUX,
   AUX01,
   BASE_IBGE,
   BASE_IBGE_BRASIL,
   i,
   ID_REG,
   nrow,
   RS)
