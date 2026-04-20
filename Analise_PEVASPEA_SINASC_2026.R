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

RS_SINASC_Serie_Historica <- read.csv(file = paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_Serie_Historica.csv"),
                                                                header = TRUE,
                                                                sep = ",")

PR_SINASC_Serie_Historica <- read.csv(file = paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_Serie_Historica.csv"),
                                      header = TRUE,
                                      sep = ",")

######   Criando objeto ID_REG. Será utilizado para selecionar
######   RS no DBF do SINAN ONLINE.

ID_REG <- as.data.frame(BASE_IBGE[which(BASE_IBGE$RS == RS), 6])

ID_REG <- as.numeric(ID_REG[1,1])

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == RS), 1])

##############################################################
##################   2026  ###################################
##############################################################

DNPR2026 <- read.dbf(file = "Base_de_Dados/DBF/DNPR2026.dbf", 
                     as.is = FALSE)

for (i in BASE_IBGE[, 2]){
  DNPR2026[which(DNPR2026$CODMUNRES == i), 107] <-  BASE_IBGE[which(BASE_IBGE$Código_IBGE == i), 1]
    
}

colnames(DNPR2026)[107] <- "Regional"
  
AUX <- DNPR2026 %>% filter(IDANOMAL == 1,
                           Regional == RS)

assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2026"), AUX)

assign("RS_ANOMALIAS_SINASC_2026", AUX)

write.csv (assign(paste0("RS", RS, "_ANOMALIAS_SINASC_2026"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_ANOMALIAS_SINASC_2026.csv"), 
           row.names = FALSE)

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
  
  AUX[which(AUX$COD_IBGE == i), 5] <- as.integer(DNPR2026 %>% 
                                                   filter(CODMUNRES == i) %>%   
                                                   count()
  )    
  
  AUX[which(AUX$COD_IBGE == i), 6] <- as.integer(DNPR2026 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 7] <- as.integer(DNPR2026 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 8] <- as.integer(DNPR2026 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q00") |
                                                            str_detect(CODANOMAL, "Q01") |
                                                            str_detect(CODANOMAL, "Q05")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 9] <- as.integer(DNPR2026 %>% 
                                                   filter(CODMUNRES == i,
                                                          IDANOMAL == 1,
                                                          str_detect(CODANOMAL, "Q02")  
                                                   ) %>%   
                                                   count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 10] <- as.integer(DNPR2026 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q2")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 11] <- as.integer(DNPR2026 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q35") |
                                                             str_detect(CODANOMAL, "Q36") |
                                                             str_detect(CODANOMAL, "Q37") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 12] <- as.integer(DNPR2026 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q54") |
                                                             str_detect(CODANOMAL, "Q56")  
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 13] <- as.integer(DNPR2026 %>% 
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
  
  AUX[which(AUX$COD_IBGE == i), 14] <- as.integer(DNPR2026 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q792") |
                                                             str_detect(CODANOMAL, "Q793") 
                                                    ) %>%   
                                                    count()
  )
  
  AUX[which(AUX$COD_IBGE == i), 15] <- as.integer(DNPR2026 %>% 
                                                    filter(CODMUNRES == i,
                                                           IDANOMAL == 1,
                                                           str_detect(CODANOMAL, "Q90")  
                                                    ) %>%   
                                                    count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("RS", RS, "_PEVASPEA_SINASC_2026"), AUX)

write.csv (assign(paste0("RS", RS, "_PEVASPEA_SINASC_2026"), AUX), 
           paste0("Tabulacoes_R/SINASC/RS", RS, "_PEVASPEA_SINASC_2026.csv"), 
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
  
  AUX[which(AUX$Código_IBGE == i), 5] <-  as.integer(DNPR2026 %>% 
                                                       filter(CODMUNRES == i) %>%   
                                                       count()
  )  
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DNPR2026 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DNPR2026 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DNPR2026 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q00") |
                                                               str_detect(CODANOMAL, "Q01") |
                                                               str_detect(CODANOMAL, "Q05")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DNPR2026 %>% 
                                                      filter(CODMUNRES == i,
                                                             IDANOMAL == 1,
                                                             str_detect(CODANOMAL, "Q02")  
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DNPR2026 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q2")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DNPR2026 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q35") |
                                                                str_detect(CODANOMAL, "Q36") |
                                                                str_detect(CODANOMAL, "Q37") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DNPR2026 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q54") |
                                                                str_detect(CODANOMAL, "Q56")  
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DNPR2026 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DNPR2026 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q792") |
                                                                str_detect(CODANOMAL, "Q793") 
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 15] <- as.integer(DNPR2026 %>% 
                                                       filter(CODMUNRES == i,
                                                              IDANOMAL == 1,
                                                              str_detect(CODANOMAL, "Q90")  
                                                       ) %>%   
                                                       count()
  )
}

AUX[(nrow(AUX) +1), 4:15] <- apply(AUX[, 4:15], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign(paste0("PR_PEVASPEA_SINASC_2026"), AUX)

write.csv (assign(paste0("PR_PEVASPEA_SINASC_2026"), AUX), 
           paste0("Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2026.csv"), 
           row.names = FALSE)

rm(DNPR2026)


