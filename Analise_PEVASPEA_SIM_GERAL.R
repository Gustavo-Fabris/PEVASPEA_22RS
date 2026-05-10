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
                     as.is = TRUE)

DOPR2016$IDADE <- as.numeric(DOPR2016$IDADE)

DOPR2016$QTDFILMORT <- as.numeric(DOPR2016$QTDFILMORT)

SINASC2016 <- read.csv(file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2016.csv",
                       header = TRUE,
                       sep = ",")

##### Tabela geral Estado do Paraná

AUX <- SINASC2016[-nrow(SINASC2016),c(1:5)]

AUX$Obitos_Fetais <- NA

AUX$Obitos_Fetais_Malformacoes <- NA

AUX$Obito_Infantil <- NA

AUX$Obito_Infantil_Malformacao <- NA

AUX$Obito_Fetal_Infantil_Geral_Nasc_Mortos <- NA

AUX$Obito_Fetal_Infantil_Anomalias_Nasc_Mortos <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2016 %>% 
                                                   filter(CODMUNRES == i,
                                                          TIPOBITO == 1) %>%   
                                                   count()
  )   
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 1,
                                                             (str_detect(CAUSABAS, "Q") |
                                                               str_detect(LINHAA, "Q") |
                                                               str_detect(LINHAB, "Q") |
                                                               str_detect(LINHAC, "Q") |
                                                               str_detect(LINHAD, "Q") |
                                                               str_detect(LINHAII, "Q"))) %>%   
                                                      count()
  ) 
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400)%>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400,  
                                                             (str_detect(CAUSABAS, "Q") |
                                                               str_detect(LINHAA, "Q") |
                                                               str_detect(LINHAB, "Q") |
                                                               str_detect(LINHAC, "Q") |
                                                               str_detect(LINHAD, "Q") |
                                                               str_detect(LINHAII, "Q"))) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             (TIPOBITO == 1 | 
                                                                (TIPOBITO == 2 & 
                                                                   IDADE >= 100 & 
                                                                   IDADE < 400)),
                                                             is.na(DTNASC),
                                                             QTDFILMORT >= 1 &
                                                               QTDFILMORT < 90) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             (TIPOBITO == 1 | 
                                                                (TIPOBITO == 2 & 
                                                                   IDADE >= 100 & 
                                                                   IDADE < 400)),
                                                             is.na(DTNASC),
                                                             QTDFILMORT >= 1 &
                                                               QTDFILMORT < 90,
                                                             str_detect(CAUSABAS, "Q") |
                                                               str_detect(LINHAA, "Q") |
                                                               str_detect(LINHAB, "Q") |
                                                               str_detect(LINHAC, "Q") |
                                                               str_detect(LINHAD, "Q") |
                                                               str_detect(LINHAII, "Q")) %>%   
                                                      count()
  
  )
}

AUX[(nrow(AUX)+ 1),5:11] <- apply(AUX[,5:11], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SIM_Congenitas_Geral", AUX)

### Estabelecendo Objetos com REGEX para simplificar busca de CID

CID_Prioritaria <- "Q00|Q01|Q02|Q05|Q2|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90"

CID_Tubo_Neural <- "Q00|Q01|Q05"

CID_Microcefalia <- "Q02"

CID_Cardiopatia <- "Q2"

CID_Fenda <- "Q35|Q36|Q37"

CID_Genitais <- "Q54|Q56"

CID_Membros <- "Q66| Q69 | Q71 | Q72 |Q73 | Q743"

CID_Abd <- "Q792 | Q793"

CID_Down <- "Q90"

##### Prioritárias Fetal

AUX <- SINASC2016[-nrow(SINASC2016),c(1:5)]

AUX$Prioritarias <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
 
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 1,
                                                             str_detect(CAUSABAS, CID_Prioritaria) |
                                                               str_detect(LINHAA, CID_Prioritaria) |
                                                               str_detect(LINHAB, CID_Prioritaria) |
                                                               str_detect(LINHAC, CID_Prioritaria) |
                                                               str_detect(LINHAD, CID_Prioritaria) |
                                                               str_detect(LINHAII, CID_Prioritaria)
                                                      ) %>%   
                                                      count()
  )


AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2016 %>% 
                                                    filter(CODMUNRES == i,
                                                           TIPOBITO == 1,
                                                           str_detect(CAUSABAS, CID_Tubo_Neural) |
                                                             str_detect(LINHAA, CID_Tubo_Neural) |
                                                             str_detect(LINHAB, CID_Tubo_Neural) |
                                                             str_detect(LINHAC, CID_Tubo_Neural) |
                                                             str_detect(LINHAD, CID_Tubo_Neural) |
                                                             str_detect(LINHAII, CID_Tubo_Neural)
                                                             
                                                    ) %>%   
                                                    count()
)

AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2016 %>% 
                                                    filter(CODMUNRES == i,
                                                           TIPOBITO == 1,
                                                           str_detect(CAUSABAS, "Q02") | 
                                                             str_detect(LINHAA, "Q02") | 
                                                             str_detect(LINHAB, "Q02") | 
                                                             str_detect(LINHAC, "Q02") | 
                                                             str_detect(LINHAD, "Q02") | 
                                                             str_detect(LINHAII, "Q02")
                                                           
                                                    ) %>%   
                                                    count()
)

AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2016 %>% 
                                                    filter(CODMUNRES == i,
                                                           TIPOBITO == 1,
                                                           str_detect(CAUSABAS, CID_Cardiopatia) |
                                                             str_detect(LINHAA, CID_Cardiopatia) |
                                                             str_detect(LINHAB, CID_Cardiopatia) |
                                                             str_detect(LINHAC, CID_Cardiopatia) |
                                                             str_detect(LINHAD, CID_Cardiopatia) |
                                                             str_detect(LINHAII, CID_Cardiopatia)
                                                           
                                                    ) %>%   
                                                    count()
)

AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2016 %>% 
                                                    filter(CODMUNRES == i,
                                                           TIPOBITO == 1,
                                                           str_detect(CAUSABAS, CID_Fenda) |
                                                             str_detect(LINHAA, CID_Fenda) |
                                                             str_detect(LINHAB, CID_Fenda) |
                                                             str_detect(LINHAC, CID_Fenda) |
                                                             str_detect(LINHAD, CID_Fenda) |
                                                             str_detect(LINHAII, CID_Fenda)
                                                           
                                                    ) %>%   
                                                    count()
)

AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2016 %>% 
                                                     filter(CODMUNRES == i,
                                                            TIPOBITO == 1,
                                                            str_detect(CAUSABAS, CID_Genitais) |
                                                              str_detect(LINHAA, CID_Genitais) |
                                                              str_detect(LINHAB, CID_Genitais) |
                                                              str_detect(LINHAC, CID_Genitais) |
                                                              str_detect(LINHAD, CID_Genitais) |
                                                              str_detect(LINHAII, CID_Genitais)
                                                            
                                                     ) %>%   
                                                     count()
)

AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DOPR2016 %>% 
                                                     filter(CODMUNRES == i,
                                                            TIPOBITO == 1,
                                                            str_detect(CAUSABAS, CID_Membros) |
                                                              str_detect(LINHAA, CID_Membros) |
                                                              str_detect(LINHAB, CID_Membros) |
                                                              str_detect(LINHAC, CID_Membros) |
                                                              str_detect(LINHAD, CID_Membros) |
                                                              str_detect(LINHAII, CID_Membros)
                                                            
                                                     ) %>%   
                                                     count()
)

AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DOPR2016 %>% 
                                                     filter(CODMUNRES == i,
                                                            TIPOBITO == 1,
                                                            str_detect(CAUSABAS, CID_Abd) |
                                                              str_detect(LINHAA, CID_Abd) |
                                                              str_detect(LINHAB, CID_Abd) |
                                                              str_detect(LINHAC, CID_Abd) |
                                                              str_detect(LINHAD, CID_Abd) |
                                                              str_detect(LINHAII, CID_Abd)
                                                            
                                                     ) %>%   
                                                     count()
)

AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DOPR2016 %>% 
                                                     filter(CODMUNRES == i,
                                                            TIPOBITO == 1,
                                                            str_detect(CAUSABAS, CID_Down) |
                                                              str_detect(LINHAA, CID_Down) |
                                                              str_detect(LINHAB, CID_Down) |
                                                              str_detect(LINHAC, CID_Down) |
                                                              str_detect(LINHAD, CID_Down) |
                                                              str_detect(LINHAII, CID_Down)
                                                            
                                                     ) %>%   
                                                     count()
)

}

AUX[(nrow(AUX)+ 1),5:14] <- apply(AUX[,5:14], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SIM_Prioritarias_Fetal", AUX)

##### Prioritárias Óbito infantil

AUX <- SINASC2016[-nrow(SINASC2016),c(1:5)]

AUX$Prioritarias <- NA

AUX$Anomalia_Prioritaria_Tubo_Neural <- NA

AUX$Anomalia_Prioritaria_Microcefalia <- NA

AUX$Anomalia_Prioritaria_Cardiopatias <- NA

AUX$Anomalia_Prioritaria_Fendas_Orais <- NA

AUX$Anomalia_Prioritaria_Genitais <- NA

AUX$Anomalia_Prioritaria_Membros <- NA

AUX$Anomalia_Prioritaria_Parede_Abd <- NA

AUX$Anomalia_Prioritaria_Sindrome_Down <- NA

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400, 
                                                             str_detect(CAUSABAS, CID_Prioritaria) |
                                                               str_detect(LINHAA, CID_Prioritaria) |
                                                               str_detect(LINHAB, CID_Prioritaria) |
                                                               str_detect(LINHAC, CID_Prioritaria) |
                                                               str_detect(LINHAD, CID_Prioritaria) |
                                                               str_detect(LINHAII, CID_Prioritaria)
                                                      ) %>%   
                                                      count()
  )
  
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400, 
                                                             str_detect(CAUSABAS, CID_Tubo_Neural) |
                                                               str_detect(LINHAA, CID_Tubo_Neural) |
                                                               str_detect(LINHAB, CID_Tubo_Neural) |
                                                               str_detect(LINHAC, CID_Tubo_Neural) |
                                                               str_detect(LINHAD, CID_Tubo_Neural) |
                                                               str_detect(LINHAII, CID_Tubo_Neural)
                                                             
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400, 
                                                             str_detect(CAUSABAS, "Q02") | 
                                                               str_detect(LINHAA, "Q02") | 
                                                               str_detect(LINHAB, "Q02") | 
                                                               str_detect(LINHAC, "Q02") | 
                                                               str_detect(LINHAD, "Q02") | 
                                                               str_detect(LINHAII, "Q02")
                                                             
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2016 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400, 
                                                             str_detect(CAUSABAS, CID_Cardiopatia) |
                                                               str_detect(LINHAA, CID_Cardiopatia) |
                                                               str_detect(LINHAB, CID_Cardiopatia) |
                                                               str_detect(LINHAC, CID_Cardiopatia) |
                                                               str_detect(LINHAD, CID_Cardiopatia) |
                                                               str_detect(LINHAII, CID_Cardiopatia)
                                                             
                                                      ) %>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2016 %>% 
                                                       filter(CODMUNRES == i,
                                                              TIPOBITO == 2,
                                                              IDADE >= 100 & 
                                                                IDADE < 400, 
                                                              str_detect(CAUSABAS, CID_Fenda) |
                                                                str_detect(LINHAA, CID_Fenda) |
                                                                str_detect(LINHAB, CID_Fenda) |
                                                                str_detect(LINHAC, CID_Fenda) |
                                                                str_detect(LINHAD, CID_Fenda) |
                                                                str_detect(LINHAII, CID_Fenda)
                                                              
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2016 %>% 
                                                       filter(CODMUNRES == i,
                                                              TIPOBITO == 2,
                                                              IDADE >= 100 & 
                                                                IDADE < 400, 
                                                              str_detect(CAUSABAS, CID_Genitais) |
                                                                str_detect(LINHAA, CID_Genitais) |
                                                                str_detect(LINHAB, CID_Genitais) |
                                                                str_detect(LINHAC, CID_Genitais) |
                                                                str_detect(LINHAD, CID_Genitais) |
                                                                str_detect(LINHAII, CID_Genitais)
                                                              
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 12] <- as.integer(DOPR2016 %>% 
                                                       filter(CODMUNRES == i,
                                                              TIPOBITO == 2,
                                                              IDADE >= 100 & 
                                                                IDADE < 400, 
                                                              str_detect(CAUSABAS, CID_Membros) |
                                                                str_detect(LINHAA, CID_Membros) |
                                                                str_detect(LINHAB, CID_Membros) |
                                                                str_detect(LINHAC, CID_Membros) |
                                                                str_detect(LINHAD, CID_Membros) |
                                                                str_detect(LINHAII, CID_Membros)
                                                              
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 13] <- as.integer(DOPR2016 %>% 
                                                       filter(CODMUNRES == i,
                                                              TIPOBITO == 2,
                                                              IDADE >= 100 & 
                                                                IDADE < 400, 
                                                              str_detect(CAUSABAS, CID_Abd) |
                                                                str_detect(LINHAA, CID_Abd) |
                                                                str_detect(LINHAB, CID_Abd) |
                                                                str_detect(LINHAC, CID_Abd) |
                                                                str_detect(LINHAD, CID_Abd) |
                                                                str_detect(LINHAII, CID_Abd)
                                                              
                                                       ) %>%   
                                                       count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 14] <- as.integer(DOPR2016 %>% 
                                                       filter(CODMUNRES == i,
                                                              TIPOBITO == 2,
                                                              IDADE >= 100 & 
                                                                IDADE < 400, 
                                                              str_detect(CAUSABAS, CID_Down) |
                                                                str_detect(LINHAA, CID_Down) |
                                                                str_detect(LINHAB, CID_Down) |
                                                                str_detect(LINHAC, CID_Down) |
                                                                str_detect(LINHAD, CID_Down) |
                                                                str_detect(LINHAII, CID_Down)
                                                              
                                                       ) %>%   
                                                       count()
  )
  
}

AUX[(nrow(AUX)+ 1),5:14] <- apply(AUX[,5:14], 2, sum)

AUX[nrow(AUX), 1] <- "Total"

assign("PR_PEVASPEA_SIM_Prioritarias_Infantil", AUX)


####### Câncer

CID_LABIO         <- "C0[0-9]|C1[0-4]"
CID_DIGESTIVO     <- "C1[5-9]|C2[0-6]"
CID_RESPIRATORIO  <- "C3[0-9]"
CID_OSSOS         <- "C4[0-1]"
CID_PELE          <- "C4[3-4]"
CID_TEC_MOLE      <- "C4[5-9]"
CID_MAMA          <- "C50"
CID_GEN_FEM       <- "C5[1-8]"
CID_GEN_MASC      <- "C6[0-3]"
CID_VIA_URINARIA  <- "C6[4-8]"
CID_CEREBRO       <- "C69|C7[0-2]"
CID_TIREOIDE      <- "C7[3-5]"
CID_MAL_DEFINIDAS <- "C7[6-9]|C80"
CID_LINFATICO     <- "C8[1-9]|C9[0-6]"

AUX <- DOPR2016 %>%
  mutate(across(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), as.character)) %>%
  mutate(
    NEOPLASIAS    = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., "C")), 1, 0, missing = 0),
    Labio_Cav_Oral = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_LABIO)), 1, 0, missing = 0),
    Digestivo      = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_DIGESTIVO)), 1, 0, missing = 0),
    Respiratorio   = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_RESPIRATORIO)), 1, 0, missing = 0),
    Ossos          = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_OSSOS)), 1, 0, missing = 0),
    Pele           = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_PELE)), 1, 0, missing = 0),
    Tec_Mole       = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_TEC_MOLE)), 1, 0, missing = 0),
    Mama           = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_MAMA)), 1, 0, missing = 0),
    Gen_Fem        = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_GEN_FEM)), 1, 0, missing = 0),
    Gen_Masc       = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_GEN_MASC)), 1, 0, missing = 0),
    Via_Urinaria   = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_VIA_URINARIA)), 1, 0, missing = 0),
    Cerebro        = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_CEREBRO)), 1, 0, missing = 0),
    Tireoide_Endo  = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_TIREOIDE)), 1, 0, missing = 0),
    Mal_Def        = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_MAL_DEFINIDAS)), 1, 0, missing = 0),
    Linfatico      = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_LINFATICO)), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)))

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2016 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(NEOPLASIAS:Linfatico, \(x) replace_na(x, 0)))

##### Por sexo

AUX <- DOPR2016 %>%
  mutate(SEXO_LABEL = case_when(
    SEXO == "M" ~ "MASC",
    SEXO == "F" ~ "FEM"
  )) %>%
  mutate(across(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), as.character)) %>%
  mutate(
    NEOPLASIAS    = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., "C")), 1, 0, missing = 0),
    Labio_Cav_Oral = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_LABIO)), 1, 0, missing = 0),
    Digestivo      = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_DIGESTIVO)), 1, 0, missing = 0),
    Respiratorio   = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_RESPIRATORIO)), 1, 0, missing = 0),
    Ossos          = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_OSSOS)), 1, 0, missing = 0),
    Pele           = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_PELE)), 1, 0, missing = 0),
    Tec_Mole       = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_TEC_MOLE)), 1, 0, missing = 0),
    Mama           = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_MAMA)), 1, 0, missing = 0),
    Gen_Fem        = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_GEN_FEM)), 1, 0, missing = 0),
    Gen_Masc       = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_GEN_MASC)), 1, 0, missing = 0),
    Via_Urinaria   = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_VIA_URINARIA)), 1, 0, missing = 0),
    Cerebro        = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_CEREBRO)), 1, 0, missing = 0),
    Tireoide_Endo  = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_TIREOIDE)), 1, 0, missing = 0),
    Mal_Def        = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_MAL_DEFINIDAS)), 1, 0, missing = 0),
    Linfatico      = if_else(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_LINFATICO)), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES, SEXO_LABEL) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = SEXO_LABEL, 
              values_from = NEOPLASIAS:Linfatico, 
              names_glue = "{.value}_{SEXO_LABEL}",
              values_fill = 0)

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2016 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
  select(-ends_with("_NA"))

####  Faixa Etária

quebras <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
rotulos <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2016 <- DOPR2016 %>%
  filter(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~ str_detect(., "C"))) %>%
  mutate(
    unidade = as.numeric(substr(IDADE, 1, 1)),
    valor   = as.numeric(substr(IDADE, 2, 3)),
    idade_anos = case_when(
      unidade < 4  ~ 0, 
      unidade == 4 ~ valor,
      unidade == 5 ~ valor + 100,
      TRUE         ~ NA_real_
    ),
    Faixa_Etaria = cut(idade_anos, breaks = quebras, labels = rotulos, right = FALSE),
    Grupo_CID = case_when(
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_LINFATICO)) ~ "Linfatico_Hematologico",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_CEREBRO)) ~ "Cerebro_SNC",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_DIGESTIVO)) ~ "Digestivo",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_RESPIRATORIO)) ~ "Respiratorio",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_GEN_FEM)) ~ "Gen_Fem",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_GEN_MASC)) ~ "Gen_Masc",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_MAMA)) ~ "Mama",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_VIA_URINARIA)) ~ "Via_Urinaria",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_PELE)) ~ "Pele",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_LABIO)) ~ "Labio_Oral",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_OSSOS)) ~ "Ossos",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_TEC_MOLE)) ~ "Tec_Mole",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_TIREOIDE)) ~ "Tireoide_Endo",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_MAL_DEFINIDAS)) ~ "Mal_Definidas",
      TRUE ~ NA_character_ 
    )
  ) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, values_from = Casos, values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()

#### Por Idade RS

Municipios_22RS <- BASE_IBGE %>% 
  filter(RS == RS) %>%
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  pull(Código_IBGE)

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016 <- DOPR2016 %>%
  filter(CODMUNRES %in% Municipios_22RS) %>% 
  filter(if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~ str_detect(., "C"))) %>%
  mutate(
    unidade = as.numeric(substr(IDADE, 1, 1)),
    valor   = as.numeric(substr(IDADE, 2, 3)),
    idade_anos = case_when(
      unidade < 4  ~ 0, 
      unidade == 4 ~ valor,
      unidade == 5 ~ valor + 100,
      TRUE         ~ NA_real_
    ),
    Faixa_Etaria = cut(idade_anos, breaks = quebras, labels = rotulos, right = FALSE),
    Grupo_CID = case_when(
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_LINFATICO)) ~ "Linfatico_Hematologico",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_CEREBRO)) ~ "Cerebro_SNC",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_DIGESTIVO)) ~ "Digestivo",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_RESPIRATORIO)) ~ "Respiratorio",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_GEN_FEM)) ~ "Gen_Fem",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_GEN_MASC)) ~ "Gen_Masc",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_MAMA)) ~ "Mama",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_VIA_URINARIA)) ~ "Via_Urinaria",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_PELE)) ~ "Pele",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_LABIO)) ~ "Labio_Oral",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_OSSOS)) ~ "Ossos",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_TEC_MOLE)) ~ "Tec_Mole",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_TIREOIDE)) ~ "Tireoide_Endo",
      if_any(c(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII), ~str_detect(., CID_MAL_DEFINIDAS)) ~ "Mal_Definidas",
      TRUE ~ NA_character_ 
    )
  ) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, 
              values_from = Casos, 
              values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()