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

ID_REG <- as.data.frame(BASE_IBGE[which(BASE_IBGE$RS == 22), 6])

ID_REG <- as.numeric(ID_REG[1,1])

####   Estabelecendo o número de municípios em cada RS

nrow <- NROW(BASE_IBGE[which(BASE_IBGE$RS == 22), 1])

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

assign("PR_PEVASPEA_SIM_Congenitas_Geral_2016", AUX)

### Estabelecendo Objetos com REGEX para simplificar busca de CID

CID_Prioritaria <- "Q00|Q01|Q02|Q05|Q2|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90"

CID_Tubo_Neural <- "Q00|Q01|Q05"

CID_Microcefalia <- "Q02"

CID_Cardiopatia <- "Q2"

CID_Fenda <- "Q35|Q36|Q37"

CID_Genitais <- "Q54|Q56"

CID_Membros <- "Q66|Q69|Q71|Q72|Q73|Q743"

CID_Abd <- "Q792|Q793"

CID_Down <- "Q90"

##### Prioritárias Fetal


DOPR2016_ESTRATIFICADA <- DOPR2016 %>%
  filter(TIPOBITO == 1) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2016_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Código_IBGE = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2016_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Fetal_2016", AUX_FINAL)

##### Prioritárias Óbito infantil

DOPR2016_ESTRATIFICADA <- DOPR2016 %>%
  filter(TIPOBITO == 2, 
         IDADE < 401) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2016_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município_sem_Código = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantis_2016_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Infantil_2016", AUX_FINAL)

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
 mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
    Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
    Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
    Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
    Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
    Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
    Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
    Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
    Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
    Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
    Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
    Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
    Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
    Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
    Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
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
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
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
  filter(str_detect(CAUSABAS, "C")) %>%
  mutate(
    unidade = as.numeric(substr(IDADE, 1, 1)),
    valor   = as.numeric(substr(IDADE, 2, 3)),
    idade_anos = case_when(
      unidade < 4  ~ 0, 
      unidade == 4 ~ valor,
      unidade == 5 ~ valor + 100,
      TRUE         ~ NA_real_
    ),
    Faixa_Etaria = cut(idade_anos,
                       breaks = quebras, 
                       labels = rotulos, 
                       right = FALSE),
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
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

#### Por Idade RS

Municipios_22RS <- BASE_IBGE %>% 
  filter(RS == 22) %>%
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  pull(Código_IBGE)

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016 <- DOPR2016 %>%
  filter(CODMUNRES %in% Municipios_22RS) %>% 
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
                          TRUE ~ NA_character_ 
    ) ) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, 
              values_from = Casos, 
              values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()

write.csv (PR_PEVASPEA_SIM_Congenitas_Geral_2016, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_2016.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Fetal_2016, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2016.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Infantil_2016, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantil_2016.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2016, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2016.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2016, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2016.csv", 
           row.names = FALSE)

write.csv (RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2016, 
           "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016.csv", 
           row.names = FALSE)

##############################################################
##################   2017  ###################################
##############################################################

DOPR2017 <- read.dbf(file = "Base_de_Dados/DBF/DOPR2017.dbf", 
                     as.is = TRUE)

DOPR2017$IDADE <- as.numeric(DOPR2017$IDADE)

DOPR2017$QTDFILMORT <- as.numeric(DOPR2017$QTDFILMORT)

SINASC2017 <- read.csv(file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2017.csv",
                       header = TRUE,
                       sep = ",")

##### Tabela geral Estado do Paraná

AUX <- SINASC2017[-nrow(SINASC2017),c(1:5)]

AUX$Obitos_Fetais <- NA

AUX$Obitos_Fetais_Malformacoes <- NA

AUX$Obito_Infantil <- NA

AUX$Obito_Infantil_Malformacao <- NA

AUX$Obito_Fetal_Infantil_Geral_Nasc_Mortos <- NA

AUX$Obito_Fetal_Infantil_Anomalias_Nasc_Mortos <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2017 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 1) %>%   
                                                      count()
  )   
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2017 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2017 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400)%>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2017 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2017 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2017 %>% 
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

assign("PR_PEVASPEA_SIM_Congenitas_Geral_2017", AUX)

##### Prioritárias Fetal

DOPR2017_ESTRATIFICADA <- DOPR2017 %>%
  filter(TIPOBITO == 1) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2017_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2017_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Fetal_2017", AUX_FINAL)

##### Prioritárias Óbito infantil

DOPR2017_ESTRATIFICADA <- DOPR2017 %>%
  filter(TIPOBITO == 2, 
         IDADE < 401) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2017_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município_sem_Código = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantis_2017_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Infantil_2017", AUX_FINAL)

####### Câncer

AUX <- DOPR2017 %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)))

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2017 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(NEOPLASIAS:Linfatico, \(x) replace_na(x, 0)))

##### Por sexo

AUX <- DOPR2017 %>%
  mutate(SEXO_LABEL = case_when(
    SEXO == "M" ~ "MASC",
    SEXO == "F" ~ "FEM"
  )) %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )


RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES, SEXO_LABEL) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = SEXO_LABEL, 
              values_from = NEOPLASIAS:Linfatico, 
              names_glue = "{.value}_{SEXO_LABEL}",
              values_fill = 0)

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2017 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
  select(-ends_with("_NA"))

####  Faixa Etária

quebras <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
rotulos <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2017 <- DOPR2017 %>%
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
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
  filter(RS == 22) %>%
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  pull(Código_IBGE)

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017 <- DOPR2017 %>%
  filter(CODMUNRES %in% Municipios_22RS) %>% 
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
                          TRUE ~ NA_character_ 
    )) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, 
              values_from = Casos, 
              values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()

write.csv (PR_PEVASPEA_SIM_Congenitas_Geral_2017, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_2017.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Fetal_2017, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2017.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Infantil_2017, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantil_2017.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2017, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2017.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2017, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2017.csv", 
           row.names = FALSE)

write.csv (RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2017, 
           "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017.csv", 
           row.names = FALSE)


##############################################################
##################   2018  ###################################
##############################################################

DOPR2018 <- read.dbf(file = "Base_de_Dados/DBF/DOPR2018.dbf", 
                     as.is = TRUE)

DOPR2018$IDADE <- as.numeric(DOPR2018$IDADE)

DOPR2018$QTDFILMORT <- as.numeric(DOPR2018$QTDFILMORT)

SINASC2018 <- read.csv(file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2018.csv",
                       header = TRUE,
                       sep = ",")

##### Tabela geral Estado do Paraná

AUX <- SINASC2018[-nrow(SINASC2018),c(1:5)]

AUX$Obitos_Fetais <- NA

AUX$Obitos_Fetais_Malformacoes <- NA

AUX$Obito_Infantil <- NA

AUX$Obito_Infantil_Malformacao <- NA

AUX$Obito_Fetal_Infantil_Geral_Nasc_Mortos <- NA

AUX$Obito_Fetal_Infantil_Anomalias_Nasc_Mortos <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2018 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 1) %>%   
                                                      count()
  )   
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2018 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2018 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400)%>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2018 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2018 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2018 %>% 
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

assign("PR_PEVASPEA_SIM_Congenitas_Geral_2018", AUX)

### Estabelecendo Objetos com REGEX para simplificar busca de CID

CID_Prioritaria <- "Q00|Q01|Q02|Q05|Q2|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90"

CID_Tubo_Neural <- "Q00|Q01|Q05"

CID_Microcefalia <- "Q02"

CID_Cardiopatia <- "Q2"

CID_Fenda <- "Q35|Q36|Q37"

CID_Genitais <- "Q54|Q56"

CID_Membros <- "Q66|Q69|Q71|Q72|Q73|Q743"

CID_Abd <- "Q792|Q793"

CID_Down <- "Q90"

##### Prioritárias Fetal

DOPR2018_ESTRATIFICADA <- DOPR2018 %>%
  filter(TIPOBITO == 1) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2018_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2018_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Fetal_2018", AUX_FINAL)

##### Prioritárias Óbito infantil


DOPR2018_ESTRATIFICADA <- DOPR2018 %>%
  filter(TIPOBITO == 2, 
         IDADE < 401) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2018_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município_sem_Código = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantis_2018_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Infantil_2018", AUX_FINAL)

####### Câncer

AUX <- DOPR2018 %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)))

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2018 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(NEOPLASIAS:Linfatico, \(x) replace_na(x, 0)))

##### Por sexo

AUX <- DOPR2018 %>%
  mutate(SEXO_LABEL = case_when(
    SEXO == "M" ~ "MASC",
    SEXO == "F" ~ "FEM"
  )) %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES, SEXO_LABEL) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = SEXO_LABEL, 
              values_from = NEOPLASIAS:Linfatico, 
              names_glue = "{.value}_{SEXO_LABEL}",
              values_fill = 0)

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2018 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
  select(-ends_with("_NA"))

####  Faixa Etária

quebras <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
rotulos <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2018 <- DOPR2018 %>%
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
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
  filter(RS == 22) %>%
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  pull(Código_IBGE)

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018 <- DOPR2018 %>%
  filter(CODMUNRES %in% Municipios_22RS) %>% 
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
                          TRUE ~ NA_character_ 
    ) ) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, 
              values_from = Casos, 
              values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()

write.csv (PR_PEVASPEA_SIM_Congenitas_Geral_2018, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_2018.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Fetal_2018, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2018.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Infantil_2018, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantil_2018.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2018, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2018.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2018, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2018.csv", 
           row.names = FALSE)

write.csv (RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2018, 
           "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018.csv", 
           row.names = FALSE)

##############################################################
##################   2019  ###################################
##############################################################

DOPR2019 <- read.dbf(file = "Base_de_Dados/DBF/DOPR2019.dbf", 
                     as.is = TRUE)

DOPR2019$IDADE <- as.numeric(DOPR2019$IDADE)

DOPR2019$QTDFILMORT <- as.numeric(DOPR2019$QTDFILMORT)

SINASC2019 <- read.csv(file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2019.csv",
                       header = TRUE,
                       sep = ",")

##### Tabela geral Estado do Paraná

AUX <- SINASC2019[-nrow(SINASC2019),c(1:5)]

AUX$Obitos_Fetais <- NA

AUX$Obitos_Fetais_Malformacoes <- NA

AUX$Obito_Infantil <- NA

AUX$Obito_Infantil_Malformacao <- NA

AUX$Obito_Fetal_Infantil_Geral_Nasc_Mortos <- NA

AUX$Obito_Fetal_Infantil_Anomalias_Nasc_Mortos <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2019 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 1) %>%   
                                                      count()
  )   
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2019 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2019 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400)%>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2019 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2019 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2019 %>% 
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

assign("PR_PEVASPEA_SIM_Congenitas_Geral_2019", AUX)

### Estabelecendo Objetos com REGEX para simplificar busca de CID

CID_Prioritaria <- "Q00|Q01|Q02|Q05|Q2|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90"

CID_Tubo_Neural <- "Q00|Q01|Q05"

CID_Microcefalia <- "Q02"

CID_Cardiopatia <- "Q2"

CID_Fenda <- "Q35|Q36|Q37"

CID_Genitais <- "Q54|Q56"

CID_Membros <- "Q66|Q69|Q71|Q72|Q73|Q743"

CID_Abd <- "Q792|Q793"

CID_Down <- "Q90"

##### Prioritárias Fetal

DOPR2019_ESTRATIFICADA <- DOPR2019 %>%
  filter(TIPOBITO == 1) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2019_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2019_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Fetal_2019", AUX_FINAL)

##### Prioritárias Óbito infantil

DOPR2019_ESTRATIFICADA <- DOPR2019 %>%
  filter(TIPOBITO == 2, 
         IDADE < 401) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2019_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município_sem_Código = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantis_2019_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Infantil_2019", AUX_FINAL)

####### Câncer

AUX <- DOPR2019 %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)))

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2019 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(NEOPLASIAS:Linfatico, \(x) replace_na(x, 0)))

##### Por sexo

AUX <- DOPR2019 %>%
  mutate(SEXO_LABEL = case_when(
    SEXO == "M" ~ "MASC",
    SEXO == "F" ~ "FEM"
  )) %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES, SEXO_LABEL) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = SEXO_LABEL, 
              values_from = NEOPLASIAS:Linfatico, 
              names_glue = "{.value}_{SEXO_LABEL}",
              values_fill = 0)

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2019 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
  select(-ends_with("_NA"))

####  Faixa Etária

quebras <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
rotulos <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2019 <- DOPR2019 %>%
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
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
  filter(RS == 22) %>%
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  pull(Código_IBGE)

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019 <- DOPR2019 %>%
  filter(CODMUNRES %in% Municipios_22RS) %>% 
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
                          TRUE ~ NA_character_ 
    ) ) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, 
              values_from = Casos, 
              values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()

write.csv (PR_PEVASPEA_SIM_Congenitas_Geral_2019, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_2019.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Fetal_2019, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2019.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Infantil_2019, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantil_2019.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2019, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2019.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2019, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2019.csv", 
           row.names = FALSE)

write.csv (RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2019, 
           "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019.csv", 
           row.names = FALSE)


##############################################################
##################   2020  ###################################
##############################################################

DOPR2020 <- read.dbf(file = "Base_de_Dados/DBF/DOPR2020.dbf", 
                     as.is = TRUE)

DOPR2020$IDADE <- as.numeric(DOPR2020$IDADE)

DOPR2020$QTDFILMORT <- as.numeric(DOPR2020$QTDFILMORT)

SINASC2020 <- read.csv(file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2020.csv",
                       header = TRUE,
                       sep = ",")

##### Tabela geral Estado do Paraná

AUX <- SINASC2020[-nrow(SINASC2020),c(1:5)]

AUX$Obitos_Fetais <- NA

AUX$Obitos_Fetais_Malformacoes <- NA

AUX$Obito_Infantil <- NA

AUX$Obito_Infantil_Malformacao <- NA

AUX$Obito_Fetal_Infantil_Geral_Nasc_Mortos <- NA

AUX$Obito_Fetal_Infantil_Anomalias_Nasc_Mortos <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2020 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 1) %>%   
                                                      count()
  )   
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2020 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2020 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400)%>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2020 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2020 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2020 %>% 
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

assign("PR_PEVASPEA_SIM_Congenitas_Geral_2020", AUX)

### Estabelecendo Objetos com REGEX para simplificar busca de CID

CID_Prioritaria <- "Q00|Q01|Q02|Q05|Q2|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90"

CID_Tubo_Neural <- "Q00|Q01|Q05"

CID_Microcefalia <- "Q02"

CID_Cardiopatia <- "Q2"

CID_Fenda <- "Q35|Q36|Q37"

CID_Genitais <- "Q54|Q56"

CID_Membros <- "Q66|Q69|Q71|Q72|Q73|Q743"

CID_Abd <- "Q792|Q793"

CID_Down <- "Q90"

##### Prioritárias Fetal

DOPR2020_ESTRATIFICADA <- DOPR2020 %>%
  filter(TIPOBITO == 1) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2020_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2020_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Fetal_2020", AUX_FINAL)

##### Prioritárias Óbito infantil

DOPR2020_ESTRATIFICADA <- DOPR2020 %>%
  filter(TIPOBITO == 2, 
         IDADE < 401) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2020_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município_sem_Código = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantis_2020_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Infantil_2020", AUX_FINAL)

####### Câncer

AUX <- DOPR2020 %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)))

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2020 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(NEOPLASIAS:Linfatico, \(x) replace_na(x, 0)))

##### Por sexo

AUX <- DOPR2020 %>%
  mutate(SEXO_LABEL = case_when(
    SEXO == "M" ~ "MASC",
    SEXO == "F" ~ "FEM"
  )) %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES, SEXO_LABEL) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = SEXO_LABEL, 
              values_from = NEOPLASIAS:Linfatico, 
              names_glue = "{.value}_{SEXO_LABEL}",
              values_fill = 0)

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2020 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
  select(-ends_with("_NA"))

####  Faixa Etária

quebras <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
rotulos <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2020 <- DOPR2020 %>%
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
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
  filter(RS == 22) %>%
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  pull(Código_IBGE)

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020 <- DOPR2020 %>%
  filter(CODMUNRES %in% Municipios_22RS) %>% 
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
                          TRUE ~ NA_character_ 
    ) ) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, 
              values_from = Casos, 
              values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()

write.csv (PR_PEVASPEA_SIM_Congenitas_Geral_2020, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_2020.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Fetal_2020, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2020.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Infantil_2020, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantil_2020.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2020, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2020.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2020, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2020.csv", 
           row.names = FALSE)

write.csv (RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2020, 
           "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020.csv", 
           row.names = FALSE)


##############################################################
##################   2021  ###################################
##############################################################

DOPR2021 <- read.dbf(file = "Base_de_Dados/DBF/DOPR2021.dbf", 
                     as.is = TRUE)

DOPR2021$IDADE <- as.numeric(DOPR2021$IDADE)

DOPR2021$QTDFILMORT <- as.numeric(DOPR2021$QTDFILMORT)

SINASC2021 <- read.csv(file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2021.csv",
                       header = TRUE,
                       sep = ",")

##### Tabela geral Estado do Paraná

AUX <- SINASC2021[-nrow(SINASC2021),c(1:5)]

AUX$Obitos_Fetais <- NA

AUX$Obitos_Fetais_Malformacoes <- NA

AUX$Obito_Infantil <- NA

AUX$Obito_Infantil_Malformacao <- NA

AUX$Obito_Fetal_Infantil_Geral_Nasc_Mortos <- NA

AUX$Obito_Fetal_Infantil_Anomalias_Nasc_Mortos <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2021 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 1) %>%   
                                                      count()
  )   
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2021 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2021 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400)%>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2021 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2021 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2021 %>% 
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

assign("PR_PEVASPEA_SIM_Congenitas_Geral_2021", AUX)

### Estabelecendo Objetos com REGEX para simplificar busca de CID

CID_Prioritaria <- "Q00|Q01|Q02|Q05|Q2|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90"

CID_Tubo_Neural <- "Q00|Q01|Q05"

CID_Microcefalia <- "Q02"

CID_Cardiopatia <- "Q2"

CID_Fenda <- "Q35|Q36|Q37"

CID_Genitais <- "Q54|Q56"

CID_Membros <- "Q66|Q69|Q71|Q72|Q73|Q743"

CID_Abd <- "Q792|Q793"

CID_Down <- "Q90"

##### Prioritárias Fetal


DOPR2021_ESTRATIFICADA <- DOPR2021 %>%
  filter(TIPOBITO == 1) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2021_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2021_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Fetal_2021", AUX_FINAL)

##### Prioritárias Óbito infantil

DOPR2021_ESTRATIFICADA <- DOPR2021 %>%
  filter(TIPOBITO == 2, 
         IDADE < 401) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2021_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município_sem_Código = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantis_2021_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Infantil_2021", AUX)

####### Câncer

AUX <- DOPR2021 %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)))

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2021 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(NEOPLASIAS:Linfatico, \(x) replace_na(x, 0)))

##### Por sexo

AUX <- DOPR2021 %>%
  mutate(SEXO_LABEL = case_when(
    SEXO == "M" ~ "MASC",
    SEXO == "F" ~ "FEM"
  )) %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES, SEXO_LABEL) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = SEXO_LABEL, 
              values_from = NEOPLASIAS:Linfatico, 
              names_glue = "{.value}_{SEXO_LABEL}",
              values_fill = 0)

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2021 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
  select(-ends_with("_NA"))

####  Faixa Etária

quebras <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
rotulos <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2021 <- DOPR2021 %>%
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
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
  filter(RS == 22) %>%
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  pull(Código_IBGE)

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021 <- DOPR2021 %>%
  filter(CODMUNRES %in% Municipios_22RS) %>% 
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
                          TRUE ~ NA_character_ 
    ) ) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, 
              values_from = Casos, 
              values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()

write.csv (PR_PEVASPEA_SIM_Congenitas_Geral_2021, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_2021.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Fetal_2021, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2021.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Infantil_2021, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantil_2021.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2021, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2021.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2021, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2021.csv", 
           row.names = FALSE)

write.csv (RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2021, 
           "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021.csv", 
           row.names = FALSE)


##############################################################
##################   2022  ###################################
##############################################################

DOPR2022 <- read.dbf(file = "Base_de_Dados/DBF/DOPR2022.dbf", 
                     as.is = TRUE)

DOPR2022$IDADE <- as.numeric(DOPR2022$IDADE)

DOPR2022$QTDFILMORT <- as.numeric(DOPR2022$QTDFILMORT)

SINASC2022 <- read.csv(file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2022.csv",
                       header = TRUE,
                       sep = ",")

##### Tabela geral Estado do Paraná

AUX <- SINASC2022[-nrow(SINASC2022),c(1:5)]

AUX$Obitos_Fetais <- NA

AUX$Obitos_Fetais_Malformacoes <- NA

AUX$Obito_Infantil <- NA

AUX$Obito_Infantil_Malformacao <- NA

AUX$Obito_Fetal_Infantil_Geral_Nasc_Mortos <- NA

AUX$Obito_Fetal_Infantil_Anomalias_Nasc_Mortos <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2022 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 1) %>%   
                                                      count()
  )   
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2022 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2022 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400)%>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2022 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2022 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2022 %>% 
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

assign("PR_PEVASPEA_SIM_Congenitas_Geral_2022", AUX)

### Estabelecendo Objetos com REGEX para simplificar busca de CID

CID_Prioritaria <- "Q00|Q01|Q02|Q05|Q2|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90"

CID_Tubo_Neural <- "Q00|Q01|Q05"

CID_Microcefalia <- "Q02"

CID_Cardiopatia <- "Q2"

CID_Fenda <- "Q35|Q36|Q37"

CID_Genitais <- "Q54|Q56"

CID_Membros <- "Q66|Q69|Q71|Q72|Q73|Q743"

CID_Abd <- "Q792|Q793"

CID_Down <- "Q90"

##### Prioritárias Fetal
DOPR2022_ESTRATIFICADA <- DOPR2022 %>%
  filter(TIPOBITO == 1) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2022_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2022_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Fetal_2022", AUX_FINAL)

##### Prioritárias Óbito infantil

DOPR2022_ESTRATIFICADA <- DOPR2022 %>%
  filter(TIPOBITO == 2, 
         IDADE < 401) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2022_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município_sem_Código = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantis_2022_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Infantil_2022", AUX_FINAL)

####### Câncer

AUX <- DOPR2022 %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)))

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2022 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(NEOPLASIAS:Linfatico, \(x) replace_na(x, 0)))

##### Por sexo

AUX <- DOPR2022 %>%
  mutate(SEXO_LABEL = case_when(
    SEXO == "M" ~ "MASC",
    SEXO == "F" ~ "FEM"
  )) %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES, SEXO_LABEL) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = SEXO_LABEL, 
              values_from = NEOPLASIAS:Linfatico, 
              names_glue = "{.value}_{SEXO_LABEL}",
              values_fill = 0)

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2022 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
  select(-ends_with("_NA"))

####  Faixa Etária

quebras <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
rotulos <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2022 <- DOPR2022 %>%
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
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
  filter(RS == 22) %>%
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  pull(Código_IBGE)

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022 <- DOPR2022 %>%
  filter(CODMUNRES %in% Municipios_22RS) %>% 
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
                          TRUE ~ NA_character_ 
    ) ) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, 
              values_from = Casos, 
              values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()

write.csv (PR_PEVASPEA_SIM_Congenitas_Geral_2022, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_2022.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Fetal_2022, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2022.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Infantil_2022, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantil_2022.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2022, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2022.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2022, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2022.csv", 
           row.names = FALSE)

write.csv (RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2022, 
           "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022.csv", 
           row.names = FALSE)
##############################################################
##################   2023  ###################################
##############################################################

DOPR2023 <- read.dbf(file = "Base_de_Dados/DBF/DOPR2023.dbf", 
                     as.is = TRUE)

DOPR2023$IDADE <- as.numeric(DOPR2023$IDADE)

DOPR2023$QTDFILMORT <- as.numeric(DOPR2023$QTDFILMORT)

SINASC2023 <- read.csv(file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2023.csv",
                       header = TRUE,
                       sep = ",")

##### Tabela geral Estado do Paraná

AUX <- SINASC2023[-nrow(SINASC2023),c(1:5)]

AUX$Obitos_Fetais <- NA

AUX$Obitos_Fetais_Malformacoes <- NA

AUX$Obito_Infantil <- NA

AUX$Obito_Infantil_Malformacao <- NA

AUX$Obito_Fetal_Infantil_Geral_Nasc_Mortos <- NA

AUX$Obito_Fetal_Infantil_Anomalias_Nasc_Mortos <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2023 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 1) %>%   
                                                      count()
  )   
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2023 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2023 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400)%>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2023 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2023 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2023 %>% 
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

assign("PR_PEVASPEA_SIM_Congenitas_Geral_2023", AUX)

### Estabelecendo Objetos com REGEX para simplificar busca de CID

CID_Prioritaria <- "Q00|Q01|Q02|Q05|Q2|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90"

CID_Tubo_Neural <- "Q00|Q01|Q05"

CID_Microcefalia <- "Q02"

CID_Cardiopatia <- "Q2"

CID_Fenda <- "Q35|Q36|Q37"

CID_Genitais <- "Q54|Q56"

CID_Membros <- "Q66|Q69|Q71|Q72|Q73|Q743"

CID_Abd <- "Q792|Q793"

CID_Down <- "Q90"

##### Prioritárias Fetal


DOPR2023_ESTRATIFICADA <- DOPR2023 %>%
  filter(TIPOBITO == 1) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2023_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2023_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Fetal_2023", AUX_FINAL)

##### Prioritárias Óbito infantil

DOPR2023_ESTRATIFICADA <- DOPR2023 %>%
  filter(TIPOBITO == 2, 
         IDADE < 401) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2023_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município_sem_Código = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantis_2023_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Infantil_2023", AUX_FINAL)

####### Câncer

AUX <- DOPR2023 %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)))

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2023 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(NEOPLASIAS:Linfatico, \(x) replace_na(x, 0)))

##### Por sexo

AUX <- DOPR2023 %>%
  mutate(SEXO_LABEL = case_when(
    SEXO == "M" ~ "MASC",
    SEXO == "F" ~ "FEM"
  )) %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES, SEXO_LABEL) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = SEXO_LABEL, 
              values_from = NEOPLASIAS:Linfatico, 
              names_glue = "{.value}_{SEXO_LABEL}",
              values_fill = 0)

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2023 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
  select(-ends_with("_NA"))

####  Faixa Etária

quebras <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
rotulos <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2023 <- DOPR2023 %>%
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
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
  filter(RS == 22) %>%
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  pull(Código_IBGE)

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023 <- DOPR2023 %>%
  filter(CODMUNRES %in% Municipios_22RS) %>% 
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
                          TRUE ~ NA_character_ 
    ) ) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, 
              values_from = Casos, 
              values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()

write.csv (PR_PEVASPEA_SIM_Congenitas_Geral_2023, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_2023.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Fetal_2023, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2023.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Infantil_2023, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantil_2023.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2023, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2023.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2023, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2023.csv", 
           row.names = FALSE)

write.csv (RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2023, 
           "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023.csv", 
           row.names = FALSE)

##############################################################
##################   2024  ###################################
##############################################################

DOPR2024 <- read.dbf(file = "Base_de_Dados/DBF/DOPR2024.dbf", 
                     as.is = TRUE)

DOPR2024$IDADE <- as.numeric(DOPR2024$IDADE)

DOPR2024$QTDFILMORT <- as.numeric(DOPR2024$QTDFILMORT)

SINASC2024 <- read.csv(file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2024.csv",
                       header = TRUE,
                       sep = ",")

##### Tabela geral Estado do Paraná

AUX <- SINASC2024[-nrow(SINASC2024),c(1:5)]

AUX$Obitos_Fetais <- NA

AUX$Obitos_Fetais_Malformacoes <- NA

AUX$Obito_Infantil <- NA

AUX$Obito_Infantil_Malformacao <- NA

AUX$Obito_Fetal_Infantil_Geral_Nasc_Mortos <- NA

AUX$Obito_Fetal_Infantil_Anomalias_Nasc_Mortos <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2024 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 1) %>%   
                                                      count()
  )   
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2024 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2024 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400)%>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2024 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2024 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2024 %>% 
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

assign("PR_PEVASPEA_SIM_Congenitas_Geral_2024", AUX)

### Estabelecendo Objetos com REGEX para simplificar busca de CID

CID_Prioritaria <- "Q00|Q01|Q02|Q05|Q2|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90"

CID_Tubo_Neural <- "Q00|Q01|Q05"

CID_Microcefalia <- "Q02"

CID_Cardiopatia <- "Q2"

CID_Fenda <- "Q35|Q36|Q37"

CID_Genitais <- "Q54|Q56"

CID_Membros <- "Q66|Q69|Q71|Q72|Q73|Q743"

CID_Abd <- "Q792|Q793"

CID_Down <- "Q90"

##### Prioritárias Fetal

DOPR2024_ESTRATIFICADA <- DOPR2024 %>%
  filter(TIPOBITO == 1) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2024_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2024_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Fetal_2024", AUX_FINAL)

##### Prioritárias Óbito infantil

DOPR2024_ESTRATIFICADA <- DOPR2024 %>%
  filter(TIPOBITO == 2, 
         IDADE < 401) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2024_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município_sem_Código = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantis_2024_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Infantil_2024", AUX_FINAL)

####### Câncer

AUX <- DOPR2024 %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)))

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2024 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(NEOPLASIAS:Linfatico, \(x) replace_na(x, 0)))

##### Por sexo

AUX <- DOPR2024 %>%
  mutate(SEXO_LABEL = case_when(
    SEXO == "M" ~ "MASC",
    SEXO == "F" ~ "FEM"
  )) %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES, SEXO_LABEL) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = SEXO_LABEL, 
              values_from = NEOPLASIAS:Linfatico, 
              names_glue = "{.value}_{SEXO_LABEL}",
              values_fill = 0)

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2024 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
  select(-ends_with("_NA"))

####  Faixa Etária

quebras <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
rotulos <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2024 <- DOPR2024 %>%
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
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
  filter(RS == 22) %>%
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  pull(Código_IBGE)

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024 <- DOPR2024 %>%
  filter(CODMUNRES %in% Municipios_22RS) %>% 
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
                          TRUE ~ NA_character_ 
    ) ) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, 
              values_from = Casos, 
              values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()

write.csv (PR_PEVASPEA_SIM_Congenitas_Geral_2024, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_2024.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Fetal_2024, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2024.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Infantil_2024, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantil_2024.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2024, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2024.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2024, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2024.csv", 
           row.names = FALSE)

write.csv (RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2024, 
           "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024.csv", 
           row.names = FALSE)

##############################################################
##################   2025  ###################################
##############################################################

DOPR2025 <- read.dbf(file = "Base_de_Dados/DBF/DOPR2025.dbf", 
                     as.is = TRUE)

DOPR2025$IDADE <- as.numeric(DOPR2025$IDADE)

DOPR2025$QTDFILMORT <- as.numeric(DOPR2025$QTDFILMORT)

SINASC2025 <- read.csv(file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2025.csv",
                       header = TRUE,
                       sep = ",")

##### Tabela geral Estado do Paraná

AUX <- SINASC2025[-nrow(SINASC2025),c(1:5)]

AUX$Obitos_Fetais <- NA

AUX$Obitos_Fetais_Malformacoes <- NA

AUX$Obito_Infantil <- NA

AUX$Obito_Infantil_Malformacao <- NA

AUX$Obito_Fetal_Infantil_Geral_Nasc_Mortos <- NA

AUX$Obito_Fetal_Infantil_Anomalias_Nasc_Mortos <- NA

#####For loop para criação de tabela por município dos dados de notificação do SINAN##############

for(i in BASE_IBGE[, 2]){
  
  AUX[which(AUX$Código_IBGE == i), 6] <- as.integer(DOPR2025 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 1) %>%   
                                                      count()
  )   
  
  AUX[which(AUX$Código_IBGE == i), 7] <- as.integer(DOPR2025 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 8] <- as.integer(DOPR2025 %>% 
                                                      filter(CODMUNRES == i,
                                                             TIPOBITO == 2,
                                                             IDADE >= 100 & 
                                                               IDADE < 400)%>%   
                                                      count()
  )
  
  AUX[which(AUX$Código_IBGE == i), 9] <- as.integer(DOPR2025 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 10] <- as.integer(DOPR2025 %>% 
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
  
  AUX[which(AUX$Código_IBGE == i), 11] <- as.integer(DOPR2025 %>% 
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

assign("PR_PEVASPEA_SIM_Congenitas_Geral_2025", AUX)

### Estabelecendo Objetos com REGEX para simplificar busca de CID

CID_Prioritaria <- "Q00|Q01|Q02|Q05|Q2|Q35|Q36|Q37|Q54|Q56|Q66|Q69|Q71|Q72|Q73|Q743|Q792|Q793|Q90"

CID_Tubo_Neural <- "Q00|Q01|Q05"

CID_Microcefalia <- "Q02"

CID_Cardiopatia <- "Q2"

CID_Fenda <- "Q35|Q36|Q37"

CID_Genitais <- "Q54|Q56"

CID_Membros <- "Q66|Q69|Q71|Q72|Q73|Q743"

CID_Abd <- "Q792|Q793"

CID_Down <- "Q90"

##### Prioritárias Fetal

DOPR2025_ESTRATIFICADA <- DOPR2025 %>%
  filter(TIPOBITO == 1) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2025_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2025_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Fetal_2025", AUX_FINAL)

##### Prioritárias Óbito infantil

DOPR2025_ESTRATIFICADA <- DOPR2025 %>%
  filter(TIPOBITO == 2, 
         IDADE < 401) %>%
  mutate(CAUSAS_CONCAT = paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII, sep = " ")) %>%
  mutate(
    Prioritarias = str_detect(CAUSAS_CONCAT, CID_Prioritaria),
    Tubo_Neural = str_detect(CAUSAS_CONCAT, CID_Tubo_Neural),
    Microcefalia = str_detect(CAUSAS_CONCAT, "Q02"),
    Cardiopatias = str_detect(CAUSAS_CONCAT, CID_Cardiopatia),
    Fendas_Orais = str_detect(CAUSAS_CONCAT, CID_Fenda),
    Genitais     = str_detect(CAUSAS_CONCAT, CID_Genitais),
    Membros      = str_detect(CAUSAS_CONCAT, CID_Membros),
    Parede_Abd   = str_detect(CAUSAS_CONCAT, CID_Abd),
    Sindrome_Down = str_detect(CAUSAS_CONCAT, CID_Down)
  )

RESUMO_MUNICIPIOS <- DOPR2025_ESTRATIFICADA %>%
  group_by(CODMUNRES) %>%
  summarise(
    Prioritarias = sum(Prioritarias, na.rm = TRUE),
    Anomalia_Prioritaria_Tubo_Neural = sum(Tubo_Neural, na.rm = TRUE),
    Anomalia_Prioritaria_Microcefalia = sum(Microcefalia, na.rm = TRUE),
    Anomalia_Prioritaria_Cardiopatias = sum(Cardiopatias, na.rm = TRUE),
    Anomalia_Prioritaria_Fendas_Orais = sum(Fendas_Orais, na.rm = TRUE),
    Anomalia_Prioritaria_Genitais = sum(Genitais, na.rm = TRUE),
    Anomalia_Prioritaria_Membros = sum(Membros, na.rm = TRUE),
    Anomalia_Prioritaria_Parede_Abd = sum(Parede_Abd, na.rm = TRUE),
    Anomalia_Prioritaria_Sindrome_Down = sum(Sindrome_Down, na.rm = TRUE)
  )

AUX <- BASE_IBGE %>%
  select(Código_IBGE, Município_sem_Código, RS) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPIOS, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0)))

AUX_FINAL <- AUX %>%
  bind_rows(
    summarise(., 
              Município_sem_Código = "Total", 
              across(where(is.numeric), sum))
  )

write.csv(AUX_FINAL, "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantis_2025_Otimizado.csv", 
          row.names = FALSE)

assign("PR_PEVASPEA_SIM_Prioritarias_Infantil_2025", AUX_FINAL)

####### Câncer

AUX <- DOPR2025 %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)))

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2025 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(NEOPLASIAS:Linfatico, \(x) replace_na(x, 0)))

##### Por sexo

AUX <- DOPR2025 %>%
  mutate(SEXO_LABEL = case_when(
    SEXO == "M" ~ "MASC",
    SEXO == "F" ~ "FEM"
  )) %>%
  mutate(CAUSABAS = as.character(CAUSABAS)) %>%
  mutate(NEOPLASIAS     = if_else(str_detect(CAUSABAS, "C"), 1, 0, missing = 0),
         Labio_Cav_Oral      = if_else(str_detect(CAUSABAS, CID_LABIO), 1, 0, missing = 0),
         Digestivo           = if_else(str_detect(CAUSABAS, CID_DIGESTIVO), 1, 0, missing = 0),
         Respiratorio   = if_else(str_detect(CAUSABAS, CID_RESPIRATORIO), 1, 0, missing = 0),
         Ossos          = if_else(str_detect(CAUSABAS, CID_OSSOS), 1, 0, missing = 0),
         Pele           = if_else(str_detect(CAUSABAS, CID_PELE), 1, 0, missing = 0),
         Tec_Mole       = if_else(str_detect(CAUSABAS, CID_TEC_MOLE), 1, 0, missing = 0),
         Mama           = if_else(str_detect(CAUSABAS, CID_MAMA), 1, 0, missing = 0),
         Gen_Fem        = if_else(str_detect(CAUSABAS, CID_GEN_FEM), 1, 0, missing = 0),
         Gen_Masc       = if_else(str_detect(CAUSABAS, CID_GEN_MASC), 1, 0, missing = 0),
         Via_Urinaria   = if_else(str_detect(CAUSABAS, CID_VIA_URINARIA), 1, 0, missing = 0),
         Cerebro        = if_else(str_detect(CAUSABAS, CID_CEREBRO), 1, 0, missing = 0),
         Tireoide_Endo  = if_else(str_detect(CAUSABAS, CID_TIREOIDE), 1, 0, missing = 0),
         Mal_Def        = if_else(str_detect(CAUSABAS, CID_MAL_DEFINIDAS), 1, 0, missing = 0),
         Linfatico      = if_else(str_detect(CAUSABAS,CID_LINFATICO), 1, 0, missing = 0)
  )

RESUMO_MUNICIPAL <- AUX %>%
  group_by(CODMUNRES, SEXO_LABEL) %>%
  summarise(across(NEOPLASIAS:Linfatico, \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_wider(names_from = SEXO_LABEL, 
              values_from = NEOPLASIAS:Linfatico, 
              names_glue = "{.value}_{SEXO_LABEL}",
              values_fill = 0)

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2025 <- BASE_IBGE %>%
  select(-c(4,6)) %>% 
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  left_join(RESUMO_MUNICIPAL, by = c("Código_IBGE" = "CODMUNRES")) %>%
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
  select(-ends_with("_NA"))

####  Faixa Etária

quebras <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
rotulos <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
             "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")

PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2025 <- DOPR2025 %>%
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
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
  filter(RS == 22) %>%
  mutate(Código_IBGE = as.character(Código_IBGE)) %>% 
  pull(Código_IBGE)

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025 <- DOPR2025 %>%
  filter(CODMUNRES %in% Municipios_22RS) %>% 
  filter(str_detect(CAUSABAS, "C")) %>%
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
    Grupo_CID = case_when(str_detect(CAUSABAS, CID_LINFATICO) ~ "Linfatico_Hematologico",
                          str_detect(CAUSABAS, CID_CEREBRO) ~ "Cerebro_SNC",
                          str_detect(CAUSABAS, CID_DIGESTIVO) ~ "Digestivo",
                          str_detect(CAUSABAS, CID_RESPIRATORIO) ~ "Respiratorio",
                          str_detect(CAUSABAS, CID_GEN_FEM) ~ "Gen_Fem",
                          str_detect(CAUSABAS, CID_GEN_MASC) ~ "Gen_Masc",
                          str_detect(CAUSABAS, CID_MAMA) ~ "Mama",
                          str_detect(CAUSABAS, CID_VIA_URINARIA) ~ "Via_Urinaria",
                          str_detect(CAUSABAS, CID_PELE) ~ "Pele",
                          str_detect(CAUSABAS, CID_LABIO) ~ "Labio_Oral",
                          str_detect(CAUSABAS, CID_OSSOS) ~ "Ossos",
                          str_detect(CAUSABAS, CID_TEC_MOLE) ~ "Tec_Mole",
                          str_detect(CAUSABAS, CID_TIREOIDE) ~ "Tireoide_Endo",
                          str_detect(CAUSABAS, CID_MAL_DEFINIDAS) ~ "Mal_Definidas",
                          TRUE ~ NA_character_ 
    ) ) %>%
  drop_na(Grupo_CID, Faixa_Etaria) %>%
  group_by(Grupo_CID, Faixa_Etaria) %>%
  summarise(Casos = n(), .groups = "drop") %>%
  pivot_wider(names_from = Faixa_Etaria, 
              values_from = Casos, 
              values_fill = 0) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(any_of(rotulos)))) %>%
  ungroup()

write.csv (PR_PEVASPEA_SIM_Congenitas_Geral_2025, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_2025.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Fetal_2025, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_2025.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_Prioritarias_Infantil_2025, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Infantil_2025.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2025, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2025.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2025, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_SEXO_2025.csv", 
           row.names = FALSE)

write.csv (RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025.csv", 
           row.names = FALSE)

write.csv (PR_PEVASPEA_SIM_NEOPLASIA_IDADE_2025, 
           "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025.csv", 
           row.names = FALSE)

#################  Criando objetos geral

#### Série Histórica PR Congênitas Geral

AUX <- PR_PEVASPEA_SIM_Congenitas_Geral_2016[nrow(PR_PEVASPEA_SIM_Congenitas_Geral_2016), ]
AUX[1, 1] <- "2016"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2017[nrow(PR_PEVASPEA_SIM_Congenitas_Geral_2017),]
AUX[2, 1] <- "2017"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2018[nrow(PR_PEVASPEA_SIM_Congenitas_Geral_2018),]
AUX[3, 1] <- "2018"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2019[nrow(PR_PEVASPEA_SIM_Congenitas_Geral_2019),]
AUX[4, 1] <- "2019"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2020[nrow(PR_PEVASPEA_SIM_Congenitas_Geral_2020),]
AUX[5, 1] <- "2020"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2021[nrow(PR_PEVASPEA_SIM_Congenitas_Geral_2021),]
AUX[6, 1] <- "2021"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2022[nrow(PR_PEVASPEA_SIM_Congenitas_Geral_2022),]
AUX[7, 1] <- "2022"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2023[nrow(PR_PEVASPEA_SIM_Congenitas_Geral_2023),]
AUX[8, 1] <- "2023"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2024[nrow(PR_PEVASPEA_SIM_Congenitas_Geral_2024),]
AUX[9, 1] <- "2024"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2025[nrow(PR_PEVASPEA_SIM_Congenitas_Geral_2025),]
AUX[10, 1] <- "2025"

write.csv (AUX, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_Serie_Hist.csv", 
           row.names = FALSE)

#### Série Histórica PR Congênitas RS
AUX <- PR_PEVASPEA_SIM_Congenitas_Geral_2016 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2016")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2017 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2017")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2018 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2018")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2019 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2019")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2020 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2020")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2021 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2021")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2022 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2022")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2023 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2023")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2024 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2024")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Congenitas_Geral_2025 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2025")

write.csv (AUX, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_Congenitas_Geral_Serie_Hist.csv", 
           row.names = FALSE)

#### Série Histórica PR Prioritarias_Fetal

AUX <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2016[nrow(PR_PEVASPEA_SIM_Prioritarias_Fetal_2016),]
AUX[1, 1] <- "2016"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2017[nrow(PR_PEVASPEA_SIM_Prioritarias_Fetal_2017), -ncol(PR_PEVASPEA_SIM_Prioritarias_Fetal_2017)]
AUX[2, 1] <- "2017"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2018[nrow(PR_PEVASPEA_SIM_Prioritarias_Fetal_2018), -ncol(PR_PEVASPEA_SIM_Prioritarias_Fetal_2018)]
AUX[3, 1] <- "2018"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2019[nrow(PR_PEVASPEA_SIM_Prioritarias_Fetal_2019), -ncol(PR_PEVASPEA_SIM_Prioritarias_Fetal_2019)]
AUX[4, 1] <- "2019"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2020[nrow(PR_PEVASPEA_SIM_Prioritarias_Fetal_2020), -ncol(PR_PEVASPEA_SIM_Prioritarias_Fetal_2020)]
AUX[5, 1] <- "2020"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2021[nrow(PR_PEVASPEA_SIM_Prioritarias_Fetal_2021), -ncol(PR_PEVASPEA_SIM_Prioritarias_Fetal_2021)]
AUX[6, 1] <- "2021"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2022[nrow(PR_PEVASPEA_SIM_Prioritarias_Fetal_2022), -ncol(PR_PEVASPEA_SIM_Prioritarias_Fetal_2022)]
AUX[7, 1] <- "2022"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2023[nrow(PR_PEVASPEA_SIM_Prioritarias_Fetal_2023), -ncol(PR_PEVASPEA_SIM_Prioritarias_Fetal_2023)]
AUX[8, 1] <- "2023"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2024[nrow(PR_PEVASPEA_SIM_Prioritarias_Fetal_2024), -ncol(PR_PEVASPEA_SIM_Prioritarias_Fetal_2024)]
AUX[9, 1] <- "2024"
AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2025[nrow(PR_PEVASPEA_SIM_Prioritarias_Fetal_2025), -ncol(PR_PEVASPEA_SIM_Prioritarias_Fetal_2025)]
AUX[10, 1] <- "2025"

write.csv (AUX, 
           "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_Serie_Hist.csv", 
           row.names = FALSE)

#### Série Histórica PR Prioritarias_Fetal RS

AUX <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2016 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2016")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2017 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2017")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2018 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2018")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2019 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2019")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2020 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2020")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2021 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2021")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2022 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2022")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2023 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2023")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2024 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2024")

AUX[nrow(AUX) +1, ] <- PR_PEVASPEA_SIM_Prioritarias_Fetal_2025 %>%
  filter(RS == 22)  %>%
  summarise(across(4:11, sum, na.rm = TRUE)) %>%
  mutate(Ano = "2025")

write.csv (AUX, 
           "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_Prioritaria_Fetal_Serie_Hist.csv", 
           row.names = FALSE)
