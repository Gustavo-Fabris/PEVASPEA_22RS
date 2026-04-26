rm(list = ls())

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

#########################################################################################

####  libraries a serem utilizadas  ###

library(foreign)
library (dplyr)
library(stringr)
library(lubridate)
library(tidyr)

####  Importando as bases de dados para formulação do Informe Epidemiológico      ####
####  As tabelas .CSV do DERAL tiveram os nomes das colunas alterados após serem baixadas

BASE_IBGE<-read.table(file="Base_de_Dados/Auxiliares/Planilha_Base_IBGE.csv", 
                      header=TRUE, 
                      sep=",")

BASE_IBGE_BRASIL <- read.csv (file = "Base_de_Dados/Auxiliares/Planilha_Base_IBGE_BRASIL.csv",
                              header = TRUE,
                              sep = ",")

VBP_2016 <- read.csv(file = "Base_de_Dados/DERAL/VBP2016.csv",
                                      header = TRUE,
                                      sep = ",")

VBP_2017 <- read.csv(file = "Base_de_Dados/DERAL/VBP2017.csv",
                     header = TRUE,
                     sep = ",")

VBP_2018 <- read.csv(file = "Base_de_Dados/DERAL/VBP2018.csv",
                     header = TRUE,
                     sep = ",")

VBP_2019 <- read.csv(file = "Base_de_Dados/DERAL/VBP2019.csv",
                     header = TRUE,
                     sep = ",")

VBP_2020 <- read.csv(file = "Base_de_Dados/DERAL/VBP2020.csv",
                     header = TRUE,
                     sep = ",")

VBP_2021 <- read.csv(file = "Base_de_Dados/DERAL/VBP2021.csv",
                     header = TRUE,
                     sep = ",")

VBP_2022 <- read.csv(file = "Base_de_Dados/DERAL/VBP2022.csv",
                     header = TRUE,
                     sep = ",")

VBP_2023 <- read.csv(file = "Base_de_Dados/DERAL/VBP2023.csv",
                     header = TRUE,
                     sep = ",")

VBP_2024 <- read.csv(file = "Base_de_Dados/DERAL/VBP2024.csv",
                     header = TRUE,
                     sep = ",")

SIAGRO_2025 <- read.csv(file = "Base_de_Dados/SIAGRO/SIAGRO_2024_atualizado_Nov_25.csv",
                        header = TRUE,
                        sep = ",")

#######  2016

### Corrigindo os nomes dos municípios das 
### bases de dados que serão utilizadas

VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "ARAPUAN", "ARAPUA")
VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "ITAPEJARA DO OESTE", "ITAPEJARA DOESTE")
VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "PEROLA DO OESTE", "PEROLA DOESTE")
VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "SAO JORGE DO OESTE", "SAO JORGE DOESTE")
VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "SANTA TEREZINHA DO ITAIPU", "SANTA TEREZINHA DE ITAIPU")
VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "RANCHO ALEGRE DO OESTE", "RANCHO ALEGRE DOESTE")
VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "SAUDADES DO IGUACU", "SAUDADE DO IGUACU")
VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "SANTA CRUZ DO MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "SANTA IZABEL DO IVAI", "SANTA ISABEL DO IVAI")
VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "MUNHOZ DE MELO", "MUNHOZ DE MELLO")
VBP_2016$MUNICIPIO <- str_replace(VBP_2016$MUNICIPIO, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

VBP_2016$VALOR_Reais <- as.numeric(gsub(",", ".", VBP_2016$VALOR_Reais))
VBP_2016$PRODUCAO <- as.numeric(gsub(",", ".", VBP_2016$PRODUCAO))
VBP_2016$AREA_HA <- as.numeric(gsub(",", ".", VBP_2016$AREA_HA))

#######   Fazendo a soma de valor total produzido de cada município

## Vai ser filtrado os cultivos em que o SIAGRO 2025 
## aponta como que utilizam agrotóxicos
Filter_Culturas <- c("SOJA SAFRA NORMAL", 
                     "SOJA SAFRINHA",
                     "SILAGEM DE MILHO E/OU SORGO",
                     "MILHO VERDE (espiga)", 
                     "MILHO SAFRA NORMAL", 
                     "MILHO SAFRINHA",
                     "TRIGO", 
                     "FEIJAO SAFRA DA SECA",
                     "FEIJAO SAFRA DAS AGUAS",
                     "FEIJAO SAFRA DE INVERNO",
                     "FEIJAO-VAGEM",
                     "PASTAGENS E FORRAGENS",
                     "BATATA DA SECA",
                     "BATATA DAS AGUAS",
                     "CANA-DE-ACUCAR",
                     "FUMO",
                     "UVA DE MESA",
                     "UVA VINIFERA",
                     "TOMATE SAFRAO",
                     "CAFE",
                     "LARANJA",
                     "LIMAO",
                     "TANGERINA MONTENEGRINA",
                     "TANGERINA PONKAN", 
                     "TANGERINA MURCOTE", 
                     "MANDIOCA CONSUMO (HUMANO)", 
                     "MANDIOCA INDUSTRIA",
                     "AVEIA BRANCA",
                     "AVEIA PRETA (GRAO)",
                     "CEBOLA",
                     "ARROZ IRRIGADO",
                     "ARROZ SEQUEIRO",
                     "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO",
                     "MACA",
                     "CEVADA",
                     "MADEIRAS - EM TORA P/LAMINADORA - PINUS", 
                     "MADEIRAS - EM TORA P/SERRARIA - PINUS",
                     "AZEVEM GRAOS",
                     "MELANCIA",
                     "BANANA",
                     "PEPINO",
                     "AMENDOIM SAFRA DAS AGUAS",
                     "PESSEGO",
                     "PIMENTAO",
                     "REPOLHO",
                     "MORANGO (moranguinho)")

AUX <- VBP_2016 %>%
  filter(CULTURA == "SOJA SAFRA NORMAL" |
           CULTURA == "SOJA SAFRINHA" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "MILHO SAFRA NORMAL" |
           CULTURA == "MILHO SAFRINHA" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJAO SAFRA DA SECA" |
           CULTURA == "FEIJAO SAFRA DAS AGUAS" |
           CULTURA == "FEIJAO SAFRA DE INVERNO" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA DA SECA" |
           CULTURA == "BATATA DAS AGUAS" |
           CULTURA == "CANA-DE-ACUCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "TOMATE SAFRAO" |
           CULTURA == "CAFE" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
           CULTURA == "MANDIOCA INDUSTRIA" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA (GRAO)" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
    group_by(MUNICIPIO) %>%
  summarise(sum(VALOR_Reais))

### Criando um objeto com os dados do DERAL/SIAGRO

PR_DERAL_2016_SIMPLIFICADO <- BASE_IBGE[, c(1, 2, 3, 5)]

PR_DERAL_2016_SIMPLIFICADO$Município_sem_Código <- iconv(PR_DERAL_2016_SIMPLIFICADO$Município_sem_Código,
                                                    from = "UTF-8", 
                                                    to = "ASCII//TRANSLIT")

PR_DERAL_2016_SIMPLIFICADO <- left_join(PR_DERAL_2016_SIMPLIFICADO,
                                   AUX,
                                   by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2016_SIMPLIFICADO)[5] <- "VALOR_REAIS"

#######   Fazendo a soma da área total cultivada de cada município

VBP_2016$AREA_HA[VBP_2016$AREA_HA == ""] <- NA

AUX <- VBP_2016 %>%
  filter(!is.na(AREA_HA),
         CULTURA == "SOJA SAFRA NORMAL" |
           CULTURA == "SOJA SAFRINHA" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "MILHO SAFRA NORMAL" |
           CULTURA == "MILHO SAFRINHA" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJAO SAFRA DA SECA" |
           CULTURA == "FEIJAO SAFRA DAS AGUAS" |
           CULTURA == "FEIJAO SAFRA DE INVERNO" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA DA SECA" |
           CULTURA == "BATATA DAS AGUAS" |
           CULTURA == "CANA-DE-ACUCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "TOMATE SAFRAO" |
           CULTURA == "CAFE" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
           CULTURA == "MANDIOCA INDUSTRIA" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA (GRAO)" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
    summarise(sum(AREA_HA))

PR_DERAL_2016_SIMPLIFICADO <- left_join(PR_DERAL_2016_SIMPLIFICADO,
                                   AUX,
                                   by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2016_SIMPLIFICADO)[6] <- "AREA_HA"

#####  Separando as cinco maiores culturas de cada município

AUX <- VBP_2016 %>%filter(CULTURA == "SOJA SAFRA NORMAL" |
                            CULTURA == "SOJA SAFRINHA" |
                            CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
                            CULTURA == "MILHO VERDE (espiga)" |
                            CULTURA == "MILHO SAFRA NORMAL" |
                            CULTURA == "MILHO SAFRINHA" |
                            CULTURA == "TRIGO" |
                            CULTURA == "FEIJAO SAFRA DA SECA" |
                            CULTURA == "FEIJAO SAFRA DAS AGUAS" |
                            CULTURA == "FEIJAO SAFRA DE INVERNO" |
                            CULTURA == "FEIJAO-VAGEM" |
                            CULTURA == "PASTAGENS E FORRAGENS" |
                            CULTURA == "BATATA DA SECA" |
                            CULTURA == "BATATA DAS AGUAS" |
                            CULTURA == "CANA-DE-ACUCAR" |
                            CULTURA == "FUMO" |
                            CULTURA == "UVA DE MESA" |
                            CULTURA == "UVA VINIFERA" |
                            CULTURA == "TOMATE SAFRAO" |
                            CULTURA == "CAFE" |
                            CULTURA == "LARANJA" |
                            CULTURA == "LIMAO" |
                            CULTURA == "TANGERINA MONTENEGRINA" |
                            CULTURA == "TANGERINA PONKAN" |
                            CULTURA == "TANGERINA MURCOTE" |
                            CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
                            CULTURA == "MANDIOCA INDUSTRIA" |
                            CULTURA == "AVEIA BRANCA" |
                            CULTURA == "AVEIA PRETA (GRAO)" |
                            CULTURA == "CEBOLA" |
                            CULTURA == "ARROZ IRRIGADO" |
                            CULTURA == "ARROZ SEQUEIRO" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
                            CULTURA == "MACA" |
                            CULTURA == "CEVADA" |
                            CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
                            CULTURA == "AZEVEM GRAOS" |
                            CULTURA == "MELANCIA" |
                            CULTURA == "BANANA" |
                            CULTURA == "PEPINO" |
                            CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
                            CULTURA == "PESSEGO" |
                            CULTURA == "PIMENTAO" |
                            CULTURA == "REPOLHO" |
                            CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  arrange(desc(AREA_HA)) %>%   #### Selecionando área como fator relevante
  arrange(MUNICIPIO) %>%
  group_by(MUNICIPIO) %>%
  slice_head(n = 6)  #### Selecionando 06 culturas com maior área utilizada

PR_DERAL_2016_CULTIVOS_MUNICIPIOS <- AUX

##### Filtrando Mata nativa

AUX <- VBP_2016 %>%filter(CULTURA == "MATA NATIVA" ) %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2016_SIMPLIFICADO <- left_join(PR_DERAL_2016_SIMPLIFICADO,
                                   AUX,
                                   by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2016_SIMPLIFICADO)[7] <- "MATA_NATIVA"

##### Incluindo dados do SIAGRO 2016

AUX <- SIAGRO_2025[, c(1, 5)]

#### Tornando a coluna "Municipio" compatível para left_join

AUX$Município <- toupper(AUX$Município)

AUX$Município <- iconv(AUX$Município,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

AUX$Município <- str_replace(AUX$Município, "ITAPEJARA D'OESTE", "ITAPEJARA DOESTE")
AUX$Município <- str_replace(AUX$Município, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
AUX$Município <- str_replace(AUX$Município, "PEROLA D'OESTE", "PEROLA DOESTE")
AUX$Município <- str_replace(AUX$Município, "SAO JORGE D'OESTE", "SAO JORGE DOESTE")
AUX$Município <- str_replace(AUX$Município, "RANCHO ALEGRE D'OESTE", "RANCHO ALEGRE DOESTE")
AUX$Município <- str_replace(AUX$Município, "SANTA CRUZ DE MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
AUX$Município <- str_replace(AUX$Município, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

PR_DERAL_2016_SIMPLIFICADO <- left_join(PR_DERAL_2016_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "Município"))

colnames(PR_DERAL_2016_SIMPLIFICADO)[8] <- "TON_AGRO_2016"

#### Incluindo a RS no data frame de cultivos dos municípios

for (i in PR_DERAL_2016_SIMPLIFICADO[, 3]){
  PR_DERAL_2016_CULTIVOS_MUNICIPIOS[which(PR_DERAL_2016_CULTIVOS_MUNICIPIOS$MUNICIPIO == i), 9] <-  PR_DERAL_2016_SIMPLIFICADO[which(PR_DERAL_2016_SIMPLIFICADO$Município_sem_Código == i), 1]
  
}

colnames(PR_DERAL_2016_CULTIVOS_MUNICIPIOS)[9] <- "RS"

PR_DERAL_2016_CULTIVOS_MUNICIPIOS <- PR_DERAL_2016_CULTIVOS_MUNICIPIOS[, -7]

rm(VBP_2016)

#######  2017

VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "ARAPUAN", "ARAPUA")
VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "ITAPEJARA DO OESTE", "ITAPEJARA DOESTE")
VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "PEROLA DO OESTE", "PEROLA DOESTE")
VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "SAO JORGE DO OESTE", "SAO JORGE DOESTE")
VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "SANTA TEREZINHA DO ITAIPU", "SANTA TEREZINHA DE ITAIPU")
VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "RANCHO ALEGRE DO OESTE", "RANCHO ALEGRE DOESTE")
VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "SAUDADES DO IGUACU", "SAUDADE DO IGUACU")
VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "SANTA CRUZ DO MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "SANTA IZABEL DO IVAI", "SANTA ISABEL DO IVAI")
VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "MUNHOZ DE MELO", "MUNHOZ DE MELLO")
VBP_2017$MUNICIPIO <- str_replace(VBP_2017$MUNICIPIO, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

VBP_2017$VALOR_Reais <- as.numeric(gsub(",", ".", VBP_2017$VALOR_Reais))
VBP_2017$PRODUCAO <- as.numeric(gsub(",", ".", VBP_2017$PRODUCAO))
VBP_2017$AREA_HA <- as.numeric(gsub(",", ".", VBP_2017$AREA_HA))

#######   Fazendo a soma de valor total produzido de cada município

## Vai ser filtrado os cultivos em que o SIAGRO 2025 
## aponta como que utilizam agrotóxicos

AUX <- VBP_2017 %>%
  filter(CULTURA == "SOJA SAFRA NORMAL" |
           CULTURA == "SOJA SAFRINHA" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "MILHO SAFRA NORMAL" |
           CULTURA == "MILHO SAFRINHA" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJAO SAFRA DA SECA" |
           CULTURA == "FEIJAO SAFRA DAS AGUAS" |
           CULTURA == "FEIJAO SAFRA DE INVERNO" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA DA SECA" |
           CULTURA == "BATATA DAS AGUAS" |
           CULTURA == "CANA-DE-ACUCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "TOMATE SAFRAO" |
           CULTURA == "CAFE" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
           CULTURA == "MANDIOCA INDUSTRIA" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA (GRAO)" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(VALOR_Reais))

### Criando um objeto com os dados do DERAL/SIAGRO

PR_DERAL_2017_SIMPLIFICADO <- BASE_IBGE[, c(1, 2, 3, 5)]

PR_DERAL_2017_SIMPLIFICADO$Município_sem_Código <- iconv(PR_DERAL_2017_SIMPLIFICADO$Município_sem_Código,
                                                         from = "UTF-8", 
                                                         to = "ASCII//TRANSLIT")

PR_DERAL_2017_SIMPLIFICADO <- left_join(PR_DERAL_2017_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2017_SIMPLIFICADO)[5] <- "VALOR_REAIS"

#######   Fazendo a soma da área total cultivada de cada município

VBP_2017$AREA_HA[VBP_2017$AREA_HA == ""] <- NA

AUX <- VBP_2017 %>%
  filter(!is.na(AREA_HA),
         CULTURA == "SOJA SAFRA NORMAL" |
           CULTURA == "SOJA SAFRINHA" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "MILHO SAFRA NORMAL" |
           CULTURA == "MILHO SAFRINHA" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJAO SAFRA DA SECA" |
           CULTURA == "FEIJAO SAFRA DAS AGUAS" |
           CULTURA == "FEIJAO SAFRA DE INVERNO" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA DA SECA" |
           CULTURA == "BATATA DAS AGUAS" |
           CULTURA == "CANA-DE-ACUCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "TOMATE SAFRAO" |
           CULTURA == "CAFE" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
           CULTURA == "MANDIOCA INDUSTRIA" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA (GRAO)" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2017_SIMPLIFICADO <- left_join(PR_DERAL_2017_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2017_SIMPLIFICADO)[6] <- "AREA_HA"

#####  Separando as cinco maiores culturas de cada município

AUX <- VBP_2017 %>%filter(CULTURA == "SOJA SAFRA NORMAL" |
                            CULTURA == "SOJA SAFRINHA" |
                            CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
                            CULTURA == "MILHO VERDE (espiga)" |
                            CULTURA == "MILHO SAFRA NORMAL" |
                            CULTURA == "MILHO SAFRINHA" |
                            CULTURA == "TRIGO" |
                            CULTURA == "FEIJAO SAFRA DA SECA" |
                            CULTURA == "FEIJAO SAFRA DAS AGUAS" |
                            CULTURA == "FEIJAO SAFRA DE INVERNO" |
                            CULTURA == "FEIJAO-VAGEM" |
                            CULTURA == "PASTAGENS E FORRAGENS" |
                            CULTURA == "BATATA DA SECA" |
                            CULTURA == "BATATA DAS AGUAS" |
                            CULTURA == "CANA-DE-ACUCAR" |
                            CULTURA == "FUMO" |
                            CULTURA == "UVA DE MESA" |
                            CULTURA == "UVA VINIFERA" |
                            CULTURA == "TOMATE SAFRAO" |
                            CULTURA == "CAFE" |
                            CULTURA == "LARANJA" |
                            CULTURA == "LIMAO" |
                            CULTURA == "TANGERINA MONTENEGRINA" |
                            CULTURA == "TANGERINA PONKAN" |
                            CULTURA == "TANGERINA MURCOTE" |
                            CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
                            CULTURA == "MANDIOCA INDUSTRIA" |
                            CULTURA == "AVEIA BRANCA" |
                            CULTURA == "AVEIA PRETA (GRAO)" |
                            CULTURA == "CEBOLA" |
                            CULTURA == "ARROZ IRRIGADO" |
                            CULTURA == "ARROZ SEQUEIRO" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
                            CULTURA == "MACA" |
                            CULTURA == "CEVADA" |
                            CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
                            CULTURA == "AZEVEM GRAOS" |
                            CULTURA == "MELANCIA" |
                            CULTURA == "BANANA" |
                            CULTURA == "PEPINO" |
                            CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
                            CULTURA == "PESSEGO" |
                            CULTURA == "PIMENTAO" |
                            CULTURA == "REPOLHO" |
                            CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  arrange(desc(AREA_HA)) %>%
  arrange(MUNICIPIO) %>%
  group_by(MUNICIPIO) %>%
  slice_head(n = 6)

PR_DERAL_2017_CULTIVOS_MUNICIPIOS <- AUX

##### Filtrando Mata nativa

AUX <- VBP_2017 %>%filter(CULTURA == "MATA NATIVA" ) %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2017_SIMPLIFICADO <- left_join(PR_DERAL_2017_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2017_SIMPLIFICADO)[7] <- "MATA_NATIVA"

##### Incluindo dados do SIAGRO 2017

AUX <- SIAGRO_2025[, c(1, 6)]

#### Tornando a coluna "Municipio" compatível para left_join

AUX$Município <- toupper(AUX$Município)

AUX$Município <- iconv(AUX$Município,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

AUX$Município <- str_replace(AUX$Município, "ITAPEJARA D'OESTE", "ITAPEJARA DOESTE")
AUX$Município <- str_replace(AUX$Município, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
AUX$Município <- str_replace(AUX$Município, "PEROLA D'OESTE", "PEROLA DOESTE")
AUX$Município <- str_replace(AUX$Município, "SAO JORGE D'OESTE", "SAO JORGE DOESTE")
AUX$Município <- str_replace(AUX$Município, "RANCHO ALEGRE D'OESTE", "RANCHO ALEGRE DOESTE")
AUX$Município <- str_replace(AUX$Município, "SANTA CRUZ DE MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
AUX$Município <- str_replace(AUX$Município, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

PR_DERAL_2017_SIMPLIFICADO <- left_join(PR_DERAL_2017_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "Município"))

colnames(PR_DERAL_2017_SIMPLIFICADO)[8] <- "TON_AGRO_2017"

for (i in PR_DERAL_2017_SIMPLIFICADO[, 3]){
  PR_DERAL_2017_CULTIVOS_MUNICIPIOS[which(PR_DERAL_2017_CULTIVOS_MUNICIPIOS$MUNICIPIO == i), 9] <-  PR_DERAL_2017_SIMPLIFICADO[which(PR_DERAL_2017_SIMPLIFICADO$Município_sem_Código == i), 1]
  
}

colnames(PR_DERAL_2017_CULTIVOS_MUNICIPIOS)[9] <- "RS"

PR_DERAL_2017_CULTIVOS_MUNICIPIOS <- PR_DERAL_2017_CULTIVOS_MUNICIPIOS[, -7]

rm(VBP_2017)

#######  2018

VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "ARAPUAN", "ARAPUA")
VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "ITAPEJARA DO OESTE", "ITAPEJARA DOESTE")
VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "PEROLA DO OESTE", "PEROLA DOESTE")
VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "SAO JORGE DO OESTE", "SAO JORGE DOESTE")
VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "SANTA TEREZINHA DO ITAIPU", "SANTA TEREZINHA DE ITAIPU")
VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "RANCHO ALEGRE DO OESTE", "RANCHO ALEGRE DOESTE")
VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "SAUDADES DO IGUACU", "SAUDADE DO IGUACU")
VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "SANTA CRUZ DO MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "SANTA IZABEL DO IVAI", "SANTA ISABEL DO IVAI")
VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "MUNHOZ DE MELO", "MUNHOZ DE MELLO")
VBP_2018$MUNICIPIO <- str_replace(VBP_2018$MUNICIPIO, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

VBP_2018$VALOR_Reais <- as.numeric(gsub(",", ".", VBP_2018$VALOR_Reais))
VBP_2018$PRODUCAO <- as.numeric(gsub(",", ".", VBP_2018$PRODUCAO))
VBP_2018$AREA_HA <- as.numeric(gsub(",", ".", VBP_2018$AREA_HA))

#######   Fazendo a soma de valor total produzido de cada município

## Vai ser filtrado os cultivos em que o SIAGRO 2025 
## aponta como que utilizam agrotóxicos

AUX <- VBP_2018 %>%
  filter(CULTURA == "SOJA SAFRA NORMAL" |
           CULTURA == "SOJA SAFRINHA" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "MILHO SAFRA NORMAL" |
           CULTURA == "MILHO SAFRINHA" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJAO SAFRA DA SECA" |
           CULTURA == "FEIJAO SAFRA DAS AGUAS" |
           CULTURA == "FEIJAO SAFRA DE INVERNO" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA DA SECA" |
           CULTURA == "BATATA DAS AGUAS" |
           CULTURA == "CANA-DE-ACUCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "TOMATE SAFRAO" |
           CULTURA == "CAFE" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
           CULTURA == "MANDIOCA INDUSTRIA" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA (GRAO)" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(VALOR_Reais))

### Criando um objeto com os dados do DERAL/SIAGRO

PR_DERAL_2018_SIMPLIFICADO <- BASE_IBGE[, c(1, 2, 3, 5)]

PR_DERAL_2018_SIMPLIFICADO$Município_sem_Código <- iconv(PR_DERAL_2018_SIMPLIFICADO$Município_sem_Código,
                                                         from = "UTF-8", 
                                                         to = "ASCII//TRANSLIT")

PR_DERAL_2018_SIMPLIFICADO <- left_join(PR_DERAL_2018_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2018_SIMPLIFICADO)[5] <- "VALOR_REAIS"

#######   Fazendo a soma da área total cultivada de cada município

VBP_2018$AREA_HA[VBP_2018$AREA_HA == ""] <- NA

AUX <- VBP_2018 %>%
  filter(!is.na(AREA_HA),
         CULTURA == "SOJA SAFRA NORMAL" |
           CULTURA == "SOJA SAFRINHA" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "MILHO SAFRA NORMAL" |
           CULTURA == "MILHO SAFRINHA" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJAO SAFRA DA SECA" |
           CULTURA == "FEIJAO SAFRA DAS AGUAS" |
           CULTURA == "FEIJAO SAFRA DE INVERNO" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA DA SECA" |
           CULTURA == "BATATA DAS AGUAS" |
           CULTURA == "CANA-DE-ACUCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "TOMATE SAFRAO" |
           CULTURA == "CAFE" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
           CULTURA == "MANDIOCA INDUSTRIA" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA (GRAO)" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2018_SIMPLIFICADO <- left_join(PR_DERAL_2018_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2018_SIMPLIFICADO)[6] <- "AREA_HA"

#####  Separando as cinco maiores culturas de cada município

AUX <- VBP_2018 %>%filter(CULTURA == "SOJA SAFRA NORMAL" |
                            CULTURA == "SOJA SAFRINHA" |
                            CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
                            CULTURA == "MILHO VERDE (espiga)" |
                            CULTURA == "MILHO SAFRA NORMAL" |
                            CULTURA == "MILHO SAFRINHA" |
                            CULTURA == "TRIGO" |
                            CULTURA == "FEIJAO SAFRA DA SECA" |
                            CULTURA == "FEIJAO SAFRA DAS AGUAS" |
                            CULTURA == "FEIJAO SAFRA DE INVERNO" |
                            CULTURA == "FEIJAO-VAGEM" |
                            CULTURA == "PASTAGENS E FORRAGENS" |
                            CULTURA == "BATATA DA SECA" |
                            CULTURA == "BATATA DAS AGUAS" |
                            CULTURA == "CANA-DE-ACUCAR" |
                            CULTURA == "FUMO" |
                            CULTURA == "UVA DE MESA" |
                            CULTURA == "UVA VINIFERA" |
                            CULTURA == "TOMATE SAFRAO" |
                            CULTURA == "CAFE" |
                            CULTURA == "LARANJA" |
                            CULTURA == "LIMAO" |
                            CULTURA == "TANGERINA MONTENEGRINA" |
                            CULTURA == "TANGERINA PONKAN" |
                            CULTURA == "TANGERINA MURCOTE" |
                            CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
                            CULTURA == "MANDIOCA INDUSTRIA" |
                            CULTURA == "AVEIA BRANCA" |
                            CULTURA == "AVEIA PRETA (GRAO)" |
                            CULTURA == "CEBOLA" |
                            CULTURA == "ARROZ IRRIGADO" |
                            CULTURA == "ARROZ SEQUEIRO" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
                            CULTURA == "MACA" |
                            CULTURA == "CEVADA" |
                            CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
                            CULTURA == "AZEVEM GRAOS" |
                            CULTURA == "MELANCIA" |
                            CULTURA == "BANANA" |
                            CULTURA == "PEPINO" |
                            CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
                            CULTURA == "PESSEGO" |
                            CULTURA == "PIMENTAO" |
                            CULTURA == "REPOLHO" |
                            CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  arrange(desc(AREA_HA)) %>%
  arrange(MUNICIPIO) %>%
  group_by(MUNICIPIO) %>%
  slice_head(n = 6)

PR_DERAL_2018_CULTIVOS_MUNICIPIOS <- AUX

##### Filtrando Mata nativa

AUX <- VBP_2018 %>%filter(CULTURA == "MATA NATIVA" ) %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2018_SIMPLIFICADO <- left_join(PR_DERAL_2018_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2018_SIMPLIFICADO)[7] <- "MATA_NATIVA"

##### Incluindo dados do SIAGRO 2018

AUX <- SIAGRO_2025[, c(1, 7)]

#### Tornando a coluna "Municipio" compatível para left_join

AUX$Município <- toupper(AUX$Município)

AUX$Município <- iconv(AUX$Município,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

AUX$Município <- str_replace(AUX$Município, "ITAPEJARA D'OESTE", "ITAPEJARA DOESTE")
AUX$Município <- str_replace(AUX$Município, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
AUX$Município <- str_replace(AUX$Município, "PEROLA D'OESTE", "PEROLA DOESTE")
AUX$Município <- str_replace(AUX$Município, "SAO JORGE D'OESTE", "SAO JORGE DOESTE")
AUX$Município <- str_replace(AUX$Município, "RANCHO ALEGRE D'OESTE", "RANCHO ALEGRE DOESTE")
AUX$Município <- str_replace(AUX$Município, "SANTA CRUZ DE MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
AUX$Município <- str_replace(AUX$Município, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

PR_DERAL_2018_SIMPLIFICADO <- left_join(PR_DERAL_2018_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "Município"))

colnames(PR_DERAL_2018_SIMPLIFICADO)[8] <- "TON_AGRO_2018"

for (i in PR_DERAL_2018_SIMPLIFICADO[, 3]){
  PR_DERAL_2018_CULTIVOS_MUNICIPIOS[which(PR_DERAL_2018_CULTIVOS_MUNICIPIOS$MUNICIPIO == i), 9] <-  PR_DERAL_2018_SIMPLIFICADO[which(PR_DERAL_2018_SIMPLIFICADO$Município_sem_Código == i), 1]
  
}

colnames(PR_DERAL_2018_CULTIVOS_MUNICIPIOS)[9] <- "RS"

PR_DERAL_2018_CULTIVOS_MUNICIPIOS <- PR_DERAL_2018_CULTIVOS_MUNICIPIOS[, -7]

rm(VBP_2018)

#######  2019

VBP_2019$MUNICIPIO <- toupper(VBP_2019$MUNICIPIO)

VBP_2019$MUNICIPIO <- str_replace(VBP_2019$MUNICIPIO, "ITAPEJARA D'OESTE", "ITAPEJARA DOESTE")
VBP_2019$MUNICIPIO <- str_replace(VBP_2019$MUNICIPIO, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
VBP_2019$MUNICIPIO <- str_replace(VBP_2019$MUNICIPIO, "PÉROLA D'OESTE", "PÉROLA DOESTE")
VBP_2019$MUNICIPIO <- str_replace(VBP_2019$MUNICIPIO, "SÃO JORGE D'OESTE", "SÃO JORGE DOESTE")
VBP_2019$MUNICIPIO <- str_replace(VBP_2019$MUNICIPIO, "SANTA TEREZINHA DO ITAIPU", "SANTA TEREZINHA DE ITAIPU")
VBP_2019$MUNICIPIO <- str_replace(VBP_2019$MUNICIPIO, "RANCHO ALEGRE D'OESTE", "RANCHO ALEGRE DOESTE")
VBP_2019$MUNICIPIO <- str_replace(VBP_2019$MUNICIPIO, "SAUDADES DO IGUACU", "SAUDADE DO IGUACU")
VBP_2019$MUNICIPIO <- str_replace(VBP_2019$MUNICIPIO, "SANTA CRUZ DE MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
VBP_2019$MUNICIPIO <- str_replace(VBP_2019$MUNICIPIO, "SANTA IZABEL DO IVAI", "SANTA ISABEL DO IVAI")
VBP_2019$MUNICIPIO <- str_replace(VBP_2019$MUNICIPIO, "MUNHOZ DE MELO", "MUNHOZ DE MELLO")
VBP_2019$MUNICIPIO <- str_replace(VBP_2019$MUNICIPIO, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

VBP_2019$VALOR_Reais <- as.numeric(gsub(",", ".", VBP_2019$VALOR_Reais))
VBP_2019$PRODUCAO <- as.numeric(gsub(",", ".", VBP_2019$PRODUCAO))
VBP_2019$AREA_HA <- as.numeric(gsub(",", ".", VBP_2019$AREA_HA))

#######   Fazendo a soma de valor total produzido de cada município

## Vai ser filtrado os cultivos em que o SIAGRO 2025 
## aponta como que utilizam agrotóxicos

AUX <- VBP_2019 %>%
  filter(CULTURA == "Soja (1ª safra)" |
           CULTURA == "Soja (2ª safra)" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "Milho (1ª safra)" |
           CULTURA == "Milho (2ª safra)" |
           CULTURA == "Trigo" |
           CULTURA == "Feijão (1ª safra)" |
           CULTURA == "Feijão (2ª safra)" |
           CULTURA == "Feijão (3ª safra)" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "Batata (1ª safra)" |
           CULTURA == "Batata (2ª safra)" |
           CULTURA == "Cana-de-açúcar" |
           CULTURA == "Fumo" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "Tomate (1ª safra)" |
           CULTURA == "Tomate (2ª safra)" |
           CULTURA == "Café" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "Mandioca Consumo Humano" |
           CULTURA == "Mandioca  Indústria/Consumo Animal" |
           CULTURA == "Aveia branca" |
           CULTURA == "Aveia preta" |
           CULTURA == "Cebola" |
           CULTURA == "Arroz irrigado" |
           CULTURA == "Arroz de sequeiro" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "Cevada" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "Amendoim (1ª safra)" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(VALOR_Reais))

### Criando um objeto com os dados do DERAL/SIAGRO

PR_DERAL_2019_SIMPLIFICADO <- BASE_IBGE[, c(1, 2, 3, 5)]

# PR_DERAL_2019_SIMPLIFICADO$Município_sem_Código <- iconv(PR_DERAL_2019_SIMPLIFICADO$Município_sem_Código,
#                                                          from = "UTF-8", 
#                                                          to = "ASCII//TRANSLIT")

PR_DERAL_2019_SIMPLIFICADO <- left_join(PR_DERAL_2019_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2019_SIMPLIFICADO)[5] <- "VALOR_REAIS"

#######   Fazendo a soma da área total cultivada de cada município

VBP_2019$AREA_HA[VBP_2019$AREA_HA == ""] <- NA

AUX <- VBP_2019 %>%
  filter(!is.na(AREA_HA),
         CULTURA == "Soja (1ª safra)" |
           CULTURA == "Soja (2ª safra)" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "Milho (1ª safra)" |
           CULTURA == "Milho (2ª safra)" |
           CULTURA == "Trigo" |
           CULTURA == "Feijão (1ª safra)" |
           CULTURA == "Feijão (2ª safra)" |
           CULTURA == "Feijão (3ª safra)" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "Batata (1ª safra)" |
           CULTURA == "Batata (2ª safra)" |
           CULTURA == "Cana-de-açúcar" |
           CULTURA == "Fumo" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "Tomate (1ª safra)" |
           CULTURA == "Tomate (2ª safra)" |
           CULTURA == "Café" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "Mandioca Consumo Humano" |
           CULTURA == "Mandioca  Indústria/Consumo Animal" |
           CULTURA == "Aveia branca" |
           CULTURA == "Aveia preta" |
           CULTURA == "Cebola" |
           CULTURA == "Arroz irrigado" |
           CULTURA == "Arroz de sequeiro" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "Cevada" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "Amendoim (1ª safra)" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2019_SIMPLIFICADO <- left_join(PR_DERAL_2019_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2019_SIMPLIFICADO)[6] <- "AREA_HA"

#####  Separando as cinco maiores culturas de cada município

AUX <- VBP_2019 %>%filter(CULTURA == "Soja (1ª safra)" |
                            CULTURA == "Soja (2ª safra)" |
                            CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
                            CULTURA == "MILHO VERDE (espiga)" |
                            CULTURA == "Milho (1ª safra)" |
                            CULTURA == "Milho (2ª safra)" |
                            CULTURA == "Trigo" |
                            CULTURA == "Feijão (1ª safra)" |
                            CULTURA == "Feijão (2ª safra)" |
                            CULTURA == "Feijão (3ª safra)" |
                            CULTURA == "FEIJAO-VAGEM" |
                            CULTURA == "PASTAGENS E FORRAGENS" |
                            CULTURA == "Batata (1ª safra)" |
                            CULTURA == "Batata (2ª safra)" |
                            CULTURA == "Cana-de-açúcar" |
                            CULTURA == "Fumo" |
                            CULTURA == "UVA DE MESA" |
                            CULTURA == "UVA VINIFERA" |
                            CULTURA == "Tomate (1ª safra)" |
                            CULTURA == "Tomate (2ª safra)" |
                            CULTURA == "Café" |
                            CULTURA == "LARANJA" |
                            CULTURA == "LIMAO" |
                            CULTURA == "TANGERINA MONTENEGRINA" |
                            CULTURA == "TANGERINA PONKAN" |
                            CULTURA == "TANGERINA MURCOTE" |
                            CULTURA == "Mandioca Consumo Humano" |
                            CULTURA == "Mandioca  Indústria/Consumo Animal" |
                            CULTURA == "Aveia branca" |
                            CULTURA == "Aveia preta" |
                            CULTURA == "Cebola" |
                            CULTURA == "Arroz irrigado" |
                            CULTURA == "Arroz de sequeiro" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
                            CULTURA == "MACA" |
                            CULTURA == "Cevada" |
                            CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
                            CULTURA == "AZEVEM GRAOS" |
                            CULTURA == "MELANCIA" |
                            CULTURA == "BANANA" |
                            CULTURA == "PEPINO" |
                            CULTURA == "Amendoim (1ª safra)" |
                            CULTURA == "PESSEGO" |
                            CULTURA == "PIMENTAO" |
                            CULTURA == "REPOLHO" |
                            CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  arrange(desc(AREA_HA)) %>%
  arrange(MUNICIPIO) %>%
  group_by(MUNICIPIO) %>%
  slice_head(n = 6)

PR_DERAL_2019_CULTIVOS_MUNICIPIOS <- AUX

##### Filtrando Mata nativa

AUX <- VBP_2019 %>%filter(CULTURA == "MATA NATIVA" ) %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2019_SIMPLIFICADO <- left_join(PR_DERAL_2019_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2019_SIMPLIFICADO)[7] <- "MATA_NATIVA"

###  Retirando acento dos municípios para poder fazer left_join com as tabelas

PR_DERAL_2019_SIMPLIFICADO$Município_sem_Código <- iconv(PR_DERAL_2019_SIMPLIFICADO$Município_sem_Código,
                                                         from = "UTF-8",
                                                         to = "ASCII//TRANSLIT")

##### Incluindo dados do SIAGRO 2019

AUX <- SIAGRO_2025[, c(1, 8)]

#### Tornando a coluna "Municipio" compatível para left_join

AUX$Município <- toupper(AUX$Município)

AUX$Município <- iconv(AUX$Município,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

AUX$Município <- str_replace(AUX$Município, "ITAPEJARA D'OESTE", "ITAPEJARA DOESTE")
AUX$Município <- str_replace(AUX$Município, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
AUX$Município <- str_replace(AUX$Município, "PEROLA D'OESTE", "PEROLA DOESTE")
AUX$Município <- str_replace(AUX$Município, "SAO JORGE D'OESTE", "SAO JORGE DOESTE")
AUX$Município <- str_replace(AUX$Município, "RANCHO ALEGRE D'OESTE", "RANCHO ALEGRE DOESTE")
AUX$Município <- str_replace(AUX$Município, "SANTA CRUZ DE MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
AUX$Município <- str_replace(AUX$Município, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

PR_DERAL_2019_SIMPLIFICADO <- left_join(PR_DERAL_2019_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "Município"))

colnames(PR_DERAL_2019_SIMPLIFICADO)[8] <- "TON_AGRO_2019"

###  Retirando acento dos municípios para poder fazer left_join com as tabelas

PR_DERAL_2019_CULTIVOS_MUNICIPIOS$MUNICIPIO <- iconv(PR_DERAL_2019_CULTIVOS_MUNICIPIOS$MUNICIPIO,
                                                         from = "UTF-8",
                                                         to = "ASCII//TRANSLIT")

for (i in PR_DERAL_2019_SIMPLIFICADO[, 3]){
  PR_DERAL_2019_CULTIVOS_MUNICIPIOS[which(PR_DERAL_2019_CULTIVOS_MUNICIPIOS$MUNICIPIO == i), 13] <-  PR_DERAL_2019_SIMPLIFICADO[which(PR_DERAL_2019_SIMPLIFICADO$Município_sem_Código == i), 1]
  
}

colnames(PR_DERAL_2019_CULTIVOS_MUNICIPIOS)[13] <- "RS"

PR_DERAL_2019_CULTIVOS_MUNICIPIOS <- PR_DERAL_2019_CULTIVOS_MUNICIPIOS[, -c(8, 9, 10)]

rm(VBP_2019)

#######  2020

VBP_2020$MUNICIPIO <- toupper(VBP_2020$MUNICIPIO)

VBP_2020$MUNICIPIO <- str_replace(VBP_2020$MUNICIPIO, "ITAPEJARA DO OESTE", "ITAPEJARA DOESTE")
VBP_2020$MUNICIPIO <- str_replace(VBP_2020$MUNICIPIO, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
VBP_2020$MUNICIPIO <- str_replace(VBP_2020$MUNICIPIO, "PEROLA DO OESTE", "PEROLA DOESTE")
VBP_2020$MUNICIPIO <- str_replace(VBP_2020$MUNICIPIO, "SAO JORGE DO OESTE", "SAO JORGE DOESTE")
VBP_2020$MUNICIPIO <- str_replace(VBP_2020$MUNICIPIO, "SANTA TEREZINHA DO ITAIPU", "SANTA TEREZINHA DE ITAIPU")
VBP_2020$MUNICIPIO <- str_replace(VBP_2020$MUNICIPIO, "RANCHO ALEGRE DO OESTE", "RANCHO ALEGRE DOESTE")
VBP_2020$MUNICIPIO <- str_replace(VBP_2020$MUNICIPIO, "SAUDADES DO IGUACU", "SAUDADE DO IGUACU")
VBP_2020$MUNICIPIO <- str_replace(VBP_2020$MUNICIPIO, "SANTA CRUZ DO MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
VBP_2020$MUNICIPIO <- str_replace(VBP_2020$MUNICIPIO, "SANTA IZABEL DO IVAI", "SANTA ISABEL DO IVAI")
VBP_2020$MUNICIPIO <- str_replace(VBP_2020$MUNICIPIO, "MUNHOZ DE MELO", "MUNHOZ DE MELLO")
VBP_2020$MUNICIPIO <- str_replace(VBP_2020$MUNICIPIO, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

VBP_2020$VALOR_Reais <- as.numeric(gsub(",", ".", VBP_2020$VALOR_Reais))
VBP_2020$PRODUCAO <- as.numeric(gsub(",", ".", VBP_2020$PRODUCAO))
VBP_2020$AREA_HA <- as.numeric(gsub(",", ".", VBP_2020$AREA_HA))

#######   Fazendo a soma de valor total produzido de cada município

## Vai ser filtrado os cultivos em que o SIAGRO 2025 
## aponta como que utilizam agrotóxicos

AUX <- VBP_2020 %>%
  filter(CULTURA == "SOJA SAFRA NORMAL" |
           CULTURA == "SOJA SAFRINHA" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "MILHO SAFRA NORMAL" |
           CULTURA == "MILHO SAFRINHA" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJAO SAFRA DA SECA" |
           CULTURA == "FEIJAO SAFRA DAS AGUAS" |
           CULTURA == "FEIJAO SAFRA DE INVERNO" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA DA SECA" |
           CULTURA == "BATATA DAS AGUAS" |
           CULTURA == "CANA-DE-ACUCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "TOMATE SAFRAO" |
           CULTURA == "CAFE" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
           CULTURA == "MANDIOCA INDUSTRIA" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA (GRAO)" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(VALOR_Reais))

### Criando um objeto com os dados do DERAL/SIAGRO

PR_DERAL_2020_SIMPLIFICADO <- BASE_IBGE[, c(1, 2, 3, 5)]

PR_DERAL_2020_SIMPLIFICADO <- left_join(PR_DERAL_2020_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2020_SIMPLIFICADO)[5] <- "VALOR_REAIS"

#######   Fazendo a soma da área total cultivada de cada município

VBP_2020$AREA_HA[VBP_2020$AREA_HA == ""] <- NA

AUX <- VBP_2020 %>%
  filter(!is.na(AREA_HA),
         CULTURA == "SOJA SAFRA NORMAL" |
           CULTURA == "SOJA SAFRINHA" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "MILHO SAFRA NORMAL" |
           CULTURA == "MILHO SAFRINHA" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJAO SAFRA DA SECA" |
           CULTURA == "FEIJAO SAFRA DAS AGUAS" |
           CULTURA == "FEIJAO SAFRA DE INVERNO" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA DA SECA" |
           CULTURA == "BATATA DAS AGUAS" |
           CULTURA == "CANA-DE-ACUCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "TOMATE SAFRAO" |
           CULTURA == "CAFE" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
           CULTURA == "MANDIOCA INDUSTRIA" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA (GRAO)" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2020_SIMPLIFICADO <- left_join(PR_DERAL_2020_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2020_SIMPLIFICADO)[6] <- "AREA_HA"

#####  Separando as cinco maiores culturas de cada município

AUX <- VBP_2020 %>%filter(CULTURA == "SOJA SAFRA NORMAL" |
                            CULTURA == "SOJA SAFRINHA" |
                            CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
                            CULTURA == "MILHO VERDE (espiga)" |
                            CULTURA == "MILHO SAFRA NORMAL" |
                            CULTURA == "MILHO SAFRINHA" |
                            CULTURA == "TRIGO" |
                            CULTURA == "FEIJAO SAFRA DA SECA" |
                            CULTURA == "FEIJAO SAFRA DAS AGUAS" |
                            CULTURA == "FEIJAO SAFRA DE INVERNO" |
                            CULTURA == "FEIJAO-VAGEM" |
                            CULTURA == "PASTAGENS E FORRAGENS" |
                            CULTURA == "BATATA DA SECA" |
                            CULTURA == "BATATA DAS AGUAS" |
                            CULTURA == "CANA-DE-ACUCAR" |
                            CULTURA == "FUMO" |
                            CULTURA == "UVA DE MESA" |
                            CULTURA == "UVA VINIFERA" |
                            CULTURA == "TOMATE SAFRAO" |
                            CULTURA == "CAFE" |
                            CULTURA == "LARANJA" |
                            CULTURA == "LIMAO" |
                            CULTURA == "TANGERINA MONTENEGRINA" |
                            CULTURA == "TANGERINA PONKAN" |
                            CULTURA == "TANGERINA MURCOTE" |
                            CULTURA == "MANDIOCA CONSUMO (HUMANO)" |
                            CULTURA == "MANDIOCA INDUSTRIA" |
                            CULTURA == "AVEIA BRANCA" |
                            CULTURA == "AVEIA PRETA (GRAO)" |
                            CULTURA == "CEBOLA" |
                            CULTURA == "ARROZ IRRIGADO" |
                            CULTURA == "ARROZ SEQUEIRO" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
                            CULTURA == "MACA" |
                            CULTURA == "CEVADA" |
                            CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
                            CULTURA == "AZEVEM GRAOS" |
                            CULTURA == "MELANCIA" |
                            CULTURA == "BANANA" |
                            CULTURA == "PEPINO" |
                            CULTURA == "AMENDOIM SAFRA DAS AGUAS" |
                            CULTURA == "PESSEGO" |
                            CULTURA == "PIMENTAO" |
                            CULTURA == "REPOLHO" |
                            CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  arrange(desc(AREA_HA)) %>%
  arrange(MUNICIPIO) %>%
  group_by(MUNICIPIO) %>%
  slice_head(n = 6)

PR_DERAL_2020_CULTIVOS_MUNICIPIOS <- AUX

##### Filtrando Mata nativa

AUX <- VBP_2020 %>%filter(CULTURA == "MATA NATIVA" ) %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2020_SIMPLIFICADO <- left_join(PR_DERAL_2020_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2020_SIMPLIFICADO)[7] <- "MATA_NATIVA"

###  Retirando acento dos municípios para poder fazer left_join com as tabelas

PR_DERAL_2020_SIMPLIFICADO$Município_sem_Código <- iconv(PR_DERAL_2020_SIMPLIFICADO$Município_sem_Código,
                                                         from = "UTF-8", 
                                                         to = "ASCII//TRANSLIT")
##### Incluindo dados do SIAGRO 2020

AUX <- SIAGRO_2025[, c(1, 9)]

#### Tornando a coluna "Municipio" compatível para left_join

AUX$Município <- toupper(AUX$Município)

AUX$Município <- iconv(AUX$Município,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

AUX$Município <- str_replace(AUX$Município, "ITAPEJARA D'OESTE", "ITAPEJARA DOESTE")
AUX$Município <- str_replace(AUX$Município, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
AUX$Município <- str_replace(AUX$Município, "PEROLA D'OESTE", "PEROLA DOESTE")
AUX$Município <- str_replace(AUX$Município, "SAO JORGE D'OESTE", "SAO JORGE DOESTE")
AUX$Município <- str_replace(AUX$Município, "RANCHO ALEGRE D'OESTE", "RANCHO ALEGRE DOESTE")
AUX$Município <- str_replace(AUX$Município, "SANTA CRUZ DE MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
AUX$Município <- str_replace(AUX$Município, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

PR_DERAL_2020_SIMPLIFICADO <- left_join(PR_DERAL_2020_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "Município"))

colnames(PR_DERAL_2020_SIMPLIFICADO)[8] <- "TON_AGRO_2020"

###  Retirando acento dos municípios para poder fazer left_join com as tabelas

PR_DERAL_2020_CULTIVOS_MUNICIPIOS$MUNICIPIO <- iconv(PR_DERAL_2020_CULTIVOS_MUNICIPIOS$MUNICIPIO,
                                                         from = "UTF-8",
                                                         to = "ASCII//TRANSLIT")

for (i in PR_DERAL_2020_SIMPLIFICADO[, 3]){
  PR_DERAL_2020_CULTIVOS_MUNICIPIOS[which(PR_DERAL_2020_CULTIVOS_MUNICIPIOS$MUNICIPIO == i), 15] <-  PR_DERAL_2020_SIMPLIFICADO[which(PR_DERAL_2020_SIMPLIFICADO$Município_sem_Código == i), 1]
  
}

colnames(PR_DERAL_2020_CULTIVOS_MUNICIPIOS)[15] <- "RS"

PR_DERAL_2020_CULTIVOS_MUNICIPIOS <- PR_DERAL_2020_CULTIVOS_MUNICIPIOS[, -c(10, 11, 12)]

rm(VBP_2020)

#######  2021

VBP_2021$MUNICIPIO <- toupper(VBP_2021$MUNICIPIO)

VBP_2021$MUNICIPIO <- str_replace(VBP_2021$MUNICIPIO, "ITAPEJARA DO OESTE", "ITAPEJARA DOESTE")
VBP_2021$MUNICIPIO <- str_replace(VBP_2021$MUNICIPIO, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
VBP_2021$MUNICIPIO <- str_replace(VBP_2021$MUNICIPIO, "PEROLA DO OESTE", "PEROLA DOESTE")
VBP_2021$MUNICIPIO <- str_replace(VBP_2021$MUNICIPIO, "SAO JORGE DO OESTE", "SAO JORGE DOESTE")
VBP_2021$MUNICIPIO <- str_replace(VBP_2021$MUNICIPIO, "SANTA TEREZINHA DO ITAIPU", "SANTA TEREZINHA DE ITAIPU")
VBP_2021$MUNICIPIO <- str_replace(VBP_2021$MUNICIPIO, "RANCHO ALEGRE DO OESTE", "RANCHO ALEGRE DOESTE")
VBP_2021$MUNICIPIO <- str_replace(VBP_2021$MUNICIPIO, "SAUDADES DO IGUACU", "SAUDADE DO IGUACU")
VBP_2021$MUNICIPIO <- str_replace(VBP_2021$MUNICIPIO, "SANTA CRUZ DO MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
VBP_2021$MUNICIPIO <- str_replace(VBP_2021$MUNICIPIO, "SANTA IZABEL DO IVAI", "SANTA ISABEL DO IVAI")
VBP_2021$MUNICIPIO <- str_replace(VBP_2021$MUNICIPIO, "MUNHOZ DE MELO", "MUNHOZ DE MELLO")
VBP_2021$MUNICIPIO <- str_replace(VBP_2021$MUNICIPIO, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

VBP_2021$VALOR_Reais <- as.numeric(gsub(",", ".", VBP_2021$VALOR_Reais))
VBP_2021$PRODUCAO <- as.numeric(gsub(",", ".", VBP_2021$PRODUCAO))
VBP_2021$AREA_HA <- as.numeric(gsub(",", ".", VBP_2021$AREA_HA))

#######   Fazendo a soma de valor total produzido de cada município

## Vai ser filtrado os cultivos em que o SIAGRO 2025 
## aponta como que utilizam agrotóxicos

AUX <- VBP_2021 %>%
  filter(CULTURA == "Soja (1ª safra)" |
           CULTURA == "Soja (2ª safra)" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "Milho (1ª safra)" |
           CULTURA == "Milho (2ª safra)" |
           CULTURA == "Trigo" |
           CULTURA == "Feijão (1ª safra)" |
           CULTURA == "Feijão (2ª safra)" |
           CULTURA == "Feijão (3ª safra)" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "Batata (1ª safra)" |
           CULTURA == "Batata (2ª safra)" |
           CULTURA == "Cana-de-açúcar" |
           CULTURA == "Fumo" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "Tomate (1ª safra)" |
           CULTURA == "Tomate (2ª safra)" |
           CULTURA == "Café" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "Mandioca Consumo Humano" |
           CULTURA == "Mandioca  Indústria/Consumo Animal" |
           CULTURA == "Aveia branca" |
           CULTURA == "Aveia preta" |
           CULTURA == "Cebola" |
           CULTURA == "Arroz irrigado" |
           CULTURA == "Arroz de sequeiro" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "Cevada" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "Amendoim (1ª safra)" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(VALOR_Reais))

### Criando um objeto com os dados do DERAL/SIAGRO

PR_DERAL_2021_SIMPLIFICADO <- BASE_IBGE[, c(1, 2, 3, 5)]

# PR_DERAL_2021_SIMPLIFICADO$Município_sem_Código <- iconv(PR_DERAL_2021_SIMPLIFICADO$Município_sem_Código,
#                                                          from = "UTF-8", 
#                                                          to = "ASCII//TRANSLIT")

PR_DERAL_2021_SIMPLIFICADO <- left_join(PR_DERAL_2021_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2021_SIMPLIFICADO)[5] <- "VALOR_REAIS"

#######   Fazendo a soma da área total cultivada de cada município

VBP_2021$AREA_HA[VBP_2021$AREA_HA == ""] <- NA

AUX <- VBP_2021 %>%
  filter(!is.na(AREA_HA),
         CULTURA == "Soja (1ª safra)" |
           CULTURA == "Soja (2ª safra)" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (espiga)" |
           CULTURA == "Milho (1ª safra)" |
           CULTURA == "Milho (2ª safra)" |
           CULTURA == "Trigo" |
           CULTURA == "Feijão (1ª safra)" |
           CULTURA == "Feijão (2ª safra)" |
           CULTURA == "Feijão (3ª safra)" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "Batata (1ª safra)" |
           CULTURA == "Batata (2ª safra)" |
           CULTURA == "Cana-de-açúcar" |
           CULTURA == "Fumo" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "Tomate (1ª safra)" |
           CULTURA == "Tomate (2ª safra)" |
           CULTURA == "Café" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "Mandioca Consumo Humano" |
           CULTURA == "Mandioca  Indústria/Consumo Animal" |
           CULTURA == "Aveia branca" |
           CULTURA == "Aveia preta" |
           CULTURA == "Cebola" |
           CULTURA == "Arroz irrigado" |
           CULTURA == "Arroz de sequeiro" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "Cevada" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "Amendoim (1ª safra)" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2021_SIMPLIFICADO <- left_join(PR_DERAL_2021_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2021_SIMPLIFICADO)[6] <- "AREA_HA"

#####  Separando as cinco maiores culturas de cada município

AUX <- VBP_2021 %>%filter(CULTURA == "Soja (1ª safra)" |
                            CULTURA == "Soja (2ª safra)" |
                            CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
                            CULTURA == "MILHO VERDE (espiga)" |
                            CULTURA == "Milho (1ª safra)" |
                            CULTURA == "Milho (2ª safra)" |
                            CULTURA == "Trigo" |
                            CULTURA == "Feijão (1ª safra)" |
                            CULTURA == "Feijão (2ª safra)" |
                            CULTURA == "Feijão (3ª safra)" |
                            CULTURA == "FEIJAO-VAGEM" |
                            CULTURA == "PASTAGENS E FORRAGENS" |
                            CULTURA == "Batata (1ª safra)" |
                            CULTURA == "Batata (2ª safra)" |
                            CULTURA == "Cana-de-açúcar" |
                            CULTURA == "Fumo" |
                            CULTURA == "UVA DE MESA" |
                            CULTURA == "UVA VINIFERA" |
                            CULTURA == "Tomate (1ª safra)" |
                            CULTURA == "Tomate (2ª safra)" |
                            CULTURA == "Café" |
                            CULTURA == "LARANJA" |
                            CULTURA == "LIMAO" |
                            CULTURA == "TANGERINA MONTENEGRINA" |
                            CULTURA == "TANGERINA PONKAN" |
                            CULTURA == "TANGERINA MURCOTE" |
                            CULTURA == "Mandioca Consumo Humano" |
                            CULTURA == "Mandioca  Indústria/Consumo Animal" |
                            CULTURA == "Aveia branca" |
                            CULTURA == "Aveia preta" |
                            CULTURA == "Cebola" |
                            CULTURA == "Arroz irrigado" |
                            CULTURA == "Arroz de sequeiro" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
                            CULTURA == "MACA" |
                            CULTURA == "Cevada" |
                            CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
                            CULTURA == "AZEVEM GRAOS" |
                            CULTURA == "MELANCIA" |
                            CULTURA == "BANANA" |
                            CULTURA == "PEPINO" |
                            CULTURA == "Amendoim (1ª safra)" |
                            CULTURA == "PESSEGO" |
                            CULTURA == "PIMENTAO" |
                            CULTURA == "REPOLHO" |
                            CULTURA == "MORANGO (moranguinho)") %>%
  group_by(MUNICIPIO) %>%
  arrange(desc(AREA_HA)) %>%
  arrange(MUNICIPIO) %>%
  group_by(MUNICIPIO) %>%
  slice_head(n = 6)

PR_DERAL_2021_CULTIVOS_MUNICIPIOS <- AUX

##### Filtrando Mata nativa

AUX <- VBP_2021 %>%filter(CULTURA == "MATA NATIVA" ) %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2021_SIMPLIFICADO <- left_join(PR_DERAL_2021_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2021_SIMPLIFICADO)[7] <- "MATA_NATIVA"

###  Retirando acento dos municípios para poder fazer left_join com as tabelas

PR_DERAL_2021_SIMPLIFICADO$Município_sem_Código <- iconv(PR_DERAL_2021_SIMPLIFICADO$Município_sem_Código,
                                                         from = "UTF-8", 
                                                         to = "ASCII//TRANSLIT")

##### Incluindo dados do SIAGRO 2021

AUX <- SIAGRO_2025[, c(1, 10)]

#### Tornando a coluna "Municipio" compatível para left_join

AUX$Município <- toupper(AUX$Município)

AUX$Município <- iconv(AUX$Município,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

AUX$Município <- str_replace(AUX$Município, "ITAPEJARA D'OESTE", "ITAPEJARA DOESTE")
AUX$Município <- str_replace(AUX$Município, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
AUX$Município <- str_replace(AUX$Município, "PEROLA D'OESTE", "PEROLA DOESTE")
AUX$Município <- str_replace(AUX$Município, "SAO JORGE D'OESTE", "SAO JORGE DOESTE")
AUX$Município <- str_replace(AUX$Município, "RANCHO ALEGRE D'OESTE", "RANCHO ALEGRE DOESTE")
AUX$Município <- str_replace(AUX$Município, "SANTA CRUZ DE MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
AUX$Município <- str_replace(AUX$Município, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

PR_DERAL_2021_SIMPLIFICADO <- left_join(PR_DERAL_2021_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "Município"))

colnames(PR_DERAL_2021_SIMPLIFICADO)[8] <- "TON_AGRO_2021"

PR_DERAL_2021_CULTIVOS_MUNICIPIOS$MUNICIPIO <- iconv(PR_DERAL_2021_CULTIVOS_MUNICIPIOS$MUNICIPIO,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

for (i in PR_DERAL_2021_SIMPLIFICADO[, 3]){
  PR_DERAL_2021_CULTIVOS_MUNICIPIOS[which(PR_DERAL_2021_CULTIVOS_MUNICIPIOS$MUNICIPIO == i), 17] <-  PR_DERAL_2021_SIMPLIFICADO[which(PR_DERAL_2021_SIMPLIFICADO$Município_sem_Código == i), 1]
  
}

colnames(PR_DERAL_2021_CULTIVOS_MUNICIPIOS)[17] <- "RS"

PR_DERAL_2021_CULTIVOS_MUNICIPIOS <- PR_DERAL_2021_CULTIVOS_MUNICIPIOS[, -c(8, 9, 10)]

rm(VBP_2021)

#######  2022

VBP_2022$MUNICIPIO <- toupper(VBP_2022$MUNICIPIO)

VBP_2022$MUNICIPIO <- str_replace(VBP_2022$MUNICIPIO, "ITAPEJARA DO OESTE", "ITAPEJARA DOESTE")
VBP_2022$MUNICIPIO <- str_replace(VBP_2022$MUNICIPIO, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
VBP_2022$MUNICIPIO <- str_replace(VBP_2022$MUNICIPIO, "PEROLA DO OESTE", "PEROLA DOESTE")
VBP_2022$MUNICIPIO <- str_replace(VBP_2022$MUNICIPIO, "SAO JORGE DO OESTE", "SAO JORGE DOESTE")
VBP_2022$MUNICIPIO <- str_replace(VBP_2022$MUNICIPIO, "SANTA TEREZINHA DO ITAIPU", "SANTA TEREZINHA DE ITAIPU")
VBP_2022$MUNICIPIO <- str_replace(VBP_2022$MUNICIPIO, "RANCHO ALEGRE DO OESTE", "RANCHO ALEGRE DOESTE")
VBP_2022$MUNICIPIO <- str_replace(VBP_2022$MUNICIPIO, "SAUDADES DO IGUACU", "SAUDADE DO IGUACU")
VBP_2022$MUNICIPIO <- str_replace(VBP_2022$MUNICIPIO, "SANTA CRUZ DO MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
VBP_2022$MUNICIPIO <- str_replace(VBP_2022$MUNICIPIO, "SANTA IZABEL DO IVAI", "SANTA ISABEL DO IVAI")
VBP_2022$MUNICIPIO <- str_replace(VBP_2022$MUNICIPIO, "MUNHOZ DE MELO", "MUNHOZ DE MELLO")
VBP_2022$MUNICIPIO <- str_replace(VBP_2022$MUNICIPIO, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

VBP_2022$VALOR_Reais <- as.numeric(gsub(",", ".", VBP_2022$VALOR_Reais))
VBP_2022$PRODUCAO <- as.numeric(gsub(",", ".", VBP_2022$PRODUCAO))
VBP_2022$AREA_HA <- as.numeric(gsub(",", ".", VBP_2022$AREA_HA))

#######   Fazendo a soma de valor total produzido de cada município

## Vai ser filtrado os cultivos em que o SIAGRO 2025 
## aponta como que utilizam agrotóxicos

AUX <- VBP_2022 %>%
  filter(CULTURA == "SOJA (1ª SAFRA)" |
           CULTURA == "SOJA (2ª SAFRA)" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (ESPIGA)" |
           CULTURA == "MILHO (1ª SAFRA)" |
           CULTURA == "MILHO (2ª SAFRA)" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJÃO (1ª SAFRA)" |
           CULTURA == "FEIJÃO (2ª SAFRA)" |
           CULTURA == "FEIJÃO (3ª SAFRA)" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA (1ª SAFRA)" |
           CULTURA == "BATATA (2ª SAFRA)" |
           CULTURA == "CANA-DE-AÇÚCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "TOMATE (1ª SAFRA)" |
           CULTURA == "TOMATE (2ª SAFRA)" |
           CULTURA == "CAFÉ" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO HUMANO" |
           CULTURA == "MANDIOCA  INDÚSTRIA/CONSUMO ANIMAL" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ DE SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM (1ª SAFRA)" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (MORANGUINHO)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(VALOR_Reais))

### Criando um objeto com os dados do DERAL/SIAGRO

PR_DERAL_2022_SIMPLIFICADO <- BASE_IBGE[, c(1, 2, 3, 5)]

PR_DERAL_2022_SIMPLIFICADO <- left_join(PR_DERAL_2022_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2022_SIMPLIFICADO)[5] <- "VALOR_REAIS"

#######   Fazendo a soma da área total cultivada de cada município

VBP_2022$AREA_HA[VBP_2022$AREA_HA == ""] <- NA

AUX <- VBP_2022 %>%
  filter(!is.na(AREA_HA),
         CULTURA == "SOJA (1ª SAFRA)" |
           CULTURA == "SOJA (2ª SAFRA)" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (ESPIGA)" |
           CULTURA == "MILHO (1ª SAFRA)" |
           CULTURA == "MILHO (2ª SAFRA)" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJÃO (1ª SAFRA)" |
           CULTURA == "FEIJÃO (2ª SAFRA)" |
           CULTURA == "FEIJÃO (3ª SAFRA)" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA (1ª SAFRA)" |
           CULTURA == "BATATA (2ª SAFRA)" |
           CULTURA == "CANA-DE-AÇÚCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA VINIFERA" |
           CULTURA == "TOMATE (1ª SAFRA)" |
           CULTURA == "TOMATE (2ª SAFRA)" |
           CULTURA == "CAFÉ" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO HUMANO" |
           CULTURA == "MANDIOCA  INDÚSTRIA/CONSUMO ANIMAL" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ DE SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM (1ª SAFRA)" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (MORANGUINHO)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2022_SIMPLIFICADO <- left_join(PR_DERAL_2022_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2022_SIMPLIFICADO)[6] <- "AREA_HA"

#####  Separando as cinco maiores culturas de cada município

AUX <- VBP_2022 %>%filter(CULTURA == "SOJA (1ª SAFRA)" |
                            CULTURA == "SOJA (2ª SAFRA)" |
                            CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
                            CULTURA == "MILHO VERDE (ESPIGA)" |
                            CULTURA == "MILHO (1ª SAFRA)" |
                            CULTURA == "MILHO (2ª SAFRA)" |
                            CULTURA == "TRIGO" |
                            CULTURA == "FEIJÃO (1ª SAFRA)" |
                            CULTURA == "FEIJÃO (2ª SAFRA)" |
                            CULTURA == "FEIJÃO (3ª SAFRA)" |
                            CULTURA == "FEIJAO-VAGEM" |
                            CULTURA == "PASTAGENS E FORRAGENS" |
                            CULTURA == "BATATA (1ª SAFRA)" |
                            CULTURA == "BATATA (2ª SAFRA)" |
                            CULTURA == "CANA-DE-AÇÚCAR" |
                            CULTURA == "FUMO" |
                            CULTURA == "UVA DE MESA" |
                            CULTURA == "UVA VINIFERA" |
                            CULTURA == "TOMATE (1ª SAFRA)" |
                            CULTURA == "TOMATE (2ª SAFRA)" |
                            CULTURA == "CAFÉ" |
                            CULTURA == "LARANJA" |
                            CULTURA == "LIMAO" |
                            CULTURA == "TANGERINA MONTENEGRINA" |
                            CULTURA == "TANGERINA PONKAN" |
                            CULTURA == "TANGERINA MURCOTE" |
                            CULTURA == "MANDIOCA CONSUMO HUMANO" |
                            CULTURA == "MANDIOCA  INDÚSTRIA/CONSUMO ANIMAL" |
                            CULTURA == "AVEIA BRANCA" |
                            CULTURA == "AVEIA PRETA" |
                            CULTURA == "CEBOLA" |
                            CULTURA == "ARROZ IRRIGADO" |
                            CULTURA == "ARROZ DE SEQUEIRO" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
                            CULTURA == "MACA" |
                            CULTURA == "CEVADA" |
                            CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
                            CULTURA == "AZEVEM GRAOS" |
                            CULTURA == "MELANCIA" |
                            CULTURA == "BANANA" |
                            CULTURA == "PEPINO" |
                            CULTURA == "AMENDOIM (1ª SAFRA)" |
                            CULTURA == "PESSEGO" |
                            CULTURA == "PIMENTAO" |
                            CULTURA == "REPOLHO" |
                            CULTURA == "MORANGO (MORANGUINHO)") %>%
  group_by(MUNICIPIO) %>%
  arrange(desc(AREA_HA)) %>%
  arrange(MUNICIPIO) %>%
  group_by(MUNICIPIO) %>%
  slice_head(n = 6)

PR_DERAL_2022_CULTIVOS_MUNICIPIOS <- AUX

##### Filtrando Mata nativa

AUX <- VBP_2022 %>%filter(CULTURA == "MATA NATIVA" ) %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2022_SIMPLIFICADO <- left_join(PR_DERAL_2022_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2022_SIMPLIFICADO)[7] <- "MATA_NATIVA"

###  Retirando acento dos municípios para poder fazer left_join com as tabelas

PR_DERAL_2022_SIMPLIFICADO$Município_sem_Código <- iconv(PR_DERAL_2022_SIMPLIFICADO$Município_sem_Código,
                                                         from = "UTF-8", 
                                                         to = "ASCII//TRANSLIT")

##### Incluindo dados do SIAGRO 2022

AUX <- SIAGRO_2025[, c(1, 11)]

#### Tornando a coluna "Municipio" compatível para left_join

AUX$Município <- toupper(AUX$Município)

AUX$Município <- iconv(AUX$Município,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

AUX$Município <- str_replace(AUX$Município, "ITAPEJARA D'OESTE", "ITAPEJARA DOESTE")
AUX$Município <- str_replace(AUX$Município, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
AUX$Município <- str_replace(AUX$Município, "PEROLA D'OESTE", "PEROLA DOESTE")
AUX$Município <- str_replace(AUX$Município, "SAO JORGE D'OESTE", "SAO JORGE DOESTE")
AUX$Município <- str_replace(AUX$Município, "RANCHO ALEGRE D'OESTE", "RANCHO ALEGRE DOESTE")
AUX$Município <- str_replace(AUX$Município, "SANTA CRUZ DE MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
AUX$Município <- str_replace(AUX$Município, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

PR_DERAL_2022_SIMPLIFICADO <- left_join(PR_DERAL_2022_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "Município"))

colnames(PR_DERAL_2022_SIMPLIFICADO)[8] <- "TON_AGRO_2022"

PR_DERAL_2022_CULTIVOS_MUNICIPIOS$MUNICIPIO <- iconv(PR_DERAL_2022_CULTIVOS_MUNICIPIOS$MUNICIPIO,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

for (i in PR_DERAL_2022_SIMPLIFICADO[, 3]){
  PR_DERAL_2022_CULTIVOS_MUNICIPIOS[which(PR_DERAL_2022_CULTIVOS_MUNICIPIOS$MUNICIPIO == i), 18] <-  PR_DERAL_2022_SIMPLIFICADO[which(PR_DERAL_2022_SIMPLIFICADO$Município_sem_Código == i), 1]
  
}

colnames(PR_DERAL_2022_CULTIVOS_MUNICIPIOS)[18] <- "RS"

PR_DERAL_2022_CULTIVOS_MUNICIPIOS <- PR_DERAL_2022_CULTIVOS_MUNICIPIOS[, -c(8, 9, 10)]

rm(VBP_2022)

#######  2023

VBP_2023$MUNICIPIO <- toupper(VBP_2023$MUNICIPIO)

VBP_2023$MUNICIPIO <- str_replace(VBP_2023$MUNICIPIO, "ITAPEJARA DO OESTE", "ITAPEJARA DOESTE")
VBP_2023$MUNICIPIO <- str_replace(VBP_2023$MUNICIPIO, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
VBP_2023$MUNICIPIO <- str_replace(VBP_2023$MUNICIPIO, "PEROLA DO OESTE", "PEROLA DOESTE")
VBP_2023$MUNICIPIO <- str_replace(VBP_2023$MUNICIPIO, "SAO JORGE DO OESTE", "SAO JORGE DOESTE")
VBP_2023$MUNICIPIO <- str_replace(VBP_2023$MUNICIPIO, "SANTA TEREZINHA DO ITAIPU", "SANTA TEREZINHA DE ITAIPU")
VBP_2023$MUNICIPIO <- str_replace(VBP_2023$MUNICIPIO, "RANCHO ALEGRE DO OESTE", "RANCHO ALEGRE DOESTE")
VBP_2023$MUNICIPIO <- str_replace(VBP_2023$MUNICIPIO, "SAUDADES DO IGUACU", "SAUDADE DO IGUACU")
VBP_2023$MUNICIPIO <- str_replace(VBP_2023$MUNICIPIO, "SANTA CRUZ DO MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
VBP_2023$MUNICIPIO <- str_replace(VBP_2023$MUNICIPIO, "SANTA IZABEL DO IVAI", "SANTA ISABEL DO IVAI")
VBP_2023$MUNICIPIO <- str_replace(VBP_2023$MUNICIPIO, "MUNHOZ DE MELO", "MUNHOZ DE MELLO")
VBP_2023$MUNICIPIO <- str_replace(VBP_2023$MUNICIPIO, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

VBP_2023$VALOR_Reais <- as.numeric(gsub(",", ".", VBP_2023$VALOR_Reais))
VBP_2023$PRODUCAO <- as.numeric(gsub(",", ".", VBP_2023$PRODUCAO))
VBP_2023$AREA_HA <- as.numeric(gsub(",", ".", VBP_2023$AREA_HA))

#######   Fazendo a soma de valor total produzido de cada município

## Vai ser filtrado os cultivos em que o SIAGRO 2025 
## aponta como que utilizam agrotóxicos

AUX <- VBP_2023 %>%
  filter(CULTURA == "SOJA (1ª SAFRA)" |
           CULTURA == "SOJA (2ª SAFRA)" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (ESPIGA)" |
           CULTURA == "MILHO (1ª SAFRA)" |
           CULTURA == "MILHO (2ª SAFRA)" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJÃO (1ª SAFRA)" |
           CULTURA == "FEIJÃO (2ª SAFRA)" |
           CULTURA == "FEIJÃO (3ª SAFRA)" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA (1ª SAFRA)" |
           CULTURA == "BATATA (2ª SAFRA)" |
           CULTURA == "CANA-DE-AÇÚCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA TRANSFORMAÇÃO" |
           CULTURA == "TOMATE (1ª SAFRA)" |
           CULTURA == "TOMATE (2ª SAFRA)" |
           CULTURA == "CAFÉ" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO HUMANO" |
           CULTURA == "MANDIOCA  INDÚSTRIA/CONSUMO ANIMAL" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ DE SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM (1ª SAFRA)" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (MORANGUINHO)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(VALOR_Reais))

### Criando um objeto com os dados do DERAL/SIAGRO

PR_DERAL_2023_SIMPLIFICADO <- BASE_IBGE[, c(1, 2, 3, 5)]

PR_DERAL_2023_SIMPLIFICADO <- left_join(PR_DERAL_2023_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2023_SIMPLIFICADO)[5] <- "VALOR_REAIS"

#######   Fazendo a soma da área total cultivada de cada município

VBP_2023$AREA_HA[VBP_2023$AREA_HA == ""] <- NA

AUX <- VBP_2023 %>%
  filter(!is.na(AREA_HA),
         CULTURA == "SOJA (1ª SAFRA)" |
           CULTURA == "SOJA (2ª SAFRA)" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (ESPIGA)" |
           CULTURA == "MILHO (1ª SAFRA)" |
           CULTURA == "MILHO (2ª SAFRA)" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJÃO (1ª SAFRA)" |
           CULTURA == "FEIJÃO (2ª SAFRA)" |
           CULTURA == "FEIJÃO (3ª SAFRA)" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA (1ª SAFRA)" |
           CULTURA == "BATATA (2ª SAFRA)" |
           CULTURA == "CANA-DE-AÇÚCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA TRANSFORMAÇÃO" |
           CULTURA == "TOMATE (1ª SAFRA)" |
           CULTURA == "TOMATE (2ª SAFRA)" |
           CULTURA == "CAFÉ" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO HUMANO" |
           CULTURA == "MANDIOCA  INDÚSTRIA/CONSUMO ANIMAL" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ DE SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM (1ª SAFRA)" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (MORANGUINHO)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2023_SIMPLIFICADO <- left_join(PR_DERAL_2023_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2023_SIMPLIFICADO)[6] <- "AREA_HA"

#####  Separando as cinco maiores culturas de cada município

AUX <- VBP_2023 %>%filter(CULTURA == "SOJA (1ª SAFRA)" |
                            CULTURA == "SOJA (2ª SAFRA)" |
                            CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
                            CULTURA == "MILHO VERDE (ESPIGA)" |
                            CULTURA == "MILHO (1ª SAFRA)" |
                            CULTURA == "MILHO (2ª SAFRA)" |
                            CULTURA == "TRIGO" |
                            CULTURA == "FEIJÃO (1ª SAFRA)" |
                            CULTURA == "FEIJÃO (2ª SAFRA)" |
                            CULTURA == "FEIJÃO (3ª SAFRA)" |
                            CULTURA == "FEIJAO-VAGEM" |
                            CULTURA == "PASTAGENS E FORRAGENS" |
                            CULTURA == "BATATA (1ª SAFRA)" |
                            CULTURA == "BATATA (2ª SAFRA)" |
                            CULTURA == "CANA-DE-AÇÚCAR" |
                            CULTURA == "FUMO" |
                            CULTURA == "UVA DE MESA" |
                            CULTURA == "UVA TRANSFORMAÇÃO" |
                            CULTURA == "TOMATE (1ª SAFRA)" |
                            CULTURA == "TOMATE (2ª SAFRA)" |
                            CULTURA == "CAFÉ" |
                            CULTURA == "LARANJA" |
                            CULTURA == "LIMAO" |
                            CULTURA == "TANGERINA MONTENEGRINA" |
                            CULTURA == "TANGERINA PONKAN" |
                            CULTURA == "TANGERINA MURCOTE" |
                            CULTURA == "MANDIOCA CONSUMO HUMANO" |
                            CULTURA == "MANDIOCA  INDÚSTRIA/CONSUMO ANIMAL" |
                            CULTURA == "AVEIA BRANCA" |
                            CULTURA == "AVEIA PRETA" |
                            CULTURA == "CEBOLA" |
                            CULTURA == "ARROZ IRRIGADO" |
                            CULTURA == "ARROZ DE SEQUEIRO" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
                            CULTURA == "MACA" |
                            CULTURA == "CEVADA" |
                            CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
                            CULTURA == "AZEVEM GRAOS" |
                            CULTURA == "MELANCIA" |
                            CULTURA == "BANANA" |
                            CULTURA == "PEPINO" |
                            CULTURA == "AMENDOIM (1ª SAFRA)" |
                            CULTURA == "PESSEGO" |
                            CULTURA == "PIMENTAO" |
                            CULTURA == "REPOLHO" |
                            CULTURA == "MORANGO (MORANGUINHO)") %>%
  group_by(MUNICIPIO) %>%
  arrange(desc(AREA_HA)) %>%
  arrange(MUNICIPIO) %>%
  group_by(MUNICIPIO) %>%
  slice_head(n = 6)

PR_DERAL_2023_CULTIVOS_MUNICIPIOS <- AUX

##### Filtrando Mata nativa

AUX <- VBP_2023 %>%filter(CULTURA == "MATA NATIVA" ) %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2023_SIMPLIFICADO <- left_join(PR_DERAL_2023_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2023_SIMPLIFICADO)[7] <- "MATA_NATIVA"

###  Retirando acento dos municípios para poder fazer left_join com as tabelas

PR_DERAL_2023_SIMPLIFICADO$Município_sem_Código <- iconv(PR_DERAL_2023_SIMPLIFICADO$Município_sem_Código,
                                                         from = "UTF-8", 
                                                         to = "ASCII//TRANSLIT")

##### Incluindo dados do SIAGRO 2023

AUX <- SIAGRO_2025[, c(1, 12)]

#### Tornando a coluna "Municipio" compatível para left_join

AUX$Município <- toupper(AUX$Município)

AUX$Município <- iconv(AUX$Município,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

AUX$Município <- str_replace(AUX$Município, "ITAPEJARA D'OESTE", "ITAPEJARA DOESTE")
AUX$Município <- str_replace(AUX$Município, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
AUX$Município <- str_replace(AUX$Município, "PEROLA D'OESTE", "PEROLA DOESTE")
AUX$Município <- str_replace(AUX$Município, "SAO JORGE D'OESTE", "SAO JORGE DOESTE")
AUX$Município <- str_replace(AUX$Município, "RANCHO ALEGRE D'OESTE", "RANCHO ALEGRE DOESTE")
AUX$Município <- str_replace(AUX$Município, "SANTA CRUZ DE MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
AUX$Município <- str_replace(AUX$Município, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

PR_DERAL_2023_SIMPLIFICADO <- left_join(PR_DERAL_2023_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "Município"))

colnames(PR_DERAL_2023_SIMPLIFICADO)[8] <- "TON_AGRO_2023"

PR_DERAL_2023_CULTIVOS_MUNICIPIOS$MUNICIPIO <- iconv(PR_DERAL_2023_CULTIVOS_MUNICIPIOS$MUNICIPIO,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

for (i in PR_DERAL_2023_SIMPLIFICADO[, 3]){
  PR_DERAL_2023_CULTIVOS_MUNICIPIOS[which(PR_DERAL_2023_CULTIVOS_MUNICIPIOS$MUNICIPIO == i), 18] <-  PR_DERAL_2023_SIMPLIFICADO[which(PR_DERAL_2023_SIMPLIFICADO$Município_sem_Código == i), 1]
  
}

colnames(PR_DERAL_2023_CULTIVOS_MUNICIPIOS)[18] <- "RS"

PR_DERAL_2023_CULTIVOS_MUNICIPIOS <- PR_DERAL_2023_CULTIVOS_MUNICIPIOS[, -c(8, 9, 10)]

rm(VBP_2023)

#######  2024

VBP_2024$MUNICIPIO <- toupper(VBP_2024$MUNICIPIO)

VBP_2024$MUNICIPIO <- str_replace(VBP_2024$MUNICIPIO, "ITAPEJARA D'OESTE", "ITAPEJARA DOESTE")
VBP_2024$MUNICIPIO <- str_replace(VBP_2024$MUNICIPIO, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
VBP_2024$MUNICIPIO <- str_replace(VBP_2024$MUNICIPIO, "PÉROLA D'OESTE", "PÉROLA DOESTE")
VBP_2024$MUNICIPIO <- str_replace(VBP_2024$MUNICIPIO, "SÃO JORGE D'OESTE", "SÃO JORGE DOESTE")
VBP_2024$MUNICIPIO <- str_replace(VBP_2024$MUNICIPIO, "SANTA TEREZINHA DO ITAIPU", "SANTA TEREZINHA DE ITAIPU")
VBP_2024$MUNICIPIO <- str_replace(VBP_2024$MUNICIPIO, "RANCHO ALEGRE D'OESTE", "RANCHO ALEGRE DOESTE")
VBP_2024$MUNICIPIO <- str_replace(VBP_2024$MUNICIPIO, "SAUDADES DO IGUACU", "SAUDADE DO IGUACU")
VBP_2024$MUNICIPIO <- str_replace(VBP_2024$MUNICIPIO, "SANTA CRUZ DE MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
VBP_2024$MUNICIPIO <- str_replace(VBP_2024$MUNICIPIO, "SANTA IZABEL DO IVAI", "SANTA ISABEL DO IVAI")
VBP_2024$MUNICIPIO <- str_replace(VBP_2024$MUNICIPIO, "MUNHOZ DE MELO", "MUNHOZ DE MELLO")
VBP_2024$MUNICIPIO <- str_replace(VBP_2024$MUNICIPIO, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

VBP_2024$VALOR_Reais <- as.numeric(gsub(",", ".", VBP_2024$VALOR_Reais))
VBP_2024$PRODUCAO <- as.numeric(gsub(",", ".", VBP_2024$PRODUCAO))
VBP_2024$AREA_HA <- as.numeric(gsub(",", ".", VBP_2024$AREA_HA))

#######   Fazendo a soma de valor total produzido de cada município

## Vai ser filtrado os cultivos em que o SIAGRO 2025 
## aponta como que utilizam agrotóxicos

AUX <- VBP_2024 %>%
  filter(CULTURA == "SOJA (1ª SAFRA)" |
           CULTURA == "SOJA (2ª SAFRA)" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (ESPIGA)" |
           CULTURA == "MILHO (1ª SAFRA)" |
           CULTURA == "MILHO (2ª SAFRA)" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJÃO (1ª SAFRA)" |
           CULTURA == "FEIJÃO (2ª SAFRA)" |
           CULTURA == "FEIJÃO (3ª SAFRA)" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA (1ª SAFRA)" |
           CULTURA == "BATATA (2ª SAFRA)" |
           CULTURA == "CANA-DE-AÇÚCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA TRANSFORMAÇÃO" |
           CULTURA == "TOMATE (1ª SAFRA)" |
           CULTURA == "TOMATE (2ª SAFRA)" |
           CULTURA == "CAFÉ" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO HUMANO" |
           CULTURA == "MANDIOCA  INDÚSTRIA/CONSUMO ANIMAL" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ DE SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM (1ª SAFRA)" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (MORANGUINHO)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(VALOR_Reais))

### Criando um objeto com os dados do DERAL/SIAGRO

PR_DERAL_2024_SIMPLIFICADO <- BASE_IBGE[, c(1, 2, 3, 5)]

PR_DERAL_2024_SIMPLIFICADO <- left_join(PR_DERAL_2024_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2024_SIMPLIFICADO)[5] <- "VALOR_REAIS"

#######   Fazendo a soma da área total cultivada de cada município

VBP_2024$AREA_HA[VBP_2024$AREA_HA == ""] <- NA

AUX <- VBP_2024 %>%
  filter(!is.na(AREA_HA),
         CULTURA == "SOJA (1ª SAFRA)" |
           CULTURA == "SOJA (2ª SAFRA)" |
           CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
           CULTURA == "MILHO VERDE (ESPIGA)" |
           CULTURA == "MILHO (1ª SAFRA)" |
           CULTURA == "MILHO (2ª SAFRA)" |
           CULTURA == "TRIGO" |
           CULTURA == "FEIJÃO (1ª SAFRA)" |
           CULTURA == "FEIJÃO (2ª SAFRA)" |
           CULTURA == "FEIJÃO (3ª SAFRA)" |
           CULTURA == "FEIJAO-VAGEM" |
           CULTURA == "PASTAGENS E FORRAGENS" |
           CULTURA == "BATATA (1ª SAFRA)" |
           CULTURA == "BATATA (2ª SAFRA)" |
           CULTURA == "CANA-DE-AÇÚCAR" |
           CULTURA == "FUMO" |
           CULTURA == "UVA DE MESA" |
           CULTURA == "UVA TRANSFORMAÇÃO" |
           CULTURA == "TOMATE (1ª SAFRA)" |
           CULTURA == "TOMATE (2ª SAFRA)" |
           CULTURA == "CAFÉ" |
           CULTURA == "LARANJA" |
           CULTURA == "LIMAO" |
           CULTURA == "TANGERINA MONTENEGRINA" |
           CULTURA == "TANGERINA PONKAN" |
           CULTURA == "TANGERINA MURCOTE" |
           CULTURA == "MANDIOCA CONSUMO HUMANO" |
           CULTURA == "MANDIOCA  INDÚSTRIA/CONSUMO ANIMAL" |
           CULTURA == "AVEIA BRANCA" |
           CULTURA == "AVEIA PRETA" |
           CULTURA == "CEBOLA" |
           CULTURA == "ARROZ IRRIGADO" |
           CULTURA == "ARROZ DE SEQUEIRO" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
           CULTURA == "MACA" |
           CULTURA == "CEVADA" |
           CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
           CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
           CULTURA == "AZEVEM GRAOS" |
           CULTURA == "MELANCIA" |
           CULTURA == "BANANA" |
           CULTURA == "PEPINO" |
           CULTURA == "AMENDOIM (1ª SAFRA)" |
           CULTURA == "PESSEGO" |
           CULTURA == "PIMENTAO" |
           CULTURA == "REPOLHO" |
           CULTURA == "MORANGO (MORANGUINHO)") %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2024_SIMPLIFICADO <- left_join(PR_DERAL_2024_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2024_SIMPLIFICADO)[6] <- "AREA_HA"

#####  Separando as cinco maiores culturas de cada município

AUX <- VBP_2024 %>%filter(CULTURA == "SOJA (1ª SAFRA)" |
                            CULTURA == "SOJA (2ª SAFRA)" |
                            CULTURA == "SILAGEM DE MILHO E/OU SORGO" |
                            CULTURA == "MILHO VERDE (ESPIGA)" |
                            CULTURA == "MILHO (1ª SAFRA)" |
                            CULTURA == "MILHO (2ª SAFRA)" |
                            CULTURA == "TRIGO" |
                            CULTURA == "FEIJÃO (1ª SAFRA)" |
                            CULTURA == "FEIJÃO (2ª SAFRA)" |
                            CULTURA == "FEIJÃO (3ª SAFRA)" |
                            CULTURA == "FEIJAO-VAGEM" |
                            CULTURA == "PASTAGENS E FORRAGENS" |
                            CULTURA == "BATATA (1ª SAFRA)" |
                            CULTURA == "BATATA (2ª SAFRA)" |
                            CULTURA == "CANA-DE-AÇÚCAR" |
                            CULTURA == "FUMO" |
                            CULTURA == "UVA DE MESA" |
                            CULTURA == "UVA TRANSFORMAÇÃO" |
                            CULTURA == "TOMATE (1ª SAFRA)" |
                            CULTURA == "TOMATE (2ª SAFRA)" |
                            CULTURA == "CAFÉ" |
                            CULTURA == "LARANJA" |
                            CULTURA == "LIMAO" |
                            CULTURA == "TANGERINA MONTENEGRINA" |
                            CULTURA == "TANGERINA PONKAN" |
                            CULTURA == "TANGERINA MURCOTE" |
                            CULTURA == "MANDIOCA CONSUMO HUMANO" |
                            CULTURA == "MANDIOCA  INDÚSTRIA/CONSUMO ANIMAL" |
                            CULTURA == "AVEIA BRANCA" |
                            CULTURA == "AVEIA PRETA" |
                            CULTURA == "CEBOLA" |
                            CULTURA == "ARROZ IRRIGADO" |
                            CULTURA == "ARROZ DE SEQUEIRO" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - EUCALIPTO" |
                            CULTURA == "MACA" |
                            CULTURA == "CEVADA" |
                            CULTURA == "MADEIRAS - EM TORA P/LAMINADORA - PINUS" |
                            CULTURA == "MADEIRAS - EM TORA P/SERRARIA - PINUS" |
                            CULTURA == "AZEVEM GRAOS" |
                            CULTURA == "MELANCIA" |
                            CULTURA == "BANANA" |
                            CULTURA == "PEPINO" |
                            CULTURA == "AMENDOIM (1ª SAFRA)" |
                            CULTURA == "PESSEGO" |
                            CULTURA == "PIMENTAO" |
                            CULTURA == "REPOLHO" |
                            CULTURA == "MORANGO (MORANGUINHO)") %>%
  group_by(MUNICIPIO) %>%
  arrange(desc(AREA_HA)) %>%
  arrange(MUNICIPIO) %>%
  group_by(MUNICIPIO) %>%
  slice_head(n = 6)

PR_DERAL_2024_CULTIVOS_MUNICIPIOS <- AUX

##### Filtrando Mata nativa

AUX <- VBP_2024 %>%filter(CULTURA == "MATA NATIVA" ) %>%
  group_by(MUNICIPIO) %>%
  summarise(sum(AREA_HA))

PR_DERAL_2024_SIMPLIFICADO <- left_join(PR_DERAL_2024_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "MUNICIPIO"))

colnames(PR_DERAL_2024_SIMPLIFICADO)[7] <- "MATA_NATIVA"

###  Retirando acento dos municípios para poder fazer left_join com as tabelas

PR_DERAL_2024_SIMPLIFICADO$Município_sem_Código <- iconv(PR_DERAL_2024_SIMPLIFICADO$Município_sem_Código,
                                                         from = "UTF-8", 
                                                         to = "ASCII//TRANSLIT")

##### Incluindo dados do SIAGRO 2024

AUX <- SIAGRO_2025[, c(1, 13)]

#### Tornando a coluna "Municipio" compatível para left_join

AUX$Município <- toupper(AUX$Município)

AUX$Município <- iconv(AUX$Município,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

AUX$Município <- str_replace(AUX$Município, "ITAPEJARA D'OESTE", "ITAPEJARA DOESTE")
AUX$Município <- str_replace(AUX$Município, "BELA VISTA DA CAROBA", "BELA VISTA DO CAROBA")
AUX$Município <- str_replace(AUX$Município, "PEROLA D'OESTE", "PEROLA DOESTE")
AUX$Município <- str_replace(AUX$Município, "SAO JORGE D'OESTE", "SAO JORGE DOESTE")
AUX$Município <- str_replace(AUX$Município, "RANCHO ALEGRE D'OESTE", "RANCHO ALEGRE DOESTE")
AUX$Município <- str_replace(AUX$Município, "SANTA CRUZ DE MONTE CASTELO", "SANTA CRUZ MONTE CASTELO")
AUX$Município <- str_replace(AUX$Município, "DIAMANTE D'OESTE", "DIAMANTE DOESTE")

PR_DERAL_2024_SIMPLIFICADO <- left_join(PR_DERAL_2024_SIMPLIFICADO,
                                        AUX,
                                        by = c("Município_sem_Código" = "Município"))

colnames(PR_DERAL_2024_SIMPLIFICADO)[8] <- "TON_AGRO_2024"

PR_DERAL_2024_CULTIVOS_MUNICIPIOS$MUNICIPIO <- iconv(PR_DERAL_2024_CULTIVOS_MUNICIPIOS$MUNICIPIO,
                       from = "UTF-8", 
                       to = "ASCII//TRANSLIT")

for (i in PR_DERAL_2024_SIMPLIFICADO[, 3]){
  PR_DERAL_2024_CULTIVOS_MUNICIPIOS[which(PR_DERAL_2024_CULTIVOS_MUNICIPIOS$MUNICIPIO == i), 18] <-  PR_DERAL_2024_SIMPLIFICADO[which(PR_DERAL_2024_SIMPLIFICADO$Município_sem_Código == i), 1]
  
}

colnames(PR_DERAL_2024_CULTIVOS_MUNICIPIOS)[18] <- "RS"

PR_DERAL_2024_CULTIVOS_MUNICIPIOS <- PR_DERAL_2024_CULTIVOS_MUNICIPIOS[, -c(8, 9, 10)]

rm(VBP_2024,
   SIAGRO_2025)

####### Consumo Agrotóxico/HA

PR_DERAL_2024_SIMPLIFICADO$TON_AGRO_2024 <- as.numeric(gsub(",", ".", PR_DERAL_2024_SIMPLIFICADO$TON_AGRO_2024))


PR_DERAL_2024_SIMPLIFICADO$AGRO_HA <- PR_DERAL_2024_SIMPLIFICADO$TON_AGRO_2024/PR_DERAL_2024_SIMPLIFICADO$AREA_HA


####### Salvando arquivos

write.csv(PR_DERAL_2016_CULTIVOS_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2016_CULTIVOS_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2016_SIMPLIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2016_SIMPLIFICADO.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2017_CULTIVOS_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2017_CULTIVOS_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2017_SIMPLIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2017_SIMPLIFICADO.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2018_CULTIVOS_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2018_CULTIVOS_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2018_SIMPLIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2018_SIMPLIFICADO.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2019_CULTIVOS_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2019_CULTIVOS_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2019_SIMPLIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2019_SIMPLIFICADO.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2020_CULTIVOS_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2020_CULTIVOS_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2020_SIMPLIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2020_SIMPLIFICADO.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2021_CULTIVOS_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2021_CULTIVOS_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2021_SIMPLIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2021_SIMPLIFICADO.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2022_CULTIVOS_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2022_CULTIVOS_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2022_SIMPLIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2022_SIMPLIFICADO.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2023_CULTIVOS_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2023_CULTIVOS_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2023_SIMPLIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2023_SIMPLIFICADO.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2024_CULTIVOS_MUNICIPIOS, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2024_CULTIVOS_MUNICIPIOS.csv",
          row.names = FALSE)

write.csv(PR_DERAL_2024_SIMPLIFICADO, 
          "/home/gustavo/Área de trabalho/Análise_de_Dados/Tabulacoes_R/DERAL/PR_DERAL_2024_SIMPLIFICADO.csv",
          row.names = FALSE)

