rm(list = ls())

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

####  Libraries

library(spdep)
library(scales)
library(patchwork)
library(foreign)
library (dplyr)
library (ggplot2)
library(stringr)
library(lubridate)
library(ggspatial)
library(sf)
library(zoo) 
library(tidyr)
library(gt)

#### Criando objetos com tabulações realizadas previamente em outros scripts

PR_PEVASPEA_SINASC_Serie_Historica <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_Serie_Historica.csv",
                                                header = TRUE,
                                                sep = ",")

PR_PEVASPEA_SINASC_Serie_historica_Mun <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_Serie_historica_Mun.csv",
                                                    header = TRUE,
                                                    sep = ",")

PR_PEVASPEA_SINASC_RS_Serie_Historica <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_RS_Serie_Historica.csv",
                                                   header = TRUE,
                                                   sep = ",")

RS22_PEVASPEA_SINASC_Serie_historica_Mun <- read.csv (file = "Tabulacoes_R/SINASC/RS22_PEVASPEA_SINASC_Serie_historica_Mun.csv",
                                                      header = TRUE,
                                                      sep = ",")

RS22_PEVASPEA_SINASC_Serie_historica <- read.csv (file = "Tabulacoes_R/SINASC/RS22_PEVASPEA_SINASC_Serie_Historica.csv",
                                                  header = TRUE,
                                                  sep = ",")

PR_PEVASPEA_SINASC_NASC_SE_GERAL <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_NASC_SE_GERAL.csv",
                                              header = TRUE,
                                              sep = ",")

PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL.csv",
                                                header = TRUE,
                                                sep = ",")

PR_DERAL_2024_SIMPLIFICADO <- read.csv (file = "Tabulacoes_R/DERAL/PR_DERAL_2024_SIMPLIFICADO.csv",
                                        header = TRUE,
                                        sep = ",")

#### Criando Objetos Fonte para servirem de base para as fontes dos gráficos/mapas

Fonte <- "Fonte: SINASC. Base DBF acessada em 10/04/2026"
Fonte1 <- "Fonte: SIAGRO. Base atualizada em 12/2025"
Fonte2 <- "Fonte: DERAL. Acesso em 04/02/2026"
Fonte3 <- "Fonte: DERAL. Acesso em 04/02/2026
                SIAGRO. Base atualizada em 12/2025"

#####   SHAPEFILES

SHAPEFILE_REGIONAL <- st_read("Shapefiles/22ª_Regional_de_Saúde/22ª_Regional_de_Saúde.shp")

SHAPEFILE_REGIONAL_Dissolvido <- st_read("Shapefiles/22ª_Regional_de_Saúde/22ª_Regional_de_Saúde_Dissolvido.shp")

SHAPEFILE_ESTADUAL <- st_read("Shapefiles/Paraná/41MUE250GC_SIR.shp")

SHAPEFILE_ESTADUAL_RS <- st_read("Shapefiles/SESA/Regionais_Dissolvidas/Regionais_Sesa.shp")

SHAPEFILE_ESTADUAL_RS$Código <- str_sub(SHAPEFILE_ESTADUAL_RS$Código, 1, 2)

SHAPEFILE_ESTADUAL_RS[14, 2] <- "22"

####################################################################################
###############################    SINASC   ########################################

########     Gráficos

############ Criando uma função Theme para ser utilizado por todos os gráficos     

Theme <- function(base_size = 12){
  theme_minimal(base_size = base_size) + 
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 12), 
      legend.text = element_text(size = 11),
      legend.box.margin = margin(t = 10),
      plot.title = element_text(face = "bold", size = 18, hjust = 0, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0, color = "grey30", margin = margin(b = 15)),
      plot.caption = element_text(size = 10, hjust = 0, face = "italic", margin = margin(t = 15)),
      axis.title = element_text(face = "bold", size = 11),
      axis.text = element_text(size = 10, color = "black"),
      plot.margin = margin(20, 20, 20, 20)
    )
}

################################################################################
################################################################################

#### Gráfico ANomalias PR 2022 - 2025

AUX <- colSums(PR_PEVASPEA_SINASC_Serie_Historica[c(7, 8, 9, 10), 3:13], na.rm = TRUE)

AUX01 <- data.frame( Evento = c("Anomalia Detectada", "Anomalias Prioritárias", "Tubo Neural", 
             "Microcefalia", "Cardiopatias", "Fendas Orais", 
             "Geniturinárias", "Membros", "Parede Abdominal", "Síndrome de Down"),
             Absoluto = AUX[2:11], 
             Nascidos = AUX[1] )

AUX01 <- AUX01 %>%
  mutate(Taxa = round((Absoluto / Nascidos) * 1000, 2)) %>%
  filter(Evento != "Anomalia Detectada",
         Evento != "Anomalias Prioritárias")

PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_22_25 <- ggplot(AUX01, 
       aes(x = reorder(Evento, 
                       Taxa), 
           y = Taxa)) +
  geom_col(fill = "#8FBC8F", 
           color = "black", 
           alpha = 0.8, 
           width = 0.7) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  geom_text(aes(label = format(Taxa, 
                               decimal.mark = ",")), 
            hjust = -0.2, 
            fontface = "bold",
            size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title = "Prevalência de Anomalias Prioritárias no Estado do Paraná",
         subtitle = "Período: 2022-2025",
         x = NULL,
         y = "Anomalias/1.000 Nascidos Vivos",
         caption = Fonte
  ) +
  Theme()

#### Gráfico ANomalias PR 2018 - 2021

AUX <- colSums(PR_PEVASPEA_SINASC_Serie_Historica[c(3, 4, 5, 6), 3:13], na.rm = TRUE)

AUX01 <- data.frame( Evento = c("Anomalia Detectada", "Anomalias Prioritárias", "Tubo Neural", 
                                "Microcefalia", "Cardiopatias", "Fendas Orais", 
                                "Geniturinárias", "Membros", "Parede Abdominal", "Síndrome de Down"),
                     Absoluto = AUX[2:11], 
                     Nascidos = AUX[1] )

AUX01 <- AUX01 %>%
  mutate(Taxa = round((Absoluto / Nascidos) * 1000, 2)) %>%
  filter(Evento != "Anomalia Detectada",
         Evento != "Anomalias Prioritárias")

PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_18_21 <- ggplot(AUX01, 
                                                   aes(x = reorder(Evento, Taxa), 
                                                       y = Taxa)) +
  geom_col(fill = "#8FBC8F", 
           color = "black", 
           alpha = 0.8, 
           width = 0.7) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  geom_text(aes(label = format(Taxa, 
                               decimal.mark = ",")), 
            hjust = -0.2, 
            fontface = "bold",
            size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Prevalência de Anomalias Prioritárias no Estado do Paraná",
       subtitle = "Período: 2018-2021",
       x = NULL,
       y = "Anomalias/1.000 Nascidos Vivos",
       caption = Fonte
  ) +
  Theme()

####  Séries Temporais

##  Nascidos

PR_PEVASPEA_SINASC_NASC_SE_GERAL <- PR_PEVASPEA_SINASC_NASC_SE_GERAL %>%
  pivot_longer(cols = c(2:54),
               names_to = "SE",
               values_to = "Casos")

PR_PEVASPEA_SINASC_NASC_SE_GERAL <- PR_PEVASPEA_SINASC_NASC_SE_GERAL %>%
  filter(Casos > 10) 

PR_PEVASPEA_SINASC_NASC_SE_GERAL <- PR_PEVASPEA_SINASC_NASC_SE_GERAL %>%
  rename(Ano = 1) %>% 
  mutate(Tempo = row_number()) 

quebras_anos <- PR_PEVASPEA_SINASC_NASC_SE_GERAL %>%
  group_by(Ano) %>%
  summarise(Primeiro_Tempo = min(Tempo)) %>%
  pull(Primeiro_Tempo)

labels_anos <- sort(unique(PR_PEVASPEA_SINASC_NASC_SE_GERAL$Ano))

Legenda <- c("Casos Brutos" = "#2c3e50",
             "LOESS" = "darkred")

PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Nasc <- ggplot(PR_PEVASPEA_SINASC_NASC_SE_GERAL, 
                                                  aes(x = Tempo, 
                                                      y = Casos)) +
  geom_line(aes(color = "Casos Brutos"),
            linewidth = 0.8, 
            alpha = 0.9) +
  geom_smooth(aes(color = "LOESS"),
              method = "loess", 
              linetype = "dashed", 
              linewidth = 0.5, 
              se = FALSE, 
              span = 0.5) +
  scale_color_manual(values = Legenda,
                     name = NULL) +
  scale_x_continuous(breaks = quebras_anos, 
                     labels = labels_anos) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".",
                                                   decimal.mark = ","),
                     expand = expansion(mult = c(0.1, 0.1))) +
  labs(caption = Fonte, 
       y = "Nascidos Vivos",
       x = NULL,
       title = "Série Temporal - Nascidos Vivos (2016 a 2025)",
       subtitle = "Número de nascimentos agrupados por semana epidemiológica") +
  Theme() + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")
  
## Anomalias

PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL <- PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL[, -54]

colnames (PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL)[2:53] <- c(1:52)

PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL <- PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL %>%
  pivot_longer(cols = c(2:53) ,
               names_to = "SE",
               values_to = "Casos") %>%
  mutate(SE = as.numeric(SE)) %>%
  mutate(Tempo = row_number()) 

colnames(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL)[1] <- "Ano"

quebras_anos <- seq(1, max(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL$Tempo), 
                    by = 52)

labels_anos <- 2016 + (0:(length(quebras_anos) - 1))

# Média Móvel

PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Anomal <- PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL %>%
  mutate(Media_Suave = rollmean(Casos,    # Média Móvel
                                k = 5,    # Alinhamento central com 5 observações
                                fill = NA))

Legenda <- c("Casos Brutos" = "grey70", 
           "Média Móvel (5 sem.)" = "#2c3e50", 
           "LOESS" = "darkred")

PR_PEVASPEA_SINASC_GRAF_Serie_Temp_ANOMAL <- ggplot(PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Anomal, 
                                                    aes(x = Tempo)) +
  annotate("rect",
           xmin = 209, 
           xmax = 313, 
           ymin = -Inf, 
           ymax = Inf, 
           alpha = 0.1, 
           fill = "red") +
  geom_line(aes(y = Casos, 
                color = "Casos Brutos"), 
            linewidth = 0.4, 
            alpha = 0.8) +
  geom_line(aes(y = Media_Suave, 
                color = "Média Móvel (5 sem.)"), 
            linewidth = 1) +
  geom_smooth(aes(y = Casos, 
                  color = "LOESS"), 
              method = "loess", 
              span = 0.3,      
              linetype = "dashed", 
              se = FALSE) +
  scale_color_manual(values = Legenda,
                     name = NULL) +
  scale_x_continuous(breaks = quebras_anos,
                     labels = labels_anos) +
  scale_y_continuous(limits = c(8, NA)) + 
  labs(caption = Fonte, 
       y = "Ocorrências de Anomalias Congênitas",
       x = NULL,
       title = "Série Temporal - Anomalias Congênitas (2016 a 2025)",
       subtitle = "Anomalias congênitas agrupadas por semana epidemiológica") +
  Theme() + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")

#### Série Histórica RS

AUX <- RS22_PEVASPEA_SINASC_Serie_historica[, c(1:4)]

AUX <- AUX %>%
  mutate(Taxa_Anomalias = (.[, 4] / .[, 3]) * 1000)

AUX$RS <- as.numeric(as.character(AUX$RS))

RS_SINASC_GRAF_SERIE_HIST_ANOMAL <- ggplot(AUX, aes(x = RS, 
                                                    y = Taxa_Anomalias, 
                                                    group = 1)) +
  geom_line(linewidth = 1.3, 
            colour = "black") +
  geom_point(fill = "grey", 
             size = 5, 
             shape = 21) +
  geom_text(aes(label = round(Taxa_Anomalias, 2)), 
            size = 4, 
            vjust = -2, 
            fontface = "bold") +
  scale_y_continuous(limits = c(0, 20), 
                     breaks = seq(0, 16, 2)) +
  scale_x_continuous(breaks = 2016:2025) +
  labs(caption = Fonte, 
       y = "Anomalias / 1.000 Nascimentos",
       x = "Ano",
       title = "Série Histórica 22ª Regional de Saúde - Ivaiporã",
       subtitle = "Anomalias Congênitas (2016-2025). Dados por município de residência.") +
  Theme() 

######## Série Histórica Paraná

AUX <- PR_PEVASPEA_SINASC_Serie_Historica[, c(1:4)]

AUX <- AUX %>%
  mutate(Taxa_Anomalias = (.[, 4] / .[, 3]) * 1000)

AUX$RS <- as.numeric(as.character(AUX$RS))

PR_SINASC_GRAF_SERIE_HIST_ANOMAL <- ggplot(AUX, 
                                           aes(x = RS, 
                                               y = Taxa_Anomalias, 
                                               group = 1)) +
  geom_line(linewidth = 1.3, 
            colour = "black") +
  geom_point(fill = "grey", 
             size = 5, 
             shape = 21) +
  geom_text(aes(label = round(Taxa_Anomalias, 2)), 
            vjust = -1.5, 
            size = 4) + 
  scale_y_continuous(limits = c(0, 20), 
                     breaks = seq(0, 16, 2)) +
  scale_x_continuous(breaks = 2016:2025) +
  labs(title = "Série Histórica Paraná - Anomalias Congênitas",
       y = "Taxa por 1.000 nascimentos", x = "Ano") +
  Theme()

###### Série Histórica 05 maiores regionais

AUX01 <- PR_PEVASPEA_SINASC_RS_Serie_Historica[, c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41)]

colnames(AUX01) <- c("RS", "2016", "2017", "2018", 
                     "2019", "2020", "2021", "2022", "2023",
                     "2024", "2025")

###  04 RS

AUX <- AUX01 %>% 
  filter(RS == 4)

AUX <- pivot_longer(AUX, 
                    2:11, 
                    names_to = "Ano", 
                    values_to = "Taxa_1000") %>%
  mutate(Taxa_1000 = as.numeric(as.character(Taxa_1000)))

PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_I <- ggplot(AUX, aes(x = Ano, 
                                                         y = Taxa_1000)) +
  geom_line(aes(x = Ano,
                y = Taxa_1000,
                group = RS),
            colour = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(y = "Anomalias/
1000 Nascimentos",
       x = NULL,
       title = "04ª Regional de Saúde (2016 a 2025)") +
  scale_y_continuous(limits = c(0, 20), 
                     breaks = seq(0, 16, 2)) +
  Theme()

#### 01 RS

AUX <- AUX01 %>% 
  filter(RS == 1)

AUX <- pivot_longer(AUX, 
                    2:11, 
                    names_to = "Ano", 
                    values_to = "Taxa_1000") %>%
  mutate(Taxa_1000 = as.numeric(as.character(Taxa_1000))) 

PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_II <- ggplot(AUX, aes(x = Ano, 
                                                          y = Taxa_1000)) +
  geom_line(aes(x = Ano,
                y = Taxa_1000,
                group = RS),
            colour = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(y = "Anomalias/
1000 Nascimentos",
       x = NULL,
       title = "01ª Regional de Saúde (2016 a 2025)") +
  scale_y_continuous(limits = c(0, 18), 
                     breaks = seq(0, 16, 2)) +
  Theme()

####  20 RS

AUX <- AUX01 %>% 
  filter(RS == 20)

AUX <- pivot_longer(AUX, 
                    2:11, 
                    names_to = "Ano", 
                    values_to = "Taxa_1000") %>%
  mutate(Taxa_1000 = as.numeric(as.character(Taxa_1000)))

PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_III <- ggplot(AUX, aes(x = Ano, 
                                                           y = Taxa_1000)) +
  geom_line(aes(x = Ano,
                y = Taxa_1000,
                group = RS),
            colour = "black",
            linewidth = 1.3)  +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(y = "Anomalias/
1000 Nascimentos",
       x = NULL,
       title = "20ª Regional de Saúde (2016 a 2025)") +
  scale_y_continuous(limits = c(0, 20), 
                     breaks = seq(0, 16, 2)) +
  Theme()

####  17 RS

AUX <- AUX01 %>% 
  filter(RS == 17)

AUX <- pivot_longer(AUX, 
                    2:11, 
                    names_to = "Ano", 
                    values_to = "Taxa_1000") %>%
  mutate(Taxa_1000 = as.numeric(as.character(Taxa_1000)))

PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_IV <- ggplot(AUX, aes(x = Ano, 
                                                          y = Taxa_1000)) +
  geom_line(aes(x = Ano,
                y = Taxa_1000,
                group = RS),
            colour = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(y = "Anomalias/
1000 Nascimentos",
       x = NULL,
       title = "17ª Regional de Saúde (2016 a 2025)") +
  scale_y_continuous(limits = c(0, 20), 
                     breaks = seq(0, 16, 2)) +
  Theme()

####  02 RS

AUX <- AUX01 %>% 
  filter(RS == 02)

AUX <- pivot_longer(AUX, 
                    2:11, 
                    names_to = "Ano", 
                    values_to = "Taxa_1000") %>%
  mutate(Taxa_1000 = as.numeric(as.character(Taxa_1000)))

PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_V <- ggplot(AUX, aes(x = Ano, 
                                                         y = Taxa_1000)) +
  geom_line(aes(x = Ano,
                y = Taxa_1000,
                group = RS),
            colour = "black",
            linewidth = 1.3)  +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(y = "Anomalias/
1000 Nascimentos",
       x = NULL,
       title = "02ª Regional de Saúde (2016 a 2025)") +
  scale_y_continuous(limits = c(0, 20), 
                     breaks = seq(0, 16, 2)) +
  Theme()

AUX_LIST <- list(PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_I,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_II,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_III,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_IV,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_V)

PR_PEVASPEA_SINASC_GRAF_Serie_Hist_05_RS <- wrap_plots(AUX_LIST, 
                                                       ncol = 1)  + 
  plot_annotation(
    title = 'Regionais de Saúde com Maior Taxa de Anomalias/1000 Nascidos Vivos em 2025',
    subtitle = 'Taxa de Anomalias Congênitas por 1.000 nascidos vivos (2016-2025)',
    caption =  Fonte,
    theme = theme(
      plot.title = element_text(size = 18, 
                                face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(hjust = 0, face = "italic", size = 10)
    )
  )

ggsave(filename = "Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_Serie_Hist_05_RS.png",
  plot = PR_PEVASPEA_SINASC_GRAF_Serie_Hist_05_RS,
  width = 21,
  height = 29.7,
  units = "cm",
  dpi = 300) 

#### Série Histórica Municipal

AUX <- PR_PEVASPEA_SINASC_Serie_historica_Mun[, c(1, 2, 3, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34)] %>% 
  filter(RS == 22)

colnames(AUX) <- c("RS", "IBGE", "Município", "2016", "2017", "2018", 
                   "2019", "2020", "2021", "2022", "2023",
                   "2024", "2025")

AUX <- pivot_longer(AUX, 
                    4:13, 
                    names_to = "Ano", 
                    values_to = "Taxa_1000")

max_global <- max(AUX$Taxa_1000, na.rm = TRUE) * 1.1 

#######   Criando função theme exclusiva para as séries históricas municipais

Theme_Mun <- function(){ 
  theme_minimal(base_size = 10) %+replace%  
    theme(
      axis.text.x = element_text(face = "bold"),
      panel.grid.major = element_line(color = "#C0C0C0"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#F5F5F5"),
      plot.title = element_text(face = "bold", 
                                hjust = 0,
                                size = 18),
      plot.caption = element_text(size = 12,
                                  hjust = 0),
    )
}

AUX$Ano <- factor(as.numeric(AUX$Ano),
                  levels = sort(unique(as.numeric(AUX$Ano))))

AUX <- AUX[, c(3, 4, 5)]

AUX$Taxa_1000 <- round(AUX$Taxa_1000, 2)

######   Criando o conjunto de gráficos dos histogramas via Lapply.  ######

AUX_LIST <- AUX %>%
  mutate(
    Ano = Ano,
    Município = gsub("_", " ", Município)
  ) %>%
  group_split(Município) %>% 
  lapply(function(dados) {
    titulo <- dados$Município %>% 
      unique() 
    
    ggplot(dados, aes(x = Ano, 
                      y = Taxa_1000)
    ) + 
      geom_col(color = "black", 
               fill = "#8FBC8F") + 
      geom_label(aes(label = Taxa_1000), 
                 alpha = 0.5, 
                 vjust = 0.1,
                 size = 4) +
      labs(x = "Ano",
           y = "Anomalias/
1000 Nascimentos",
           title = titulo
      ) +
      scale_y_continuous(limits = c(0, max_global), 
                         expand = expansion(mult = c(0, 0.1))) +
      Theme_Mun()
  })

RS_PEVASPEA_SINASC_GRAF_Taxa_Mun <- wrap_plots(AUX_LIST, ncol = 2) +
  plot_annotation(caption = Fonte,
                  theme = theme(
                    plot.caption = element_text(
                      size = 16,       
                      hjust = 0,      
                      face = "italic",  
                      margin = margin(t = 20) 
                    ) 
                  )
  )

###    MAPAS      ###

MAPA_BASE <- SHAPEFILE_ESTADUAL

MAPA_BASE$NM_MUN <- toupper(MAPA_BASE$NM_MUN)

PR_PEVASPEA_SINASC_Serie_historica_Mun[104, 3] <- "BELA VISTA DA CAROBA"
PR_PEVASPEA_SINASC_Serie_historica_Mun[360, 3] <- "DIAMANTE D'OESTE"
PR_PEVASPEA_SINASC_Serie_historica_Mun[93, 3] <- "ITAPEJARA D'OESTE"
PR_PEVASPEA_SINASC_Serie_historica_Mun[265, 3] <- "MUNHOZ DE MELO"
PR_PEVASPEA_SINASC_Serie_historica_Mun[117, 3] <- "PÉROLA D'OESTE"
PR_PEVASPEA_SINASC_Serie_historica_Mun[184, 3] <- "RANCHO ALEGRE D'OESTE"
PR_PEVASPEA_SINASC_Serie_historica_Mun[239, 3] <- "SANTA CRUZ DE MONTE CASTELO"
PR_PEVASPEA_SINASC_Serie_historica_Mun[127, 3] <- "SÃO JORGE D'OESTE"

###### Fazendo o left-join do shapefile com os dados do estado

MAPA_BASE_PR <- left_join(MAPA_BASE, 
                          PR_PEVASPEA_SINASC_Serie_historica_Mun, 
                          by = c("NM_MUN" = "Município_sem_Código"))

###### Taxa de anomalias/1000 nascidos vivos municípios/PR anual

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = Taxa_2025,
                                           breaks = c(-Inf, 0, 5, 10, 15, 20, 25, 30, 50, 1000),
                                           labels = c("0 casos", "1 - 5", "6 - 10", "11 - 15", 
                                                      "16 - 20", "21 - 25", "26 - 30", "31 - 50", ">50"),
                                           right = FALSE)
)

PR_SINASC_MAP_TAXA_2025_ANOMAL <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  scale_fill_viridis_d(option = "viridis", 
                       name = "Casos/1000 \nNascidos Vivos",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +   
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Taxa Anomalias/1000 Nascidos Vivos em 2025 - 
Paraná",
       subtitle = "Referente ao Município de Residência") +
  Theme()

####  Taxa de nascidos vivos com anomalias Municípios 2018 - 21 (por 1000 nasc)

AUX <- PR_PEVASPEA_SINASC_Serie_historica_Mun[, 1]

AUX <- PR_PEVASPEA_SINASC_Serie_historica_Mun %>%
  mutate(TAXA_4a_18_21 = ((PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2021 + 
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2020 +
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2019 +
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2018)/
                            (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2021 + 
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2020 +
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2019 +
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2018 ) *
                            1000), 
         Nascidos_4a_18_21 = (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2021 + 
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2020 +
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2019 +
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2018 ),
         Anomalias_4a_18_21 = (PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2021 + 
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2020 +
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2019 +
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2018)
  )

AUX <- AUX[-nrow(AUX),]

MAPA_BASE_PR <- left_join(MAPA_BASE, 
                          AUX %>% 
                            select(Município_sem_Código, 35, 36, 37),
                          by = c("NM_MUN" = "Município_sem_Código"))

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = TAXA_4a_18_21,
                                           breaks = c(0, 0.001, 3.5, 5.5, 7.5, 10.0, 15.0, 22.0, Inf),
                                           labels = c("0 casos", "0,1 - 3,4", "3,5 - 5,4", "5,5 - 7,4", 
                                                      "7,5 - 9,9", "10,0 - 14,9", "15,0 - 21,9", "Acima de 22,0"),
                                           right = FALSE
                                           ))

PR_SINASC_MAP_TAXA_4A_ANOMAL_18_21 <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "grey30", 
          linewidth = 0.1, 
          aes(fill = Cat)) +
  geom_sf(data = SHAPEFILE_ESTADUAL_RS,
          color = "black",    
          linewidth = 0.5,   
          fill = NA) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno", 
                       direction = -1,
                       begin = 0.05,       
                       end = 0.95,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "Anomalias/1000 \nNascimentos",
       title = "2018 - 2021")  +
  Theme() 

############  Taxa 04 anos (22 - 25) municípios estado

AUX <- PR_PEVASPEA_SINASC_Serie_historica_Mun[, 1]

AUX <- PR_PEVASPEA_SINASC_Serie_historica_Mun %>%
  mutate(TAXA_4a_22_25 = ((PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2025 + 
                       PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2024 +
                       PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2023 +
                       PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2022)/
                      (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2025 + 
                         PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2024 +
                         PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2023 +
                         PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2022 ) *
                      1000), 
         Nascidos_4a_22_25 = (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2025 + 
                          PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2024 +
                          PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2023 +
                          PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2022 ),
         Anomalias_4a_22_25 = (PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2025 + 
                           PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2024 +
                           PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2023 +
                           PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2022)
  )

AUX <- AUX[-nrow(AUX),]

MAPA_BASE_PR <- left_join(MAPA_BASE_PR, 
                          AUX %>% 
                            select(Município_sem_Código, 35, 36, 37),
                          by = c("NM_MUN" = "Município_sem_Código"))

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = TAXA_4a_22_25,
                                           breaks = c(0, 0.001, 3.5, 5.5, 7.5, 10.0, 15.0, 22.0, Inf),
                                           labels = c("0 casos", "0,1 - 3,4", "3,5 - 5,4", "5,5 - 7,4", 
                                                      "7,5 - 9,9", "10,0 - 14,9", "15,0 - 21,9", "Acima de 22,0"),
                                           right = FALSE
))

PR_SINASC_MAP_TAXA_4A_ANOMAL_22_25 <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "grey30", 
          linewidth = 0.1, 
          aes(fill = Cat)) +
  geom_sf(data = SHAPEFILE_ESTADUAL_RS,
          color = "black",   
          linewidth = 0.5,   
          fill = NA) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno", 
                       direction = -1,
                       begin = 0.05,       
                       end = 0.95,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "Anomalias/1000 \nNascimentos",
       title = "2022 - 2025")  +
  Theme() 

PR_SINASC_MAP_TAXA_4A_ANOMAL_18_21_22_25 <- PR_SINASC_MAP_TAXA_4A_ANOMAL_18_21 + 
  PR_SINASC_MAP_TAXA_4A_ANOMAL_22_25 + 
  plot_layout(ncol = 2, 
              guides = "collect") + 
  plot_annotation(title = 'Evolução Espacial da Taxa de Anomalias Congênitas em Municípios do Paraná',
                  subtitle = 'Comparativo entre os quadriênios 2018-2021 e 2022-2025 (Casos/1000 Nascimentos)',
                  caption =  Fonte,
                  theme = theme(
                    plot.title = element_text(size = 20, 
                                              face = "bold"),
                    plot.subtitle = element_text(size = 14),
                    legend.position = "bottom",
                    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
                  ))

#############   Taxa Anomalias/1000 nascimentos 18 - 21 somados
#############   por Regionais de Saúde

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica[, 1]

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica %>%
  mutate(TAXA_4a_18_21 = ((PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2021 + 
                             PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2020 +
                             PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2019 +
                             PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2018 )/
                            (PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2021 + 
                               PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2020 +
                               PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2019 +
                               PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2018) *
                            1000)
  )

#### Incluindo 0 em regionais com um dígito 

AUX$RS <- str_pad(AUX$RS, 
                  width = 2, 
                  side = "left", 
                  pad = "0")

AUX <- AUX[-nrow(AUX),]

#### Fazendo o left_join da base de dados com o shapefile

MAPA_BASE_RS <- left_join(AUX,
                          SHAPEFILE_ESTADUAL_RS,
                          by = (c("RS" = "Código")))

MAPA_BASE_RS <- MAPA_BASE_RS[, c(1, 42, 43, 44)]

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = TAXA_4a_18_21,
                                           breaks = c(0, 4.5, 5.5, 6.5, 7.5, 8.5, 10.0, 12.0, Inf),
                                           labels = c("Até 4,5", "4,5 - 5,5", 
                                                      "5,5 - 6,5", "6,5 - 7,5", 
                                                      "7,5 - 8,5", "8,5 - 10,0", 
                                                      "10,0 - 12,0", "Acima de 12,0"),
                                           right = FALSE))

MAPA_BASE_RS <- st_as_sf(MAPA_BASE_RS)

PR_SINASC_MAP_TAXA_4_ANOS_18_21_ANOMAL_RS <- ggplot() +
  geom_sf(data = MAPA_BASE_RS, 
          color = "grey30", 
          linewidth = 0.1, 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno", 
                       direction = -1,
                       begin = 0.05,       
                       end = 0.95,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "Anomalias/1000 \nNascimentos",
       title = "2018 - 2021")  +
  Theme() 

#############   Taxa Anomalias/1000 nascimentos nos últimos 04 anos somados
#############   por Regionais de Saúde

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica[, 1]

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica %>%
  mutate(TAXA_4a_22_25 = ((PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2025 + 
                       PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2024 +
                       PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2023 +
                       PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2022 )/
                      (PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2025 + 
                         PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2024 +
                         PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2023 +
                         PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2022) *
                      1000)
  )

#### Incluindo 0 em regionais com um dígito 

AUX$RS <- str_pad(AUX$RS, 
                  width = 2, 
                  side = "left", 
                  pad = "0")

AUX <- AUX[-nrow(AUX),]

#### Fazendo o left_join da base de dados com o shapefile

MAPA_BASE_RS <- left_join(AUX,
                          SHAPEFILE_ESTADUAL_RS,
                          by = (c("RS" = "Código")))

MAPA_BASE_RS <- MAPA_BASE_RS[, c(1, 42, 43, 44)]

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = TAXA_4a_22_25,
                                           breaks = c(0, 4.5, 5.5, 6.5, 7.5, 8.5, 10.0, 12.0, Inf),
                                           labels = c("Até 4,5", "4,5 - 5,5", 
                                                      "5,5 - 6,5", "6,5 - 7,5", 
                                                      "7,5 - 8,5", "8,5 - 10,0", 
                                                      "10,0 - 12,0", "Acima de 12,0"),
                                           right = FALSE))

MAPA_BASE_RS <- st_as_sf(MAPA_BASE_RS)

PR_SINASC_MAP_TAXA_4_ANOS_22_25_ANOMAL_RS <- ggplot() +
  geom_sf(data = MAPA_BASE_RS, 
          color = "grey30", 
          linewidth = 0.1, 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno", 
                       direction = -1,
                       begin = 0.05,       
                       end = 0.95,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "Anomalias/1000 \nNascimentos",
       title = "2022 - 2025")  +
  Theme() 

PR_SINASC_MAP_TAXA_4_ANOS_18_21_22_25_ANOMAL_RS <- PR_SINASC_MAP_TAXA_4_ANOS_18_21_ANOMAL_RS + 
  PR_SINASC_MAP_TAXA_4_ANOS_22_25_ANOMAL_RS+ 
  plot_layout(ncol = 2, 
              guides = "collect") + 
  plot_annotation(title = 'Evolução Espacial da Taxa de Anomalias Congênitas em Regionais do Paraná',
                  subtitle = 'Comparativo entre os quadriênios 2018-2021 e 2022-2025 (Casos/1000 Nascimentos)',
                  caption =  Fonte,
                  theme = theme(
                    plot.title = element_text(size = 20, 
                                              face = "bold"),
                    plot.subtitle = element_text(size = 14),
                    legend.position = "bottom",
                    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
                  ))

##### Trabalhando Global Moran/LISA

### Criando Matriz de vizinhança (QUEEN) Verificar se o queen é padrão no spdep

Matriz_Viz_MAPA_BASE_PR <- poly2nb(MAPA_BASE_PR,
                                   queen = TRUE)

### Verificando a malha queen

Centroides_Matriz_VIZ <- st_centroid(MAPA_BASE_PR)

Centroides_Matriz_VIZ_coordenadas <- st_coordinates(Centroides_Matriz_VIZ)

Matriz_VIZ_Linhas <- nb2lines(nb = Matriz_Viz_MAPA_BASE_PR,
                              coords = Centroides_Matriz_VIZ_coordenadas) %>%
  st_as_sf()

st_crs(Matriz_VIZ_Linhas) <- st_crs(MAPA_BASE_PR)

PR_PEVASPEA_SINASC_MAP_MAPA_VIZINHANCA <-ggplot(MAPA_BASE_PR, 
                                                aes(geometry = geometry)) +
  geom_sf(fill = "lightblue") +
  geom_sf(data = Matriz_VIZ_Linhas, 
          aes(geometry = geometry)) +
  ggtitle("Matriz de Vizinhança Queen, Paraná.") +
  theme_void()

### Explicação do Gemini para usar o style = "W"

# 3. style = "W" (A Padronização por Linha)
# Este é o ponto mais importante. O estilo "W" (Row-standardized) faz o seguinte:
#   Atribui pesos iguais para todos os vizinhos de uma unidade, de modo 
# que a soma dos pesos de cada linha seja igual a 1.
# Na prática: Se um município tem 4 vizinhos, cada um recebe peso 0, 25. 
# Se outro tem 10 vizinhos, cada um recebe peso 0,1
# Por que usar? Isso é padrão para o Índice de Moran Global pois 
# permite comparar regiões com diferentes números de vizinhos de 
# forma justa (evita que municípios muito "conectados" distorçam o 
# resultado apenas por terem mais fronteiras).

Matriz_Viz_Pesos <- Matriz_Viz_MAPA_BASE_PR %>% 
  nb2listw(style = "W")

#####  Análise Global e Local Moran

#### 2018 a 2021 INÍCIO

### Realizando a suavização dos dados com Método Bayesiano Empírico 2022 - 2025

Taxa_Suavizada <- EBlocal(ri = MAPA_BASE_PR$Anomalias_4a_18_21,
                          ni = MAPA_BASE_PR$Nascidos_4a_18_21,
                          nb = Matriz_Viz_MAPA_BASE_PR)

MAPA_BASE_PR$TAXA_RAW_18_21 <- Taxa_Suavizada$raw * 1000

MAPA_BASE_PR$TAXA_EST_18_21 <- Taxa_Suavizada$est * 1000

Global_Moran_18_21 <- moran.test(MAPA_BASE_PR$TAXA_EST_18_21,
                           Matriz_Viz_Pesos)

### Scatterplot

moran.plot(MAPA_BASE_PR$TAXA_EST_18_21, 
           Matriz_Viz_Pesos, 
           labels = FALSE, 
           pch = 15, 
           col = "blue", 
           xlab = "Variável Original", 
           ylab = "Média dos Vizinhos (Spatial Lag)",
           main = "Moran Scatterplot")

#### Calculando o Local Moran
### Travando o local moran
set.seed(321)

Lisa <- localmoran_perm(MAPA_BASE_PR$TAXA_EST_18_21, 
                        Matriz_Viz_Pesos, 
                        nsim = 9999,
                        zero.policy = TRUE)

MAPA_BASE_PR$local_I_18_21 <- Lisa[,1]

MAPA_BASE_PR$local_I_p_valor_18_21 <- Lisa[,5]

ggplot(MAPA_BASE_PR, 
       aes(geometry = geometry)) +
  geom_sf(aes(fill = local_I_18_21)) +
  scale_fill_gradient2(low = "blue", high = "red", 
                       mid = "white", 
                       midpoint = 0,
                       name = "I local") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))

quadrantes <- attr(Lisa, 
                   "quadr")$mean

MAPA_BASE_PR$quadrante_18_21 <- case_when(quadrantes == "High-High" ~ "Alto-Alto",
                                    quadrantes == "Low-Low" ~ "Baixo-Baixo",
                                    quadrantes == "High-Low" ~ "Alto-Baixo",
                                    quadrantes == "Low-High" ~ "Baixo-Alto")

MAPA_BASE_PR <- MAPA_BASE_PR %>%
  mutate(Lisa_resultado_18_21 = case_when(
    local_I_p_valor_18_21 < 0.05 ~ quadrante_18_21,
    local_I_p_valor_18_21 >= 0.05 ~ "Não significativo"
  ))

Niveis_LISA <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não significativo")

MAPA_BASE_PR$Lisa_resultado_18_21 <- factor(MAPA_BASE_PR$Lisa_resultado_18_21, levels = Niveis_LISA)

PR_PEVASPEA_SINASC_LOCAL_MORAN_18_21 <- ggplot(MAPA_BASE_PR, 
                                               aes(geometry = geometry)) +
  geom_sf(color = "grey30", 
          linewidth = 0.1, 
          aes(fill = Lisa_resultado_18_21)) +
  scale_fill_manual(name = "LISA \nClusters",
                    drop = FALSE,
                    values = c("Alto-Alto" = "red",        
                               "Baixo-Baixo" = "blue",      
                               "Alto-Baixo" = "pink",       
                               "Baixo-Alto" = "lightblue",  
                               "Não significativo" = "grey90")
  ) +
  geom_sf(data = SHAPEFILE_ESTADUAL_RS,
          color = "black",   
          linewidth = 0.5,   
          fill = NA) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal()) +
  
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       title = "2018 - 2021",
       subtitle = "Taxa suavizada utilizando Método Bayesiano Empírico \nGlobal Moran I = 0.589 (p < 0.001)") +
  Theme() +
  theme(legend.key.width = unit(1.5, "cm")) +
  theme(legend.position = "bottom")

#### Fim da análise Global and Local Moran 2018 - 2021

### Realizando a suavização dos dados com Método Bayesiano Empírico 2022 - 2025

Taxa_Suavizada <- EBlocal(ri = MAPA_BASE_PR$Anomalias_4a_22_25, 
                          ni = MAPA_BASE_PR$Nascidos_4a_22_25,
                          nb = Matriz_Viz_MAPA_BASE_PR)

MAPA_BASE_PR$TAXA_RAW_22_25 <- Taxa_Suavizada$raw * 1000

MAPA_BASE_PR$TAXA_EST_22_25 <- Taxa_Suavizada$est * 1000

Global_Moran <- moran.test(MAPA_BASE_PR$TAXA_EST_22_25,
                           Matriz_Viz_Pesos)

### Scatterplot

moran.plot(MAPA_BASE_PR$TAXA_EST_22_25, 
           Matriz_Viz_Pesos, 
           labels = FALSE, 
           pch = 15, 
           col = "blue", 
           xlab = "Variável Original", 
           ylab = "Média dos Vizinhos (Spatial Lag)",
           main = "Moran Scatterplot")

#### Calculando o Local Moran
#### Travando o local moran
set.seed(321)

Lisa <- localmoran_perm(MAPA_BASE_PR$TAXA_EST_22_25, 
                        Matriz_Viz_Pesos, 
                        nsim = 9999,
                        zero.policy = TRUE)

MAPA_BASE_PR$local_I_22_25 <- Lisa[,1]

MAPA_BASE_PR$local_I_p_valor_22_25 <- Lisa[,5]

quadrantes <- attr(Lisa, 
                   "quadr")$mean

MAPA_BASE_PR$quadrante_22_25 <- case_when(quadrantes == "High-High" ~ "Alto-Alto",
                                    quadrantes == "Low-Low" ~ "Baixo-Baixo",
                                    quadrantes == "High-Low" ~ "Alto-Baixo",
                                    quadrantes == "Low-High" ~ "Baixo-Alto")

MAPA_BASE_PR <- MAPA_BASE_PR %>%
  mutate(Lisa_resultado_22_25 = case_when(
    local_I_p_valor_22_25 < 0.05 ~ quadrante_22_25,
    local_I_p_valor_22_25 >= 0.05 ~ "Não significativo"
  ))

MAPA_BASE_PR$Lisa_resultado_22_25 <- factor(MAPA_BASE_PR$Lisa_resultado_22_25, levels = Niveis_LISA)

PR_PEVASPEA_SINASC_LOCAL_MORAN_22_25 <- ggplot(MAPA_BASE_PR, 
                                               aes(geometry = geometry)) +
  geom_sf(color = "grey30", 
          linewidth = 0.1, 
          aes(fill = Lisa_resultado_22_25)) +
  scale_fill_manual(name = "LISA \nClusters", 
                    drop = FALSE,
                    values = c("Alto-Alto" = "red",        
                               "Baixo-Baixo" = "blue",      
                               "Alto-Baixo" = "pink",       
                               "Baixo-Alto" = "lightblue",  
                               "Não significativo" = "grey90")
  ) +
  geom_sf(data = SHAPEFILE_ESTADUAL_RS,
          color = "black",   
          linewidth = 0.5,   
          fill = NA) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal()) +
  
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       title = "2022 - 2025",
       subtitle = "Taxa suavizada utilizando Método Bayesiano Empírico \nGlobal Moran I = 0.580 (p < 0.001) \n999 Permutações") +
  Theme() +
  theme(legend.key.width = unit(1.5, "cm")) +
  theme(legend.position = "bottom")

PR_PEVASPEA_SINASC_LOCAL_MORAN_18_21_22_25 <- PR_PEVASPEA_SINASC_LOCAL_MORAN_18_21 + 
  PR_PEVASPEA_SINASC_LOCAL_MORAN_22_25 + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(
    title = "Progressão de Agrupamentos das Taxas de Anomalias/1000 Nascidos no Paraná",
    subtitle = 'Comparativo entre os quadriênios 2018-2021 e 2022-2025',
    caption = Fonte  
  ) & 
  theme(
    plot.title = element_text(size = 16, 
                              face = "bold", 
                              hjust = 0), 
    plot.subtitle = element_text(size = 12, 
                                 hjust = 0),
    plot.caption = element_text(hjust = 0, 
                                face = "italic", 
                                size = 10),
    legend.position = "bottom",        
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.justification = "center",
    legend.key.width = unit(1.2, "cm")   
  )
######  Tabelas 
################################################################################
################################################################################

#### Tabela ANomalias Regionais 2018 - 2021
# 
AUX2018 <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2018.csv",
                     header = TRUE,
                     sep = ",")

AUX2019 <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2019.csv",
                     header = TRUE,
                     sep = ",")

AUX2020 <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2020.csv",
                     header = TRUE,
                     sep = ",")

AUX2021 <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2021.csv",
                     header = TRUE,
                     sep = ",")

AUX <- bind_rows(AUX2018 %>% mutate(Ano = 2018),
                 AUX2019 %>% mutate(Ano = 2019),
                 AUX2020 %>% mutate(Ano = 2020),
                 AUX2021 %>% mutate(Ano = 2021))

AUX01 <- AUX %>%
  group_by(RS) %>%
  summarise(across(4:14, \(x) sum(x, na.rm = TRUE)))

AUX <- AUX01 %>%
    pivot_longer(
    cols = Anomalia_Detectada:last_col(), 
    names_to = "Evento", 
    values_to = "Absoluto"
  ) %>%
  mutate(Percentual = round((Absoluto / Nascidos) * 1000, 2)
  ) %>%
  rename(`(n)` = Absoluto, `(%)` = Percentual) %>% 
  pivot_wider(names_from = Evento, 
    values_from = c(`(n)`, `(%)`),
    names_glue = "{Evento} {.value}")

AUX <- AUX[c(1, 12, 16:22, 2:11, 13, 14, 15, 23), c(1, 2, 3, 13, 4, 14, 5, 15, 6, 16, 7, 17, 8, 18, 9, 19, 10, 20, 11, 21, 12, 22)]

c( "Nascidos", "Nº Anomalias", "Anomalias 
Prioritárias", "Tubo 
Neural", "Microcefalia",
  "Cardiopatias", "Fendas 
Orais", "Genito-
-urinárias", "Membros", 
  "Parede 
Abdominal", "Sindrome 
de Down")

PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_18_21 <- gt(AUX[, c(1, 2, 7:22)]) %>%
  tab_header(
    title = md("**Prevalência de Anomalias Congênitas Prioritárias por Regional de Saúde**"),
    subtitle = md("Paraná, 2018 – 2021")
  ) %>%
  tab_options(
    heading.align = "left",
    table.border.top.style = "none",
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2),
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(2),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1),
    table.font.size = px(12),
    data_row.padding = px(3)
  ) %>%
  tab_spanner(label = "Tubo Neural",
              columns = c(3:4),
              id = "Tubo Neural") %>%
  tab_spanner(label = "Microcefalia",
              columns = c(5:6),
              id = "Microcefalia") %>%
  tab_spanner(label = "Cardíacas",
              columns = c(7:8),
              id = "Cardíacas") %>%
  tab_spanner(label = "Fendas Orais",
              columns = c(9:10),
              id = "Fendas Orais") %>%
  tab_spanner(label = "Urinárias",
              columns = c(11:12),
              id = "Urinárias") %>%
  tab_spanner(label = "Membros",
              columns = c(13:14),
              id = "Membros") %>%
  tab_spanner(label = "Parede Abd.",
              columns = c(15:16),
              id = "Parede Abd.") %>%
  tab_spanner(label = "Síndrome de Down",
              columns = c(17:18),
              id = "Síndrome de Down") %>%
  cols_align(align = "left", columns = 1) %>%
  cols_align(align = "center", columns = 2:18) %>%
  cols_label(
    Nascidos = "Nascidos (N)",
    contains("(n)") ~ "n",
    contains("(%)") ~ "Prev."
  ) %>%
  tab_footnote(
    footnote = "Prevalência calculada por 1.000 nascidos vivos (NV).",
    locations = cells_column_labels(columns = contains("(%)"))
  ) %>%
  tab_footnote(
    footnote = "Uma mesma ficha (DN) pode conter o registro de múltiplas anomalias.",
    locations = cells_column_labels(columns = contains("(n)"))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  opt_row_striping() %>%
  tab_style(
    style = list(
      cell_fill(color = "yellow", alpha = 0.2),
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = RS == "22")
  )

### Gráfico de municípios da 22ªRS

AUX <- bind_rows(AUX2018 %>% mutate(Ano = 2018),
                 AUX2019 %>% mutate(Ano = 2019),
                 AUX2020 %>% mutate(Ano = 2020),
                 AUX2021 %>% mutate(Ano = 2021))

colnames(AUX)[c(5:15)] <- c("Nascidos", "Nº Anomalias", "Anomalias Prioritárias", 
                            "Tubo Neural", "Microcefalia", "Cardiopatias", 
                            "Fendas Orais", "Urinárias", "Membros", "Parede Abdominal",
                            "Sindrome de Down")

AUX01 <- AUX[, c(1, 3, 5, 7:ncol(AUX))] %>%
  filter(RS == 22) %>%
  group_by(Município_sem_Código) %>%
  summarise(across(2:12, \(x) sum(x, na.rm = TRUE)))

AUX01 <- AUX01[, c(-ncol(AUX01))]

AUX01 <- AUX01 %>%
  pivot_longer(
    cols = 3:11, 
    names_to = "Evento", 
    values_to = "Absoluto") %>%
  mutate(Percentual = round((Absoluto / Nascidos) * 1000, 2),
         Evento_Curto = str_wrap(Evento, width = 15))

#######   Criando função theme exclusiva para os gráficos de anomalias

Theme_Mun <- function(){ 
  theme_minimal(base_size = 8) %+replace%  
    theme(
      axis.text.x = element_text(face = "bold"),
      panel.grid.major = element_line(color = "#C0C0C0"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#F5F5F5"),
      plot.title = element_text(face = "bold", 
                                hjust = 0,
                                size = 18),
      plot.caption = element_text(size = 12,
                                  hjust = 0),
    )
}

AUX01$Evento <- factor(AUX01$Evento,
                  levels = sort(unique(AUX01$Evento)))

max_global <- max(AUX01$Percentual, na.rm = TRUE) 

######   Criando o conjunto de gráficos dos histogramas via Lapply.  ######

AUX_LIST <- AUX01 %>%
  mutate(
    Município_sem_Código = gsub("_", " ", Município_sem_Código)
  ) %>%
  group_split(Município_sem_Código) %>% 
  lapply(function(dados) {
    nome_mun <- unique(dados$Município_sem_Código)
    
    ggplot(dados, aes(x = Evento, 
                      y = Percentual)) + 
      geom_col(color = "black", 
               fill = "#8FBC8F") + 
      geom_label(aes(label = Percentual), 
                 hjust = -0.1, 
                 size = 3) +
      labs(x = "Tipo de Anomalia", 
           y = "Anomalias/1000 Nascimentos",
           title = paste0(nome_mun, " - Prioritárias")) +
      coord_flip() +
     scale_y_continuous(limits = c(0, max_global), 
                        expand = expansion(mult = c(0, 0.3))) + 
      Theme_Mun()
  })

RS_PEVASPEA_SINASC_GRAF_Prioritarias_Mun <- wrap_plots(AUX_LIST, ncol = 2) +
  plot_annotation( title = 'Perfil Epidemiológico de Anomalias Congênitas na 22ª RS',
                   subtitle = 'Taxa por 1.000 nascidos vivos por município de residência (2018-2021)',
                   caption = Fonte, 
                   theme = theme(
                     plot.title = element_text(size = 22, 
                                               face = "bold",
                                               hjust = 0.5),
                     plot.subtitle = element_text(size = 16, 
                                                  hjust = 0.5, 
                                                  margin = margin(b = 20)),
                     plot.caption = element_text(size = 12, 
                                                 face = "italic", 
                                                 hjust = 0)
                   )
  )

#### Tabela ANomalias Regionais 2022 - 2025
# 
AUX2022 <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2022.csv",
                         header = TRUE,
                         sep = ",")

AUX2023 <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2023.csv",
                     header = TRUE,
                     sep = ",")

AUX2024 <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2024.csv",
                     header = TRUE,
                     sep = ",")

AUX2025 <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2025.csv",
                     header = TRUE,
                     sep = ",")

AUX <- bind_rows(AUX2022 %>% mutate(Ano = 2022),
                 AUX2023 %>% mutate(Ano = 2023),
                 AUX2024 %>% mutate(Ano = 2024),
                 AUX2025 %>% mutate(Ano = 2025))

AUX01 <- AUX %>%
  group_by(RS) %>%
  summarise(across(4:14, \(x) sum(x, na.rm = TRUE)))

AUX <- AUX01 %>%
  pivot_longer(
    cols = Anomalia_Detectada:last_col(), 
    names_to = "Evento", 
    values_to = "Absoluto"
  ) %>%
  mutate(Percentual = round((Absoluto / Nascidos) * 1000, 2)
  ) %>%
  rename(`(n)` = Absoluto, `(%)` = Percentual) %>% 
  pivot_wider(names_from = Evento, 
              values_from = c(`(n)`, `(%)`),
              names_glue = "{Evento} {.value}")

AUX <- AUX[c(1, 12, 16:22, 2:11, 13, 14, 15, 23), c(1, 2, 3, 13, 4, 14, 5, 15, 6, 16, 7, 17, 8, 18, 9, 19, 10, 20, 11, 21, 12, 22)]

PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_22_25 <- gt(AUX[, c(1, 2, 7:22)]) %>%
  tab_header(
    title = md("**Prevalência de Anomalias Congênitas Prioritárias por Regional de Saúde**"),
    subtitle = md("Paraná, 2022 – 2025")
  ) %>%
  tab_options(
    heading.align = "left",
    table.border.top.style = "none",
    table.border.bottom.color = "black",
    table.border.bottom.width = px(2),
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(2),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(1),
    table.font.size = px(12),
    data_row.padding = px(3)
  ) %>%
  tab_spanner(label = "Tubo Neural",
              columns = c(3:4),
              id = "Tubo Neural") %>%
  tab_spanner(label = "Microcefalia",
              columns = c(5:6),
              id = "Microcefalia") %>%
  tab_spanner(label = "Cardíacas",
              columns = c(7:8),
              id = "Cardíacas") %>%
  tab_spanner(label = "Fendas Orais",
              columns = c(9:10),
              id = "Fendas Orais") %>%
  tab_spanner(label = "Urinárias",
              columns = c(11:12),
              id = "Urinárias") %>%
  tab_spanner(label = "Membros",
              columns = c(13:14),
              id = "Membros") %>%
  tab_spanner(label = "Parede Abd.",
              columns = c(15:16),
              id = "Parede Abd.") %>%
  tab_spanner(label = "Síndrome de Down",
              columns = c(17:18),
              id = "Síndrome de Down") %>%
  cols_align(align = "left", columns = 1) %>%
  cols_align(align = "center", columns = 2:18) %>%
  cols_label(
    Nascidos = "Nascidos (N)",
    contains("(n)") ~ "n",
    contains("(%)") ~ "Prev."
  ) %>%
  tab_footnote(
    footnote = "Prevalência calculada por 1.000 nascidos vivos (NV).",
    locations = cells_column_labels(columns = contains("(%)"))
  ) %>%
  tab_footnote(
    footnote = "Uma mesma ficha (DN) pode conter o registro de múltiplas anomalias.",
    locations = cells_column_labels(columns = contains("(n)"))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  opt_row_striping() %>%
  tab_style(
    style = list(
      cell_fill(color = "yellow", alpha = 0.2),
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = RS == "22")
  )

### Gráfico de municípios da 22ªRS

AUX <- bind_rows(AUX2022 %>% mutate(Ano = 2022),
                 AUX2023 %>% mutate(Ano = 2023),
                 AUX2024 %>% mutate(Ano = 2024),
                 AUX2025 %>% mutate(Ano = 2025))

colnames(AUX)[c(5:15)] <- c("Nascidos", "Nº Anomalias", "Anomalias Prioritárias", 
                            "Tubo Neural", "Microcefalia", "Cardiopatias", 
                            "Fendas Orais", "Urinárias", "Membros", "Parede Abdominal",
                            "Sindrome de Down")

AUX01 <- AUX[, c(1, 3, 5, 7:ncol(AUX))] %>%
  filter(RS == 22) %>%
  group_by(Município_sem_Código) %>%
  summarise(across(2:12, \(x) sum(x, na.rm = TRUE)))

AUX01 <- AUX01[, c(-ncol(AUX01))]

AUX01 <- AUX01 %>%
  pivot_longer(
    cols = 3:11, 
    names_to = "Evento", 
    values_to = "Absoluto") %>%
  mutate(Percentual = round((Absoluto / Nascidos) * 1000, 2),
         Evento_Curto = str_wrap(Evento, width = 15))

AUX01$Evento <- factor(AUX01$Evento,
                       levels = sort(unique(AUX01$Evento)))

max_global <- max(AUX01$Percentual, na.rm = TRUE) 

######   Criando o conjunto de gráficos dos histogramas via Lapply.  ######

AUX_LIST <- AUX01 %>%
  mutate(
    Município_sem_Código = gsub("_", " ", Município_sem_Código)
  ) %>%
  group_split(Município_sem_Código) %>% 
  lapply(function(dados) {
    nome_mun <- unique(dados$Município_sem_Código)
    
    ggplot(dados, aes(x = Evento, 
                      y = Percentual)) + 
      geom_col(color = "black", 
               fill = "#8FBC8F") + 
      geom_label(aes(label = Percentual), 
                 hjust = -0.1, 
                 size = 3) +
      labs(x = "Tipo de Anomalia", 
           y = "Anomalias/1000 Nascimentos",
           title = paste0(nome_mun, " - Prioritárias")) +
      coord_flip() +
      scale_y_continuous(limits = c(0, max_global), 
                         expand = expansion(mult = c(0, 0.3))) + 
      Theme_Mun()
  })

RS_PEVASPEA_SINASC_GRAF_Prioritarias_Mun_II <- wrap_plots(AUX_LIST, ncol = 2) +
  plot_annotation( title = 'Perfil Epidemiológico de Anomalias Congênitas na 22ª RS',
                   subtitle = 'Taxa por 1.000 nascidos vivos por município de residência (2022-2025)',
                   caption = Fonte, 
                   theme = theme(
                     plot.title = element_text(size = 22, 
                                               face = "bold",
                                               hjust = 0.5),
                     plot.subtitle = element_text(size = 16, 
                                                  hjust = 0.5, 
                                                  margin = margin(b = 20)),
                     plot.caption = element_text(size = 12, 
                                                 face = "italic", 
                                                 hjust = 0)
                   )
  )

####  Salvando os gráficos, mapas e tabelas
ggsave(filename = "Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_Serie_Hist_05_RS.png",
       plot = PR_PEVASPEA_SINASC_GRAF_Serie_Hist_05_RS,
       width = 26,
       height = 25.7,
       units = "cm",
       dpi = 300) 


ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Nasc.png",
       PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Nasc,
       width = 18,          
       height = 10,          
       units = "cm",
       dpi = 300,            
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Anomal.png",
       PR_PEVASPEA_SINASC_GRAF_Serie_Temp_ANOMAL,
       width = 18,          
       height = 10,          
       units = "cm",
       dpi = 300,            
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/RS_SINASC_GRAF_SERIE_HIST_ANOMAL.png",
       RS_SINASC_GRAF_SERIE_HIST_ANOMAL,
       width = 18,
       height = 10,
       units = "cm",
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_SINASC_GRAF_SERIE_HIST_ANOMAL.png",
       PR_SINASC_GRAF_SERIE_HIST_ANOMAL,
       width = 18,
       height = 10,
       units = "cm",
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_18_21.png",
       PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_18_21,
       width = 20, 
       height = 12, 
       units = "cm", 
       dpi = 300,
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_22_25.png",
       PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_22_25,
       width = 20, 
       height = 12, 
       units = "cm", 
       dpi = 300,
       bg = "white")

ggsave(filename = "Imagens/SINASC/RS_PEVASPEA_SINASC_GRAF_Taxa_Mun.png",
       plot = RS_PEVASPEA_SINASC_GRAF_Taxa_Mun,
       width = 26,
       height = 35.7,
       units = "cm",
       dpi = 300) 

ggsave(filename = "Imagens/SINASC/RS_PEVASPEA_SINASC_GRAF_Prioritarias_Mun.png",
       plot = RS_PEVASPEA_SINASC_GRAF_Prioritarias_Mun,
       width = 38,
       height = 50.7,
       units = "cm",
       dpi = 300) 

ggsave(filename = "Imagens/SINASC/RS_PEVASPEA_SINASC_GRAF_Prioritarias_Mun_II.png",
       plot = RS_PEVASPEA_SINASC_GRAF_Prioritarias_Mun_II,
       width = 38,
       height = 50.7,
       units = "cm",
       dpi = 300) 

ggsave(filename = "Imagens/SINASC/PR_PEVASPEA_SINASC_LOCAL_MORAN_18_21_22_25.png", 
  plot = PR_PEVASPEA_SINASC_LOCAL_MORAN_18_21_22_25, 
  width = 35,                               
  height = 18,                               
  units = "cm",                               
  dpi = 300,                                   
  bg = "white"                                
)

ggsave(filename = "Imagens/SINASC/PR_PEVASPEA_SINASC_LOCAL_MORAN_18_21_22_25.png", 
       plot = PR_PEVASPEA_SINASC_LOCAL_MORAN_18_21_22_25, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/SINASC/PR_SINASC_MAP_TAXA_4A_ANOMAL_18_21_22_25.png", 
      plot = PR_SINASC_MAP_TAXA_4A_ANOMAL_18_21_22_25, 
      width = 35,                               
      height = 18,                               
      units = "cm",                               
      dpi = 300,                                   
      bg = "white"                                
)

ggsave(filename = "Imagens/SINASC/PR_SINASC_MAP_TAXA_4_ANOS_18_21_22_25_ANOMAL_RS.png",
       plot = PR_SINASC_MAP_TAXA_4_ANOS_18_21_22_25_ANOMAL_RS,
       width = 35,        
       height = 18,       
       units = "cm",
       dpi = 300,         
       bg = "white"
)

#########  Tabelas

gtsave(data = PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_18_21,
  filename = "Imagens/SINASC/PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_18_21.pdf")

gtsave(data = PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_22_25,
       filename = "Imagens/SINASC/PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_22_25.pdf")

###############################################################################################
###############################################################################################
################         DERAL     ############################################################
###############################################################################################
options(scipen = 999)

PR_DERAL_GERAL <- read.csv (file = "Tabulacoes_R/DERAL/PR_DERAL_GERAL.csv",
                                                header = TRUE,
                                                sep = ",")

AUX <- PR_DERAL_GERAL %>%
  summarise(across(4:57, ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), 
               names_to = "Variavel", 
               values_to = "Total") %>%
  mutate(Total = round(Total, 2)) 
         

###### Área total em HA no Estado

AUX01 <- AUX %>%
  filter(str_detect(Variavel, "^AREA_HA_")) %>%
  mutate(Variavel = gsub("AREA_HA_", "", Variavel))

##### Gráfico de linhas

PR_DERAL_GRAF_HA_CULTIVADO <- ggplot(AUX01, aes(x = Variavel, 
                                               y = Total,
                                               group = 1)) +
  geom_line(color = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(caption = Fonte1, 
       y = "Hectares Cultivados",
       x = NULL,
       title = "Total de Hectares Cultivados no Paraná - (2016 - 2024)",
       subtitle = "Cultivos de interesse") +
  scale_y_continuous(limits = c(12000000, 19000000), 
                     labels = label_number(decimal.mark = ",", 
                                           big.mark = "."),
                     expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

##############  Toneladas de Agrotóxico

AUX01 <- AUX %>%
  filter(row_number() %in% seq(3, n(), by = 6)) %>%
  mutate(Variavel = gsub("TON_AGRO_", "", Variavel))

PR_DERAL_GRAF_TON_AGRO <- ggplot(AUX01, aes(x = Variavel, 
                  y = Total,
                  group = 1)) +
  geom_line(color = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(caption = Fonte, 
       y = "Hectares Cultivados",
       x = NULL,
       title = "Consumo Agrotóxico (TON) no Paraná - (2016 - 2024)",
       subtitle = "Consumo em Toneladas") +
  scale_y_continuous(limits = c(0, 180000), 
labels = label_number(decimal.mark = ",", 
                      big.mark = "."),
expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

##############  Agrotóxico/HA

AUX01 <- AUX %>%
  filter(str_detect(Variavel, "AREA_HA") |
         str_detect(Variavel, "TON_AGRO"))

AUX02 <- AUX01 %>%
  mutate(Ano = str_extract(Variavel, "\\d{4}"),
         Tipo = ifelse(str_detect(Variavel, "AREA"), "Area", "Consumo")) %>%
  pivot_wider(id_cols = Ano, 
              names_from = Tipo, 
              values_from = Total) %>%
  mutate(Consumo_HA = (Consumo / Area) * 1000) 

PR_DERAL_GRAF_AGRO_HA <- ggplot(AUX02, aes(x = Ano, 
                  y = Consumo_HA,
                  group = 1)) +
  geom_line(color = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(caption = Fonte1, 
       y = "Hectares Cultivados",
       x = NULL,
       title = "Relação Agrotóxico/HA (Kg) no Paraná - (2016 - 2024)",
       subtitle = "Kg de Agrotóxico/HA") +
  scale_y_continuous(limits = c(3, 11), 
                     labels = label_number(decimal.mark = ","),
                     expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

################  Producao

AUX01 <- AUX %>%
  filter(str_detect(Variavel, "PRODUCAO") )  %>%
  mutate(Variavel = gsub("PRODUCAO_", "", Variavel))


PR_DERAL_GRAF_PRODUCAO <- ggplot(AUX01, aes(x = Variavel, 
                  y = Total,
                  group = 1)) +
  geom_line(color = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(caption = Fonte, 
       y = "Toneladas",
       x = NULL,
       title = "Produção em Toneladas no Paraná - (2016 - 2024)",
       subtitle = "Cultivos de interesse") +
  scale_y_continuous(limits = c(50000000, 120000000), 
                     labels = label_number(decimal.mark = ",", 
                                           big.mark = "."),
                     expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

#####   Regionais

AUX <- PR_DERAL_GERAL %>%
  group_by(RS) %>%
  summarise(
    across(
      c(AREA_HA_2021, AREA_HA_2022, AREA_HA_2023, AREA_HA_2024,
        TON_AGRO_2021, TON_AGRO_2022, TON_AGRO_2023, TON_AGRO_2024,
        PRODUCAO_2021, PRODUCAO_2022, PRODUCAO_2023, PRODUCAO_2024),
      ~ sum(.x, na.rm = TRUE)
    )
  )   

AUX$AGRO_HA_21_24 <- ((AUX$TON_AGRO_2021 + AUX$TON_AGRO_2022 + AUX$TON_AGRO_2023 + AUX$TON_AGRO_2024)/
  (AUX$AREA_HA_2021 + AUX$AREA_HA_2022 + AUX$AREA_HA_2023 + AUX$AREA_HA_2024)) *1000

AUX$TON_AGRO_21_24 <- (AUX$TON_AGRO_2021 + AUX$TON_AGRO_2022 + AUX$TON_AGRO_2023 + AUX$TON_AGRO_2024)

AUX$HA_21_24 <- (AUX$AREA_HA_2021 + AUX$AREA_HA_2022 + AUX$AREA_HA_2023 + AUX$AREA_HA_2024)

AUX$RS <- str_pad(AUX$RS, 
                  width = 2, 
                  side = "left", 
                  pad = "0")

MAPA_BASE_PR_RS <- left_join(MAPA_BASE_RS, 
                          AUX, 
                          by = c("RS" = "RS"))

MAPA_BASE_PR_RS$Cat <- with(MAPA_BASE_PR_RS, cut(x = AGRO_HA_21_24,
                                                 breaks = c(0, 5.5, 6.5, 7.5, 8.5, 10.0, 12.0, 14.0, Inf),
                                                 labels = c("Até 5,5", 
                                                            "5,5 - 6,5", 
                                                            "6,5 - 7,5", 
                                                            "7,5 - 8,5", 
                                                            "8,5 - 10,0", 
                                                            "10,0 - 12,0", 
                                                            "12,0 - 14,0", 
                                                            "Acima de 14,0"),
                                                 right = FALSE))

PR_DERAL_MAP_AGRO_HA_21_24 <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR_RS, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "Kg/HA",
       title = "2021 - 2024",
       subtitle = "Dados agredados por Regional de Saúde")  +
  Theme()

####  Ton de agrotóxicos bruto 2021 - 2024

MAPA_BASE_PR_RS$Cat <- with(MAPA_BASE_PR_RS, cut(x = TON_AGRO_21_24,
                                                 breaks = c(0, 1000, 5000, 10000, 18000, 26000, 35000, 45000, Inf),
                                                 labels = c("Até 1.000", 
                                                            "1.001 - 5.000", 
                                                            "5.001 - 10.000", 
                                                            "10.001 - 18.000", 
                                                            "18.001 - 26.000", 
                                                            "26.001 - 35.000", 
                                                            "35.001 - 45.000", 
                                                            "Acima de 45.000"),
                                                 right = FALSE))

PR_DERAL_MAP_TON_AGRO_21_24 <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR_RS, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno", 
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "Toneladas \nAgrotóxicos",
       title = "2017 - 2020",
       subtitle = "Dados agregados por Regional de Saúde") +
  Theme()

##### HA Cultivado

MAPA_BASE_PR_RS$Cat <- with(MAPA_BASE_PR_RS, cut(x = HA_21_24,
                                                 breaks = c(0, 1000000, 1800000, 2500000, 3200000, 3700000, 4600000, 5200000, Inf),
                                                 labels = c("Até 1 Milhão", 
                                                            "1,0 - 1,8 Milhão",
                                                            "1,8 - 2,5 Milhões", 
                                                            "2,5 - 3,2 Milhões", 
                                                            "3,2 - 3,7 Milhões", 
                                                            "3,7 - 4,6 Milhões", 
                                                            "4,6 - 5,2 Milhões", 
                                                            "Acima de 5,2 Milhões"),
                                                 right = FALSE))

PR_DERAL_MAP_HA_21_24 <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR_RS, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(
    title = "2021 - 2024",
    subtitle = "Dados agregados por Regionais de Saúde",
    fill = "Hectares\nCultivados",
    x = NULL,
    y = NULL
  )  +
  Theme() 
  
#################### 2017 - 2020

AUX <- PR_DERAL_GERAL %>%
  group_by(RS) %>%
  summarise(
    across(
      c(AREA_HA_2017, AREA_HA_2018, AREA_HA_2019, AREA_HA_2020,
        TON_AGRO_2017, TON_AGRO_2018, TON_AGRO_2019, TON_AGRO_2020),
      ~ sum(.x, na.rm = TRUE)
    )
  )   

AUX$AGRO_HA_17_20 <- ((AUX$TON_AGRO_2017 + AUX$TON_AGRO_2018 + AUX$TON_AGRO_2019 + AUX$TON_AGRO_2020)/
                        (AUX$AREA_HA_2017 + AUX$AREA_HA_2018 + AUX$AREA_HA_2019 + AUX$AREA_HA_2020)) *1000

AUX$TON_AGRO_17_20 <- (AUX$TON_AGRO_2017 + AUX$TON_AGRO_2018 + AUX$TON_AGRO_2019 + AUX$TON_AGRO_2020)

AUX$HA_17_20 <- (AUX$AREA_HA_2017 + AUX$AREA_HA_2018 + AUX$AREA_HA_2019 + AUX$AREA_HA_2020)

AUX$RS <- str_pad(AUX$RS, 
                  width = 2, 
                  side = "left", 
                  pad = "0")

MAPA_BASE_PR_RS <- left_join(MAPA_BASE_RS, 
                             AUX, 
                             by = c("RS" = "RS"))

MAPA_BASE_PR_RS$Cat <- with(MAPA_BASE_PR_RS, cut(x = AGRO_HA_17_20,
                                                 breaks = c(0, 5.5, 6.5, 7.5, 8.5, 10.0, 12.0, 14.0, Inf),
                                                 labels = c("Até 5,5", 
                                                            "5,5 - 6,5", 
                                                            "6,5 - 7,5", 
                                                            "7,5 - 8,5", 
                                                            "8,5 - 10,0", 
                                                            "10,0 - 12,0", 
                                                            "12,0 - 14,0", 
                                                            "Acima de 14,0"),
                                                 right = FALSE))

PR_DERAL_MAP_AGRO_HA_17_20 <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR_RS, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "Kg/HA", 
       title = "2017 - 2020",
       subtitle = "Dados agredados por Regional de Saúde")  +
  Theme()

####  Ton de agrotóxicos bruto 2017 - 2020

MAPA_BASE_PR_RS$Cat <- with(MAPA_BASE_PR_RS, cut(x = TON_AGRO_17_20,
                                                 breaks = c(0, 1000, 5000, 10000, 18000, 26000, 35000, 45000, Inf),
                                                 labels = c("Até 1.000", 
                                                            "1.001 - 5.000", 
                                                            "5.001 - 10.000", 
                                                            "10.001 - 18.000", 
                                                            "18.001 - 26.000", 
                                                            "26.001 - 35.000", 
                                                            "35.001 - 45.000", 
                                                            "Acima de 45.000"),
                                                 right = FALSE))

PR_DERAL_MAP_TON_AGRO_17_20 <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR_RS, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL, 
       fill = "Toneladas \nAgrotóxicos",
       title = "2017 - 2020",
       subtitle = "Dados agregados por Regional de Saúde")  +
  Theme()

####### Área Cultivada

MAPA_BASE_PR_RS$Cat <- with(MAPA_BASE_PR_RS, cut(x = HA_17_20,
                                                 breaks = c(0, 1000000, 1800000, 2500000, 3200000, 3700000, 4600000, 5200000, Inf),
                                                 labels = c("Até 1 Milhão", 
                                                            "1,0 - 1,8 Milhão",
                                                            "1,8 - 2,5 Milhões", 
                                                            "2,5 - 3,2 Milhões", 
                                                            "3,2 - 3,7 Milhões", 
                                                            "3,7 - 4,6 Milhões", 
                                                            "4,6 - 5,2 Milhões", 
                                                            "Acima de 5,2 Milhões"),
                                                 right = FALSE))

PR_DERAL_MAP_HA_17_20 <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR_RS, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(
    title = "2017 - 2020",
    subtitle = "Dados agregados por Regionais de Saúde",
    fill = "Hectares\nCultivados", 
    x = NULL,
    y = NULL
  )  +
  Theme() 

#### Juntando os mapas 

##HA

PR_DERAL_MAP_HA_17_20_21_24 <- PR_DERAL_MAP_HA_17_20 + 
  PR_DERAL_MAP_HA_21_24 + 
  plot_layout(ncol = 2, 
              guides = "collect") + 
  plot_annotation(title = 'Evolução Espacial de Área Cultivada no Paraná',
                  subtitle = 'Comparativo entre os quadriênios 2017-2020 e 2021-2024 (HA)',
                  caption =  Fonte2,
                  theme = theme(
                    plot.title = element_text(size = 20, 
                                              face = "bold"),
                    plot.subtitle = element_text(size = 14),
                    legend.position = "bottom",
                    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
                  ))

##TON de Agro

PR_DERAL_MAP_TON_AGRO_17_20_21_24 <- PR_DERAL_MAP_TON_AGRO_17_20 + 
  PR_DERAL_MAP_TON_AGRO_21_24 + 
  plot_layout(ncol = 2, 
              guides = "collect") + 
  plot_annotation(title = 'Evolução Espacial do Uso de Agrotóxico no Paraná',
                  subtitle = 'Comparativo entre os quadriênios 2017-2020 e 2021-2024 (Toneladas)',
                  caption =  paste(Fonte3),
                  theme = theme(
                    plot.title = element_text(size = 20, 
                                              face = "bold"),
                    plot.subtitle = element_text(size = 14),
                    legend.position = "bottom",
                    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
                  ))


##AGRO/HA

PR_DERAL_MAP_AGRO_HA_17_20_21_24 <- PR_DERAL_MAP_AGRO_HA_17_20 + 
  PR_DERAL_MAP_AGRO_HA_21_24 + 
  plot_layout(ncol = 2, 
              guides = "collect") + 
  plot_annotation(title = 'Evolução Espacial do Consumo de Agrotóxico no Paraná',
                  subtitle = 'Comparativo entre os quadriênios 2017-2020 e 2021-2024 (Kg/HA)',
                  caption =  Fonte2,
                  theme = theme(
                    plot.title = element_text(size = 20, 
                                              face = "bold"),
                    plot.subtitle = element_text(size = 14),
                    legend.position = "bottom",
                    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
                  ))
  
######  Municípios

AUX <- PR_DERAL_GERAL %>%
  group_by(Município_sem_Código) %>%
  summarise(across(c(AREA_HA_2021, AREA_HA_2022, AREA_HA_2023, AREA_HA_2024,
                     AREA_HA_2017, AREA_HA_2018, AREA_HA_2019, AREA_HA_2020,
                     TON_AGRO_2021, TON_AGRO_2022, TON_AGRO_2023, TON_AGRO_2024,
                     TON_AGRO_2017, TON_AGRO_2018, TON_AGRO_2019, TON_AGRO_2020,
                     PRODUCAO_2021, PRODUCAO_2022, PRODUCAO_2023, PRODUCAO_2024,
                     PRODUCAO_2017, PRODUCAO_2018, PRODUCAO_2019, PRODUCAO_2020),
      ~ sum(.x, na.rm = TRUE)
    ))

AUX$AGRO_HA_21_24 <- ((AUX$TON_AGRO_2021 + AUX$TON_AGRO_2022 + AUX$TON_AGRO_2023 + AUX$TON_AGRO_2024)/
                        (AUX$AREA_HA_2021 + AUX$AREA_HA_2022 + AUX$AREA_HA_2023 + AUX$AREA_HA_2024)) *1000


AUX$AGRO_HA_17_20 <- ((AUX$TON_AGRO_2017 + AUX$TON_AGRO_2018 + AUX$TON_AGRO_2019 + AUX$TON_AGRO_2020)/
                        (AUX$AREA_HA_2017 + AUX$AREA_HA_2018 + AUX$AREA_HA_2019 + AUX$AREA_HA_2020)) *1000

AUX$TON_AGRO_21_24 <- (AUX$TON_AGRO_2021 + AUX$TON_AGRO_2022 + AUX$TON_AGRO_2023 + AUX$TON_AGRO_2024)

AUX$TON_AGRO_17_20 <- (AUX$TON_AGRO_2017 + AUX$TON_AGRO_2018 + AUX$TON_AGRO_2019 + AUX$TON_AGRO_2020)

AUX$HA_21_24 <- (AUX$AREA_HA_2021 + AUX$AREA_HA_2022 + AUX$AREA_HA_2023 + AUX$AREA_HA_2024)

AUX$HA_17_20 <- (AUX$AREA_HA_2017 + AUX$AREA_HA_2018 + AUX$AREA_HA_2019 + AUX$AREA_HA_2020)

MAPA_BASE_Mun <- SHAPEFILE_ESTADUAL

MAPA_BASE_Mun$NM_MUN <- toupper(MAPA_BASE$NM_MUN)

MAPA_BASE_Mun$NM_MUN <- iconv(MAPA_BASE_Mun$NM_MUN,
                              from = "UTF-8", 
                              to = "ASCII//TRANSLIT")

AUX[34, 1] <- "BELA VISTA DA CAROBA"
AUX[99, 1] <- "DIAMANTE D'OESTE"
AUX[161, 1] <- "ITAPEJARA D'OESTE"
AUX[228, 1] <- "MUNHOZ DE MELO"
AUX[265, 1] <- "PEROLA D'OESTE"
AUX[299, 1] <- "RANCHO ALEGRE D'OESTE"
AUX[323, 1] <- "SANTA CRUZ DE MONTE CASTELO"
AUX[349, 1] <- "SAO JORGE D'OESTE"

###### Fazendo o left-join do shapefile com os dados do estado

MAPA_BASE_Mun <- left_join(MAPA_BASE_Mun, 
                           AUX, 
                           by = c("NM_MUN" = "Município_sem_Código"))

MAPA_BASE_Mun$Cat <- with(MAPA_BASE_Mun, cut(x = AGRO_HA_17_20,
                                             breaks = c(0, 1.5, 3.0, 4.5, 6.0, 7.5, 9.0, 10.5, Inf),
                                             labels = c("Até 1,5", 
                                                        "1,5 - 3,0", 
                                                        "3,0 - 4,5", 
                                                        "4,5 - 6,0", 
                                                        "6,0 - 7,5", 
                                                        "7,5 - 9,0", 
                                                        "9,0 - 10,5", 
                                                        "Acima de 10,5"), 
                                             right = FALSE
                                             ))

PR_DERAL_MAP_Mun_AGRO_HA_17_20 <- ggplot() + 
  geom_sf(data = MAPA_BASE_Mun, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "Kg/HA", 
       title = "2017 - 2020")  +
  Theme()

MAPA_BASE_Mun$Cat <- with(MAPA_BASE_Mun, cut(x = AGRO_HA_21_24,
                                             breaks = c(0, 1.5, 3.0, 4.5, 6.0, 7.5, 9.0, 10.5, Inf),
                                             labels = c("Até 1,5", 
                                                        "1,5 - 3,0", 
                                                        "3,0 - 4,5", 
                                                        "4,5 - 6,0", 
                                                        "6,0 - 7,5", 
                                                        "7,5 - 9,0", 
                                                        "9,0 - 10,5", 
                                                        "Acima de 10,5"), 
                                             right = FALSE
))

PR_DERAL_MAP_Mun_AGRO_HA_21_24 <- ggplot() + 
  geom_sf(data = MAPA_BASE_Mun, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "Kg/HA", 
       title = "2021 - 2024")  +
  Theme()

PR_DERAL_MAP_AGRO_HA_Mun_17_20_21_24 <- PR_DERAL_MAP_Mun_AGRO_HA_17_20 + 
  PR_DERAL_MAP_Mun_AGRO_HA_21_24 + 
  plot_layout(ncol = 2, 
              guides = "collect") + 
  plot_annotation(title = 'Evolução Espacial do Consumo de Agrotóxico/Hectare no Paraná',
                  subtitle = 'Comparativo entre os quadriênios 2017-2020 e 2021-2024 (Kg/HA)',
                  caption =  Fonte3,
                  theme = theme(
                    plot.title = element_text(size = 20, 
                                              face = "bold"),
                    plot.subtitle = element_text(size = 14),
                    legend.position = "bottom",
                    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
                  ))

##### TON DE AGRO

MAPA_BASE_Mun$Cat <- with(MAPA_BASE_Mun, cut(x = TON_AGRO_17_20,
                                                 breaks = c(0, 250, 500, 750, 1000, 1750, 3500, 5500, Inf),
                                                 labels = c("Até 250", 
                                                            "250 - 500",
                                                            "500 - 750",    
                                                            "750 - 1.000", 
                                                            "1.000 - 1.750", 
                                                            "1.750 - 3.500", 
                                                            "3.500 - 5.500", 
                                                            "Acima de 5.500"),
                                                 right = FALSE
                                                 ))

PR_DERAL_MAP_Mun_TON_AGRO_17_20 <- ggplot() + 
  geom_sf(data = MAPA_BASE_Mun, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "TONELADAS", 
       title = "2017 - 2020")  +
  Theme()

MAPA_BASE_Mun$Cat <- with(MAPA_BASE_Mun, cut(x = TON_AGRO_21_24,
                                             breaks = c(0, 250, 500, 750, 1000, 1750, 3500, 5500, Inf),
                                             labels = c("Até 250", 
                                                        "250 - 500",
                                                        "500 - 750",    
                                                        "750 - 1.000", 
                                                        "1.000 - 1.750", 
                                                        "1.750 - 3.500", 
                                                        "3.500 - 5.500", 
                                                        "Acima de 5.500"),
                                             right = FALSE
))

PR_DERAL_MAP_Mun_TON_AGRO_21_24 <- ggplot() + 
  geom_sf(data = MAPA_BASE_Mun, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "TONELADAS", 
       title = "2021 - 2024")  +
  Theme()

PR_DERAL_MAP_TON_AGRO_Mun_17_20_21_24 <- PR_DERAL_MAP_Mun_TON_AGRO_17_20 + 
  PR_DERAL_MAP_Mun_TON_AGRO_21_24 + 
  plot_layout(ncol = 2, 
              guides = "collect") + 
  plot_annotation(title = 'Evolução Espacial do Consumo de Agrotóxico no Paraná',
                  subtitle = 'Comparativo entre os quadriênios 2017-2020 e 2021-2024 (TONELADAS)',
                  caption =  Fonte3) &
                  theme(
                    plot.title = element_text(size = 20, 
                                              face = "bold"),
                    plot.subtitle = element_text(size = 14),
                    legend.position = "bottom",
                    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
                  )
#### HA cultivado

MAPA_BASE_Mun$Cat <- with(MAPA_BASE_Mun, cut(x = HA_17_20,
                                                      breaks = c(0, 25000, 50000, 100000, 150000, 200000, 300000, 500000, Inf),
                                                      labels = c("Até 25.000", 
                                                                 "25.000 - 50.000", 
                                                                 "50.000 - 100.000", 
                                                                 "100.000 - 150.000", 
                                                                 "150.000 - 200.000", 
                                                                 "200.000 - 300.000", 
                                                                 "300.000 - 500.000", 
                                                                 "Acima de 500.000"), 
                                                      right = FALSE
                                                      ))
PR_DERAL_MAP_Mun_HA_17_20 <- ggplot() + 
  geom_sf(data = MAPA_BASE_Mun, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "HECTARES", 
       title = "2017 - 2020")  +
  Theme()

MAPA_BASE_Mun$Cat <- with(MAPA_BASE_Mun, cut(x = HA_21_24,
                                             breaks = c(0, 25000, 50000, 100000, 150000, 200000, 300000, 500000, Inf),
                                             labels = c("Até 25.000", 
                                                        "25.000 - 50.000", 
                                                        "50.000 - 100.000", 
                                                        "100.000 - 150.000", 
                                                        "150.000 - 200.000", 
                                                        "200.000 - 300.000", 
                                                        "300.000 - 500.000", 
                                                        "Acima de 500.000"), 
                                             right = FALSE
))
PR_DERAL_MAP_Mun_HA_21_24 <- ggplot() + 
  geom_sf(data = MAPA_BASE_Mun, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_minimal()) +
  scale_fill_viridis_d(option = "inferno",
                       direction = -1,
                       begin = 0.1,       
                       end = 0.9,        
                       drop = FALSE) +
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       fill = "HECTARES", 
       title = "2021 - 2024")  +
  Theme()

PR_DERAL_MAP_HA_Mun_17_20_21_24 <- PR_DERAL_MAP_Mun_HA_17_20 + 
  PR_DERAL_MAP_Mun_HA_21_24 + 
  plot_layout(ncol = 2, 
              guides = "collect") + 
  plot_annotation(title = 'Evolução Espacial da Área Cultivada no Paraná',
                  subtitle = 'Comparativo entre os quadriênios 2017-2020 e 2021-2024 (TONELADAS)',
                  caption =  Fonte3) &
  theme(
    plot.title = element_text(size = 20, 
                              face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  )

################ Global MOran AGRO/HA

### Criando Matriz de vizinhança (QUEEN) Verificar se o queen é padrão no spdep

Matriz_Viz_MAPA_BASE_Mun <- poly2nb(MAPA_BASE_Mun,
                                   queen = TRUE)

### Verificando a malha queen

Centroides_Matriz_VIZ <- st_centroid(MAPA_BASE_Mun)

Centroides_Matriz_VIZ_coordenadas <- st_coordinates(Centroides_Matriz_VIZ)

Matriz_VIZ_Linhas <- nb2lines(nb = Matriz_Viz_MAPA_BASE_Mun,
                              coords = Centroides_Matriz_VIZ_coordenadas) %>%
  st_as_sf()

st_crs(Matriz_VIZ_Linhas) <- st_crs(MAPA_BASE_Mun)

PR_PEVASPEA_DERAL_MAP_MAPA_VIZINHANCA <-ggplot(MAPA_BASE_Mun, 
                                                aes(geometry = geometry)) +
  geom_sf(fill = "lightblue") +
  geom_sf(data = Matriz_VIZ_Linhas, 
          aes(geometry = geometry)) +
  ggtitle("Matriz de Vizinhança Queen, Paraná.") +
  theme_void()

### Explicação do Gemini para usar o style = "W"

# 3. style = "W" (A Padronização por Linha)
# Este é o ponto mais importante. O estilo "W" (Row-standardized) faz o seguinte:
#   Atribui pesos iguais para todos os vizinhos de uma unidade, de modo 
# que a soma dos pesos de cada linha seja igual a 1.
# Na prática: Se um município tem 4 vizinhos, cada um recebe peso 0, 25. 
# Se outro tem 10 vizinhos, cada um recebe peso 0,1
# Por que usar? Isso é padrão para o Índice de Moran Global pois 
# permite comparar regiões com diferentes números de vizinhos de 
# forma justa (evita que municípios muito "conectados" distorçam o 
# resultado apenas por terem mais fronteiras).

Matriz_Viz_Pesos <- Matriz_Viz_MAPA_BASE_Mun %>% 
  nb2listw(style = "W")

#####  Análise Global e Local Moran

#### 2017 a 2020 INÍCIO

### Realizando a suavização dos dados com Método Bayesiano Empírico 2022 - 2025
# 
# Taxa_Suavizada <- EBlocal(ri = MAPA_BASE_Mun$TON_AGRO_17_20,
#                           ni = MAPA_BASE_Mun$HA_17_20,
#                           nb = Matriz_Viz_MAPA_BASE_PR)
# 
# MAPA_BASE_Mun$TAXA_RAW_17_20 <- Taxa_Suavizada$raw * 1000
# 
# MAPA_BASE_Mun$TAXA_EST_17_20 <- Taxa_Suavizada$est * 1000

Global_Moran_17_20 <- moran.test(MAPA_BASE_Mun$AGRO_HA_17_20,
                                 Matriz_Viz_Pesos)

### Scatterplot

moran.plot(MAPA_BASE_Mun$AGRO_HA_17_20, 
           Matriz_Viz_Pesos, 
           labels = FALSE, 
           pch = 15, 
           col = "blue", 
           xlab = "Variável Original", 
           ylab = "Média dos Vizinhos (Spatial Lag)",
           main = "Moran Scatterplot")

#### Calculando o Local Moran
### Travando o local moran
set.seed(321)

Lisa <- localmoran_perm(MAPA_BASE_Mun$AGRO_HA_17_20, 
                        Matriz_Viz_Pesos, 
                        nsim = 9999,
                        zero.policy = TRUE)

MAPA_BASE_Mun$local_I_17_20 <- Lisa[,1]

MAPA_BASE_Mun$local_I_p_valor_17_20 <- Lisa[,5]

ggplot(MAPA_BASE_Mun, 
       aes(geometry = geometry)) +
  geom_sf(aes(fill = local_I_17_20)) +
  scale_fill_gradient2(low = "blue", high = "red", 
                       mid = "white", 
                       midpoint = 0,
                       name = "I local") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))

quadrantes <- attr(Lisa, 
                   "quadr")$mean

MAPA_BASE_Mun$quadrante_17_20 <- case_when(quadrantes == "High-High" ~ "Alto-Alto",
                                          quadrantes == "Low-Low" ~ "Baixo-Baixo",
                                          quadrantes == "High-Low" ~ "Alto-Baixo",
                                          quadrantes == "Low-High" ~ "Baixo-Alto")

MAPA_BASE_Mun <- MAPA_BASE_Mun %>%
  mutate(Lisa_resultado_17_20 = case_when(
    local_I_p_valor_17_20 < 0.05 ~ quadrante_17_20,
    local_I_p_valor_17_20 >= 0.05 ~ "Não significativo"
  ))

Niveis_LISA <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não significativo")

MAPA_BASE_Mun$Lisa_resultado_17_20 <- factor(MAPA_BASE_Mun$Lisa_resultado_17_20, levels = Niveis_LISA)

PR_PEVASPEA_DERAL_LOCAL_MORAN_17_20 <- ggplot(MAPA_BASE_Mun, 
                                               aes(geometry = geometry)) +
  geom_sf(color = "grey30", 
          linewidth = 0.1, 
          aes(fill = Lisa_resultado_17_20)) +
  scale_fill_manual(name = "LISA \nClusters",
                    drop = FALSE,
                    values = c("Alto-Alto" = "red",        
                               "Baixo-Baixo" = "blue",      
                               "Alto-Baixo" = "pink",       
                               "Baixo-Alto" = "lightblue",  
                               "Não significativo" = "grey90")
  ) +
  geom_sf(data = SHAPEFILE_ESTADUAL_RS,
          color = "black",   
          linewidth = 0.5,   
          fill = NA) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal()) +
  
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       title = "2017 - 2020",
       subtitle = "Global Moran I = 0.159 (p < 0.001)") +
  Theme() +
  theme(legend.key.width = unit(1.5, "cm")) +
  theme(legend.position = "bottom")

########  2021 - 2024

Global_Moran_21_24 <- moran.test(MAPA_BASE_Mun$AGRO_HA_21_24,
                                 Matriz_Viz_Pesos)

### Scatterplot

moran.plot(MAPA_BASE_Mun$AGRO_HA_21_24, 
           Matriz_Viz_Pesos, 
           labels = FALSE, 
           pch = 15, 
           col = "blue", 
           xlab = "Variável Original", 
           ylab = "Média dos Vizinhos (Spatial Lag)",
           main = "Moran Scatterplot")

#### Calculando o Local Moran
### Travando o local moran
set.seed(321)

Lisa <- localmoran_perm(MAPA_BASE_Mun$AGRO_HA_21_24, 
                        Matriz_Viz_Pesos, 
                        nsim = 9999,
                        zero.policy = TRUE)

MAPA_BASE_Mun$local_I_21_24 <- Lisa[,1]

MAPA_BASE_Mun$local_I_p_valor_21_24 <- Lisa[,5]

ggplot(MAPA_BASE_Mun, 
       aes(geometry = geometry)) +
  geom_sf(aes(fill = local_I_21_24)) +
  scale_fill_gradient2(low = "blue", high = "red", 
                       mid = "white", 
                       midpoint = 0,
                       name = "I local") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))

quadrantes <- attr(Lisa, 
                   "quadr")$mean

MAPA_BASE_Mun$quadrante_21_24 <- case_when(quadrantes == "High-High" ~ "Alto-Alto",
                                           quadrantes == "Low-Low" ~ "Baixo-Baixo",
                                           quadrantes == "High-Low" ~ "Alto-Baixo",
                                           quadrantes == "Low-High" ~ "Baixo-Alto")

MAPA_BASE_Mun <- MAPA_BASE_Mun %>%
  mutate(Lisa_resultado_21_24 = case_when(
    local_I_p_valor_21_24 < 0.05 ~ quadrante_21_24,
    local_I_p_valor_21_24 >= 0.05 ~ "Não significativo"
  ))

Niveis_LISA <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não significativo")

MAPA_BASE_Mun$Lisa_resultado_21_24 <- factor(MAPA_BASE_Mun$Lisa_resultado_21_24, levels = Niveis_LISA)

PR_PEVASPEA_DERAL_LOCAL_MORAN_21_24 <- ggplot(MAPA_BASE_Mun, 
                                               aes(geometry = geometry)) +
  geom_sf(color = "grey30", 
          linewidth = 0.1, 
          aes(fill = Lisa_resultado_21_24)) +
  scale_fill_manual(name = "LISA \nClusters",
                    drop = FALSE,
                    values = c("Alto-Alto" = "red",        
                               "Baixo-Baixo" = "blue",      
                               "Alto-Baixo" = "pink",       
                               "Baixo-Alto" = "lightblue",  
                               "Não significativo" = "grey90")
  ) +
  geom_sf(data = SHAPEFILE_ESTADUAL_RS,
          color = "black",   
          linewidth = 0.5,   
          fill = NA) +
  annotation_scale(location = "bl") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true",
                         style = north_arrow_minimal()) +
  
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       title = "2021 - 2024",
       subtitle = "Global Moran I = 0.249 (p < 0.001)") +
  Theme() +
  theme(legend.key.width = unit(1.5, "cm")) +
  theme(legend.position = "bottom")

PR_PEVASPEA_DERAL_LOCAL_MORAN_18_21_22_25 <- PR_PEVASPEA_DERAL_LOCAL_MORAN_17_20 + 
  PR_PEVASPEA_DERAL_LOCAL_MORAN_21_24 + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(
    title = "Progressão de Agrupamentos do Consumo de Agrotóxico/Hectare no Paraná",
    subtitle = 'Comparativo entre os quadriênios 2018-2021 e 2022-2025',
    caption = Fonte  
  ) & 
  theme(
    plot.title = element_text(size = 16, 
                              face = "bold", 
                              hjust = 0), 
    plot.subtitle = element_text(size = 12, 
                                 hjust = 0),
    plot.caption = element_text(hjust = 0, 
                                face = "italic", 
                                size = 10),
    legend.position = "bottom",        
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.justification = "center",
    legend.key.width = unit(1.2, "cm")   
  )


