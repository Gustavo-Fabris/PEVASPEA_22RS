rm(list = ls())

setwd("/home/gustavo/Área de trabalho/Análise_de_Dados/")

####  Libraries

library(spdep)
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

Theme <- function(){
  theme(axis.text.x = element_text(angle = 50, 
                                   vjust = .5,
                                   face = "bold",
                                   size = 14),
        panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.title = element_text(face = "bold",
                                  size = 24,
                                  colour = "black"),
        legend.position = "bottom") +
    theme(legend.title = element_text(face = "bold",
                                      size = 14), 
          legend.text = element_text(size = 14), 
          plot.subtitle = element_text(hjust = 0,
                                       size = 12),
          plot.caption = element_text(size = 12,
                                      hjust = 0),
          plot.title = element_text(hjust = 0, 
                                    face = "bold",
                                    size = 20)    
    )
}

################################################################################
################################################################################

#### Gráfico ANomalias PR 2022 - 2025

AUX <- as.data.frame(apply(PR_PEVASPEA_SINASC_Serie_Historica[c(7, 8, 9, 10),3:13], 2, sum))

colnames(AUX) <- "Valores"

AUX$Evento <- c("Nascidos", "Nº Anomalias", "Anomalias 
Prioritárias", "Tubo 
Neural", "Microcefalia",
              "Cardiopatias", "Fendas 
Orais", "Genito-
-urinárias", "Membros", 
              "Parede 
Abdominal", "Sindrome 
de Down")

AUX$Evento <- factor(AUX$Evento, levels = AUX$Evento)

PR_ANOMAL_22_25 <- ggplot(AUX[c(2:11), ], aes(x = Evento, 
                y = Valores)) +
  geom_bar(stat = "identity", 
           color = "black", 
           fill = "#8FBC8F", 
           width = 0.7) + 
  geom_text(aes(label = Valores),
            hjust = -0.2, 
            fontface = "bold", 
            size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  coord_flip() +
  theme(axis.text.y = element_text(face = "bold", 
                                   size = 11,
                                   color = "black"),
        axis.text.x = element_text(face = "bold", 
                                   size = 10),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.grid.major.x = element_line(color = "#E5E5E5"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold",
                              size = 18, 
                              hjust = 0),
    plot.subtitle = element_text(size = 12, 
                                 color = "gray30",
                                 margin = margin(b = 15)),
    plot.caption = element_text(size = 10, 
                                hjust = 0, 
                                face = "italic"),
    legend.position = "none"
    ) +
  labs(title = "Série Histórica de Anomalias (PR)",
    subtitle = "Dados consolidados de 2022 a 2025",
    caption = Fonte,
    x = NULL,
    y = "Nº de ocorrências")

#### Gráfico ANomalias PR 2018 - 2021

AUX <- as.data.frame(apply(PR_PEVASPEA_SINASC_Serie_Historica[c(7, 8, 9, 10),3:13], 2, sum))

colnames(AUX) <- "Valores"

AUX$Evento <- c("Nascidos", "Nº Anomalias", "Anomalias 
Prioritárias", "Tubo 
Neural", "Microcefalia",
                "Cardiopatias", "Fendas 
Orais", "Genito-
-urinárias", "Membros", 
                "Parede 
Abdominal", "Sindrome 
de Down")

AUX$Evento <- factor(AUX$Evento, levels = AUX$Evento)

PR_ANOMAL_22_25 <- ggplot(AUX[c(2:11), ], aes(x = Evento, 
                                              y = Valores)) +
  geom_bar(color = "black", 
           fill = "#8FBC8F",
           stat = "identity") + 
  Theme() +
  theme(axis.text.x = element_text(angle = 60))


####  Séries Temporais

##  Nascidos

PR_PEVASPEA_SINASC_NASC_SE_GERAL <- PR_PEVASPEA_SINASC_NASC_SE_GERAL[, -54]

colnames (PR_PEVASPEA_SINASC_NASC_SE_GERAL)[2:53] <- c(1:52)

PR_PEVASPEA_SINASC_NASC_SE_GERAL <- PR_PEVASPEA_SINASC_NASC_SE_GERAL %>%
  pivot_longer(cols = c(2:53) ,
               names_to = "SE",
               values_to = "Casos") %>%
  mutate(SE = as.numeric(SE)) %>%
  mutate(Tempo = row_number()) 

colnames(PR_PEVASPEA_SINASC_NASC_SE_GERAL)[1] <- "Ano"

quebras_anos <- seq(1, max(PR_PEVASPEA_SINASC_NASC_SE_GERAL$Tempo), 
                    by = 52)

labels_anos <- 2016 + (0:(length(quebras_anos) - 1))

PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Nasc <- ggplot(PR_PEVASPEA_SINASC_NASC_SE_GERAL, 
                                                  aes(x = Tempo, 
                                                      y = Casos)) +
  geom_line(color = "steelblue", 
            size = 0.8) +
  scale_x_continuous(
    breaks = quebras_anos, 
    labels = labels_anos
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey85"),
    axis.text.x = element_text(angle = 50, 
                               vjust = .5,
                               face = "bold",
                               size = 14),
    plot.title = element_text(face = "bold",
                              size = 24,
                              colour = "black"),
    legend.position = "bottom",  
    plot.subtitle = element_text(hjust = 0,
                                 size = 12),
    plot.caption = element_text(size = 12,
                                hjust = 0),
  ) +
  labs(title = "Série Temporal de Nascidos Vivos no PR",
       x = "Ano",
       y = "Qtd. de Nascimentos")

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

PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL <- PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL %>%
  mutate(Media_Suave = rollmean(Casos,    # Média Móvel
                                k = 5,    # Alinhamento central com 5 observações
                                fill = NA))

cores <- c("Casos Brutos" = "grey70", 
           "Média Móvel (5 sem.)" = "#2c3e50", 
           "LOESS" = "darkred")

PR_PEVASPEA_SINASC_GRAF_Serie_Temp_ANOMAL <- ggplot(PR_PEVASPEA_SINASC_ANOMAL_SE_GERAL, 
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
            size = 1) +
  geom_smooth(aes(y = Casos, 
                  color = "LOESS"), 
              method = "loess", 
              span = 0.3,      
              linetype = "dashed", 
              se = FALSE) +
  scale_color_manual(values = cores,
                     name = NULL) +
  scale_x_continuous(breaks = quebras_anos,
                     labels = labels_anos) +
  scale_y_continuous(limits = c(8, NA)) + 
  labs(caption = Fonte, 
       y = "Ocorrências de Anomalias Congênitas",
       x = "Anos",
       title = "Série Temporal - Anomalias Congênitas (2016 a 2025)",
       subtitle = "Anomalias congênitas agrupadas por semana epidemiológica") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    plot.title = element_text(hjust = 0, 
                              face = "bold",
                              size = 20),
    axis.title = element_text(size = 14, 
                              face = "bold")
  )

#### Série Histórica RS

AUX <- RS22_PEVASPEA_SINASC_Serie_historica[, c(1:4)]

AUX <- mutate(AUX, 
              Taxa_Anomalias = (AUX[, 4]/AUX[, 3]) *1000)

AUX$Taxa_Anomalias <- format(round(AUX[, 5], 2))

AUX$RS <- as.factor(AUX$RS)

RS_SINASC_GRAF_SERIE_HIST_ANOMAL <- ggplot(AUX, aes(x = RS,
                                                    y = Taxa_Anomalias, 
                                                    group = 1)
) +
  geom_line(linewidth = 1.3,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  labs(caption = Fonte, 
       y = "Anomalias/1000 Nascimentos",
       x = NULL,
       title = "Série Histórica Regional - Anomalias Congênitas (2016 a 2025)",
       subtitle = "Notificações referentes ao município de residência") +
  geom_label(aes(label = Taxa_Anomalias), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.5)  +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.2),
  )
  ) +
  Theme() 

######## Série Histórica Paraná

AUX <- PR_PEVASPEA_SINASC_Serie_Historica[, c(1:4)]

AUX <- mutate(AUX, 
              Taxa_Anomalias = (AUX[, 4]/AUX[, 3]) *1000)

AUX$Taxa_Anomalias <- format(round(AUX[, 5], 2))

AUX$RS <- as.factor(AUX$RS)


PR_SINASC_GRAF_SERIE_HIST_ANOMAL <- ggplot(AUX, aes(x = RS,
                                                    y = Taxa_Anomalias, 
                                                    group = 1)
) +
  geom_line(linewidth = 1.3,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  labs(caption = Fonte, 
       y = "Anomalias/1000 Nascimentos",
       x = NULL,
       title = "Série Histórica Paraná - Anomalias Congênitas (2016 a 2025)",
       subtitle = "Notificações referentes ao município de residência") +
  geom_label(aes(label = Taxa_Anomalias), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.5)  +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.2),
  )
  ) +
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
                    values_to = "Taxa_1000")

PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_I <- ggplot(AUX, aes(x = Ano, 
                                                         y = Taxa_1000)) +
  geom_line(aes(x = Ano,
                y = Taxa_1000,
                colour = RS,
                group = RS),
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = NULL,
       title = "Comparativo de Séries Históricas - 04 RS (2016 a 2025)",
       subtitle = "Taxa de Anomalias/1000 nascimentos - Seis RS com maior Taxa em 2025") +
  scale_colour_manual(name = "Animal",
                      values = c("4" = "#4F4F4F")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

#### 01 RS

AUX <- AUX01 %>% 
  filter(RS == 1)

AUX <- pivot_longer(AUX, 
                    2:11, 
                    names_to = "Ano", 
                    values_to = "Taxa_1000")

PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_II <- ggplot(AUX, aes(x = Ano, 
                                                          y = Taxa_1000)) +
  geom_line(aes(x = Ano,
                y = Taxa_1000,
                colour = RS,
                group = RS),
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = NULL,
       title = "Comparativo de Séries Históricas - 01 RS (2016 a 2025)",
       subtitle = "Taxa de Anomalias/1000 nascimentos - Seis RS com maior Taxa em 2025") +
  scale_colour_manual(name = "Animal",
                      values = c("1" = "#4F4F4F")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

####  20 RS

AUX <- AUX01 %>% 
  filter(RS == 20)

AUX <- pivot_longer(AUX, 
                    2:11, 
                    names_to = "Ano", 
                    values_to = "Taxa_1000")

PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_III <- ggplot(AUX, aes(x = Ano, 
                                                           y = Taxa_1000)) +
  geom_line(aes(x = Ano,
                y = Taxa_1000,
                colour = RS,
                group = RS),
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = NULL,
       title = "Comparativo de Séries Históricas - 20 RS (2016 a 2025)",
       subtitle = "Taxa de Anomalias/1000 nascimentos - Seis RS com maior Taxa em 2025") +
  scale_colour_manual(name = "Animal",
                      values = c("20" = "#4F4F4F")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

####  20 RS

AUX <- AUX01 %>% 
  filter(RS == 17)

AUX <- pivot_longer(AUX, 
                    2:11, 
                    names_to = "Ano", 
                    values_to = "Taxa_1000")

PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_IV <- ggplot(AUX, aes(x = Ano, 
                                                          y = Taxa_1000)) +
  geom_line(aes(x = Ano,
                y = Taxa_1000,
                colour = RS,
                group = RS),
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = NULL,
       title = "Comparativo de Séries Históricas - 17 RS (2016 a 2025)",
       subtitle = "Taxa de Anomalias/1000 nascimentos - Seis RS com maior Taxa em 2025") +
  scale_colour_manual(name = "Animal",
                      values = c("17" = "#4F4F4F")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

####  02 RS

AUX <- AUX01 %>% 
  filter(RS == 02)

AUX <- pivot_longer(AUX, 
                    2:11, 
                    names_to = "Ano", 
                    values_to = "Taxa_1000")

PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_V <- ggplot(AUX, aes(x = Ano, 
                                                         y = Taxa_1000)) +
  geom_line(aes(x = Ano,
                y = Taxa_1000,
                colour = RS,
                group = RS),
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(caption = Fonte, 
       y = "Número de Casos",
       x = NULL,
       title = "Comparativo de Séries Históricas - 02 RS (2016 a 2025)",
       subtitle = "Taxa de Anomalias/1000 nascimentos - Seis RS com maior Taxa em 2025") +
  scale_colour_manual(name = "Animal",
                      values = c("2" = "#4F4F4F")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

AUX_LIST <- list(PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_I,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_II,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_III,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_IV,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_V)

PR_PEVASPEA_SINASC_GRAF_05_RS <- wrap_plots(AUX_LIST, ncol = 1)

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
  # pivot_longer(-Ano, names_to = "Municipios") %>%
  mutate(
    Ano = Ano,
    Município = gsub("_", " ", Município)
  ) %>%
  group_split(Município) %>% 
  lapply(function(dados) {
    titulo <- dados$Município %>% 
      unique() %>% 
      paste0(" - Anomalias/1000 Nascimentos")
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
           y = "Anomalias/1000 Nascimentos",
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

Theme <- function(){
  theme(panel.grid.major = element_line(color = "#C0C0C0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#F5F5F5"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold",
                                    size = 14), 
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(hjust = 0,
                                     size = 12),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        plot.title = element_text(hjust = 0, 
                                  face = "bold",
                                  size = 24)
  )
}

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

####  Taxa de nascidos vivos com anomalias 2018 - 21 (por 1000 nasc)

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
                          AUX, 
                          by = c("NM_MUN" = "Município_sem_Código"))

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = TAXA_4a_18_21,
                                           breaks = c(-Inf, 0, 5, 10, 15, 20, 25, 30, 50, 1000),
                                           labels = c("0 casos", "1 - 5", "6 - 10", "11 - 15", 
                                                      "16 - 20", "21 - 25", "26 - 30", "31 - 50", ">50"),
                                           right = FALSE))

PR_SINASC_MAP_TAXA_4A_ANOMAL_18_21 <- ggplot() + 
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
       title = "Taxa Anomalias/1000 Nascidos Vivos (2018 - 2021) - 
Paraná",
       subtitle = "Referente ao Município de Residência")  +
  Theme()

############  Taxa 04 anos (22 - 25) municípios estado

AUX <- PR_PEVASPEA_SINASC_Serie_historica_Mun[, 1]

AUX <- PR_PEVASPEA_SINASC_Serie_historica_Mun %>%
  mutate(TAXA_4a = ((PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2025 + 
                       PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2024 +
                       PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2023 +
                       PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2022)/
                      (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2025 + 
                         PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2024 +
                         PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2023 +
                         PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2022 ) *
                      1000), 
         Nascidos_4a = (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2025 + 
                          PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2024 +
                          PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2023 +
                          PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2022 ),
         Anomalias_4a = (PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2025 + 
                           PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2024 +
                           PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2023 +
                           PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2022)
  )

AUX <- AUX[-nrow(AUX),]

MAPA_BASE_PR <- left_join(MAPA_BASE_PR, 
                          AUX, 
                          by = c("NM_MUN" = "Município_sem_Código"))

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = TAXA_4a,
                                           breaks = c(-Inf, 0, 5, 10, 15, 20, 25, 30, 50, 1000),
                                           labels = c("0 casos", "1 - 5", "6 - 10", "11 - 15", 
                                                      "16 - 20", "21 - 25", "26 - 30", "31 - 50", ">50"),
                                           right = FALSE))

PR_SINASC_MAP_TAXA_4A_ANOMAL <- ggplot() + 
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
       title = "Taxa Anomalias/1000 Nascidos Vivos (2022 - 2025) - 
Paraná",
       subtitle = "Referente ao Município de Residência")  +
  Theme()

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

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, 
                         cut(x = TAXA_4a_18_21,
                             breaks = c(-Inf, 0, 3, 5, 7, 9, 11, 14, 20, 1000),
                             labels = c("0", "1 - 3", "4 - 5", "6 - 7", 
                                        "8 - 9", "10 - 11", "12 - 14", "15 - 20", "> 20"),
                             right = FALSE))

MAPA_BASE_RS <- st_as_sf(MAPA_BASE_RS)

PR_SINASC_MAP_TAXA_4_ANOS_18_21_ANOMAL_RS <- ggplot() +
  geom_sf(data = MAPA_BASE_RS, 
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
       title = "Taxa Anomalias/1000 Nascidos Vivos em 2018 - 2021 - 
Regionais Paraná",
       subtitle = "Referente ao Município de Residência") + 
  Theme()

#############   Taxa Anomalias/1000 nascimentos nos últimos 04 anos somados
#############   por Regionais de Saúde

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica[, 1]

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica %>%
  mutate(TAXA_4a = ((PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2025 + 
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

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, 
                         cut(x = TAXA_4a,
                             breaks = c(-Inf, 0, 3, 5, 7, 9, 11, 14, 20, 1000),
                             labels = c("0", "1 - 3", "4 - 5", "6 - 7", 
                                        "8 - 9", "10 - 11", "12 - 14", "15 - 20", "> 20"),
                             right = FALSE))

MAPA_BASE_RS <- st_as_sf(MAPA_BASE_RS)

PR_SINASC_MAP_TAXA_4_ANOS_ANOMAL_RS <- ggplot() +
  geom_sf(data = MAPA_BASE_RS, 
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
       title = "Taxa Anomalias/1000 Nascidos Vivos em 2022 - 2025 - 
Regionais Paraná",
       subtitle = "Referente ao Município de Residência") + 
  Theme()

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
  ggtitle("Matriz de Vizinhança por Contiguidade, Paraná.") +
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

MAPA_BASE_PR$TAXA_RAW_18_21 <- Taxa_Suavizada$raw

MAPA_BASE_PR$TAXA_EST_18_21 <- Taxa_Suavizada$est

Global_Moran <- moran.test(MAPA_BASE_PR$TAXA_EST_18_21,
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

lisa <- localmoran_perm(MAPA_BASE_PR$TAXA_EST_18_21, 
                        Matriz_Viz_Pesos, 
                        nsim = 9999,
                        zero.policy = TRUE)

MAPA_BASE_PR$local_I <- lisa[,1]

MAPA_BASE_PR$local_I_p_valor <- lisa[,5]

ggplot(MAPA_BASE_PR, 
       aes(geometry = geometry)) +
  geom_sf(aes(fill = local_I)) +
  scale_fill_gradient2(low = "blue", high = "red", 
                       mid = "white", 
                       midpoint = 0,
                       name = "I local") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))

quadrantes <- attr(lisa, 
                   "quadr")$mean

MAPA_BASE_PR$quadrante <- case_when(quadrantes == "High-High" ~ "Alto-Alto",
                                    quadrantes == "Low-Low" ~ "Baixo-Baixo",
                                    quadrantes == "High-Low" ~ "Alto-Baixo",
                                    quadrantes == "Low-High" ~ "Baixo-Alto")

MAPA_BASE_PR <- MAPA_BASE_PR %>%
  mutate(LISA_resultado_18_21 = case_when(
    local_I_p_valor < 0.05 ~ quadrante,
    local_I_p_valor >= 0.05 ~ "Não significativo"
  ))

ggplot(MAPA_BASE_PR, 
       aes(geometry = geometry)) +
  geom_sf(aes(fill = LISA_resultado_18_21)) +
  scale_fill_manual(name = NULL,
                    values = c("Alto-Alto" = "red",        
                               "Baixo-Baixo" = "blue",      
                               "Alto-Baixo" = "pink",       
                               "Baixo-Alto" = "lightblue",  
                               "Não significativo" = "white")
  ) +
  Theme() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))

#### Fim da análise Global and Local Moran 2018 - 2021

### Realizando a suavização dos dados com Método Bayesiano Empírico 2022 - 2025

Taxa_Suavizada <- EBlocal(ri = MAPA_BASE_PR$Anomalias_4a, 
                          ni = MAPA_BASE_PR$Nascidos_4a,
                          nb = Matriz_Viz_MAPA_BASE_PR)

MAPA_BASE_PR$TAXA_RAW <- Taxa_Suavizada$raw

MAPA_BASE_PR$TAXA_EST <- Taxa_Suavizada$est

Global_Moran <- moran.test(MAPA_BASE_PR$TAXA_EST,
                           Matriz_Viz_Pesos)

### Scatterplot

moran.plot(MAPA_BASE_PR$TAXA_EST, 
           Matriz_Viz_Pesos, 
           labels = FALSE, 
           pch = 15, 
           col = "blue", 
           xlab = "Variável Original", 
           ylab = "Média dos Vizinhos (Spatial Lag)",
           main = "Moran Scatterplot")

#### Calculando o Local Moran

lisa <- localmoran_perm(MAPA_BASE_PR$TAXA_EST, 
                        Matriz_Viz_Pesos, 
                        nsim = 9999,
                        zero.policy = TRUE)

MAPA_BASE_PR$local_I <- lisa[,1]

MAPA_BASE_PR$local_I_p_valor <- lisa[,5]

ggplot(MAPA_BASE_PR, 
       aes(geometry = geometry)) +
  geom_sf(aes(fill = local_I)) +
  scale_fill_gradient2(low = "blue", high = "red", 
                       mid = "white", 
                       midpoint = 0,
                       name = "I local") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))

quadrantes <- attr(lisa, 
                   "quadr")$mean

MAPA_BASE_PR$quadrante <- case_when(quadrantes == "High-High" ~ "Alto-Alto",
                                    quadrantes == "Low-Low" ~ "Baixo-Baixo",
                                    quadrantes == "High-Low" ~ "Alto-Baixo",
                                    quadrantes == "Low-High" ~ "Baixo-Alto")

MAPA_BASE_PR <- MAPA_BASE_PR %>%
  mutate(LISA_resultado = case_when(
    local_I_p_valor < 0.05 ~ quadrante,
    local_I_p_valor >= 0.05 ~ "Não significativo"
  ))

PR_PEVASPEA_SINASC_LOCAL_MORAN_22_25 <- ggplot(MAPA_BASE_PR, 
                                               aes(geometry = geometry)) +
  geom_sf(aes(fill = LISA_resultado)) +
  scale_fill_manual(name = NULL,
                    values = c("Alto-Alto" = "red",        
                               "Baixo-Baixo" = "blue",      
                               "Alto-Baixo" = "pink",       
                               "Baixo-Alto" = "lightblue",  
                               "Não significativo" = "white")
  ) + annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +  
  coord_sf(expand = FALSE)+
  labs(x = NULL,
       y = NULL,
       caption = Fonte, 
       title = "Local Moran Anomalias/1000 nascimentos 
Paraná (2022 - 2025)",
       subtitle = "Taxa suavizada utilizando Método Bayesiano Empírico") +
  Theme() +
  theme(legend.key.width = unit(1.5, "cm"))

######  Tabelas 
################################################################################
################################################################################


#### Tabela ANomalias Regionais 2022 - 2025
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
  mutate(Percentual = round((Absoluto / Nascidos) * 100, 2)
  ) %>%
  pivot_wider(names_from = Evento, 
    values_from = c(Absoluto, Percentual),
    names_glue = "{Evento}_{.value}")

AUX <- AUX[c(1, 12, 16:22, 2:11, 13, 14, 15, 23), c(1, 2, 3, 13, 4, 14, 5, 15, 6, 16, 7, 17, 8, 18, 9, 19, 10, 20, 11, 21, 12, 22)]

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

#   
#   
#   as.data.frame(apply(PR_PEVASPEA_SINASC_Serie_Historica[c(7, 8, 9, 10),3:13], 2, sum))
# 
# colnames(AUX) <- "Valores"
# 
# AUX$Evento <- c("Nascidos", "Nº Anomalias", "Anomalias 
# Prioritárias", "Tubo 
# Neural", "Microcefalia",
#                 "Cardiopatias", "Fendas 
# Orais", "Genito-
# -urinárias", "Membros", 
#                 "Parede 
# Abdominal", "Sindrome 
# de Down")

####  Salvando os gráficos, mapas e tabelas

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/RS_SINASC_GRAF_SERIE_HIST.png",
       RS_SINASC_GRAF_SERIE_HIST_ANOMAL,
       width = 56,
       height = 24,
       units = "cm",)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_SINASC_GRAF_SERIE_HIST.png",
       PR_SINASC_GRAF_SERIE_HIST_ANOMAL,
       width = 56,
       height = 24,
       units = "cm",)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/RS_SINASC_MAP_TAXA_4_ANOS_ANOMAL.png",
       PR_SINASC_MAP_TAXA_4A_ANOMAL,
       width = 56,
       height = 46,
       units = "cm",)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/RS_SINASC_MAP_TAXA_4_ANOS_ANOMAL.png",
       PR_SINASC_MAP_TAXA_4A_ANOMAL,
       width = 56,
       height = 46,
       units = "cm",)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/RS_SINASC_MAP_TAXA_4_ANOS_ANOMAL_18_21.png",
       PR_SINASC_MAP_TAXA_4A_ANOMAL_18_21,
       width = 56,
       height = 46,
       units = "cm",)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_SINASC_MAP_TAXA_4_ANOS_ANOMAL_RS.png",
       PR_SINASC_MAP_TAXA_4_ANOS_ANOMAL_RS,
       width = 56,
       height = 46,
       units = "cm",)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_SINASC_MAP_TAXA_4_18_21_ANOS_ANOMAL_RS.png",
       PR_SINASC_MAP_TAXA_4_ANOS_18_21_ANOMAL_RS,
       width = 56,
       height = 46,
       units = "cm",)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_MAP_LOCAL_MORAN_22_25.png",
       PR_PEVASPEA_SINASC_LOCAL_MORAN_22_25,
       width = 56,
       height = 46,
       units = "cm",)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Nasc.png",
       PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Nasc,
       width = 25,    
       height = 10,     
       dpi = 300,
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_Serie_Temp_ANOMAL.png",
       PR_PEVASPEA_SINASC_GRAF_Serie_Temp_ANOMAL,
       width = 25,    
       height = 10,     
       dpi = 300,
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Nasc.png",
       PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Nasc,
       width = 25,    
       height = 10,     
       dpi = 300,
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_05_RS.png",
       PR_PEVASPEA_SINASC_GRAF_05_RS,
       width = 38,
       height = 46,
       units = "cm",)

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/RS_PEVASPEA_SINASC_GRAF_Taxa_Mun.png",
       RS_PEVASPEA_SINASC_GRAF_Taxa_Mun,
       width = 38,
       height = 46,
       units = "cm",)

rm(cores,
   Fonte,
   labels_anos,
   max_global,
   quadrantes,
   quebras_anos,
   AUX,
   AUX_LIST,
   AUX01,
   Centroides_Matriz_VIZ,
   Centroides_Matriz_VIZ_coordenadas,
   Theme,
   Theme_Mun,
   lisa,
   MAPA_BASE,
   MAPA_BASE_PR,
   MAPA_BASE_RS,
   Matriz_VIZ_Linhas,
   Matriz_Viz_MAPA_BASE_PR,
   Matriz_Viz_Pesos,
   Global_Moran,
   Taxa_Suavizada,
   SHAPEFILE_ESTADUAL,
   SHAPEFILE_ESTADUAL_RS,
   SHAPEFILE_REGIONAL,
   SHAPEFILE_REGIONAL_Dissolvido)
