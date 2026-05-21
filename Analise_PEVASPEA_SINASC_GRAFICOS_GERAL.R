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

Base_Populacional <- read.csv (file = "Base_de_Dados/Auxiliares/Planilha_Base_Populacional_IBGE_Estimada.csv",
                               header = TRUE,
                               sep = ",")

Base_Populacional_Masculina_2022 <- read.csv (file = "Base_de_Dados/Auxiliares/Planilha_Base_Populacional_IBGE_Estimada_MASCULINA.csv",
                                              header = TRUE,
                                              sep = ",")

Base_Populacional_Feminina_2022 <- read.csv (file = "Base_de_Dados/Auxiliares/Planilha_Base_Populacional_IBGE_Estimada_FEMININA.csv",
                                             header = TRUE,
                                             sep = ",")

Base_IBGE <- read.csv (file = "Base_de_Dados/Auxiliares/Planilha_Base_IBGE.csv",
                       header = TRUE,
                       sep = ",")

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
      panel.grid.major = element_line(color = "grey90", 
                                      linewidth = 0.2),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", 
                                  size = 12), 
      legend.text = element_text(size = 11),
      legend.box.margin = margin(t = 10),
      plot.title = element_text(face = "bold",
                                size = 18, 
                                hjust = 0, 
                                margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, 
                                   hjust = 0, 
                                   color = "grey30", 
                                   margin = margin(b = 15)),
      plot.caption = element_text(size = 10, 
                                  hjust = 0, 
                                  face = "italic", 
                                  margin = margin(t = 15)),
      axis.title = element_text(face = "bold", 
                                size = 11),
      axis.text = element_text(size = 10, 
                               color = "black"),
      plot.margin = margin(20, 20, 20, 20)
    )
}

################################################################################
################################################################################

#### Gráfico ANomalias PR 2021 - 2025

AUX <- colSums(PR_PEVASPEA_SINASC_Serie_Historica[c(6, 7, 8, 9, 10), 3:13], na.rm = TRUE)

AUX01 <- data.frame( Evento = c("Anomalia Detectada", "Anomalias Prioritárias", "Tubo Neural", 
                                "Microcefalia", "Cardiopatias", "Fendas Orais", 
                                "Geniturinárias", "Membros", "Parede Abdominal", "Síndrome de Down"),
                     Absoluto = AUX[2:11], 
                     Nascidos = AUX[1] )

AUX01 <- AUX01 %>%
  mutate(Taxa = round((Absoluto / Nascidos) * 1000, 2)) %>%
  filter(Evento != "Anomalia Detectada",
         Evento != "Anomalias Prioritárias")

PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_21_25 <- ggplot(AUX01, 
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
  labs(title = "Incidência de Anomalias Prioritárias no Estado do Paraná",
       subtitle = "Período: 2021-2025",
       x = NULL,
       y = "Anomalias/1.000 Nascidos Vivos",
       caption = Fonte
  ) +
  Theme()

#### Gráfico ANomalias PR 2016 - 2020

AUX <- colSums(PR_PEVASPEA_SINASC_Serie_Historica[c(1, 2, 3, 4, 5), 3:13], na.rm = TRUE)

AUX01 <- data.frame( Evento = c("Anomalia Detectada", "Anomalias Prioritárias", "Tubo Neural", 
                                "Microcefalia", "Cardiopatias", "Fendas Orais", 
                                "Geniturinárias", "Membros", "Parede Abdominal", "Síndrome de Down"),
                     Absoluto = AUX[2:11], 
                     Nascidos = AUX[1] )

AUX01 <- AUX01 %>%
  mutate(Taxa = round((Absoluto / Nascidos) * 1000, 2)) %>%
  filter(Evento != "Anomalia Detectada",
         Evento != "Anomalias Prioritárias")

PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_16_20 <- ggplot(AUX01, 
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
       subtitle = "Período: 2018-2020",
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
  scale_y_continuous(limits = c(2, 18), 
                     breaks = seq(0, 16, 2)) +
  scale_x_continuous(breaks = 2016:2025) +
  labs(caption = Fonte, 
       y = "Anomalias/1.000 Nascimentos",
       x = NULL,
       title = "Série Histórica 22ª Regional de Saúde - Ivaiporã",
       subtitle = "Anomalias Congênitas (2016-2025).") +
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
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(2, 18), 
                     breaks = seq(0, 16, 2)) +
  scale_x_continuous(breaks = 2016:2025) +
  labs(title = "Série Histórica Paraná - Anomalias Congênitas",
       y = "Taxa por 1.000 nascimentos", 
       x = NULL) +
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
  geom_text(aes(label = round(Taxa_1000, 2)), 
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(2, 18), 
                     breaks = seq(0, 16, 2)) +
  scale_x_discrete(breaks = 2016:2025) +
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
  geom_text(aes(label = round(Taxa_1000, 2)), 
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(2, 18), 
                     breaks = seq(0, 16, 2)) +
  scale_x_discrete(breaks = 2016:2025) +
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
  geom_text(aes(label = round(Taxa_1000, 2)), 
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(2, 18), 
                     breaks = seq(0, 16, 2)) +
  scale_x_discrete(breaks = 2016:2025) +
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
  geom_text(aes(label = round(Taxa_1000, 2)), 
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(2, 18), 
                     breaks = seq(0, 16, 2)) +
  scale_x_discrete(breaks = 2016:2025) +
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
  geom_text(aes(label = round(Taxa_1000, 2)), 
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(2, 18), 
                     breaks = seq(0, 16, 2)) +
  scale_x_discrete(breaks = 2016:2025) +
  Theme()

AUX_LIST <- list(PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_I,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_II,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_III,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_IV,
                 PR_SINASC_GRAF_SERIE_HIST_ANOMA_6RS_V)

PR_PEVASPEA_SINASC_GRAF_Serie_Hist_05_RS <- wrap_plots(AUX_LIST, 
                                                       ncol = 1)  + 
  plot_annotation(
    title = 'Série Histórica das Regionais de Saúde com Maior Incidência de Anomalias em 2025',
    subtitle = 'Taxa de Anomalias Congênitas por 1.000 nascidos vivos (2016-2025)',
    caption =  Fonte,
    theme = theme(
      plot.title = element_text(size = 18, 
                                face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(hjust = 0, 
                                  face = "italic", 
                                  size = 10)
    )
  )

ggsave(filename = "Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_Serie_Hist_05_RS.png",
       plot = PR_PEVASPEA_SINASC_GRAF_Serie_Hist_05_RS,
       width = 29,
       height = 37,
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
      labs(x = NULL,
           y = "Anomalias/
1000 Nascimentos",
           title = titulo
      ) +
      scale_y_continuous(limits = c(0, max_global), 
                         expand = expansion(mult = c(0, 0.1))) +
      Theme_Mun()
  })

RS_PEVASPEA_SINASC_GRAF_Taxa_Mun <- wrap_plots(AUX_LIST, ncol = 2) + 
  plot_annotation(
    title = 'Série Histórica Municípios da 22ª Regional de Saúde',
    subtitle = 'Taxa de Anomalias Congênitas por 1.000 nascidos vivos (2016-2025)',
    caption =  Fonte,
    theme = theme(
      plot.title = element_text(size = 18, 
                                face = "bold"),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(hjust = 0, 
                                  face = "italic", 
                                  size = 10)
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
                                           breaks = c(-1, 0.00001, 3.0, 6.5, 10.0, 14.0, 20.0, 40.0, Inf),
                                           labels = c("0", 
                                                      ">0 a 3,0", 
                                                      ">3,0 a 6,5 (Mediana)", 
                                                      ">6,5 a 10,0 (Média)", 
                                                      ">10,0 a 14,0", 
                                                      ">14,0 a 20,0", 
                                                      ">20,0 a 40,0", 
                                                      ">40,0 (Outliers)"),
                                           right = TRUE))

PR_SINASC_MAP_TAXA_2025_ANOMAL <- ggplot() + 
  geom_sf(data = MAPA_BASE_PR, 
          color = "black", 
          aes(fill = Cat)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         which_north = "true") +
  scale_fill_viridis_d(option = "inferno", 
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
  mutate(TAXA_4a_16_20 = ((PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2020 + 
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2019 +
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2018 +
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2017 +
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2016)/
                            (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2020 + 
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2019 +
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2018 +
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2017 +
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2016) *
                            1000), 
         Nascidos_4a_16_20 = (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2020 + 
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2019 +
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2018 +
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2017 +
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2016),
         Anomalias_4a_16_20 = (PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2020 + 
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2019 +
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2018 +
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2017 +
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2016)
  )

AUX <- AUX[-nrow(AUX),]

MAPA_BASE_PR <- left_join(MAPA_BASE, 
                          AUX %>% 
                            select(Município_sem_Código, contains("4a_16_20")),
                          by = c("NM_MUN" = "Município_sem_Código"))

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = TAXA_4a_16_20,
                                           breaks = c(-Inf, 0.00001, 3.50, 5.50, 7.00, 9.50, 14.00, 20.00, Inf),
                                           labels = c("0", 
                                                      ">0 a 3,5", 
                                                      ">3,5 a 5,5", 
                                                      ">5,5 a 7,0", 
                                                      ">7,0 a 9,5", 
                                                      ">9,5 a 14,0", 
                                                      ">14,0 a 20,0", 
                                                      ">20,0"),
                                           right = FALSE
))

PR_SINASC_MAP_TAXA_4A_ANOMAL_16_20 <- ggplot() + 
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
       title = "2016 - 2020")  +
  Theme() 

############  Taxa 04 anos (21 - 25) municípios estado

AUX <- PR_PEVASPEA_SINASC_Serie_historica_Mun[, 1]

AUX <- PR_PEVASPEA_SINASC_Serie_historica_Mun %>%
  mutate(TAXA_4a_21_25 = ((PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2025 + 
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2024 +
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2023 +
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2022 +
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2021)/
                            (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2025 + 
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2024 +
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2023 +
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2022 +
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2021) *
                            1000), 
         Nascidos_4a_21_25 = (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2025 + 
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2024 +
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2023 +
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2022 +
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2021),
         Anomalias_4a_21_25 = (PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2025 + 
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2024 +
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2023 +
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2022 +
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2021)
  )

AUX <- AUX[-nrow(AUX),]

MAPA_BASE_PR <- left_join(MAPA_BASE_PR, 
                          AUX %>% 
                            select(Município_sem_Código, contains("4a_21_25")),
                          by = c("NM_MUN" = "Município_sem_Código"))

MAPA_BASE_PR$Cat <- with(MAPA_BASE_PR, cut(x = TAXA_4a_21_25,
                                           breaks = c(-Inf, 0.00001, 3.50, 5.50, 7.00, 9.50, 14.00, 20.00, Inf),
                                           labels = c("0", 
                                                      ">0 a 3,5", 
                                                      ">3,5 a 5,5", 
                                                      ">5,5 a 7,0", 
                                                      ">7,0 a 9,5", 
                                                      ">9,5 a 14,0", 
                                                      ">14,0 a 20,0", 
                                                      ">20,0"),
                                           right = FALSE
))

PR_SINASC_MAP_TAXA_4A_ANOMAL_21_25 <- ggplot() + 
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

PR_SINASC_MAP_TAXA_4A_ANOMAL_16_20_21_25 <- PR_SINASC_MAP_TAXA_4A_ANOMAL_16_20 + 
  PR_SINASC_MAP_TAXA_4A_ANOMAL_21_25 + 
  plot_layout(ncol = 2, 
              guides = "collect") + 
  plot_annotation(title = 'Evolução Espacial da Taxa de Anomalias Congênitas em Municípios do Paraná',
                  subtitle = 'Comparativo entre os quinquênios 2016-2020 e 2021-2025 (Casos/1000 Nascimentos)',
                  caption =  Fonte,
                  theme = theme(
                    plot.title = element_text(size = 20, 
                                              face = "bold"),
                    plot.subtitle = element_text(size = 14),
                    legend.position = "bottom",
                    plot.caption = element_text(hjust = 0, 
                                                face = "italic", 
                                                size = 10)
                  ))

##### Trabalhando Global Moran/LISA

### Criando Matriz de vizinhança (QUEEN) Verificar se o queen é padrão no spdep

Matriz_Viz_MAPA_BASE_PR <- poly2nb(MAPA_BASE_PR, ## Definindo quem faz fronteira com quem.
                                   queen = TRUE)

### Verificando a malha queen

Centroides_Matriz_VIZ <- st_centroid(MAPA_BASE_PR)  ## Estabelecendo centróides

Centroides_Matriz_VIZ_coordenadas <- st_coordinates(Centroides_Matriz_VIZ) ## Coordenadas dos centróides

Matriz_VIZ_Linhas <- nb2lines(nb = Matriz_Viz_MAPA_BASE_PR,  ## Une os centróides com lines
                              coords = Centroides_Matriz_VIZ_coordenadas) %>%
  st_as_sf()  ## transforma tudo em objeto sf

st_crs(Matriz_VIZ_Linhas) <- st_crs(MAPA_BASE_PR) ## atribui Sistema de referência de coordenadas do MAPA_BASE_PR no matriz VIZ Linhas

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
  nb2listw(style = "W")  ##define matriz de pesos espaciais para rodar moran e local moran

#####  Análise Global e Local Moran

#### 2016 a 2020 INÍCIO

### Realizando a suavização dos dados com Método Bayesiano Empírico 2016 - 2020

Taxa_Suavizada <- EBlocal(ri = MAPA_BASE_PR$Anomalias_4a_16_20,
                          ni = MAPA_BASE_PR$Nascidos_4a_16_20,
                          nb = Matriz_Viz_MAPA_BASE_PR)

MAPA_BASE_PR$TAXA_RAW_16_20 <- Taxa_Suavizada$raw * 1000

MAPA_BASE_PR$TAXA_EST_16_20 <- Taxa_Suavizada$est * 1000

Global_Moran_16_20 <- moran.test(MAPA_BASE_PR$TAXA_EST_16_20,
                                 Matriz_Viz_Pesos)

### Scatterplot

moran.plot(MAPA_BASE_PR$TAXA_EST_16_20, 
           Matriz_Viz_Pesos, 
           labels = FALSE, 
           pch = 15, 
           col = "blue", 
           xlab = "Variável Original", 
           ylab = "Média dos Vizinhos (Spatial Lag)",
           main = "Moran Scatterplot")

#### Calculando o Local Moran
### Travando o local moran
set.seed(1)

Lisa <- localmoran_perm(MAPA_BASE_PR$TAXA_EST_16_20, 
                        Matriz_Viz_Pesos, 
                        nsim = 9999,
                        zero.policy = TRUE)

MAPA_BASE_PR$local_I_16_20 <- Lisa[,1]

MAPA_BASE_PR$local_I_p_valor_16_20 <- Lisa[,5]

ggplot(MAPA_BASE_PR, 
       aes(geometry = geometry)) +
  geom_sf(aes(fill = local_I_16_20)) +
  scale_fill_gradient2(low = "blue", high = "red", 
                       mid = "white", 
                       midpoint = 0,
                       name = "I local") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))

quadrantes <- attr(Lisa, 
                   "quadr")$mean

MAPA_BASE_PR$quadrante_16_20 <- case_when(quadrantes == "High-High" ~ "Alto-Alto",
                                          quadrantes == "Low-Low" ~ "Baixo-Baixo",
                                          quadrantes == "High-Low" ~ "Alto-Baixo",
                                          quadrantes == "Low-High" ~ "Baixo-Alto")

MAPA_BASE_PR <- MAPA_BASE_PR %>%
  mutate(Lisa_resultado_16_20 = case_when(
    local_I_p_valor_16_20 < 0.05 ~ quadrante_16_20,
    local_I_p_valor_16_20 >= 0.05 ~ "Não significativo"
  ))

Niveis_LISA <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não significativo")

MAPA_BASE_PR$Lisa_resultado_16_20 <- factor(MAPA_BASE_PR$Lisa_resultado_16_20, 
                                            levels = Niveis_LISA)

PR_PEVASPEA_SINASC_LOCAL_MORAN_16_20 <- ggplot(MAPA_BASE_PR, 
                                               aes(geometry = geometry)) +
  geom_sf(color = "grey30", 
          linewidth = 0.1, 
          aes(fill = Lisa_resultado_16_20)) +
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
       title = "2016 - 2020",
       subtitle = "Taxa suavizada utilizando Método Bayesiano Empírico \nGlobal Moran I = 0.63 (p < 0.001) \n9999 permutações") +
  Theme() +
  theme(legend.key.width = unit(1.5, "cm")) +
  theme(legend.position = "bottom")

#### Fim da análise Global and Local Moran 2018 - 2021

### Realizando a suavização dos dados com Método Bayesiano Empírico 2022 - 2025

Taxa_Suavizada <- EBlocal(ri = MAPA_BASE_PR$Anomalias_4a_21_25, 
                          ni = MAPA_BASE_PR$Nascidos_4a_21_25,
                          nb = Matriz_Viz_MAPA_BASE_PR)

MAPA_BASE_PR$TAXA_RAW_21_25 <- Taxa_Suavizada$raw * 1000

MAPA_BASE_PR$TAXA_EST_21_25 <- Taxa_Suavizada$est * 1000

Global_Moran <- moran.test(MAPA_BASE_PR$TAXA_EST_21_25,
                           Matriz_Viz_Pesos)

### Scatterplot

moran.plot(MAPA_BASE_PR$TAXA_EST_21_25, 
           Matriz_Viz_Pesos, 
           labels = FALSE, 
           pch = 15, 
           col = "blue", 
           xlab = "Variável Original", 
           ylab = "Média dos Vizinhos (Spatial Lag)",
           main = "Moran Scatterplot")

#### Calculando o Local Moran
#### Travando o local moran
set.seed(5)

Lisa <- localmoran_perm(MAPA_BASE_PR$TAXA_EST_21_25, 
                        Matriz_Viz_Pesos, 
                        nsim = 9999,
                        zero.policy = TRUE)

MAPA_BASE_PR$local_I_21_25 <- Lisa[,1]

MAPA_BASE_PR$local_I_p_valor_21_25 <- Lisa[,5]

quadrantes <- attr(Lisa, 
                   "quadr")$mean

MAPA_BASE_PR$quadrante_21_25 <- case_when(quadrantes == "High-High" ~ "Alto-Alto",
                                          quadrantes == "Low-Low" ~ "Baixo-Baixo",
                                          quadrantes == "High-Low" ~ "Alto-Baixo",
                                          quadrantes == "Low-High" ~ "Baixo-Alto")

MAPA_BASE_PR <- MAPA_BASE_PR %>%
  mutate(Lisa_resultado_21_25 = case_when(
    local_I_p_valor_21_25 < 0.05 ~ quadrante_21_25,
    local_I_p_valor_21_25 >= 0.05 ~ "Não significativo"
  ))

MAPA_BASE_PR$Lisa_resultado_21_25 <- factor(MAPA_BASE_PR$Lisa_resultado_21_25, 
                                            levels = Niveis_LISA)

PR_PEVASPEA_SINASC_LOCAL_MORAN_21_25 <- ggplot(MAPA_BASE_PR, 
                                               aes(geometry = geometry)) +
  geom_sf(color = "grey30", 
          linewidth = 0.1, 
          aes(fill = Lisa_resultado_21_25)) +
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
       title = "2021 - 2025",
       subtitle = "Taxa suavizada utilizando Método Bayesiano Empírico \nGlobal Moran I = 0.580 (p < 0.001) \n9999 Permutações") +
  Theme() +
  theme(legend.key.width = unit(1.5, "cm")) +
  theme(legend.position = "bottom")

PR_PEVASPEA_SINASC_LOCAL_MORAN_16_20_21_25 <- PR_PEVASPEA_SINASC_LOCAL_MORAN_16_20 + 
  PR_PEVASPEA_SINASC_LOCAL_MORAN_21_25 + 
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

#############   Taxa Anomalias/1000 nascimentos 18 - 21 somados
#############   por Regionais de Saúde

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica[, 1]

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica %>%
  mutate(TAXA_4a_16_20 = ((PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2020 + 
                             PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2019 +
                             PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2018 +
                             PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2017 +
                             PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2016)/
                            (PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2020 + 
                               PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2019 +
                               PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2018 +
                               PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2017 +
                               PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2016) *
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

MAPA_BASE_RS <- MAPA_BASE_RS %>%
  select(RS, contains("16_20"), "geometry")

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = TAXA_4a_16_20,
                                           breaks = c(-Inf, 4.50, 5.50, 6.50, 7.30, 8.20, 9.20, 10.50, Inf),
                                           labels = c("Até 4,5", 
                                                      "4,5 - 5,5", 
                                                      "5,5 - 6,5", 
                                                      "6,5 - 7,3", 
                                                      "7,3 - 8,2", 
                                                      "8,2 - 9,2", 
                                                      "9,2 - 10,5", 
                                                      "Acima de 10,5"),
                                           right = FALSE))

MAPA_BASE_RS <- st_as_sf(MAPA_BASE_RS)

PR_SINASC_MAP_TAXA_4_ANOS_16_20_ANOMAL_RS <- ggplot() +
  geom_sf(data = MAPA_BASE_RS, 
          color = "grey30", 
          linewidth = 0.5, 
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
       title = "2016 - 2020")  +
  Theme() 

#############   Taxa Anomalias/1000 nascimentos nos últimos 04 anos somados
#############   por Regionais de Saúde

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica[, 1]

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica %>%
  mutate(TAXA_4a_21_25 = ((PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2025 + 
                             PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2024 +
                             PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2023 +
                             PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2022 +
                             PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2021)/
                            (PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2025 + 
                               PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2024 +
                               PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2023 +
                               PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2022 +
                               PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2021) *
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

MAPA_BASE_RS <- MAPA_BASE_RS %>%
  select(RS, contains("4a_21_25"), geometry)

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = TAXA_4a_21_25,
                                           breaks = c(-Inf, 4.50, 5.50, 6.50, 7.30, 8.20, 9.20, 10.50, Inf),
                                           labels = c("Até 4,5", 
                                                      "4,5 - 5,5", 
                                                      "5,5 - 6,5", 
                                                      "6,5 - 7,3", 
                                                      "7,3 - 8,2", 
                                                      "8,2 - 9,2", 
                                                      "9,2 - 10,5", 
                                                      "Acima de 10,5"),
                                           right = FALSE))

MAPA_BASE_RS <- st_as_sf(MAPA_BASE_RS)

PR_SINASC_MAP_TAXA_4_ANOS_21_25_ANOMAL_RS <- ggplot() +
  geom_sf(data = MAPA_BASE_RS, 
          color = "grey30", 
          linewidth = 0.5, 
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
       title = "2021 - 2025")  +
  Theme() 

PR_SINASC_MAP_TAXA_4_ANOS_16_20_21_25_ANOMAL_RS <- PR_SINASC_MAP_TAXA_4_ANOS_16_20_ANOMAL_RS + 
  PR_SINASC_MAP_TAXA_4_ANOS_21_25_ANOMAL_RS+ 
  plot_layout(ncol = 2, 
              guides = "collect") + 
  plot_annotation(title = 'Evolução Espacial da Taxa de Anomalias Congênitas em Regionais do Paraná',
                  subtitle = 'Comparativo entre os quadriênios 2016-2020 e 2021-2025 (Casos/1000 Nascimentos)',
                  caption =  Fonte,
                  theme = theme(
                    plot.title = element_text(size = 20, 
                                              face = "bold"),
                    plot.subtitle = element_text(size = 14),
                    legend.position = "bottom",
                    plot.caption = element_text(hjust = 0,
                                                face = "italic", 
                                                size = 10)
                  ))

#### Mapa regional com taxa de 2025

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica[, 1]

AUX <- PR_PEVASPEA_SINASC_RS_Serie_Historica %>%
  mutate(TAXA_2025 = ((PR_PEVASPEA_SINASC_RS_Serie_Historica$Anomalias_2025)/
                        (PR_PEVASPEA_SINASC_RS_Serie_Historica$Nascidos_2025 ) *
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

MAPA_BASE_RS <- MAPA_BASE_RS %>%
  select(RS, TAXA_2025, "geometry")

MAPA_BASE_RS$Cat <- with(MAPA_BASE_RS, cut(x = TAXA_2025,
                                           breaks = c(-Inf, 4.50, 6.00, 7.30, 8.20, 9.20, 10.50, 13.00, Inf),
                                           labels = c("Até 4,5", 
                                                      "4,5 - 6,0", 
                                                      "6,0 - 7,3", 
                                                      "7,3 - 8,2", 
                                                      "8,2 - 9,2", 
                                                      "9,2 - 10,5", 
                                                      "10,5 - 13,0",
                                                      "Acima de 13,0"),
                                           right = FALSE))

MAPA_BASE_RS <- st_as_sf(MAPA_BASE_RS)

PR_SINASC_MAP_TAXA_2025_ANOMAL_RS <- ggplot() +
  geom_sf(data = MAPA_BASE_RS, 
          color = "grey30", 
          linewidth = 0.5, 
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
       title = "2025")  +
  Theme() 
######  Tabelas 
################################################################################
################################################################################

#### Tabela ANomalias Regionais 2016 - 2020
# 
AUX2016 <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2016.csv",
                     header = TRUE,
                     sep = ",")

AUX2017 <- read.csv (file = "Tabulacoes_R/SINASC/PR_PEVASPEA_SINASC_2017.csv",
                     header = TRUE,
                     sep = ",")

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

AUX <- bind_rows(AUX2016 %>% mutate(Ano = 2016),
                 AUX2017 %>% mutate(Ano = 2017),
                 AUX2018 %>% mutate(Ano = 2018),
                 AUX2019 %>% mutate(Ano = 2019),
                 AUX2020 %>% mutate(Ano = 2020)) %>%
  filter(!RS == "Total")

AUX01 <- AUX %>%
  group_by(RS) %>%
  summarise(across(4:14, \(x) sum(x, na.rm = TRUE)))

AUX01 <- as.data.frame(AUX01)

AUX01[(nrow(AUX01) +1), 2:12] <- apply(AUX01[, 2:12], 2, sum)

AUX01[23,1] <- "Estado"

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

PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_16_20 <- gt(AUX[, c(1, 2, 7:22)]) %>%
  tab_header(
    title = md("**Incidência de Anomalias Congênitas Prioritárias por Regional de Saúde**"),
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
    contains("(%)") ~ "Inc."
  ) %>%
  tab_footnote(
    footnote = "Incidência calculada por 1.000 nascidos vivos (NV).",
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

AUX <- bind_rows(AUX2016 %>% mutate(Ano = 2016),
                 AUX2017 %>% mutate(Ano = 2017),
                 AUX2018 %>% mutate(Ano = 2018),
                 AUX2019 %>% mutate(Ano = 2019),
                 AUX2020 %>% mutate(Ano = 2020)) %>%
  filter(!RS == "Total")

colnames(AUX)[c(5:15)] <- c("Nascidos", "Anomalias", "Anom. Prioritárias", 
                            "Tubo Neural", "Microcefalia", "Cardiopatias", 
                            "Fendas Orais", "Urinárias", "Membros", "Parede Abdominal",
                            "Sind. de Down")

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
                   subtitle = 'Taxa por 1.000 nascidos vivos por município de residência (2016-2020)',
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

AUX <- bind_rows(AUX2021 %>% mutate(Ano = 2021),
                 AUX2022 %>% mutate(Ano = 2022),
                 AUX2023 %>% mutate(Ano = 2023),
                 AUX2024 %>% mutate(Ano = 2024),
                 AUX2025 %>% mutate(Ano = 2025))  %>%
  filter(!RS == "Total")

AUX01 <- AUX %>%
  group_by(RS) %>%
  summarise(across(4:14, \(x) sum(x, na.rm = TRUE)))

AUX01 <- as.data.frame(AUX01)

AUX01[(nrow(AUX01) +1), 2:12] <- apply(AUX01[, 2:12], 2, sum)

AUX01[23,1] <- "Estado"

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

PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_21_25 <- gt(AUX[, c(1, 2, 7:22)]) %>%
  tab_header(
    title = md("**Incidência de Anomalias Congênitas Prioritárias por Regional de Saúde**"),
    subtitle = md("Paraná, 2021 – 2025")
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
    contains("(%)") ~ "Inc."
  ) %>%
  tab_footnote(
    footnote = "Incidência calculada por 1.000 nascidos vivos (NV).",
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

AUX <- bind_rows(AUX2021 %>% mutate(Ano = 2021),
                 AUX2022 %>% mutate(Ano = 2022),
                 AUX2023 %>% mutate(Ano = 2023),
                 AUX2024 %>% mutate(Ano = 2024),
                 AUX2025 %>% mutate(Ano = 2025)) %>%
  filter(!RS == "Total")

colnames(AUX)[c(5:15)] <- c("Nascidos", "Anomalias", "Anom. Prioritárias", 
                            "Tubo Neural", "Microcefalia", "Cardiopatias", 
                            "Fendas Orais", "Urinárias", "Membros", "Parede Abdominal",
                            "Sind. de Down")


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
                   subtitle = 'Taxa por 1.000 nascidos vivos por município de residência (2021-2025)',
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
       height = 35.7,
       units = "cm",
       dpi = 300) 


ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Nasc.png",
       PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Nasc,
       width = 25,          
       height = 15,          
       units = "cm",
       dpi = 300,            
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_Serie_Temp_Anomal.png",
       PR_PEVASPEA_SINASC_GRAF_Serie_Temp_ANOMAL,
       width = 25,          
       height = 15,          
       units = "cm",
       dpi = 300,            
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/RS_SINASC_GRAF_SERIE_HIST_ANOMAL.png",
       RS_SINASC_GRAF_SERIE_HIST_ANOMAL,
       width = 25,
       height = 15,
       units = "cm",
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_SINASC_GRAF_SERIE_HIST_ANOMAL.png",
       PR_SINASC_GRAF_SERIE_HIST_ANOMAL,
       width = 25,
       height = 15,
       units = "cm",
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_16_20.png",
       PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_16_20,
       width = 25, 
       height = 15, 
       units = "cm", 
       dpi = 300,
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/SINASC/PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_21_25.png",
       PR_PEVASPEA_SINASC_GRAF_PRIORITARIAS_PR_21_25,
       width = 25, 
       height = 15, 
       units = "cm", 
       dpi = 300,
       bg = "white")

ggsave(filename = "Imagens/SINASC/RS_PEVASPEA_SINASC_GRAF_Taxa_Mun.png",
       plot = RS_PEVASPEA_SINASC_GRAF_Taxa_Mun,
       width = 38,
       height = 50.7,
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

ggsave(filename = "Imagens/SINASC/PR_PEVASPEA_SINASC_LOCAL_MORAN_16_20_21_25.png", 
       plot = PR_PEVASPEA_SINASC_LOCAL_MORAN_16_20_21_25, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/SINASC/PR_SINASC_MAP_TAXA_4A_ANOMAL_16_20_21_25.png", 
       plot = PR_SINASC_MAP_TAXA_4A_ANOMAL_16_20_21_25, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/SINASC/PR_SINASC_MAP_TAXA_4_ANOS_16_20_21_25_ANOMAL_RS.png",
       plot = PR_SINASC_MAP_TAXA_4_ANOS_16_20_21_25_ANOMAL_RS,
       width = 35,        
       height = 18,       
       units = "cm",
       dpi = 300,         
       bg = "white"
)

#########  Tabelas

gtsave(data = PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_16_20,
       filename = "Imagens/SINASC/PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_16_20.pdf")

gtsave(data = PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_21_25,
       filename = "Imagens/SINASC/PR_PEVASPEA_SINASC_TAB_PRIORITARIAS_RS_21_25.pdf")

##################  SIM  

PR_PEVASPEA_SIM_Congenitas_Geral_Serie_Hist <- read.csv (file = "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Congenitas_Geral_Serie_Hist.csv",
                                                         header = TRUE,
                                                         sep = ",")

RS_PEVASPEA_SIM_Congenitas_Geral_Serie_Hist <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_Congenitas_Geral_Serie_Hist.csv",
                                                         header = TRUE,
                                                         sep = ",")

RS_PEVASPEA_SIM_Prioritaria_Fetal_Serie_Hist <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_Prioritaria_Fetal_Serie_Hist.csv",
                                                          header = TRUE,
                                                          sep = ",")

PR_PEVASPEA_SIM_Prioritarias_Fetal_Serie_Hist <- read.csv (file = "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_Prioritarias_Fetal_Serie_Hist.csv",
                                                           header = TRUE,
                                                           sep = ",")

PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016 <- read.csv (file = "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016.csv",
                                                  header = TRUE,
                                                  sep = ",")

PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017 <- read.csv (file = "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017.csv",
                                                  header = TRUE,
                                                  sep = ",")

PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018 <- read.csv (file = "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018.csv",
                                                  header = TRUE,
                                                  sep = ",")

PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019 <- read.csv (file = "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019.csv",
                                                  header = TRUE,
                                                  sep = ",")

PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020 <- read.csv (file = "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020.csv",
                                                  header = TRUE,
                                                  sep = ",")

PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021 <- read.csv (file = "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021.csv",
                                                  header = TRUE,
                                                  sep = ",")

PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022 <- read.csv (file = "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022.csv",
                                                  header = TRUE,
                                                  sep = ",")

PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023 <- read.csv (file = "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023.csv",
                                                  header = TRUE,
                                                  sep = ",")

PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024 <- read.csv (file = "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024.csv",
                                                  header = TRUE,
                                                  sep = ",")

PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025 <- read.csv (file = "Tabulacoes_R/SIM/PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025.csv",
                                                  header = TRUE,
                                                  sep = ",")



RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016 <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016.csv",
                                                  header = TRUE,
                                                  sep = ",")

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017 <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017.csv",
                                                  header = TRUE,
                                                  sep = ",")

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018 <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018.csv",
                                                  header = TRUE,
                                                  sep = ",")

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019 <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019.csv",
                                                  header = TRUE,
                                                  sep = ",")

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020 <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020.csv",
                                                  header = TRUE,
                                                  sep = ",")

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021 <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021.csv",
                                                  header = TRUE,
                                                  sep = ",")

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022 <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022.csv",
                                                  header = TRUE,
                                                  sep = ",")

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023 <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023.csv",
                                                  header = TRUE,
                                                  sep = ",")

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024 <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024.csv",
                                                  header = TRUE,
                                                  sep = ",")

RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025 <- read.csv (file = "Tabulacoes_R/SIM/RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025.csv",
                                                  header = TRUE,
                                                  sep = ",")

PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2016 <- read.csv (file = "Tabulacoes_R/SIM/PR_PEVASPEA_SIM_NEOPLASIA_GERAL_2016.csv",
                                                  header = TRUE,
                                                  sep = ",")

AUX01 <- Base_Populacional %>%
  filter(MUNICÍPIO.ESTADO == "ESTADO DO PARANÁ")

AUX <- PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016 %>%
  select(Grupo_CID, TOTAL) %>%
  rename(`2016` = TOTAL) %>%
  mutate(`2016` = as.numeric(`2016`),
         pop_limpa = as.numeric(gsub("\\.", "", AUX01$X2016)),
         Inc_2016 = case_when(
           Grupo_CID == "Mama"    ~ (`2016` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2016` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2016` / as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022[1, 2]))) * 100000,
           TRUE                   ~ (`2016` / pop_limpa) * 100000),
         Inc_2016 = round(Inc_2016, 2)
  ) %>% 
  select(-pop_limpa)  

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2017` = TOTAL),
  by = "Grupo_CID" ) 

AUX <- AUX %>% 
  ungroup() %>%
  mutate(`2017` = as.numeric(`2017`),
         pop_limpa = as.numeric(gsub("\\.", "", AUX01$X2017)),
         Inc_2017 = case_when(
           Grupo_CID == "Mama"    ~ (`2017` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2017` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2017` / as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022[1, 2]))) * 100000,
           TRUE                   ~ (`2017` / pop_limpa) * 100000),
         Inc_2017 = round(Inc_2017, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2018` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  ungroup() %>%
  mutate(`2018` = as.numeric(`2018`),
         pop_limpa = as.numeric(gsub("\\.", "", AUX01$X2018)),
         Inc_2018 = case_when(
           Grupo_CID == "Mama"    ~ (`2018` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2018` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2018` / as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022[1, 2]))) * 100000,
           TRUE                   ~ (`2018` / pop_limpa) * 100000),
         Inc_2018 = round(Inc_2018, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2019` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  ungroup() %>%
  mutate(`2019` = as.numeric(`2019`),
         pop_limpa = as.numeric(gsub("\\.", "", AUX01$X2019)),
         Inc_2019 = case_when(
           Grupo_CID == "Mama"    ~ (`2019` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2019` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2019` / as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022[1, 2]))) * 100000,
           TRUE                   ~ (`2019` / pop_limpa) * 100000),
         Inc_2019 = round(Inc_2019, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2020` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  ungroup() %>%
  mutate(`2020` = as.numeric(`2020`),
         pop_limpa = as.numeric(gsub("\\.", "", AUX01$X2020)),
         Inc_2020 = case_when(
           Grupo_CID == "Mama"    ~ (`2020` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2020` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2020` / as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022[1, 2]))) * 100000,
           TRUE                   ~ (`2020` / pop_limpa) * 100000),
         Inc_2020 = round(Inc_2020, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2021` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  ungroup() %>%
  mutate(`2021` = as.numeric(`2021`),
         pop_limpa = as.numeric(gsub("\\.", "", AUX01$X2021)),
         Inc_2021 = case_when(
           Grupo_CID == "Mama"    ~ (`2021` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2021` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2021` / as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022[1, 2]))) * 100000,
           TRUE                   ~ (`2021` / pop_limpa) * 100000),
         Inc_2021 = round(Inc_2021, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2022` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  ungroup() %>%
  mutate(`2022` = as.numeric(`2022`),
         pop_limpa = as.numeric(gsub("\\.", "", AUX01$X2021)),
         Inc_2022 = case_when(
           Grupo_CID == "Mama"    ~ (`2022` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2022` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2022` / as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022[1, 2]))) * 100000,
           TRUE                   ~ (`2022` / pop_limpa) * 100000),
         Inc_2022 = round(Inc_2022, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2023` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  ungroup() %>%
  mutate(`2023` = as.numeric(`2023`),
         pop_limpa = as.numeric(gsub("\\.", "", AUX01$X2021)),
         Inc_2023 = case_when(
           Grupo_CID == "Mama"    ~ (`2023` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2023` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2023` / as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022[1, 2]))) * 100000,
           TRUE                   ~ (`2023` / pop_limpa) * 100000),
         Inc_2023 = round(Inc_2023, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2024` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  ungroup() %>%
  mutate(`2024` = as.numeric(`2024`),
         pop_limpa = as.numeric(gsub("\\.", "", AUX01$X2024)),
         Inc_2024 = case_when(
           Grupo_CID == "Mama"    ~ (`2024` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2024` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2024` / as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022[1, 2]))) * 100000,
           TRUE                   ~ (`2024` / pop_limpa) * 100000),
         Inc_2024 = round(Inc_2024, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2025` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  ungroup() %>%
  mutate(`2025` = as.numeric(`2025`),
         pop_limpa = as.numeric(gsub("\\.", "", AUX01$X2025)),
         Inc_2025 = case_when(
           Grupo_CID == "Mama"    ~ (`2025` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2025` / as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022[1, 2]))) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2025` / as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022[1, 2]))) * 100000,
           TRUE                   ~ (`2025` / pop_limpa) * 100000),
         Inc_2025 = round(Inc_2025, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- AUX %>%
  mutate(Grupo_CID = case_when(
    Grupo_CID == "Cerebro_SNC" ~ "Cérebro e SNC",
    Grupo_CID == "Linfatico_Hematologico" ~ "Linfático e Hematológico",
    Grupo_CID == "Digestivo" ~ "Aparelho Digestivo",
    Grupo_CID == "Respiratorio" ~ "Aparelho Respiratório",
    Grupo_CID == "Gen_Fem" ~ "Órgãos Genitais Femininos",
    Grupo_CID == "Gen_Masc" ~ "Órgãos Genitais Masculinos",
    Grupo_CID == "Mama" ~ "Mama",
    Grupo_CID == "Via_Urinaria" ~ "Vias Urinárias",
    Grupo_CID == "Pele" ~ "Pele (Melanoma/Outros)",
    Grupo_CID == "Labio_Oral" ~ "Lábio e Cavidade Oral",
    Grupo_CID == "Ossos" ~ "Ossos e Cartilagens",
    Grupo_CID == "Tec_Mole" ~ "Tecidos Moles",
    Grupo_CID == "Tireoide_Endo" ~ "Tireoide e Endócrinas",
    Grupo_CID == "Mal_Definidas" ~ "Mal Definidas/Outras",
    TRUE ~ Grupo_CID 
  ))

PR_PEVASPEA_SIM_TAB_NEOPLASIAS_GRUPOS <- gt(AUX) %>%
  tab_header(
    title = md("**Incidência de Neoplasias Malignas por Localização**"),
    subtitle = md("Paraná, 2016 – 2025")
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
  tab_spanner(label = "2016",
              columns = c(2:3),
              id = "1") %>%
  tab_spanner(label = "2017",
              columns = c(4:5),
              id = "2") %>%
  tab_spanner(label = "2018",
              columns = c(6:7),
              id = "3") %>%
  tab_spanner(label = "2019",
              columns = c(8:9),
              id = "4") %>%
  tab_spanner(label = "2020",
              columns = c(10:11),
              id = "5") %>%
  tab_spanner(label = "2021",
              columns = c(12:13),
              id = "6") %>%
  tab_spanner(label = "2022",
              columns = c(14:15),
              id = "7") %>%
  tab_spanner(label = "2023",
              columns = c(16:17),
              id = "8") %>%
  tab_spanner(label = "2024",
              columns = c(18:19),
              id = "9") %>%
  tab_spanner(label = "2025",
              columns = c(20:21),
              id = "10") %>%
  cols_align(align = "left", columns = 1) %>%
  cols_align(align = "center", columns = 2:21) %>%
  cols_label(contains("Inc_")     ~ "Inc.",
             matches("^20\\d{2}$") ~ "n"
  ) %>%
  fmt_number(
    columns = contains("Inc_"),
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  tab_footnote(
    footnote = "Fonte: Sistema de Informações de Mortalidade. Base DBF acessada em 04/05/2026."
  ) %>%
  tab_footnote(
    footnote = "Nota¹:Incidência calculada por 100.000 habitantes (População Estimada IBGE)."
  ) %>%
  tab_footnote(
    footnote = "Nota²: Incidência de câncer de mama e genitais femininos calculada usando a população feminina do Censo de 2022."
  ) %>%
  tab_footnote(
    footnote = "Nota³: Incidência de câncer de genitais masculinos calculada usando a população masculina do Censo de 2022."
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4F4F4"),
    locations = cells_body(columns = c(4:5, 8:9, 12:13, 16:17, 20:21)) 
  ) %>%
  tab_options(footnotes.padding = px(1),
              footnotes.font.size = px(10))

Total <- data.frame(
  Grupo_CID = "TOTAL",
  `2016` = sum(AUX$`2016`, na.rm = TRUE),
  `2017` = sum(AUX$`2017`, na.rm = TRUE),
  `2018` = sum(AUX$`2018`, na.rm = TRUE),
  `2019` = sum(AUX$`2019`, na.rm = TRUE),
  `2020` = sum(AUX$`2020`, na.rm = TRUE),
  `2021` = sum(AUX$`2021`, na.rm = TRUE),
  `2022` = sum(AUX$`2022`, na.rm = TRUE),
  `2023` = sum(AUX$`2023`, na.rm = TRUE),
  `2024` = sum(AUX$`2024`, na.rm = TRUE),
  `2025` = sum(AUX$`2025`, na.rm = TRUE),
  check.names = FALSE 
)

Total$Inc_2016 <- round((Total$`2016` / as.numeric(gsub("\\.", "", AUX01$X2016))) * 100000, 2)
Total$Inc_2017 <- round((Total$`2017` / as.numeric(gsub("\\.", "", AUX01$X2017))) * 100000, 2)
Total$Inc_2018 <- round((Total$`2018` / as.numeric(gsub("\\.", "", AUX01$X2018))) * 100000, 2)
Total$Inc_2019 <- round((Total$`2019` / as.numeric(gsub("\\.", "", AUX01$X2019))) * 100000, 2)
Total$Inc_2020 <- round((Total$`2020` / as.numeric(gsub("\\.", "", AUX01$X2020))) * 100000, 2)
Total$Inc_2021 <- round((Total$`2021` / as.numeric(gsub("\\.", "", AUX01$X2021))) * 100000, 2)
Total$Inc_2022 <- round((Total$`2022` / as.numeric(gsub("\\.", "", AUX01$X2024))) * 100000, 2)
Total$Inc_2023 <- round((Total$`2023` / as.numeric(gsub("\\.", "", AUX01$X2024))) * 100000, 2)
Total$Inc_2024 <- round((Total$`2024` / as.numeric(gsub("\\.", "", AUX01$X2024))) * 100000, 2)
Total$Inc_2025 <- round((Total$`2025` / as.numeric(gsub("\\.", "", AUX01$X2025))) * 100000, 2)

Total_Inc <- Total %>% 
  select(contains("Inc")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    Incidencia = as.numeric(V1)     
  ) %>%
  select(Ano, Incidencia)

Total_N <- Total %>% 
  select(matches("^20(1[6-9]|2[0-9])$")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    N = as.numeric(V1)     
  ) %>%
  select(Ano, N)

Total <- left_join(Total_Inc,
                   Total_N,
                   by = "Ano")

fator_escala <- (max(Total$N, na.rm = TRUE) / 150)

PR_PEVASPEA_SIM_GRAF_NEOPLASIAS_Incidencia <- ggplot(Total, 
                                                     aes(x = Ano, y = Incidencia)) + 
  geom_col(aes(y = N / fator_escala), 
           fill = "#dcdde1", 
           width = 0.6)+
  geom_text(aes(y = N / fator_escala / 2, 
                label = format(N, big.mark = ".")), 
            size = 4, 
            fontface = "bold") +
  geom_line(aes(x = Ano,
                y = Incidencia,
                group = 1),
            colour = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(y = "Casos/100.000 habitantes",
       x = NULL,
       title = "Mortalidade por Câncer/100.000 habitantes no Paraná") +
  geom_text(aes(label = format(round(Incidencia, 2),
                               decimal.mark = ",")), 
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(0, 180), 
                     breaks = seq(0, 180, 20),
                     sec.axis = sec_axis(~ . * fator_escala, 
                                         name = "Casos",
                                         labels = label_number(big.mark = "."))) +
  scale_x_discrete(breaks = 2016:2025) +
  Theme()

##### Regional

AUX01 <- Base_Populacional %>%
  filter(MUNICÍPIO.ESTADO %in% c(
    "ARAPUÃ", "ARIRANHA DO IVAÍ", "CÂNDIDO DE ABREU", "CRUZMALTINA",
    "GODOY MOREIRA", "IVAIPORÃ", "JARDIM ALEGRE", "LIDIANÓPOLIS",
    "LUNARDELLI", "MANOEL RIBAS", "MATO RICO", "NOVA TEBAS",
    "RIO BRANCO DO IVAÍ", "ROSÁRIO DO IVAÍ", "SANTA MARIA DO OESTE", 
    "SÃO JOÃO DO IVAÍ"
  ))

AUX02 <- AUX01 %>%
  summarise(
    across(matches("^X20\\d{2}$"), 
           ~ sum(as.numeric(gsub("\\.", "", .x)), na.rm = TRUE) 
    )
  )

AUX03 <- Base_Populacional_Feminina_2022 %>%
  filter(MUN_ESTADO %in% c(
    "ARAPUÃ", "ARIRANHA DO IVAÍ", "CÂNDIDO DE ABREU", "CRUZMALTINA",
    "GODOY MOREIRA", "IVAIPORÃ", "JARDIM ALEGRE", "LIDIANÓPOLIS",
    "LUNARDELLI", "MANOEL RIBAS", "MATO RICO", "NOVA TEBAS",
    "RIO BRANCO DO IVAÍ", "ROSÁRIO DO IVAÍ", "SANTA MARIA DO OESTE", 
    "SÃO JOÃO DO IVAÍ"
  ))    

AUX03 <- sum(as.numeric(gsub("\\.", "", AUX03$Total)))

AUX04 <- Base_Populacional_Masculina_2022 %>%
  filter(MUN_ESTADO %in% c(
    "ARAPUÃ", "ARIRANHA DO IVAÍ", "CÂNDIDO DE ABREU", "CRUZMALTINA",
    "GODOY MOREIRA", "IVAIPORÃ", "JARDIM ALEGRE", "LIDIANÓPOLIS",
    "LUNARDELLI", "MANOEL RIBAS", "MATO RICO", "NOVA TEBAS",
    "RIO BRANCO DO IVAÍ", "ROSÁRIO DO IVAÍ", "SANTA MARIA DO OESTE", 
    "SÃO JOÃO DO IVAÍ"
  ))    

AUX04 <- sum(as.numeric(gsub("\\.", "", AUX04$Total)))

AUX <- RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016 %>%
  select(Grupo_CID, TOTAL) %>%
  rename(`2016` = TOTAL) %>%
  mutate(`2016` = as.numeric(`2016`),
         pop_limpa = AUX02$X2016,
         Inc_2016 = case_when(
           Grupo_CID == "Mama"    ~ (`2016` / AUX03) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2016` / AUX03) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2016` / AUX04) * 100000,
           TRUE                   ~ (`2016` / pop_limpa) * 100000),
         Inc_2016 = round(Inc_2016, 2)
  ) %>% 
  select(-pop_limpa) 

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2017` = TOTAL),
  by = "Grupo_CID" ) 

AUX <- AUX %>%
  mutate(`2017` = as.numeric(`2017`),
         pop_limpa = AUX02$X2017,
         Inc_2017 = case_when(
           Grupo_CID == "Mama"    ~ (`2017` / AUX03) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2017` / AUX03) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2017` / AUX04) * 100000,
           TRUE                   ~ (`2017` / pop_limpa) * 100000),
         Inc_2017 = round(Inc_2017, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2018` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  mutate(`2018` = as.numeric(`2018`),
         pop_limpa = AUX02$X2018,
         Inc_2018 = case_when(
           Grupo_CID == "Mama"    ~ (`2018` / AUX03) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2018` / AUX03) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2018` / AUX04) * 100000,
           TRUE                   ~ (`2018` / pop_limpa) * 100000),
         Inc_2018 = round(Inc_2018, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2019` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  mutate(`2019` = as.numeric(`2019`),
         pop_limpa = AUX02$X2019,
         Inc_2019 = case_when(
           Grupo_CID == "Mama"    ~ (`2019` / AUX03) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2019` / AUX03) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2019` / AUX04) * 100000,
           TRUE                   ~ (`2019` / pop_limpa) * 100000),
         Inc_2019 = round(Inc_2019, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2020` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  mutate(`2020` = as.numeric(`2020`),
         pop_limpa = AUX02$X2020,
         Inc_2020 = case_when(
           Grupo_CID == "Mama"    ~ (`2020` / AUX03) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2020` / AUX03) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2020` / AUX04) * 100000,
           TRUE                   ~ (`2020` / pop_limpa) * 100000),
         Inc_2020 = round(Inc_2020, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2021` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  mutate(`2021` = as.numeric(`2021`),
         pop_limpa = AUX02$X2021,
         Inc_2021 = case_when(
           Grupo_CID == "Mama"    ~ (`2021` / AUX03) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2021` / AUX03) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2021` / AUX04) * 100000,
           TRUE                   ~ (`2021` / pop_limpa) * 100000),
         Inc_2021 = round(Inc_2021, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2022` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  mutate(`2022` = as.numeric(`2022`),
         pop_limpa = AUX02$X2024,
         Inc_2022 = case_when(
           Grupo_CID == "Mama"    ~ (`2022` / AUX03) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2022` / AUX03) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2022` / AUX04) * 100000,
           TRUE                   ~ (`2022` / pop_limpa) * 100000),
         Inc_2022 = round(Inc_2022, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2023` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  mutate(`2023` = as.numeric(`2023`),
         pop_limpa = AUX02$X2024,
         Inc_2023 = case_when(
           Grupo_CID == "Mama"    ~ (`2023` / AUX03) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2023` / AUX03) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2023` / AUX04) * 100000,
           TRUE                   ~ (`2023` / pop_limpa) * 100000),
         Inc_2023 = round(Inc_2023, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2024` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  mutate(`2024` = as.numeric(`2024`),
         pop_limpa = AUX02$X2024,
         Inc_2024 = case_when(
           Grupo_CID == "Mama"    ~ (`2024` / AUX03) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2024` / AUX03) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2024` / AUX04) * 100000,
           TRUE                   ~ (`2024` / pop_limpa) * 100000),
         Inc_2024 = round(Inc_2024, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025 %>% 
    select(Grupo_CID, TOTAL) %>% 
    rename(`2025` = TOTAL),
  by = "Grupo_CID" )

AUX <- AUX %>%
  mutate(`2025` = as.numeric(`2025`),
         pop_limpa = AUX02$X2025,
         Inc_2025 = case_when(
           Grupo_CID == "Mama"    ~ (`2025` / AUX03) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2025` / AUX03) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2025` / AUX04) * 100000,
           TRUE                   ~ (`2025` / pop_limpa) * 100000),
         Inc_2025 = round(Inc_2025, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- AUX %>%
  mutate(Grupo_CID = case_when(
    Grupo_CID == "Cerebro_SNC" ~ "Cérebro e SNC",
    Grupo_CID == "Linfatico_Hematologico" ~ "Linfático e Hematológico",
    Grupo_CID == "Digestivo" ~ "Aparelho Digestivo",
    Grupo_CID == "Respiratorio" ~ "Aparelho Respiratório",
    Grupo_CID == "Gen_Fem" ~ "Órgãos Genitais Femininos",
    Grupo_CID == "Gen_Masc" ~ "Órgãos Genitais Masculinos",
    Grupo_CID == "Mama" ~ "Mama",
    Grupo_CID == "Via_Urinaria" ~ "Vias Urinárias",
    Grupo_CID == "Pele" ~ "Pele (Melanoma/Outros)",
    Grupo_CID == "Labio_Oral" ~ "Lábio e Cavidade Oral",
    Grupo_CID == "Ossos" ~ "Ossos e Cartilagens",
    Grupo_CID == "Tec_Mole" ~ "Tecidos Moles",
    Grupo_CID == "Tireoide_Endo" ~ "Tireoide e Endócrinas",
    Grupo_CID == "Mal_Definidas" ~ "Mal Definidas/Outras",
    TRUE ~ Grupo_CID # Mantém o nome original caso algum não entre nas regras
  ))

RS_PEVASPEA_SIM_TAB_NEOPLASIAS_GRUPOS <- gt(AUX) %>%
  tab_header(
    title = md("**Incidência de Neoplasias Malignas por Localização**"),
    subtitle = md("22 ª Regional de Saúde, 2016 – 2025")
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
  tab_spanner(label = "2016",
              columns = c(2:3),
              id = "1") %>%
  tab_spanner(label = "2017",
              columns = c(4:5),
              id = "2") %>%
  tab_spanner(label = "2018",
              columns = c(6:7),
              id = "3") %>%
  tab_spanner(label = "2019",
              columns = c(8:9),
              id = "4") %>%
  tab_spanner(label = "2020",
              columns = c(10:11),
              id = "5") %>%
  tab_spanner(label = "2021",
              columns = c(12:13),
              id = "6") %>%
  tab_spanner(label = "2022",
              columns = c(14:15),
              id = "7") %>%
  tab_spanner(label = "2023",
              columns = c(16:17),
              id = "8") %>%
  tab_spanner(label = "2024",
              columns = c(18:19),
              id = "9") %>%
  tab_spanner(label = "2025",
              columns = c(20:21),
              id = "10") %>%
  cols_align(align = "left", columns = 1) %>%
  cols_align(align = "center", columns = 2:21) %>%
  cols_label(contains("Inc_")     ~ "Inc.",
             matches("^20\\d{2}$") ~ "n"
  ) %>%
  fmt_number(
    columns = contains("Inc_"),
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  sub_missing(columns = everything(), missing_text = "-") %>%
  tab_footnote(
    footnote = "Fonte: Sistema de Informações de Mortalidade. Base DBF acessada em 04/05/2026."
  ) %>%
  tab_footnote(
    footnote = "Nota¹:Incidência calculada por 100.000 habitantes (População Estimada IBGE)."
  ) %>%
  tab_footnote(
    footnote = "Nota²: Incidência de câncer de mama e genitais femininos calculada usando a população feminina do Censo de 2022."
  ) %>%
  tab_footnote(
    footnote = "Nota³: Incidência de câncer de genitais masculinos calculada usando a população masculina do Censo de 2022."
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4F4F4"),
    locations = cells_body(columns = c(4:5, 8:9, 12:13, 16:17, 20:21)) 
  ) %>%
  tab_options(footnotes.padding = px(1),
              footnotes.font.size = px(10))

Total <- data.frame(
  Grupo_CID = "TOTAL",
  `2016` = sum(AUX$`2016`, na.rm = TRUE),
  `2017` = sum(AUX$`2017`, na.rm = TRUE),
  `2018` = sum(AUX$`2018`, na.rm = TRUE),
  `2019` = sum(AUX$`2019`, na.rm = TRUE),
  `2020` = sum(AUX$`2020`, na.rm = TRUE),
  `2021` = sum(AUX$`2021`, na.rm = TRUE),
  `2022` = sum(AUX$`2022`, na.rm = TRUE),
  `2023` = sum(AUX$`2023`, na.rm = TRUE),
  `2024` = sum(AUX$`2024`, na.rm = TRUE),
  `2025` = sum(AUX$`2025`, na.rm = TRUE),
  check.names = FALSE 
)

Total$Inc_2016 <- round((Total$`2016` / as.numeric(gsub("\\.", "", AUX02$X2016))) * 100000, 2)
Total$Inc_2017 <- round((Total$`2017` / as.numeric(gsub("\\.", "", AUX02$X2017))) * 100000, 2)
Total$Inc_2018 <- round((Total$`2018` / as.numeric(gsub("\\.", "", AUX02$X2018))) * 100000, 2)
Total$Inc_2019 <- round((Total$`2019` / as.numeric(gsub("\\.", "", AUX02$X2019))) * 100000, 2)
Total$Inc_2020 <- round((Total$`2020` / as.numeric(gsub("\\.", "", AUX02$X2020))) * 100000, 2)
Total$Inc_2021 <- round((Total$`2021` / as.numeric(gsub("\\.", "", AUX02$X2021))) * 100000, 2)
Total$Inc_2022 <- round((Total$`2022` / as.numeric(gsub("\\.", "", AUX02$X2024))) * 100000, 2)
Total$Inc_2023 <- round((Total$`2023` / as.numeric(gsub("\\.", "", AUX02$X2024))) * 100000, 2)
Total$Inc_2024 <- round((Total$`2024` / as.numeric(gsub("\\.", "", AUX02$X2024))) * 100000, 2)
Total$Inc_2025 <- round((Total$`2025` / as.numeric(gsub("\\.", "", AUX02$X2025))) * 100000, 2)

Total_Inc <- Total %>% 
  select(contains("Inc")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    Incidencia = as.numeric(V1)     
  ) %>%
  select(Ano, Incidencia)

Total_N <- Total %>% 
  select(matches("^20(1[6-9]|2[0-9])$")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    N = as.numeric(V1)     
  ) %>%
  select(Ano, N)

Total <- left_join(Total_Inc,
                   Total_N,
                   by = "Ano")

fator_escala <- (max(Total$N, na.rm = TRUE) / 190)

PR_PEVASPEA_SIM_GRAF_NEOPLASIAS_Incidencia <- ggplot(Total, 
                                                     aes(x = Ano, y = Incidencia)) + 
  geom_col(aes(y = N / fator_escala), 
           fill = "#dcdde1", 
           width = 0.6)+
  geom_text(aes(y = N / fator_escala / 2, 
                label = format(N, big.mark = ".")), 
            size = 4, 
            fontface = "bold") +
  geom_line(aes(x = Ano,
                y = Incidencia,
                group = 1),
            colour = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(y = "Casos/100.000 habitantes",
       x = NULL,
       title = "Mortalidade por Câncer/100.000 habitantes e Número de Casos na 22ª RS") +
  geom_text(aes(label = format(round(Incidencia, 2),
                               decimal.mark = ",")), 
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(0, 200), 
                     breaks = seq(0, 200, 20),
                     sec.axis = sec_axis(~ . * fator_escala, 
                                         name = "Casos",
                                         breaks = c(0, 100, 200, 300),
                                         labels = c("0", "100", "200", "300"))) +
  scale_x_discrete(breaks = 2016:2025) +
  Theme()

#### Mesmo procedimento para idade de 30 a 69 anos

Base_Populacional_Masculina_2022_30_69 <- Base_Populacional_Masculina_2022 %>%
  mutate(across(c(X30.a.34.anos, X35.a.39.anos, X40.a.44.anos, X45.a.49.anos,
                  X50.a.54.anos, X55.a.59.anos, X60.a.64.anos, X65.a.69.anos),
                ~ as.numeric(gsub("\\.", "", .x)))) %>%
  mutate(
    `30_69` = rowSums(across(c(X30.a.34.anos, X35.a.39.anos, X40.a.44.anos,
                               X45.a.49.anos, X50.a.54.anos, X55.a.59.anos,
                               X60.a.64.anos, X65.a.69.anos)), na.rm = TRUE)
  )

Base_Populacional_Feminina_2022_30_69 <- Base_Populacional_Feminina_2022 %>%
  mutate(across(c(X30.a.34.anos, X35.a.39.anos, X40.a.44.anos, X45.a.49.anos,
                  X50.a.54.anos, X55.a.59.anos, X60.a.64.anos, X65.a.69.anos),
                ~ as.numeric(gsub("\\.", "", .x)))) %>%
  mutate(
    `30_69` = rowSums(across(c(X30.a.34.anos, X35.a.39.anos, X40.a.44.anos,
                               X45.a.49.anos, X50.a.54.anos, X55.a.59.anos,
                               X60.a.64.anos, X65.a.69.anos)), na.rm = TRUE)
  )

AUX <-  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016 %>%
  mutate(Óbitos_Menores_30 = rowSums(across(c(X0.4, X5.9, X10.14, X15.19, X20.24, X25.29)), na.rm = TRUE),
         `2016` = rowSums(across(c(X30.34, X35.39, X40.44, X45.49, X50.54, X55.59, X60.64, X65.69)), na.rm = TRUE)
  ) %>%
  select(Grupo_CID, 
         `2016`) %>%
  mutate(pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1]),
         Inc_2016 = case_when(
           Grupo_CID == "Mama"    ~ (`2016` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2016` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2016` / Base_Populacional_Masculina_2022_30_69$`30_69`[1]) * 100000,
           TRUE                   ~ (`2016` / pop_limpa) * 100000),
         Inc_2016 = round(Inc_2016, 2)) %>%
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017  %>%
    mutate(`2017` = rowSums(across(c(X30.34, X35.39, X40.44, X45.49, X50.54, X55.59, X60.64, X65.69)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2017`), 
  by = "Grupo_CID") 

AUX <- AUX %>%
  mutate(`2017` = as.numeric(`2017`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1]),
         Inc_2017 = case_when(
           Grupo_CID == "Mama"    ~ (`2017` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2017` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2017` / Base_Populacional_Masculina_2022_30_69$`30_69`[1]) * 100000,
           TRUE                   ~ (`2017` / pop_limpa) * 100000),
         Inc_2017 = round(Inc_2017, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018 %>%
    mutate(`2018` = rowSums(across(c(X30.34, X35.39, X40.44, X45.49, X50.54, X55.59, X60.64, X65.69)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2018`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2018` = as.numeric(`2018`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1]),
         Inc_2018 = case_when(
           Grupo_CID == "Mama"    ~ (`2018` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2018` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2018` / Base_Populacional_Masculina_2022_30_69$`30_69`[1]) * 100000,
           TRUE                   ~ (`2018` / pop_limpa) * 100000),
         Inc_2018 = round(Inc_2018, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019 %>%
    mutate(`2019` = rowSums(across(c(X30.34, X35.39, X40.44, X45.49, X50.54, X55.59, X60.64, X65.69)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2019`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2019` = as.numeric(`2019`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1]),
         Inc_2019 = case_when(
           Grupo_CID == "Mama"    ~ (`2019` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2019` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2019` / Base_Populacional_Masculina_2022_30_69$`30_69`[1]) * 100000,
           TRUE                   ~ (`2019` / pop_limpa) * 100000),
         Inc_2019 = round(Inc_2019, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020 %>%
    mutate(`2020` = rowSums(across(c(X30.34, X35.39, X40.44, X45.49, X50.54, X55.59, X60.64, X65.69)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2020`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2020` = as.numeric(`2020`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1]),
         Inc_2020 = case_when(
           Grupo_CID == "Mama"    ~ (`2020` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2020` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2020` / Base_Populacional_Masculina_2022_30_69$`30_69`[1]) * 100000,
           TRUE                   ~ (`2020` / pop_limpa) * 100000),
         Inc_2020 = round(Inc_2020, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021 %>%
    mutate(`2021` = rowSums(across(c(X30.34, X35.39, X40.44, X45.49, X50.54, X55.59, X60.64, X65.69)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2021`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2021` = as.numeric(`2021`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1]),
         Inc_2021 = case_when(
           Grupo_CID == "Mama"    ~ (`2021` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2021` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2021` / Base_Populacional_Masculina_2022_30_69$`30_69`[1]) * 100000,
           TRUE                   ~ (`2021` / pop_limpa) * 100000),
         Inc_2021 = round(Inc_2021, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022 %>%
    mutate(`2022` = rowSums(across(c(X30.34, X35.39, X40.44, X45.49, X50.54, X55.59, X60.64, X65.69)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2022`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2022` = as.numeric(`2022`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1]),
         Inc_2022 = case_when(
           Grupo_CID == "Mama"    ~ (`2022` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2022` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2022` / Base_Populacional_Masculina_2022_30_69$`30_69`[1]) * 100000,
           TRUE                   ~ (`2022` / pop_limpa) * 100000),
         Inc_2022 = round(Inc_2022, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023 %>%
    mutate(`2023` = rowSums(across(c(X30.34, X35.39, X40.44, X45.49, X50.54, X55.59, X60.64, X65.69)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2023`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2023` = as.numeric(`2023`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1]),
         Inc_2023 = case_when(
           Grupo_CID == "Mama"    ~ (`2023` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2023` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2023` / Base_Populacional_Masculina_2022_30_69$`30_69`[1]) * 100000,
           TRUE                   ~ (`2023` / pop_limpa) * 100000),
         Inc_2023 = round(Inc_2023, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024 %>%
    mutate(`2024` = rowSums(across(c(X30.34, X35.39, X40.44, X45.49, X50.54, X55.59, X60.64, X65.69)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2024`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2024` = as.numeric(`2024`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1]),
         Inc_2024 = case_when(
           Grupo_CID == "Mama"    ~ (`2024` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2024` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2024` / Base_Populacional_Masculina_2022_30_69$`30_69`[1]) * 100000,
           TRUE                   ~ (`2024` / pop_limpa) * 100000),
         Inc_2024 = round(Inc_2024, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025 %>%
    mutate(`2025` = rowSums(across(c(X30.34, X35.39, X40.44, X45.49, X50.54, X55.59, X60.64, X65.69)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2025`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2025` = as.numeric(`2025`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1]),
         Inc_2025 = case_when(
           Grupo_CID == "Mama"    ~ (`2025` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2025` / Base_Populacional_Feminina_2022_30_69$`30_69`[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2025` / Base_Populacional_Masculina_2022_30_69$`30_69`[1]) * 100000,
           TRUE                   ~ (`2025` / pop_limpa) * 100000),
         Inc_2025 = round(Inc_2025, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- AUX %>%
  mutate(Grupo_CID = case_when(
    Grupo_CID == "Cerebro_SNC" ~ "Cérebro e SNC",
    Grupo_CID == "Linfatico_Hematologico" ~ "Linfático e Hematológico",
    Grupo_CID == "Digestivo" ~ "Aparelho Digestivo",
    Grupo_CID == "Respiratorio" ~ "Aparelho Respiratório",
    Grupo_CID == "Gen_Fem" ~ "Órgãos Genitais Femininos",
    Grupo_CID == "Gen_Masc" ~ "Órgãos Genitais Masculinos",
    Grupo_CID == "Mama" ~ "Mama",
    Grupo_CID == "Via_Urinaria" ~ "Vias Urinárias",
    Grupo_CID == "Pele" ~ "Pele (Melanoma/Outros)",
    Grupo_CID == "Labio_Oral" ~ "Lábio e Cavidade Oral",
    Grupo_CID == "Ossos" ~ "Ossos e Cartilagens",
    Grupo_CID == "Tec_Mole" ~ "Tecidos Moles",
    Grupo_CID == "Tireoide_Endo" ~ "Tireoide e Endócrinas",
    Grupo_CID == "Mal_Definidas" ~ "Mal Definidas/Outras",
    TRUE ~ Grupo_CID 
  ))

PR_PEVASPEA_SIM_TAB_NEOPLASIAS_GRUPOS_30_69 <- gt(AUX) %>%
  tab_header(
    title = md("**Incidência de Neoplasias Malignas por Localização em  População de 30 a 69 anos**"),
    subtitle = md("Paraná, 2016 – 2025")
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
  tab_spanner(label = "2016",
              columns = c(2:3),
              id = "1") %>%
  tab_spanner(label = "2017",
              columns = c(4:5),
              id = "2") %>%
  tab_spanner(label = "2018",
              columns = c(6:7),
              id = "3") %>%
  tab_spanner(label = "2019",
              columns = c(8:9),
              id = "4") %>%
  tab_spanner(label = "2020",
              columns = c(10:11),
              id = "5") %>%
  tab_spanner(label = "2021",
              columns = c(12:13),
              id = "6") %>%
  tab_spanner(label = "2022",
              columns = c(14:15),
              id = "7") %>%
  tab_spanner(label = "2023",
              columns = c(16:17),
              id = "8") %>%
  tab_spanner(label = "2024",
              columns = c(18:19),
              id = "9") %>%
  tab_spanner(label = "2025",
              columns = c(20:21),
              id = "10") %>%
  cols_align(align = "left", columns = 1) %>%
  cols_align(align = "center", columns = 2:21) %>%
  cols_label(contains("Inc_")     ~ "Inc.",
             matches("^20\\d{2}$") ~ "n"
  ) %>%
  fmt_number(
    columns = contains("Inc_"),
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  sub_missing(columns = everything(), missing_text = "-") %>%
  tab_footnote(
    footnote = "Fonte: Sistema de Informações de Mortalidade. Base DBF acessada em 04/05/2026."
  ) %>%
  tab_footnote(
    footnote = "Nota¹:Incidência calculada por 100.000 habitantes (IBGE Censo 2022)."
  ) %>%
  tab_footnote(
    footnote = "Nota²: Incidência de câncer de mama e genitais femininos calculada usando a população feminina do Censo de 2022."
  ) %>%
  tab_footnote(
    footnote = "Nota³: Incidência de câncer de genitais masculinos calculada usando a população masculina do Censo de 2022."
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4F4F4"),
    locations = cells_body(columns = c(4:5, 8:9, 12:13, 16:17, 20:21)) 
  ) %>%
  tab_options(footnotes.padding = px(1),
              footnotes.font.size = px(10))

Total <- data.frame(
  Grupo_CID = "TOTAL",
  `2016` = sum(AUX$`2016`, na.rm = TRUE),
  `2017` = sum(AUX$`2017`, na.rm = TRUE),
  `2018` = sum(AUX$`2018`, na.rm = TRUE),
  `2019` = sum(AUX$`2019`, na.rm = TRUE),
  `2020` = sum(AUX$`2020`, na.rm = TRUE),
  `2021` = sum(AUX$`2021`, na.rm = TRUE),
  `2022` = sum(AUX$`2022`, na.rm = TRUE),
  `2023` = sum(AUX$`2023`, na.rm = TRUE),
  `2024` = sum(AUX$`2024`, na.rm = TRUE),
  `2025` = sum(AUX$`2025`, na.rm = TRUE),
  check.names = FALSE 
)

Total$Inc_2016 <- round((Total$`2016` / as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1])) * 100000, 2)
Total$Inc_2017 <- round((Total$`2017` / as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1])) * 100000, 2)
Total$Inc_2018 <- round((Total$`2018` / as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1])) * 100000, 2)
Total$Inc_2019 <- round((Total$`2019` / as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1])) * 100000, 2)
Total$Inc_2020 <- round((Total$`2020` / as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1])) * 100000, 2)
Total$Inc_2021 <- round((Total$`2021` / as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1])) * 100000, 2)
Total$Inc_2022 <- round((Total$`2022` / as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1])) * 100000, 2)
Total$Inc_2023 <- round((Total$`2023` / as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1])) * 100000, 2)
Total$Inc_2024 <- round((Total$`2024` / as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1])) * 100000, 2)
Total$Inc_2025 <- round((Total$`2025` / as.numeric(Base_Populacional_Feminina_2022_30_69$`30_69`[1] + Base_Populacional_Masculina_2022_30_69$`30_69`[1])) * 100000, 2)

Total_Inc <- Total %>% 
  select(contains("Inc")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    Incidencia = as.numeric(V1)     
  ) %>%
  select(Ano, Incidencia)

Total_N <- Total %>% 
  select(matches("^20(1[6-9]|2[0-9])$")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    N = as.numeric(V1)     
  ) %>%
  select(Ano, N)

Total <- left_join(Total_Inc,
                   Total_N,
                   by = "Ano")

fator_escala <- (max(Total$N, na.rm = TRUE) / 140)

PR_PEVASPEA_SIM_GRAF_NEOPLASIAS_Incidencia_30_69 <- ggplot(Total, 
                                                     aes(x = Ano, y = Incidencia)) + 
  geom_col(aes(y = N / fator_escala), 
           fill = "#dcdde1", 
           width = 0.6)+
  geom_text(aes(y = N / fator_escala / 2, 
                label = format(N, big.mark = ".")), 
            size = 4, 
            fontface = "bold") +
  geom_line(aes(x = Ano,
                y = Incidencia,
                group = 1),
            colour = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(y = "Casos/100.000 habitantes",
       x = NULL,
       title = "Mortalidade por Câncer/100.000 habitantes no Paraná") +
  geom_text(aes(label = format(round(Incidencia, 2),
                               decimal.mark = ",")), 
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(0, 180), 
                     breaks = seq(0, 180, 20),
                     sec.axis = sec_axis(~ . * fator_escala, 
                                         name = "Casos",
                                         labels = label_number(big.mark = "."))) +
  scale_x_discrete(breaks = 2016:2025) +
  Theme()

#### Mesmo procedimento para idade de 30 a 69 anos REGIONAL

Base_Populacional_Masculina_2022_30_69_RS <- Base_Populacional_Masculina_2022 %>%
  filter(MUN_ESTADO %in% c(
    "ARAPUÃ", "ARIRANHA DO IVAÍ", "CÂNDIDO DE ABREU", "CRUZMALTINA",
    "GODOY MOREIRA", "IVAIPORÃ", "JARDIM ALEGRE", "LIDIANÓPOLIS",
    "LUNARDELLI", "MANOEL RIBAS", "MATO RICO", "NOVA TEBAS",
    "RIO BRANCO DO IVAÍ", "ROSÁRIO DO IVAÍ", "SANTA MARIA DO OESTE", 
    "SÃO JOÃO DO IVAÍ"
  )) %>% 
  mutate(across(c(X30.a.34.anos, X35.a.39.anos, X40.a.44.anos, X45.a.49.anos,
                  X50.a.54.anos, X55.a.59.anos, X60.a.64.anos, X65.a.69.anos),
                ~ as.numeric(gsub("\\.", "", .x)))) %>%
  mutate(
    `30_69` = rowSums(across(c(X30.a.34.anos, X35.a.39.anos, X40.a.44.anos,
                               X45.a.49.anos, X50.a.54.anos, X55.a.59.anos,
                               X60.a.64.anos, X65.a.69.anos)), na.rm = TRUE)
  )

Base_Populacional_Masculina_2022_30_69_RS$Total <- as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022_30_69_RS$Total))

linha_total_masc_regional <- Base_Populacional_Masculina_2022_30_69_RS %>%
  summarise(
    MUN_ESTADO = "TOTAL REGIONAL",
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    Até.14.anos = NA,
    X15.a.64.anos = NA
  )

Base_Populacional_Masculina_2022_30_69_RS <- bind_rows(
  Base_Populacional_Masculina_2022_30_69_RS, 
  linha_total_masc_regional
)

Base_Populacional_Feminina_2022_30_69_RS <- Base_Populacional_Feminina_2022 %>%
  filter(MUN_ESTADO %in% c(
    "ARAPUÃ", "ARIRANHA DO IVAÍ", "CÂNDIDO DE ABREU", "CRUZMALTINA",
    "GODOY MOREIRA", "IVAIPORÃ", "JARDIM ALEGRE", "LIDIANÓPOLIS",
    "LUNARDELLI", "MANOEL RIBAS", "MATO RICO", "NOVA TEBAS",
    "RIO BRANCO DO IVAÍ", "ROSÁRIO DO IVAÍ", "SANTA MARIA DO OESTE", 
    "SÃO JOÃO DO IVAÍ"
  )) %>% 
  mutate(across(c(X30.a.34.anos, X35.a.39.anos, X40.a.44.anos, X45.a.49.anos,
                  X50.a.54.anos, X55.a.59.anos, X60.a.64.anos, X65.a.69.anos),
                ~ as.numeric(gsub("\\.", "", .x)))) %>%
  mutate(
    `30_69` = rowSums(across(c(X30.a.34.anos, X35.a.39.anos, X40.a.44.anos,
                               X45.a.49.anos, X50.a.54.anos, X55.a.59.anos,
                               X60.a.64.anos, X65.a.69.anos)), na.rm = TRUE)
  )

Base_Populacional_Feminina_2022_30_69_RS$Total <- as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022_30_69_RS$Total))

linha_total_fem_regional <- Base_Populacional_Feminina_2022_30_69_RS %>%
  summarise(
    MUN_ESTADO = "TOTAL REGIONAL",
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    Até.14.anos = NA,
    X15.a.64.anos = NA
  )

Base_Populacional_Feminina_2022_30_69_RS <- bind_rows(
  Base_Populacional_Feminina_2022_30_69_RS, 
  linha_total_fem_regional
)

AUX <-  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016 %>%
  mutate(`2016` = rowSums(across(any_of(c("X30.34", "X35.39", "X40.44", "X45.49", "X50.54", "X55.59", "X60.64", "X65.69"))), na.rm = TRUE)
  ) %>%
  select(Grupo_CID, 
         `2016`) %>%
  mutate(pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]),
         Inc_2016 = case_when(
           Grupo_CID == "Mama"    ~ (`2016` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2016` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2016` / Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]) * 100000,
           TRUE                   ~ (`2016` / pop_limpa) * 100000),
         Inc_2016 = round(Inc_2016, 2)) %>%
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017 %>%
    mutate(`2017` = rowSums(across(any_of(c("X30.34", "X35.39", "X40.44", "X45.49", 
                                            "X50.54", "X55.59", "X60.64", "X65.69"))), na.rm = TRUE)) %>% 
    select(Grupo_CID, `2017`), 
  by = "Grupo_CID"
)  

AUX <- AUX %>%
  mutate(`2017` = as.numeric(`2017`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]),
         Inc_2017 = case_when(
           Grupo_CID == "Mama"    ~ (`2017` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2017` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2017` / Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]) * 100000,
           TRUE                   ~ (`2017` / pop_limpa) * 100000),
         Inc_2017 = round(Inc_2017, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018 %>%
    mutate(`2018` = rowSums(across(any_of(c("X30.34", "X35.39", "X40.44", "X45.49", 
                                            "X50.54", "X55.59", "X60.64", "X65.69"))), na.rm = TRUE)) %>% 
    select(Grupo_CID, `2018`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2018` = as.numeric(`2018`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]),
         Inc_2018 = case_when(
           Grupo_CID == "Mama"    ~ (`2018` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2018` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2018` / Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]) * 100000,
           TRUE                   ~ (`2018` / pop_limpa) * 100000),
         Inc_2018 = round(Inc_2018, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019 %>%
    mutate(`2019` = rowSums(across(any_of(c("X30.34", "X35.39", "X40.44", "X45.49", 
                                            "X50.54", "X55.59", "X60.64", "X65.69"))), na.rm = TRUE)) %>%    
    select(Grupo_CID, `2019`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2019` = as.numeric(`2019`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]),
         Inc_2019 = case_when(
           Grupo_CID == "Mama"    ~ (`2019` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2019` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2019` / Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]) * 100000,
           TRUE                   ~ (`2019` / pop_limpa) * 100000),
         Inc_2019 = round(Inc_2019, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020 %>%
    mutate(`2020` = rowSums(across(any_of(c("X30.34", "X35.39", "X40.44", "X45.49", 
                                            "X50.54", "X55.59", "X60.64", "X65.69"))), na.rm = TRUE)) %>% 
    select(Grupo_CID, `2020`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2020` = as.numeric(`2020`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]),
         Inc_2020 = case_when(
           Grupo_CID == "Mama"    ~ (`2020` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2020` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2020` / Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]) * 100000,
           TRUE                   ~ (`2020` / pop_limpa) * 100000),
         Inc_2020 = round(Inc_2020, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021 %>%
    mutate(`2021` = rowSums(across(any_of(c("X30.34", "X35.39", "X40.44", "X45.49", 
                                            "X50.54", "X55.59", "X60.64", "X65.69"))), na.rm = TRUE)) %>% 
    select(Grupo_CID, `2021`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2021` = as.numeric(`2021`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]),
         Inc_2021 = case_when(
           Grupo_CID == "Mama"    ~ (`2021` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2021` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2021` / Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]) * 100000,
           TRUE                   ~ (`2021` / pop_limpa) * 100000),
         Inc_2021 = round(Inc_2021, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022 %>%
    mutate(`2022` = rowSums(across(any_of(c("X30.34", "X35.39", "X40.44", "X45.49", 
                                            "X50.54", "X55.59", "X60.64", "X65.69"))), na.rm = TRUE)) %>%
    select(Grupo_CID, `2022`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2022` = as.numeric(`2022`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]),
         Inc_2022 = case_when(
           Grupo_CID == "Mama"    ~ (`2022` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2022` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2022` / Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]) * 100000,
           TRUE                   ~ (`2022` / pop_limpa) * 100000),
         Inc_2022 = round(Inc_2022, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023 %>%
    mutate(`2023` = rowSums(across(any_of(c("X30.34", "X35.39", "X40.44", "X45.49", 
                                            "X50.54", "X55.59", "X60.64", "X65.69"))), na.rm = TRUE)) %>%
    select(Grupo_CID, `2023`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2023` = as.numeric(`2023`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]),
         Inc_2023 = case_when(
           Grupo_CID == "Mama"    ~ (`2023` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2023` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2023` / Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]) * 100000,
           TRUE                   ~ (`2023` / pop_limpa) * 100000),
         Inc_2023 = round(Inc_2023, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024 %>%
    mutate(`2024` = rowSums(across(any_of(c("X30.34", "X35.39", "X40.44", "X45.49", 
                                            "X50.54", "X55.59", "X60.64", "X65.69"))), na.rm = TRUE)) %>%
    select(Grupo_CID, `2024`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2024` = as.numeric(`2024`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]),
         Inc_2024 = case_when(
           Grupo_CID == "Mama"    ~ (`2024` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2024` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2024` / Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]) * 100000,
           TRUE                   ~ (`2024` / pop_limpa) * 100000),
         Inc_2024 = round(Inc_2024, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025 %>%
    mutate(`2025` = rowSums(across(any_of(c("X30.34", "X35.39", "X40.44", "X45.49", 
                                            "X50.54", "X55.59", "X60.64", "X65.69"))), na.rm = TRUE)) %>%
    select(Grupo_CID, `2025`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2025` = as.numeric(`2025`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]),
         Inc_2025 = case_when(
           Grupo_CID == "Mama"    ~ (`2025` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2025` / Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2025` / Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17]) * 100000,
           TRUE                   ~ (`2025` / pop_limpa) * 100000),
         Inc_2025 = round(Inc_2025, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- AUX %>%
  mutate(Grupo_CID = case_when(
    Grupo_CID == "Cerebro_SNC" ~ "Cérebro e SNC",
    Grupo_CID == "Linfatico_Hematologico" ~ "Linfático e Hematológico",
    Grupo_CID == "Digestivo" ~ "Aparelho Digestivo",
    Grupo_CID == "Respiratorio" ~ "Aparelho Respiratório",
    Grupo_CID == "Gen_Fem" ~ "Órgãos Genitais Femininos",
    Grupo_CID == "Gen_Masc" ~ "Órgãos Genitais Masculinos",
    Grupo_CID == "Mama" ~ "Mama",
    Grupo_CID == "Via_Urinaria" ~ "Vias Urinárias",
    Grupo_CID == "Pele" ~ "Pele (Melanoma/Outros)",
    Grupo_CID == "Labio_Oral" ~ "Lábio e Cavidade Oral",
    Grupo_CID == "Ossos" ~ "Ossos e Cartilagens",
    Grupo_CID == "Tec_Mole" ~ "Tecidos Moles",
    Grupo_CID == "Tireoide_Endo" ~ "Tireoide e Endócrinas",
    Grupo_CID == "Mal_Definidas" ~ "Mal Definidas/Outras",
    TRUE ~ Grupo_CID 
  ))

RS_PEVASPEA_SIM_TAB_NEOPLASIAS_GRUPOS_30_69 <- gt(AUX) %>%
  tab_header(
    title = md("**Incidência de Neoplasias Malignas por Localização em  População de 30 a 69 anos**"),
    subtitle = md("22ª Regional de Saúde, 2016 – 2025")
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
  tab_spanner(label = "2016",
              columns = c(2:3),
              id = "1") %>%
  tab_spanner(label = "2017",
              columns = c(4:5),
              id = "2") %>%
  tab_spanner(label = "2018",
              columns = c(6:7),
              id = "3") %>%
  tab_spanner(label = "2019",
              columns = c(8:9),
              id = "4") %>%
  tab_spanner(label = "2020",
              columns = c(10:11),
              id = "5") %>%
  tab_spanner(label = "2021",
              columns = c(12:13),
              id = "6") %>%
  tab_spanner(label = "2022",
              columns = c(14:15),
              id = "7") %>%
  tab_spanner(label = "2023",
              columns = c(16:17),
              id = "8") %>%
  tab_spanner(label = "2024",
              columns = c(18:19),
              id = "9") %>%
  tab_spanner(label = "2025",
              columns = c(20:21),
              id = "10") %>%
  cols_align(align = "left", columns = 1) %>%
  cols_align(align = "center", columns = 2:21) %>%
  cols_label(contains("Inc_")     ~ "Inc.",
             matches("^20\\d{2}$") ~ "n"
  ) %>%
  fmt_number(
    columns = contains("Inc_"),
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  sub_missing(columns = everything(), missing_text = "-") %>%
  tab_footnote(
    footnote = "Fonte: Sistema de Informações de Mortalidade. Base DBF acessada em 04/05/2026."
  ) %>%
  tab_footnote(
    footnote = "Nota¹:Incidência calculada por 100.000 habitantes (IBGE Censo 2022)."
  ) %>%
  tab_footnote(
    footnote = "Nota²: Incidência de câncer de mama e genitais femininos calculada usando a população feminina do Censo de 2022."
  ) %>%
  tab_footnote(
    footnote = "Nota³: Incidência de câncer de genitais masculinos calculada usando a população masculina do Censo de 2022."
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4F4F4"),
    locations = cells_body(columns = c(4:5, 8:9, 12:13, 16:17, 20:21)) 
  ) %>%
  tab_options(footnotes.padding = px(1),
              footnotes.font.size = px(10))

Total <- data.frame(
  Grupo_CID = "TOTAL",
  `2016` = sum(AUX$`2016`, na.rm = TRUE),
  `2017` = sum(AUX$`2017`, na.rm = TRUE),
  `2018` = sum(AUX$`2018`, na.rm = TRUE),
  `2019` = sum(AUX$`2019`, na.rm = TRUE),
  `2020` = sum(AUX$`2020`, na.rm = TRUE),
  `2021` = sum(AUX$`2021`, na.rm = TRUE),
  `2022` = sum(AUX$`2022`, na.rm = TRUE),
  `2023` = sum(AUX$`2023`, na.rm = TRUE),
  `2024` = sum(AUX$`2024`, na.rm = TRUE),
  `2025` = sum(AUX$`2025`, na.rm = TRUE),
  check.names = FALSE 
)

Total$Inc_2016 <- round((Total$`2016` / as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17])) * 100000, 2)
Total$Inc_2017 <- round((Total$`2017` / as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17])) * 100000, 2)
Total$Inc_2018 <- round((Total$`2018` / as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17])) * 100000, 2)
Total$Inc_2019 <- round((Total$`2019` / as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17])) * 100000, 2)
Total$Inc_2020 <- round((Total$`2020` / as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17])) * 100000, 2)
Total$Inc_2021 <- round((Total$`2021` / as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17])) * 100000, 2)
Total$Inc_2022 <- round((Total$`2022` / as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17])) * 100000, 2)
Total$Inc_2023 <- round((Total$`2023` / as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17])) * 100000, 2)
Total$Inc_2024 <- round((Total$`2024` / as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17])) * 100000, 2)
Total$Inc_2025 <- round((Total$`2025` / as.numeric(Base_Populacional_Feminina_2022_30_69_RS$`30_69`[17] + Base_Populacional_Masculina_2022_30_69_RS$`30_69`[17])) * 100000, 2)

Total_Inc <- Total %>% 
  select(contains("Inc")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    Incidencia = as.numeric(V1)     
  ) %>%
  select(Ano, Incidencia)

Total_N <- Total %>% 
  select(matches("^20(1[6-9]|2[0-9])$")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    N = as.numeric(V1)     
  ) %>%
  select(Ano, N)

Total <- left_join(Total_Inc,
                   Total_N,
                   by = "Ano")

fator_escala <- (max(Total$N, na.rm = TRUE) / 140)

RS_PEVASPEA_SIM_GRAF_NEOPLASIAS_Incidencia_30_69 <- ggplot(Total, 
                                                     aes(x = Ano, y = Incidencia)) + 
  geom_col(aes(y = N / fator_escala), 
           fill = "#dcdde1", 
           width = 0.6)+
  geom_text(aes(y = N / fator_escala / 2, 
                label = format(N, big.mark = ".")), 
            size = 4, 
            fontface = "bold") +
  geom_line(aes(x = Ano,
                y = Incidencia,
                group = 1),
            colour = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(y = "Casos/100.000 habitantes",
       x = NULL,
       title = "Mortalidade por Câncerem População entre 30 e 69 Anos/100.000 habitantes e Número de Casos na 22ª RS") +
  geom_text(aes(label = format(round(Incidencia, 2),
                               decimal.mark = ",")), 
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(0, 200), 
                     breaks = seq(0, 200, 20),
                     sec.axis = sec_axis(~ . * fator_escala, 
                                         name = "Casos",
                                         breaks = c(0, 100, 200, 300),
                                         labels = c("0", "100", "200", "300"))) +
  scale_x_discrete(breaks = 2016:2025) +
  Theme()

#### Mesmo procedimento para menores de 30 anos

Base_Populacional_Masculina_2022_Men_30 <- Base_Populacional_Masculina_2022 %>%
  mutate(across(c(Menores.de.1.ano, X1.a.4.anos, X5.a.9.anos, X10.a.14.anos,
                  X15.a.19.anos, X20.a.24.anos, X25.a.29.anos),
                ~ as.numeric(gsub("\\.", "", .x)))) %>%
  mutate(Men_30  = rowSums(across(c(Menores.de.1.ano, X1.a.4.anos, X5.a.9.anos, X10.a.14.anos,
                                    X15.a.19.anos, X20.a.24.anos, X25.a.29.anos)), na.rm = TRUE)
  )

Base_Populacional_Feminina_2022_Men_30 <- Base_Populacional_Feminina_2022 %>%
  mutate(across(c(Menores.de.1.ano, X1.a.4.anos, X5.a.9.anos, X10.a.14.anos,
                  X15.a.19.anos, X20.a.24.anos, X25.a.29.anos),
                ~ as.numeric(gsub("\\.", "", .x)))) %>%
  mutate(Men_30 = rowSums(across(c(Menores.de.1.ano, X1.a.4.anos, X5.a.9.anos, X10.a.14.anos,
                                   X15.a.19.anos, X20.a.24.anos, X25.a.29.anos)), na.rm = TRUE)
  )

AUX <-  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016 %>%
  mutate(`2016` = rowSums(across(c(X0.4, X5.9, X10.14, X15.19, X20.24, X25.29)), na.rm = TRUE)
  ) %>%
  select(Grupo_CID, 
         `2016`) %>%
  mutate(pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1]),
         Inc_2016 = case_when(
           Grupo_CID == "Mama"    ~ (`2016` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2016` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2016` / Base_Populacional_Masculina_2022_Men_30$Men_30[1]) * 100000,
           TRUE                   ~ (`2016` / pop_limpa) * 100000),
         Inc_2016 = round(Inc_2016, 2)) %>%
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017  %>%
    mutate(`2017` = rowSums(across(c(X0.4, X5.9, X10.14, X15.19, X20.24, X25.29)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2017`), 
  by = "Grupo_CID") 

AUX <- AUX %>%
  mutate(`2017` = as.numeric(`2017`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1]),
         Inc_2017 = case_when(
           Grupo_CID == "Mama"    ~ (`2017` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2017` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2017` / Base_Populacional_Masculina_2022_Men_30$Men_30[1]) * 100000,
           TRUE                   ~ (`2017` / pop_limpa) * 100000),
         Inc_2017 = round(Inc_2017, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018 %>%
    mutate(`2018` = rowSums(across(c(X0.4, X5.9, X10.14, X15.19, X20.24, X25.29)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2018`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2018` = as.numeric(`2018`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1]),
         Inc_2018 = case_when(
           Grupo_CID == "Mama"    ~ (`2018` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2018` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2018` / Base_Populacional_Masculina_2022_Men_30$Men_30[1]) * 100000,
           TRUE                   ~ (`2018` / pop_limpa) * 100000),
         Inc_2018 = round(Inc_2018, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019 %>%
    mutate(`2019` = rowSums(across(c(X0.4, X5.9, X10.14, X15.19, X20.24, X25.29)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2019`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2019` = as.numeric(`2019`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1]),
         Inc_2019 = case_when(
           Grupo_CID == "Mama"    ~ (`2019` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2019` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2019` / Base_Populacional_Masculina_2022_Men_30$Men_30[1]) * 100000,
           TRUE                   ~ (`2019` / pop_limpa) * 100000),
         Inc_2019 = round(Inc_2019, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020 %>%
    mutate(`2020` = rowSums(across(c(X0.4, X5.9, X10.14, X15.19, X20.24, X25.29)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2020`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2020` = as.numeric(`2020`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1]),
         Inc_2020 = case_when(
           Grupo_CID == "Mama"    ~ (`2020` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2020` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2020` / Base_Populacional_Masculina_2022_Men_30$Men_30[1]) * 100000,
           TRUE                   ~ (`2020` / pop_limpa) * 100000),
         Inc_2020 = round(Inc_2020, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021 %>%
    mutate(`2021` = rowSums(across(c(X0.4, X5.9, X10.14, X15.19, X20.24, X25.29)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2021`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2021` = as.numeric(`2021`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1]),
         Inc_2021 = case_when(
           Grupo_CID == "Mama"    ~ (`2021` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2021` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2021` / Base_Populacional_Masculina_2022_Men_30$Men_30[1]) * 100000,
           TRUE                   ~ (`2021` / pop_limpa) * 100000),
         Inc_2021 = round(Inc_2021, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022 %>%
    mutate(`2022` = rowSums(across(c(X0.4, X5.9, X10.14, X15.19, X20.24, X25.29)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2022`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2022` = as.numeric(`2022`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1]),
         Inc_2022 = case_when(
           Grupo_CID == "Mama"    ~ (`2022` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2022` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2022` / Base_Populacional_Masculina_2022_Men_30$Men_30[1]) * 100000,
           TRUE                   ~ (`2022` / pop_limpa) * 100000),
         Inc_2022 = round(Inc_2022, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023 %>%
    mutate(`2023` = rowSums(across(c(X0.4, X5.9, X10.14, X15.19, X20.24, X25.29)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2023`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2023` = as.numeric(`2023`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1]),
         Inc_2023 = case_when(
           Grupo_CID == "Mama"    ~ (`2023` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2023` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2023` / Base_Populacional_Masculina_2022_Men_30$Men_30[1]) * 100000,
           TRUE                   ~ (`2023` / pop_limpa) * 100000),
         Inc_2023 = round(Inc_2023, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024 %>%
    mutate(`2024` = rowSums(across(c(X0.4, X5.9, X10.14, X15.19, X20.24, X25.29)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2024`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2024` = as.numeric(`2024`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1]),
         Inc_2024 = case_when(
           Grupo_CID == "Mama"    ~ (`2024` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2024` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2024` / Base_Populacional_Masculina_2022_Men_30$Men_30[1]) * 100000,
           TRUE                   ~ (`2024` / pop_limpa) * 100000),
         Inc_2024 = round(Inc_2024, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  PS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025 %>%
    mutate(`2025` = rowSums(across(c(X0.4, X5.9, X10.14, X15.19, X20.24, X25.29)), na.rm = TRUE)) %>%
    select(Grupo_CID, `2025`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2025` = as.numeric(`2025`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1]),
         Inc_2025 = case_when(
           Grupo_CID == "Mama"    ~ (`2025` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2025` / Base_Populacional_Feminina_2022_Men_30$Men_30[1]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2025` / Base_Populacional_Masculina_2022_Men_30$Men_30[1]) * 100000,
           TRUE                   ~ (`2025` / pop_limpa) * 100000),
         Inc_2025 = round(Inc_2025, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- AUX %>%
  mutate(Grupo_CID = case_when(
    Grupo_CID == "Cerebro_SNC" ~ "Cérebro e SNC",
    Grupo_CID == "Linfatico_Hematologico" ~ "Linfático e Hematológico",
    Grupo_CID == "Digestivo" ~ "Aparelho Digestivo",
    Grupo_CID == "Respiratorio" ~ "Aparelho Respiratório",
    Grupo_CID == "Gen_Fem" ~ "Órgãos Genitais Femininos",
    Grupo_CID == "Gen_Masc" ~ "Órgãos Genitais Masculinos",
    Grupo_CID == "Mama" ~ "Mama",
    Grupo_CID == "Via_Urinaria" ~ "Vias Urinárias",
    Grupo_CID == "Pele" ~ "Pele (Melanoma/Outros)",
    Grupo_CID == "Labio_Oral" ~ "Lábio e Cavidade Oral",
    Grupo_CID == "Ossos" ~ "Ossos e Cartilagens",
    Grupo_CID == "Tec_Mole" ~ "Tecidos Moles",
    Grupo_CID == "Tireoide_Endo" ~ "Tireoide e Endócrinas",
    Grupo_CID == "Mal_Definidas" ~ "Mal Definidas/Outras",
    TRUE ~ Grupo_CID 
  ))

PR_PEVASPEA_SIM_TAB_NEOPLASIAS_GRUPOS_Men_30 <- gt(AUX) %>%
  tab_header(
    title = md("**Incidência de Neoplasias Malignas por Localização em  População de 30 a 69 anos**"),
    subtitle = md("Paraná, 2016 – 2025")
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
  tab_spanner(label = "2016",
              columns = c(2:3),
              id = "1") %>%
  tab_spanner(label = "2017",
              columns = c(4:5),
              id = "2") %>%
  tab_spanner(label = "2018",
              columns = c(6:7),
              id = "3") %>%
  tab_spanner(label = "2019",
              columns = c(8:9),
              id = "4") %>%
  tab_spanner(label = "2020",
              columns = c(10:11),
              id = "5") %>%
  tab_spanner(label = "2021",
              columns = c(12:13),
              id = "6") %>%
  tab_spanner(label = "2022",
              columns = c(14:15),
              id = "7") %>%
  tab_spanner(label = "2023",
              columns = c(16:17),
              id = "8") %>%
  tab_spanner(label = "2024",
              columns = c(18:19),
              id = "9") %>%
  tab_spanner(label = "2025",
              columns = c(20:21),
              id = "10") %>%
  cols_align(align = "left", columns = 1) %>%
  cols_align(align = "center", columns = 2:21) %>%
  cols_label(contains("Inc_")     ~ "Inc.",
             matches("^20\\d{2}$") ~ "n"
  ) %>%
  fmt_number(
    columns = contains("Inc_"),
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  sub_missing(columns = everything(), missing_text = "-") %>%
  tab_footnote(
    footnote = "Fonte: Sistema de Informações de Mortalidade. Base DBF acessada em 04/05/2026."
  ) %>%
  tab_footnote(
    footnote = "Nota¹:Incidência calculada por 100.000 habitantes (IBGE Censo 2022)."
  ) %>%
  tab_footnote(
    footnote = "Nota²: Incidência de câncer de mama e genitais femininos calculada usando a população feminina do Censo de 2022."
  ) %>%
  tab_footnote(
    footnote = "Nota³: Incidência de câncer de genitais masculinos calculada usando a população masculina do Censo de 2022."
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4F4F4"),
    locations = cells_body(columns = c(4:5, 8:9, 12:13, 16:17, 20:21)) 
  ) %>%
  tab_options(footnotes.padding = px(1),
              footnotes.font.size = px(10))

Total <- data.frame(
  Grupo_CID = "TOTAL",
  `2016` = sum(AUX$`2016`, na.rm = TRUE),
  `2017` = sum(AUX$`2017`, na.rm = TRUE),
  `2018` = sum(AUX$`2018`, na.rm = TRUE),
  `2019` = sum(AUX$`2019`, na.rm = TRUE),
  `2020` = sum(AUX$`2020`, na.rm = TRUE),
  `2021` = sum(AUX$`2021`, na.rm = TRUE),
  `2022` = sum(AUX$`2022`, na.rm = TRUE),
  `2023` = sum(AUX$`2023`, na.rm = TRUE),
  `2024` = sum(AUX$`2024`, na.rm = TRUE),
  `2025` = sum(AUX$`2025`, na.rm = TRUE),
  check.names = FALSE 
)

Total$Inc_2016 <- round((Total$`2016` / as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1])) * 100000, 2)
Total$Inc_2017 <- round((Total$`2017` / as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1])) * 100000, 2)
Total$Inc_2018 <- round((Total$`2018` / as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1])) * 100000, 2)
Total$Inc_2019 <- round((Total$`2019` / as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1])) * 100000, 2)
Total$Inc_2020 <- round((Total$`2020` / as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1])) * 100000, 2)
Total$Inc_2021 <- round((Total$`2021` / as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1])) * 100000, 2)
Total$Inc_2022 <- round((Total$`2022` / as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1])) * 100000, 2)
Total$Inc_2023 <- round((Total$`2023` / as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1])) * 100000, 2)
Total$Inc_2024 <- round((Total$`2024` / as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1])) * 100000, 2)
Total$Inc_2025 <- round((Total$`2025` / as.numeric(Base_Populacional_Feminina_2022_Men_30$Men_30[1] + Base_Populacional_Masculina_2022_Men_30$Men_30[1])) * 100000, 2)

Total_Inc <- Total %>% 
  select(contains("Inc")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    Incidencia = as.numeric(V1)     
  ) %>%
  select(Ano, Incidencia)

Total_N <- Total %>% 
  select(matches("^20(1[6-9]|2[0-9])$")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    N = as.numeric(V1)     
  ) %>%
  select(Ano, N)

Total <- left_join(Total_Inc,
                   Total_N,
                   by = "Ano")

fator_escala <- (max(Total$N, na.rm = TRUE) / 8)

PR_PEVASPEA_SIM_GRAF_NEOPLASIAS_Incidencia_Men_30 <- ggplot(Total, 
                                                           aes(x = Ano, y = Incidencia)) + 
  geom_col(aes(y = N / fator_escala), 
           fill = "#dcdde1", 
           width = 0.6)+
  geom_text(aes(y = N / fator_escala / 2, 
                label = format(N, big.mark = ".")), 
            size = 4, 
            fontface = "bold") +
  geom_line(aes(x = Ano,
                y = Incidencia,
                group = 1),
            colour = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(y = "Casos/100.000 habitantes",
       x = NULL,
       title = "Mortalidade por Câncer/100.000 habitantes no Paraná") +
  geom_text(aes(label = format(round(Incidencia, 2),
                               decimal.mark = ",")), 
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(0, 15), 
                     breaks = seq(0, 15, 5),
                     sec.axis = sec_axis(~ . * fator_escala, 
                                         name = "Casos",
                                         labels = label_number(big.mark = "."))) +
  scale_x_discrete(breaks = 2016:2025) +
  Theme()

#### Mesmo procedimento para menores de 30 anos Regional

Base_Populacional_Masculina_2022_Men_30_RS <- Base_Populacional_Masculina_2022 %>%
  filter(MUN_ESTADO %in% c(
    "ARAPUÃ", "ARIRANHA DO IVAÍ", "CÂNDIDO DE ABREU", "CRUZMALTINA",
    "GODOY MOREIRA", "IVAIPORÃ", "JARDIM ALEGRE", "LIDIANÓPOLIS",
    "LUNARDELLI", "MANOEL RIBAS", "MATO RICO", "NOVA TEBAS",
    "RIO BRANCO DO IVAÍ", "ROSÁRIO DO IVAÍ", "SANTA MARIA DO OESTE", 
    "SÃO JOÃO DO IVAÍ"
  )) %>% 
  mutate(across(c(Menores.de.1.ano, X1.a.4.anos, X5.a.9.anos, X10.a.14.anos,
                  X15.a.19.anos, X20.a.24.anos, X25.a.29.anos),
                ~ as.numeric(gsub("\\.", "", .x)))) %>%
  mutate(Men_30 = rowSums(across(c(Menores.de.1.ano, X1.a.4.anos, X5.a.9.anos, X10.a.14.anos,
                                   X15.a.19.anos, X20.a.24.anos, X25.a.29.anos)), na.rm = TRUE)
  )

Base_Populacional_Masculina_2022_Men_30_RS$Total <- as.numeric(gsub("\\.", "", Base_Populacional_Masculina_2022_Men_30_RS$Total))

linha_total_masc_regional <- Base_Populacional_Masculina_2022_Men_30_RS %>%
  summarise(
    MUN_ESTADO = "TOTAL REGIONAL",
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    Até.14.anos = NA,
    X15.a.64.anos = NA
  )

Base_Populacional_Masculina_2022_Men_30_RS <- bind_rows(
  Base_Populacional_Masculina_2022_Men_30_RS, 
  linha_total_masc_regional
)

Base_Populacional_Feminina_2022_Men_30_RS <- Base_Populacional_Feminina_2022 %>%
  filter(MUN_ESTADO %in% c(
    "ARAPUÃ", "ARIRANHA DO IVAÍ", "CÂNDIDO DE ABREU", "CRUZMALTINA",
    "GODOY MOREIRA", "IVAIPORÃ", "JARDIM ALEGRE", "LIDIANÓPOLIS",
    "LUNARDELLI", "MANOEL RIBAS", "MATO RICO", "NOVA TEBAS",
    "RIO BRANCO DO IVAÍ", "ROSÁRIO DO IVAÍ", "SANTA MARIA DO OESTE", 
    "SÃO JOÃO DO IVAÍ"
  )) %>% 
  mutate(across(c(Menores.de.1.ano, X1.a.4.anos, X5.a.9.anos, X10.a.14.anos,
                  X15.a.19.anos, X20.a.24.anos, X25.a.29.anos),
                ~ as.numeric(gsub("\\.", "", .x)))) %>%
  mutate(
    Men_30 = rowSums(across(c(Menores.de.1.ano, X1.a.4.anos, X5.a.9.anos, X10.a.14.anos,
                              X15.a.19.anos, X20.a.24.anos, X25.a.29.anos)), na.rm = TRUE)
  )

Base_Populacional_Feminina_2022_Men_30_RS$Total <- as.numeric(gsub("\\.", "", Base_Populacional_Feminina_2022_Men_30_RS$Total))

linha_total_fem_regional <- Base_Populacional_Feminina_2022_Men_30_RS %>%
  summarise(
    MUN_ESTADO = "TOTAL REGIONAL",
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    Até.14.anos = NA,
    X15.a.64.anos = NA
  )

Base_Populacional_Feminina_2022_Men_30_RS <- bind_rows(
  Base_Populacional_Feminina_2022_Men_30_RS, 
  linha_total_fem_regional
)

AUX <-  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2016 %>%
  mutate(`2016` = rowSums(across(any_of(c("X0.4", "X5.9", "X5.a.9.anos", "X10.14",
                                          "X15.19", "X20.24", "X25.29"))), na.rm = TRUE)
  ) %>%
  select(Grupo_CID, 
         `2016`) %>%
  mutate(pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]),
         Inc_2016 = case_when(
           Grupo_CID == "Mama"    ~ (`2016` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2016` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2016` / Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]) * 100000,
           TRUE                   ~ (`2016` / pop_limpa) * 100000),
         Inc_2016 = round(Inc_2016, 2)) %>%
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2017 %>%
    mutate(`2017` = rowSums(across(any_of(c("X0.4", "X5.9", "X5.a.9.anos", "X10.14",
                                            "X15.19", "X20.24", "X25.29"))), na.rm = TRUE)) %>% 
    select(Grupo_CID, `2017`), 
  by = "Grupo_CID"
)  

AUX <- AUX %>%
  mutate(`2017` = as.numeric(`2017`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]),
         Inc_2017 = case_when(
           Grupo_CID == "Mama"    ~ (`2017` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2017` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2017` / Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]) * 100000,
           TRUE                   ~ (`2017` / pop_limpa) * 100000),
         Inc_2017 = round(Inc_2017, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2018 %>%
    mutate(`2018` = rowSums(across(any_of(c("X0.4", "X5.9", "X5.a.9.anos", "X10.14",
                                            "X15.19", "X20.24", "X25.29"))), na.rm = TRUE)) %>% 
    select(Grupo_CID, `2018`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2018` = as.numeric(`2018`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]),
         Inc_2018 = case_when(
           Grupo_CID == "Mama"    ~ (`2018` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2018` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2018` / Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]) * 100000,
           TRUE                   ~ (`2018` / pop_limpa) * 100000),
         Inc_2018 = round(Inc_2018, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2019 %>%
    mutate(`2019` = rowSums(across(any_of(c("X0.4", "X5.9", "X5.a.9.anos", "X10.14",
                                            "X15.19", "X20.24", "X25.29"))), na.rm = TRUE)) %>%    
    select(Grupo_CID, `2019`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2019` = as.numeric(`2019`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]),
         Inc_2019 = case_when(
           Grupo_CID == "Mama"    ~ (`2019` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2019` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2019` / Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]) * 100000,
           TRUE                   ~ (`2019` / pop_limpa) * 100000),
         Inc_2019 = round(Inc_2019, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2020 %>%
    mutate(`2020` = rowSums(across(any_of(c("X0.4", "X5.9", "X5.a.9.anos", "X10.14",
                                            "X15.19", "X20.24", "X25.29"))), na.rm = TRUE)) %>% 
    select(Grupo_CID, `2020`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2020` = as.numeric(`2020`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]),
         Inc_2020 = case_when(
           Grupo_CID == "Mama"    ~ (`2020` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2020` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2020` / Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]) * 100000,
           TRUE                   ~ (`2020` / pop_limpa) * 100000),
         Inc_2020 = round(Inc_2020, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2021 %>%
    mutate(`2021` = rowSums(across(any_of(c("X0.4", "X5.9", "X5.a.9.anos", "X10.14",
                                            "X15.19", "X20.24", "X25.29"))), na.rm = TRUE)) %>% 
    select(Grupo_CID, `2021`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2021` = as.numeric(`2021`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]),
         Inc_2021 = case_when(
           Grupo_CID == "Mama"    ~ (`2021` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2021` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2021` / Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]) * 100000,
           TRUE                   ~ (`2021` / pop_limpa) * 100000),
         Inc_2021 = round(Inc_2021, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2022 %>%
    mutate(`2022` = rowSums(across(any_of(c("X0.4", "X5.9", "X5.a.9.anos", "X10.14",
                                            "X15.19", "X20.24", "X25.29"))), na.rm = TRUE)) %>%
    select(Grupo_CID, `2022`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2022` = as.numeric(`2022`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]),
         Inc_2022 = case_when(
           Grupo_CID == "Mama"    ~ (`2022` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2022` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2022` / Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]) * 100000,
           TRUE                   ~ (`2022` / pop_limpa) * 100000),
         Inc_2022 = round(Inc_2022, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2023 %>%
    mutate(`2023` = rowSums(across(any_of(c("X0.4", "X5.9", "X5.a.9.anos", "X10.14",
                                            "X15.19", "X20.24", "X25.29"))), na.rm = TRUE)) %>%
    select(Grupo_CID, `2023`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2023` = as.numeric(`2023`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]),
         Inc_2023 = case_when(
           Grupo_CID == "Mama"    ~ (`2023` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2023` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2023` / Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]) * 100000,
           TRUE                   ~ (`2023` / pop_limpa) * 100000),
         Inc_2023 = round(Inc_2023, 2)
  ) %>% 
  select(-pop_limpa)


AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2024 %>%
    mutate(`2024` = rowSums(across(any_of(c("X0.4", "X5.9", "X5.a.9.anos", "X10.14",
                                            "X15.19", "X20.24", "X25.29"))), na.rm = TRUE)) %>%
    select(Grupo_CID, `2024`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2024` = as.numeric(`2024`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]),
         Inc_2024 = case_when(
           Grupo_CID == "Mama"    ~ (`2024` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2024` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2024` / Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]) * 100000,
           TRUE                   ~ (`2024` / pop_limpa) * 100000),
         Inc_2024 = round(Inc_2024, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- left_join(
  AUX,
  RS_PEVASPEA_SIM_NEOPLASIA_IDADE_2025 %>%
    mutate(`2025` = rowSums(across(any_of(c("X0.4", "X5.9", "X5.a.9.anos", "X10.14",
                                            "X15.19", "X20.24", "X25.29"))), na.rm = TRUE)) %>%
    select(Grupo_CID, `2025`), 
  by = "Grupo_CID"
) 

AUX <- AUX %>%
  mutate(`2025` = as.numeric(`2025`),
         pop_limpa = as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]),
         Inc_2025 = case_when(
           Grupo_CID == "Mama"    ~ (`2025` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Fem" ~ (`2025` / Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17]) * 100000,
           Grupo_CID == "Gen_Masc" ~ (`2025` / Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17]) * 100000,
           TRUE                   ~ (`2025` / pop_limpa) * 100000),
         Inc_2025 = round(Inc_2025, 2)
  ) %>% 
  select(-pop_limpa)

AUX <- AUX %>%
  mutate(Grupo_CID = case_when(
    Grupo_CID == "Cerebro_SNC" ~ "Cérebro e SNC",
    Grupo_CID == "Linfatico_Hematologico" ~ "Linfático e Hematológico",
    Grupo_CID == "Digestivo" ~ "Aparelho Digestivo",
    Grupo_CID == "Respiratorio" ~ "Aparelho Respiratório",
    Grupo_CID == "Gen_Fem" ~ "Órgãos Genitais Femininos",
    Grupo_CID == "Gen_Masc" ~ "Órgãos Genitais Masculinos",
    Grupo_CID == "Mama" ~ "Mama",
    Grupo_CID == "Via_Urinaria" ~ "Vias Urinárias",
    Grupo_CID == "Pele" ~ "Pele (Melanoma/Outros)",
    Grupo_CID == "Labio_Oral" ~ "Lábio e Cavidade Oral",
    Grupo_CID == "Ossos" ~ "Ossos e Cartilagens",
    Grupo_CID == "Tec_Mole" ~ "Tecidos Moles",
    Grupo_CID == "Tireoide_Endo" ~ "Tireoide e Endócrinas",
    Grupo_CID == "Mal_Definidas" ~ "Mal Definidas/Outras",
    TRUE ~ Grupo_CID 
  ))

RS_PEVASPEA_SIM_TAB_NEOPLASIAS_GRUPOS_Men_30 <- gt(AUX) %>%
  tab_header(
    title = md("**Incidência de Neoplasias Malignas por Localização em  População Menor de 30 anos**"),
    subtitle = md("22ª Regional de Saúde, 2016 – 2025")
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
  tab_spanner(label = "2016",
              columns = c(2:3),
              id = "1") %>%
  tab_spanner(label = "2017",
              columns = c(4:5),
              id = "2") %>%
  tab_spanner(label = "2018",
              columns = c(6:7),
              id = "3") %>%
  tab_spanner(label = "2019",
              columns = c(8:9),
              id = "4") %>%
  tab_spanner(label = "2020",
              columns = c(10:11),
              id = "5") %>%
  tab_spanner(label = "2021",
              columns = c(12:13),
              id = "6") %>%
  tab_spanner(label = "2022",
              columns = c(14:15),
              id = "7") %>%
  tab_spanner(label = "2023",
              columns = c(16:17),
              id = "8") %>%
  tab_spanner(label = "2024",
              columns = c(18:19),
              id = "9") %>%
  tab_spanner(label = "2025",
              columns = c(20:21),
              id = "10") %>%
  cols_align(align = "left", columns = 1) %>%
  cols_align(align = "center", columns = 2:21) %>%
  cols_label(contains("Inc_")     ~ "Inc.",
             matches("^20\\d{2}$") ~ "n"
  ) %>%
  fmt_number(
    columns = contains("Inc_"),
    decimals = 2,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  sub_missing(columns = everything(), missing_text = "-") %>%
  tab_footnote(
    footnote = "Fonte: Sistema de Informações de Mortalidade. Base DBF acessada em 04/05/2026."
  ) %>%
  tab_footnote(
    footnote = "Nota¹:Incidência calculada por 100.000 habitantes (IBGE Censo 2022)."
  ) %>%
  tab_footnote(
    footnote = "Nota²: Incidência de câncer de mama e genitais femininos calculada usando a população feminina do Censo de 2022."
  ) %>%
  tab_footnote(
    footnote = "Nota³: Incidência de câncer de genitais masculinos calculada usando a população masculina do Censo de 2022."
  ) %>%
  tab_style(
    style = cell_fill(color = "#F4F4F4"),
    locations = cells_body(columns = c(4:5, 8:9, 12:13, 16:17, 20:21)) 
  ) %>%
  tab_options(footnotes.padding = px(1),
              footnotes.font.size = px(10))

Total <- data.frame(
  Grupo_CID = "TOTAL",
  `2016` = sum(AUX$`2016`, na.rm = TRUE),
  `2017` = sum(AUX$`2017`, na.rm = TRUE),
  `2018` = sum(AUX$`2018`, na.rm = TRUE),
  `2019` = sum(AUX$`2019`, na.rm = TRUE),
  `2020` = sum(AUX$`2020`, na.rm = TRUE),
  `2021` = sum(AUX$`2021`, na.rm = TRUE),
  `2022` = sum(AUX$`2022`, na.rm = TRUE),
  `2023` = sum(AUX$`2023`, na.rm = TRUE),
  `2024` = sum(AUX$`2024`, na.rm = TRUE),
  `2025` = sum(AUX$`2025`, na.rm = TRUE),
  check.names = FALSE 
)

Total$Inc_2016 <- round((Total$`2016` / as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17])) * 100000, 2)
Total$Inc_2017 <- round((Total$`2017` / as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17])) * 100000, 2)
Total$Inc_2018 <- round((Total$`2018` / as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17])) * 100000, 2)
Total$Inc_2019 <- round((Total$`2019` / as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17])) * 100000, 2)
Total$Inc_2020 <- round((Total$`2020` / as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17])) * 100000, 2)
Total$Inc_2021 <- round((Total$`2021` / as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17])) * 100000, 2)
Total$Inc_2022 <- round((Total$`2022` / as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17])) * 100000, 2)
Total$Inc_2023 <- round((Total$`2023` / as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17])) * 100000, 2)
Total$Inc_2024 <- round((Total$`2024` / as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17])) * 100000, 2)
Total$Inc_2025 <- round((Total$`2025` / as.numeric(Base_Populacional_Feminina_2022_Men_30_RS$Men_30[17] + Base_Populacional_Masculina_2022_Men_30_RS$Men_30[17])) * 100000, 2)

Total_Inc <- Total %>% 
  select(contains("Inc")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    Incidencia = as.numeric(V1)     
  ) %>%
  select(Ano, Incidencia)

Total_N <- Total %>% 
  select(matches("^20(1[6-9]|2[0-9])$")) %>%
  t() %>%
  as.data.frame() %>% 
  mutate(
    Ano = as.factor(seq(2016, 2016 + n() - 1)),
    N = as.numeric(V1)     
  ) %>%
  select(Ano, N)

Total <- left_join(Total_Inc,
                   Total_N,
                   by = "Ano")

fator_escala <- (max(Total$N, na.rm = TRUE) / 5)

RS_PEVASPEA_SIM_GRAF_NEOPLASIAS_Incidencia_Men_30 <- ggplot(Total, 
                                                           aes(x = Ano, y = Incidencia)) + 
  geom_col(aes(y = N / fator_escala), 
           fill = "#dcdde1", 
           width = 0.6)+
  geom_text(aes(y = N / fator_escala / 2, 
                label = format(N, big.mark = ".")), 
            size = 4, 
            fontface = "bold") +
  geom_line(aes(x = Ano,
                y = Incidencia,
                group = 1),
            colour = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  labs(y = "Casos/100.000 habitantes",
       x = NULL,
       title = "Mortalidade por Câncer em População Menor de 30 Anos/100.000 habitantes e Número de Casos na 22ª RS") +
  geom_text(aes(label = format(round(Incidencia, 2),
                               decimal.mark = ",")), 
            size = 4, 
            vjust = -2, 
            fontface = "bold")  + 
  scale_y_continuous(limits = c(0, 20), 
                     breaks = seq(0, 20, 5),
                     sec.axis = sec_axis(~ . * fator_escala, 
                                         name = "Casos",
                                         breaks = c(0, 100, 200, 300),
                                         labels = c("0", "100", "200", "300"))) +
  scale_x_discrete(breaks = 2016:2025) +
  Theme()

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
                    plot.caption = element_text(hjust = 0, 
                                                face = "italic", 
                                                size = 10)
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
                    plot.caption = element_text(hjust = 0, 
                                                face = "italic", 
                                                size = 10)
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
                    plot.caption = element_text(hjust = 0, 
                                                face = "italic",
                                                size = 10)
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

MAPA_BASE_Mun$Lisa_resultado_17_20 <- factor(MAPA_BASE_Mun$Lisa_resultado_17_20,
                                             levels = Niveis_LISA)

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

PR_PEVASPEA_DERAL_LOCAL_MORAN_16_20_21_25 <- PR_PEVASPEA_DERAL_LOCAL_MORAN_17_20 + 
  PR_PEVASPEA_DERAL_LOCAL_MORAN_21_24 + 
  plot_layout(ncol = 2, guides = "collect") + 
  plot_annotation(
    title = "Progressão de Agrupamentos do Consumo de Agrotóxico/Hectare no Paraná",
    subtitle = 'Comparativo entre os quadriênios 2017-2020 e 2021-2024',
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


#### Dados Regional

AUX <- PR_DERAL_GERAL %>%
  filter(RS == 22) 

AUX01 <- AUX %>%
  summarise(., across(where(is.numeric), sum, na.rm = TRUE))

AUX02 <- AUX01 %>%
  select(contains("AREA_HA")) %>% 
  pivot_longer(cols = contains("AREA_HA"),
               names_to = "Variavel", 
               values_to = "Total") 

RS_DERAL_GRAF_HA_CULTIVADO <- ggplot(AUX02, aes(x = str_replace(Variavel, "AREA_HA_", ""), 
                                                y = Total,
                                                group = 1)) +
  geom_line(color = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  geom_text(aes(label = comma(Total, 
                              accuracy = 0.01,    
                              decimal.mark = ",", 
                              big.mark = ".")), 
            vjust = -1.5, 
            size = 3.5,
            fontface = "bold") + 
  labs(caption = Fonte1, 
       y = "Hectares Cultivados",
       x = NULL,
       title = "Total de Hectares Cultivados na 22ª RS - (2016 - 2024)",
       subtitle = "Cultivos de interesse") +
  scale_y_continuous(limits = c(200000, 1000000), 
                     labels = label_number(decimal.mark = ",", 
                                           big.mark = "."),
                     expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

####  TON AGRO

AUX03 <- AUX01 %>%
  select(contains("TON_AGRO")) %>% 
  pivot_longer(cols = contains("TON_AGRO"),
               names_to = "Variavel_1", 
               values_to = "Total_1") 

RS_DERAL_GRAF_TON_AGRO_CULTIVADO <- ggplot(AUX03, aes(x = str_replace(Variavel_1, "AREA_HA_", ""), 
                                                      y = Total_1,
                                                      group = 1)) +
  geom_line(color = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) + 
  geom_text(aes(label = comma(Total_1, 
                              accuracy = 0.01,    
                              decimal.mark = ",", 
                              big.mark = ".")), 
            vjust = -1.5, 
            size = 3.5,
            fontface = "bold") + 
  labs(caption = Fonte2, 
       y = "Toneladas",
       x = NULL,
       title = "Consumo de Agrotóxicos em Toneladas na 22ª RS - (2016 - 2024)") +
  scale_y_continuous(limits = c(500, 8000), 
                     labels = label_number(decimal.mark = ",", 
                                           big.mark = "."),
                     expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

####  Agro/HA

AUX02[, 3:4] <- AUX03 

AUX02[,5] <- (AUX02$Total_1/AUX02$Total) *1000

colnames(AUX02)[5] <- "AGRO_HA"

glimpse(AUX02)

RS_DERAL_GRAF_AGRO_HA_CULTIVADO <- ggplot(AUX02, aes(x = str_replace(Variavel, "AREA_HA_", ""), 
                                                     y = AGRO_HA,
                                                     group = 1)) +
  geom_line(color = "black",
            linewidth = 1.3) +
  geom_point(fill = "grey",
             size = 4,
             shape = 21) +
  geom_text(aes(label = comma(AGRO_HA, 
                              accuracy = 0.01,    
                              decimal.mark = ",", 
                              big.mark = ".")), 
            vjust = -1.5, 
            size = 3.5,
            fontface = "bold") + 
  labs(caption = Fonte1, 
       y = "Kg/Hectare",
       x = NULL,
       title = "Uso de Agrotóxicos/Hectare na 22ª RS - (2016 - 2024)",
       subtitle = "Cultivos de interesse") +
  scale_y_continuous(limits = c(3, 10), 
                     labels = label_number(decimal.mark = ",", 
                                           big.mark = "."),
                     expand = expansion(mult = c(0.2, 0.2))) +
  Theme()

###### SCATTERPLOT INCIDÊNCIA/CONSUMO DE HA


library(ggrepel)
AUX <- PR_PEVASPEA_SINASC_Serie_historica_Mun[, 1]

AUX <- PR_PEVASPEA_SINASC_Serie_historica_Mun %>%
  mutate(TAXA_4a_21_24 = ((PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2024 + 
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2023 +
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2022 +
                             PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2021)/
                            (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2024 + 
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2023 +
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2022 +
                               PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2021 ) *
                            1000), 
         Nascidos_4a_21_24 = (PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2024 + 
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2023 +
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2022 +
                                PR_PEVASPEA_SINASC_Serie_historica_Mun$Nascidos_2021 ),
         Anomalias_4a_21_24 = (PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2024 + 
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2023 +
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2022 +
                                 PR_PEVASPEA_SINASC_Serie_historica_Mun$N_2021)
  )

MAPA_BASE_PR <- left_join(MAPA_BASE_PR, 
                          AUX %>% 
                            select(Município_sem_Código, 35, 36, 37),
                          by = c("NM_MUN" = "Município_sem_Código"))

Taxa_Suavizada <- EBlocal(ri = MAPA_BASE_PR$Anomalias_4a_21_24,
                          ni = MAPA_BASE_PR$Nascidos_4a_21_24,
                          nb = Matriz_Viz_MAPA_BASE_PR)

MAPA_BASE_PR$TAXA_RAW_21_24 <- Taxa_Suavizada$raw * 1000

MAPA_BASE_PR$TAXA_EST_21_24 <- Taxa_Suavizada$est * 1000

Global_Moran_18_21 <- moran.test(MAPA_BASE_PR$TAXA_EST_21_24,
                                 Matriz_Viz_Pesos)

MAPA_BASE_PR <- MAPA_BASE_PR %>%
  mutate(CD_MUN = substr(as.character(CD_MUN), 1, 6))


AUX<- PR_DERAL_GERAL %>%
  select(Código_IBGE, Município_sem_Código, contains("AREA_HA"), contains("TON_AGRO")) %>%
  mutate(AGRO_HA_21_24 = (TON_AGRO_2021 +TON_AGRO_2022 + TON_AGRO_2023 + TON_AGRO_2024)/
           (AREA_HA_2021 + AREA_HA_2022 + AREA_HA_2023 + AREA_HA_2024) * 1000)

AUX$Código_IBGE <- as.character(AUX$Código_IBGE)

MAPA_BASE_PR <- left_join(MAPA_BASE_PR,
                          AUX %>%
                            filter(!Município_sem_Código == "PARANAGUA") %>%
                            select(Código_IBGE, AGRO_HA_21_24),
                          by = c("CD_MUN" = "Código_IBGE"))
ggplot(st_drop_geometry(MAPA_BASE_PR), 
       aes(x = AGRO_HA_21_24, 
           y = TAXA_EST_21_24)) +
  geom_point(color = "#4A5568", 
             alpha = 0.6, 
             size = 2) +
  geom_smooth(method = "lm", 
              color = "#E53E3E", 
              linetype = "solid", 
              size = 1, 
              se = TRUE, 
              fill = "gray85") +
  labs(
    title = "Análise de Dispersão: Carga de Agrotóxicos vs. Anomalias Congênitas no Paraná",
    subtitle = "Dados do SINASC Suavizados por Estimador Bayesiano Local (2021-2024)",
    x = "Kg de Agrotóxico/Hectare",
    y = "Anomalias/1.000 Nascidos Vivos"
  ) +
  Theme()

##### Salvando objetos

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/DERAL/RS_DERAL_GRAF_AGRO_HA_CULTIVADO.png",
       RS_DERAL_GRAF_AGRO_HA_CULTIVADO,
       width = 25,
       height = 15,
       units = "cm",
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/DERAL/RS_DERAL_GRAF_HA_CULTIVADO.png",
       RS_DERAL_GRAF_HA_CULTIVADO,
       width = 25,
       height = 15,
       units = "cm",
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/DERAL/RS_DERAL_GRAF_TON_AGRO_CULTIVADO.png",
       RS_DERAL_GRAF_TON_AGRO_CULTIVADO,
       width = 25,
       height = 15,
       units = "cm",
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/DERAL/PR_DERAL_GRAF_HA_CULTIVADO.png",
       PR_DERAL_GRAF_HA_CULTIVADO,
       width = 25,
       height = 15,
       units = "cm",
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/DERAL/PR_DERAL_GRAF_AGRO_HA.png",
       PR_DERAL_GRAF_AGRO_HA,
       width = 25,
       height = 15,
       units = "cm",
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/DERAL/PR_DERAL_GRAF_TON_AGRO.png",
       PR_DERAL_GRAF_TON_AGRO,
       width = 25,
       height = 15,
       units = "cm",
       bg = "white")

ggsave("/home/gustavo/Área de trabalho/Análise_de_Dados/Imagens/DERAL/R_DERAL_GRAF_PRODUCAO.png",
       PR_DERAL_GRAF_PRODUCAO,
       width = 25,
       height = 15,
       units = "cm",
       bg = "white")

ggsave(filename = "Imagens/DERAL/PR_DERAL_MAP_AGRO_HA_17_20_21_24.png", 
       plot = PR_DERAL_MAP_AGRO_HA_17_20_21_24, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/DERAL/PR_DERAL_MAP_AGRO_HA_Mun_17_20_21_24.png", 
       plot = PR_DERAL_MAP_AGRO_HA_Mun_17_20_21_24, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/DERAL/PR_PEVASPEA_DERAL_LOCAL_MORAN_16_20_21_25.png", 
       plot = PR_PEVASPEA_DERAL_LOCAL_MORAN_16_20_21_25, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/DERAL/PR_DERAL_MAP_AGRO_HA_17_20_21_24.png", 
       plot = PR_DERAL_MAP_AGRO_HA_17_20_21_24, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/DERAL/PR_DERAL_MAP_HA_17_20_21_24.png", 
       plot = PR_DERAL_MAP_HA_17_20_21_24, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/DERAL/PR_DERAL_MAP_AGRO_HA_Mun_17_20_21_24.png", 
       plot = PR_DERAL_MAP_AGRO_HA_Mun_17_20_21_24, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/DERAL/PR_DERAL_MAP_TON_AGRO_Mun_17_20_21_24.png", 
       plot = PR_DERAL_MAP_TON_AGRO_Mun_17_20_21_24, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/DERAL/PR_DERAL_MAP_HA_Mun_17_20_21_24.png", 
       plot = PR_DERAL_MAP_HA_Mun_17_20_21_24, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/DERAL/PR_DERAL_MAP_AGRO_HA_Mun_17_20_21_24.png", 
       plot = PR_DERAL_MAP_AGRO_HA_Mun_17_20_21_24, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

ggsave(filename = "Imagens/DERAL/PR_DERAL_MAP_TON_AGRO_17_20_21_24.png", 
       plot = PR_DERAL_MAP_TON_AGRO_17_20_21_24, 
       width = 35,                               
       height = 18,                               
       units = "cm",                               
       dpi = 300,                                   
       bg = "white"                                
)

rm(Fonte,
   Fonte1,
   Fonte2,
   Fonte3,
   labels_anos,
   Legenda,
   max_global,
   Niveis_LISA,
   quebras_anos,
   Theme,
   Theme_Mun,
   Taxa_Suavizada,
   SHAPEFILE_ESTADUAL,
   SHAPEFILE_ESTADUAL_RS,
   SHAPEFILE_REGIONAL,
   SHAPEFILE_REGIONAL_Dissolvido,
   Matriz_VIZ_Linhas,
   Matriz_Viz_Pesos,
   Matriz_Viz_MAPA_BASE_PR,
   Matriz_Viz_MAPA_BASE_Mun,
   MAPA_BASE,
   MAPA_BASE_Mun,
   MAPA_BASE_PR,
   MAPA_BASE_PR_RS,
   MAPA_BASE_RS,
   Lisa,
   Global_Moran,
   Global_Moran_17_20,
   Global_Moran_18_21,
   Global_Moran_21_24,
   AUX,
   AUX_LIST,
   AUX01,
   AUX02,
   AUX03,
   AUX2018,
   AUX2019,
   AUX2020,
   AUX2021,
   AUX2023,
   AUX2022,
   AUX2024,
   AUX2025,
   quadrantes,
   Centroides_Matriz_VIZ,
   Centroides_Matriz_VIZ_coordenadas)
