#################     Gráficos

############ Criando uma função Theme para ser utilizado por todos os gráficos      ##################################################

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
    theme(legend.position = "bottom",  
          legend.title = element_text(face = "bold",
                                      size = 14), 
          axis.text.x = element_text(angle = 0),
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

#### Anomalias Série Histórica RS

AUX <- RS_SINASC_Serie_Historica[, c(1:4)]

AUX <- mutate(AUX, 
              Taxa_Anomalias = (AUX[, 4]/AUX[, 3]) *1000)

AUX$Taxa_Anomalias <- format(round(AUX[, 5], 2))

AUX$RS <- as.factor(AUX$RS)


RS_SINASC_GRAF_SERIE_HIST_ANOMAL <- ggplot(AUX, aes(x = RS,
                                                        y = Taxa_Anomalias, 
                                                        group = 1)
) +
  geom_line(linewidth = 1.8,
            colour = "black") +
  geom_point(fill = "grey",
             size = 7,
             shape = 21) +
  labs(caption = "Fonte", 
       y = "Anomalias/1000 Nascimentos",
       x = NULL,
       title = "Série Histórica - Anomalias Congênitas (2016 a 2026)",
       subtitle = "Notificações referentes ao município de residência") +
  geom_label(aes(label = Taxa_Anomalias), 
             size = 6, 
             alpha = 0.5,
             vjust = -0.5)  +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.2),
                                        )
  ) +
  Theme() 

####
