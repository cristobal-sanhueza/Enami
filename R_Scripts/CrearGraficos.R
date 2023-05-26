library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)
library(grid)


Volcanes <- read_delim("ENAMI/Volcanes_Oct_Data.csv",
                       delim = ";", escape_double = FALSE, col_types = cols(Fecha = col_character()),
                       locale = locale(decimal_mark = ","),
                       trim_ws = TRUE)


Volcanes <- Volcanes %>% 
  mutate(Semana = case_when(as.numeric(rownames(Volcanes)) <= 180 ~ "Semana 1",
                            as.numeric(rownames(Volcanes)) <= 360 ~ "Semana 2",
                            as.numeric(rownames(Volcanes)) <= 540 ~ "Semana 3",
                            as.numeric(rownames(Volcanes)) <= 720 ~ "Semana 4"))

Volcanes <- Volcanes %>% 
  mutate(Semana = case_when(as.numeric(rownames(Volcanes)) <= 360 ~ "Primera mitad del mes",
                            as.numeric(rownames(Volcanes)) <= 720 ~ "Segunda mitad del mes"))

Volcanes <- Volcanes %>% 
  mutate(MyTime = as.POSIXct(paste(Fecha, Hora), format = "%d-%m-%y %H"))
                            





  
Volcanes2 <- Volcanes[c(3:6)]


Volcanes_Long <- gather(Volcanes2, Tipo, SO2, Observado:Modelado, factor_key = TRUE)


grob <- grobTree(textGrob("Norma Horaria", x=0.85,  y=0.7, hjust=0,
                          gp=gpar(col="red", fontsize=11, fontface="italic")))


Volcanes_Long %>% 
  ggplot(aes(x = MyTime, y = SO2)) +
  geom_line(aes(color = Tipo)) +
  facet_wrap(~Semana, scales = 'free_x', nrow = 2) +
  theme_bw() +
  labs(y = "Concentraciones horarias de SO2(ug/Nm3)") +
  theme(axis.title.x = element_blank()) +
  geom_hline(yintercept =  350, color = "red") +
  annotation_custom(grob)












































































































































































































































































