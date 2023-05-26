library(readr)
library(tidyverse)
library(lubridate)

#### Combine data


## FIRST IMPORT:


## PAIPOTE

Raw_Obs_Paipote_wk1 <- read_csv("ENAMI/Observados/Diciembre/enami-nov29-dic05/RAW_S001T01_202112060804.csv")

Raw_Obs_Paipote_wk2 <- read_csv("ENAMI/Observados/Diciembre/enami-dic06-12/RAW_S001T01_202112130934.csv")

Raw_Obs_Paipote_wk3 <- read_csv("ENAMI/Observados/Diciembre/enami-dic13-19/RAW_S001T01_202112200725.csv")

Raw_Obs_Paipote_wk4 <- read_csv("ENAMI/Observados/Diciembre/enami-dic20-26/RAW_S001T01_202112281040.csv")

#Raw_Obs_Paipote_wk5 <- read_csv("ENAMI/Observados/Diciembre/enami-dic27-ene02/RAW_S001T01_202112060804.csv")


## COPIAPO

Raw_Obs_Copiapo_wk1 <- read_csv("ENAMI/Observados/Diciembre/enami-nov29-dic05/RAW_S001T01_202112060804.csv")

Raw_Obs_Copiapo_wk2 <- read_csv("ENAMI/Observados/Diciembre/enami-dic06-12/RAW_S002T01_202112130935.csv")

Raw_Obs_Copiapo_wk3 <- read_csv("ENAMI/Observados/Diciembre/enami-dic13-19/RAW_S002T01_202112200725.csv")

Raw_Obs_Copiapo_wk4 <- read_csv("ENAMI/Observados/Diciembre/enami-dic20-26/RAW_S002T01_202112281040.csv")

#Raw_Obs_Copiapo_wk5 <- read_csv("ENAMI/Observados/Diciembre/enami-nov29-dic05/RAW_S002T01_202112060804.csv")


## SAN FERNANDO

Raw_Obs_SanFdo_wk1 <- read_csv("ENAMI/Observados/Diciembre/enami-nov29-dic05/RAW_S003T01_202112060804.csv")

Raw_Obs_SanFdo_wk2 <- read_csv("ENAMI/Observados/Diciembre/enami-dic06-12/RAW_S003T01_202112130935.csv")

Raw_Obs_SanFdo_wk3 <- read_csv("ENAMI/Observados/Diciembre/enami-dic13-19/RAW_S003T01_202112200726.csv")

Raw_Obs_SanFdo_wk4 <- read_csv("ENAMI/Observados/Diciembre/enami-dic20-26/RAW_S003T01_202112281039.csv")

#Raw_Obs_SanFdo_wk5 <- read_csv("ENAMI/Observados/Diciembre/enami-nov29-dic05/RAW_S003T01_202112060804.csv")




## TIERRA AMARILLA

Raw_Obs_TA_wk1 <- read_csv("ENAMI/Observados/Diciembre/enami-nov29-dic05/RAW_S004T01_202112060803.csv")

Raw_Obs_TA_wk2 <- read_csv("ENAMI/Observados/Diciembre/enami-dic06-12/RAW_S004T01_202112130935.csv")

Raw_Obs_TA_wk3 <- read_csv("ENAMI/Observados/Diciembre/enami-dic13-19/RAW_S004T01_202112200726.csv")

Raw_Obs_TA_wk4 <- read_csv("ENAMI/Observados/Diciembre/enami-dic20-26/RAW_S004T01_202112281039.csv")

#Raw_Obs_TA_wk5 <- read_csv("ENAMI/Observados/Diciembre/enami-nov29-dic05/RAW_S004T01_202112060803.csv")




## VOLCANES

Raw_Obs_Volcanes_wk1 <- read_csv("ENAMI/Observados/Diciembre/enami-nov29-dic05/RAW_S005T01_202112060803.csv")

Raw_Obs_Volcanes_wk2 <- read_csv("ENAMI/Observados/Diciembre/enami-dic06-12/RAW_S005T01_202112130935.csv")

Raw_Obs_Volcanes_wk3 <- read_csv("ENAMI/Observados/Diciembre/enami-dic13-19/RAW_S005T01_202112200727.csv")

Raw_Obs_Volcanes_wk4 <- read_csv("ENAMI/Observados/Diciembre/enami-dic20-26/RAW_S005T01_202112281039.csv")

#Raw_Obs_Volcanes_wk5 <- read_csv("ENAMI/Observados/Diciembre/enami-nov29-dic05/RAW_S005T01_202112060803.csv")



## NOW COMBINE


Raw_Obs_Paipote <- rbind(Raw_Obs_Paipote_wk1, Raw_Obs_Paipote_wk2, Raw_Obs_Paipote_wk3, Raw_Obs_Paipote_wk4)

Raw_Obs_Paipote <- Raw_Obs_Paipote[2881:40320,]


Raw_Obs_Copiapo <- rbind(Raw_Obs_Copiapo_wk1, Raw_Obs_Copiapo_wk2, Raw_Obs_Copiapo_wk3, Raw_Obs_Copiapo_wk4)

Raw_Obs_Copiapo <- Raw_Obs_Copiapo[2881:40320,]


Raw_Obs_SanFdo <- rbind(Raw_Obs_SanFdo_wk1, Raw_Obs_SanFdo_wk2, Raw_Obs_SanFdo_wk3, Raw_Obs_SanFdo_wk4)

Raw_Obs_SanFdo <- Raw_Obs_SanFdo[2877:38478,]


Raw_Obs_TA <- rbind(Raw_Obs_TA_wk1, Raw_Obs_TA_wk2, Raw_Obs_TA_wk3, Raw_Obs_TA_wk4)

Raw_Obs_TierraAmarilla <- Raw_Obs_TA[2881:40320,] 


Raw_Obs_Volcanes <- rbind(Raw_Obs_Volcanes_wk1, Raw_Obs_Volcanes_wk2, Raw_Obs_Volcanes_wk3, Raw_Obs_Volcanes_wk4)

Raw_Obs_Volcanes <- Raw_Obs_Volcanes[2881:40320,]


## DONE


## Ahora copiar todo del otro R_Script (Pronostico SO2 Enami)


## Reduce Data

Obs_Paipote <- Raw_Obs_Paipote[c(1,4)]

Obs_Copiapo <- Raw_Obs_Copiapo[c(1,4)]

Obs_SanFdo <- Raw_Obs_SanFdo[c(1,4)]

Obs_TierraAmarilla <- Raw_Obs_TierraAmarilla[c(1,4)]

Obs_Volcanes <- Raw_Obs_Volcanes[c(1,4)]

## Delete Rows with -9999

Obs_Paipote <- subset(Obs_Paipote, Value2 != -9999)

Obs_Copiapo <- subset(Obs_Copiapo, Value2 != -9999)

Obs_SanFdo <- subset(Obs_SanFdo, Value2 != -9999)

Obs_TierraAmarilla <- subset(Obs_TierraAmarilla, Value2 != -9999)

Obs_Volcanes <- subset(Obs_Volcanes, Value2 != -9999)

## Delete Negative Rows

Obs_Paipote <- subset(Obs_Paipote, Value2 > 0)

Obs_Copiapo <- subset(Obs_Copiapo, Value2 > 0)

Obs_SanFdo <- subset(Obs_SanFdo, Value2 > 0)

Obs_TierraAmarilla <- subset(Obs_TierraAmarilla, Value2 > 0)

Obs_Volcanes <- subset(Obs_Volcanes, Value2 > 0)


## Wrangle


Avg_Obs_Paipote <- Obs_Paipote %>% 
  mutate(Hora = hour(Date_Time),
         Dia = day(Date_Time))

#check if the code below is necessary (it most likely wont be)
#Avg_Obs_Paipote$Hora[5761:5820] <- 0 (beginning of the day nov 5th had some issues in the past)

Avg_Obs_Paipote <- Avg_Obs_Paipote %>%
  group_by(Dia, Hora) %>% 
  summarise(PromedioHorario = mean(Value2))


Avg_Obs_Copiapo <- Obs_Copiapo %>% 
  mutate(Hora = hour(Date_Time),
         Dia = day(Date_Time))

#check if the code below is necessary (it most likely wont be)
#Avg_Obs_Copiapo$Hora[5761:5820] <- 0

Avg_Obs_Copiapo <- Avg_Obs_Copiapo %>% 
  group_by(Dia, Hora) %>% 
  summarise(PromedioHorario = mean(Value2))


Avg_Obs_SanFdo <- Obs_SanFdo %>% 
  mutate(Hora = hour(Date_Time),
         Dia = day(Date_Time))
#check if the code below is necessary (it most likely wont be)
#Avg_Obs_SanFdo$Hora[5750:5809] <- 0

Avg_Obs_SanFdo <- Avg_Obs_SanFdo %>% 
  group_by(Dia, Hora) %>% 
  summarise(PromedioHorario = mean(Value2))


Avg_Obs_TierraAmarilla <- Obs_TierraAmarilla %>% 
  mutate(Hora = hour(Date_Time),
         Dia = day(Date_Time))

#check if the code below is necessary (it most likely wont be)
#Avg_Obs_TierraAmarilla$Hora[5761:5820] <- 0

Avg_Obs_TierraAmarilla <- Avg_Obs_TierraAmarilla %>%
  group_by(Dia, Hora) %>% 
  summarise(PromedioHorario = mean(Value2))


Avg_Obs_Volcanes <- Obs_Volcanes %>% 
  mutate(Hora = hour(Date_Time),
         Dia = day(Date_Time))

#check if the code below is necessary (it most likely wont be)
#Avg_Obs_Volcanes$Hora[5761:5820] <- 0

Avg_Obs_Volcanes <- Avg_Obs_Volcanes %>% 
  group_by(Dia, Hora) %>% 
  summarise(PromedioHorario = mean(Value2))


## exportar

write.csv(Avg_Obs_Paipote, file = "ENAMI/Observados/Diciembre/Limpios/Paipote_Obs.csv")

write.csv(Avg_Obs_Copiapo, file = "ENAMI/Observados/Diciembre/Limpios/Copiapo_Obs.csv")

write.csv(Avg_Obs_SanFdo, file = "ENAMI/Observados/Diciembre/Limpios/SanFdo_Obs.csv")

write.csv(Avg_Obs_TierraAmarilla, file = "ENAMI/Observados/Diciembre/Limpios/TierraAmarilla_Obs.csv")

write.csv(Avg_Obs_Volcanes, file = "ENAMI/Observados/Diciembre/Limpios/Volcanes_Obs.csv")

