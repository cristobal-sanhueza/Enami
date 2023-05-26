#### Combine data


## FIRST IMPORT:


## PAIPOTE

Raw_Obs_Paipote_nov01_07 <- read_csv("ENAMI/Observados/Noviembre/enami-nov01-07/RAW_S001T01_202111080652.csv")

Raw_Obs_Paipote_nov08_14 <- read_csv("ENAMI/Observados/Noviembre/enami-nov08-14/RAW_S001T01_202111150943.csv")

Raw_Obs_Paipote_nov15_21 <- read_csv("ENAMI/Observados/Noviembre/enami-nov15-21/RAW_S001T01_202111220738.csv")

Raw_Obs_Paipote_nov22_28 <- read_csv("ENAMI/Observados/Noviembre/enami-nov22-28/RAW_S001T01_202111290858.csv")

Raw_Obs_Paipote_nov29_dic05 <- read_csv("ENAMI/Observados/Noviembre/enami-nov29-dic05/RAW_S001T01_202112060804.csv")


## COPIAPO

Raw_Obs_Copiapo_nov01_07 <- read_csv("ENAMI/Observados/Noviembre/enami-nov01-07/RAW_S002T01_202111080652.csv")

Raw_Obs_Copiapo_nov08_14 <- read_csv("ENAMI/Observados/Noviembre/enami-nov08-14/RAW_S002T01_202111150942.csv")

Raw_Obs_Copiapo_nov15_21 <- read_csv("ENAMI/Observados/Noviembre/enami-nov15-21/RAW_S002T01_202111220738.csv")

Raw_Obs_Copiapo_nov22_28 <- read_csv("ENAMI/Observados/Noviembre/enami-nov22-28/RAW_S002T01_202111290858.csv")

Raw_Obs_Copiapo_nov29_dic05 <- read_csv("ENAMI/Observados/Noviembre/enami-nov29-dic05/RAW_S002T01_202112060804.csv")


## SAN FERNANDO

Raw_Obs_SanFdo_nov01_07 <- read_csv("ENAMI/Observados/Noviembre/enami-nov01-07/RAW_S003T01_202111080654.csv")

Raw_Obs_SanFdo_nov08_14 <- read_csv("ENAMI/Observados/Noviembre/enami-nov08-14/RAW_S003T01_202111150942.csv")

Raw_Obs_SanFdo_nov15_21 <- read_csv("ENAMI/Observados/Noviembre/enami-nov15-21/RAW_S003T01_202111220738.csv")

Raw_Obs_SanFdo_nov22_28 <- read_csv("ENAMI/Observados/Noviembre/enami-nov22-28/RAW_S003T01_202111290858.csv")

Raw_Obs_SanFdo_nov29_dic05 <- read_csv("ENAMI/Observados/Noviembre/enami-nov29-dic05/RAW_S003T01_202112060804.csv")




## TIERRA AMARILLA

Raw_Obs_TA_nov01_07 <- read_csv("ENAMI/Observados/Noviembre/enami-nov01-07/RAW_S004T01_202111080654.csv")

Raw_Obs_TA_nov08_14 <- read_csv("ENAMI/Observados/Noviembre/enami-nov08-14/RAW_S004T01_202111150942.csv")

Raw_Obs_TA_nov15_21 <- read_csv("ENAMI/Observados/Noviembre/enami-nov15-21/RAW_S004T01_202111220737.csv")

Raw_Obs_TA_nov22_28 <- read_csv("ENAMI/Observados/Noviembre/enami-nov22-28/RAW_S004T01_202111290858.csv")

Raw_Obs_TA_nov29_dic05 <- read_csv("ENAMI/Observados/Noviembre/enami-nov29-dic05/RAW_S004T01_202112060803.csv")




## VOLCANES

Raw_Obs_Volcanes_nov01_07 <- read_csv("ENAMI/Observados/Noviembre/enami-nov01-07/RAW_S005T01_202111080654.csv")

Raw_Obs_Volcanes_nov08_14 <- read_csv("ENAMI/Observados/Noviembre/enami-nov08-14/RAW_S005T01_202111150942.csv")

Raw_Obs_Volcanes_nov15_21 <- read_csv("ENAMI/Observados/Noviembre/enami-nov15-21/RAW_S005T01_202111220737.csv")

Raw_Obs_Volcanes_nov22_28 <- read_csv("ENAMI/Observados/Noviembre/enami-nov22-28/RAW_S005T01_202111290859.csv")

Raw_Obs_Volcanes_nov29_dic05 <- read_csv("ENAMI/Observados/Noviembre/enami-nov29-dic05/RAW_S005T01_202112060803.csv")



## NOW COMBINE


Raw_Obs_Paipote <- rbind(Raw_Obs_Paipote_nov01_07, Raw_Obs_Paipote_nov08_14, Raw_Obs_Paipote_nov15_21, Raw_Obs_Paipote_nov22_28, Raw_Obs_Paipote_nov29_dic05)

Raw_Obs_Paipote <- Raw_Obs_Paipote[0:43200,]


Raw_Obs_Copiapo <- rbind(Raw_Obs_Copiapo_nov01_07, Raw_Obs_Copiapo_nov08_14, Raw_Obs_Copiapo_nov15_21, Raw_Obs_Copiapo_nov22_28, Raw_Obs_Copiapo_nov29_dic05)

Raw_Obs_Copiapo <- Raw_Obs_Copiapo[0:43200,]


Raw_Obs_SanFdo <- rbind(Raw_Obs_SanFdo_nov01_07, Raw_Obs_SanFdo_nov08_14, Raw_Obs_SanFdo_nov15_21, Raw_Obs_SanFdo_nov22_28, Raw_Obs_SanFdo_nov29_dic05)

Raw_Obs_SanFdo <- Raw_Obs_SanFdo[0:43127,]


Raw_Obs_TA <- rbind(Raw_Obs_TA_nov01_07, Raw_Obs_TA_nov08_14, Raw_Obs_TA_nov15_21, Raw_Obs_TA_nov22_28, Raw_Obs_TA_nov29_dic05)

Raw_Obs_TierraAmarilla <- Raw_Obs_TA[0:43200,] 


Raw_Obs_Volcanes <- rbind(Raw_Obs_Volcanes_nov01_07, Raw_Obs_Volcanes_nov08_14, Raw_Obs_Volcanes_nov15_21, Raw_Obs_Volcanes_nov22_28, Raw_Obs_Volcanes_nov29_dic05)

Raw_Obs_Volcanes <- Raw_Obs_Volcanes[0:43069,]


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

write.csv(Avg_Obs_Paipote, file = "ENAMI/Observados/Noviembre/Limpios/Paipote_Obs.csv")

write.csv(Avg_Obs_Copiapo, file = "ENAMI/Observados/Noviembre/Limpios/Copiapo_Obs.csv")

write.csv(Avg_Obs_SanFdo, file = "ENAMI/Observados/Noviembre/Limpios/SanFdo_Obs.csv")

write.csv(Avg_Obs_TierraAmarilla, file = "ENAMI/Observados/Noviembre/Limpios/TierraAmarilla_Obs.csv")

write.csv(Avg_Obs_Volcanes, file = "ENAMI/Observados/Noviembre/Limpios/Volcanes_Obs.csv")
















































































































































































































































































































































































































































































