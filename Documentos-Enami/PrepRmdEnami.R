library(dplyr)
library(readr)
library(readxl)
library(openair)
library(RColorBrewer)
library(gplots)
library(tidyr)
library(tidyverse)
setwd("/Users/cristobal512/Desktop/geoaire/GeoaireProject/GeoAire")

# SO2 MOD #

SO2_Modelados <- list.files(path = "ENAMI/Modelados/2023/2.Febrero/SO2",
                            pattern = "*.xlsx",
                            full.names = TRUE) 

sheets_SO2Mod <- excel_sheets(SO2_Modelados[1])

SO2Mod_Copiapo <- map_df(.x = SO2_Modelados,
                               .f = ~ read_excel(path = .x,
                                                 sheet = sheets_SO2Mod[1]))

SO2Mod_LosVolcanes <- map_df(.x = SO2_Modelados,
                                .f = ~ read_excel(path = .x,
                                                  sheet = sheets_SO2Mod[2]))

SO2Mod_SanFdo <- map_df(.x = SO2_Modelados,
                               .f = ~ read_excel(path = .x,
                                                 sheet = sheets_SO2Mod[3]))

SO2Mod_Paipote <- map_df(.x = SO2_Modelados,
                                .f = ~ read_excel(path = .x,
                                                  sheet = sheets_SO2Mod[4]))

SO2Mod_TierraAmarilla <- map_df(.x = SO2_Modelados,
                               .f = ~ read_excel(path = .x,
                                                 sheet = sheets_SO2Mod[5]))


####################################################################################################################################

NumberOfDaysInMonth = 28

MyLogicVector <- rep(c(T,F,F), NumberOfDaysInMonth) # T,F,F    T,F,F    T,F,F   ... 31 times (93 items)

MyLogicVector_2 <- rep(MyLogicVector, each = 24)  # T,T,T... 24 times, F,F,F,... 24 times, F,F,F,... 24 times, ...(93*24) times = 2232 times

Mysequence <- seq(1:(NumberOfDaysInMonth*24*3))  # 1,2,3,4...2232 (744 * 3) = 744 hours in a period of 31 days and 3 because each excel file has 3 days.

MyNewIndex <- Mysequence[MyLogicVector_2]  # 1,2,3,...24, 73,74,75,...96, 145,146,147,...168, 217,218,219,... ... 2182,2183,2184 (only correct indeces)

####################################################################################################################################

SO2Mod_Copiapo <- SO2Mod_Copiapo[MyNewIndex,]  # correct dataframe.

SO2Mod_LosVolcanes <- SO2Mod_LosVolcanes[MyNewIndex,]  # correct dataframe.

SO2Mod_SanFdo <- SO2Mod_SanFdo[MyNewIndex,]  # correct dataframe.

SO2Mod_Paipote <- SO2Mod_Paipote[MyNewIndex,]  # correct dataframe.

SO2Mod_TierraAmarilla <- SO2Mod_TierraAmarilla[MyNewIndex,]  # correct dataframe.

####################################################################################################################################


AvgHourlySO2Mod_Copiapo <- SO2Mod_Copiapo %>% 
  select(hora, VALOR) %>% 
  group_by(hora) %>%
  summarise_all(mean)

AvgHourlySO2Mod_LosVolcanes <- SO2Mod_LosVolcanes %>% 
  select(hora, VALOR) %>% 
  group_by(hora) %>%
  summarise_all(mean)

AvgHourlySO2Mod_Sanfdo <- SO2Mod_SanFdo %>% 
  select(hora, VALOR) %>% 
  group_by(hora) %>%
  summarise_all(mean)

AvgHourlySO2Mod_Paipote <- SO2Mod_Paipote %>% 
  select(hora, VALOR) %>% 
  group_by(hora) %>%
  summarise_all(mean)

AvgHourlySO2Mod_TierraAmarilla <- SO2Mod_TierraAmarilla %>% 
  select(hora, VALOR) %>% 
  group_by(hora) %>%
  summarise_all(mean)

# SO2 OBS #

SO2Obs_Copiapo <- read_csv("ENAMI/Observados/2023/2.Febrero/SO2/calidad-aire-dia-hora_Copiapo_01-02-2023_a_28-02-2023.csv")
SO2Obs_LosVolcanes <- read_csv("ENAMI/Observados/2023/2.Febrero/SO2/calidad-aire-dia-hora_Los_Volcanes_01-02-2023_a_28-02-2023.csv")
SO2Obs_SanFdo <- read_csv("ENAMI/Observados/2023/2.Febrero/SO2/calidad-aire-dia-hora_San_Fernando_01-02-2023_a_28-02-2023.csv")
SO2Obs_Paipote <- read_csv("ENAMI/Observados/2023/2.Febrero/SO2/calidad-aire-dia-hora_Paipote_01-02-2023_a_28-02-2023.csv")
SO2Obs_TierraAmarilla <- read_csv("ENAMI/Observados/2023/2.Febrero/SO2/calidad-aire-dia-hora_Tierra_Amarilla_01-02-2023_a_28-02-2023.csv")

names(SO2Obs_Copiapo) <- c("Fecha", "Hora", "VALOR")
names(SO2Obs_LosVolcanes) <- c("Fecha", "Hora", "VALOR")
names(SO2Obs_SanFdo) <- c("Fecha", "Hora", "VALOR")
names(SO2Obs_Paipote) <- c("Fecha", "Hora", "VALOR")
names(SO2Obs_TierraAmarilla) <- c("Fecha", "Hora", "VALOR")


AvgHourlySO2Obs_Copiapo <- SO2Obs_Copiapo %>% 
  select(Hora, VALOR) %>% 
  group_by(Hora) %>%
  summarise_all(mean)

AvgHourlySO2Obs_LosVolcanes <- SO2Obs_LosVolcanes %>% 
  select(Hora, VALOR) %>% 
  group_by(Hora) %>%
  summarise_all(mean, na.rm = T)

AvgHourlySO2Obs_SanFdo <- SO2Obs_SanFdo %>% 
  select(Hora, VALOR) %>% 
  group_by(Hora) %>%
  summarise_all(mean)

AvgHourlySO2Obs_Paipote <- SO2Obs_Paipote %>% 
  select(Hora, VALOR) %>% 
  group_by(Hora) %>%
  summarise_all(mean)

AvgHourlySO2Obs_TierraAmarilla <- SO2Obs_TierraAmarilla %>% 
  select(Hora, VALOR) %>% 
  group_by(Hora) %>%
  summarise_all(mean)

# MET MOD #

Met_Modelados <- list.files(path = "ENAMI/Modelados/2023/2.Febrero/Meteorologia",
                            pattern = "*.xlsx",
                            full.names = TRUE) 

sheets_MetMod <- excel_sheets(Met_Modelados[1])

MetMod_Copiapo <- map_df(.x = Met_Modelados,
                         .f = ~ read_excel(path = .x,
                                           sheet = sheets_MetMod[2]))

MetMod_SanFdo <- map_df(.x = Met_Modelados,
                        .f = ~ read_excel(path = .x,
                                          sheet = sheets_MetMod[6]))

MetMod_Paipote <- map_df(.x = Met_Modelados,
                         .f = ~ read_excel(path = .x,
                                           sheet = sheets_MetMod[1]))

MetMod_TierraAmarilla <- map_df(.x = Met_Modelados,
                                .f = ~ read_excel(path = .x,
                                                  sheet = sheets_MetMod[5]))

MetMod_Principal <- map_df(.x = Met_Modelados,
                                .f = ~ read_excel(path = .x,
                                                  sheet = sheets_MetMod[7]))

MetMod_Copiapo <- MetMod_Copiapo[MyNewIndex,]  # correct dataframe.

MetMod_SanFdo <- MetMod_SanFdo[MyNewIndex,]  # correct dataframe.

MetMod_Paipote <- MetMod_Paipote[MyNewIndex,]  # correct dataframe.

MetMod_TierraAmarilla <- MetMod_TierraAmarilla[MyNewIndex,]  # correct dataframe.

MetMod_Principal <- MetMod_Principal[MyNewIndex,]  # correct dataframe.

MetMod_Copiapo <- MetMod_Copiapo %>% 
  rename(date = FECHA, wd = WDIR, ws = WSPEED) %>% 
  select(date, wd, ws)

MetMod_SanFdo <- MetMod_SanFdo %>% 
  rename(date = FECHA, wd = WDIR, ws = WSPEED) %>% 
  select(date, wd, ws)

MetMod_Paipote <- MetMod_Paipote %>% 
  rename(date = FECHA, wd = WDIR, ws = WSPEED) %>% 
  select(date, wd, ws)

MetMod_TierraAmarilla <- MetMod_TierraAmarilla %>% 
  rename(date = FECHA, wd = WDIR, ws = WSPEED) %>% 
  select(date, wd, ws)

MetMod_Principal <- MetMod_Principal %>% 
  rename(date = FECHA, wd = WDIR, ws = WSPEED, t = TEMP) %>% 
  select(date, wd, ws, t)
 


# MET OBS #

MetObs_Copiapo <- read_csv("ENAMI/Observados/2023/2.Febrero/Meteorologia/Copiapo_01-02-2023_a_28-02-2023.csv",
                           col_types = cols(`Velocidad del viento (m/s)` = col_number()),
                           locale = locale(decimal_mark = ",", grouping_mark = "."))

MetObs_SanFdo <- read_csv("ENAMI/Observados/2023/2.Febrero/Meteorologia/San_Fernando_01-02-2023_a_28-02-2023.csv",
                           col_types = cols(`Velocidad del viento (m/s)` = col_number()),
                           locale = locale(decimal_mark = ",", grouping_mark = "."))

MetObs_Paipote <- read_csv("ENAMI/Observados/2023/2.Febrero/Meteorologia/Paipote_01-02-2023_a_28-02-2023.csv",
                           col_types = cols(`Velocidad del viento (m/s)` = col_number()),
                           locale = locale(decimal_mark = ",", grouping_mark = "."))

MetObs_TierraAmarilla <- read_csv("ENAMI/Observados/2023/2.Febrero/Meteorologia/Tierra_Amarilla_01-02-2023_a_28-02-2023.csv",
                           col_types = cols(`Velocidad del viento (m/s)` = col_number()),
                           locale = locale(decimal_mark = ",", grouping_mark = "."))

MetObs_Principal <- read_csv("ENAMI/Observados/2023/2.Febrero/Meteorologia/Principal_01-02-2023_a_28-02-2023.csv",
                           col_types = cols(`Velocidad del viento (m/s)` = col_number()),
                           locale = locale(decimal_mark = ",", grouping_mark = "."))

MetObs_Copiapo <- MetObs_Copiapo %>% 
  rename(wd = `Direccion viento (°)`, ws = `Velocidad del viento (m/s)`) %>% 
  mutate(date = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H")) %>% 
  select(date, wd, ws)

MetObs_SanFdo <- MetObs_SanFdo %>% 
  rename(wd = `Direccion viento (°)`, ws = `Velocidad del viento (m/s)`) %>% 
  mutate(date = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H")) %>% 
  select(date, wd, ws)

MetObs_Paipote <- MetObs_Paipote %>% 
  rename(wd = `Direccion viento (°)`, ws = `Velocidad del viento (m/s)`) %>% 
  mutate(date = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H")) %>% 
  select(date, wd, ws)

MetObs_TierraAmarilla <- MetObs_TierraAmarilla %>% 
  rename(wd = `Direccion viento (°)`, ws = `Velocidad del viento (m/s)`) %>% 
  mutate(date = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H")) %>% 
  select(date, wd, ws)

MetObs_Principal <- MetObs_Principal %>% 
  rename(wd = `Direccion viento (°)`, ws = `Velocidad del viento (m/s)`, t = `Temperatura (°C)`) %>% 
  mutate(date = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H")) %>% 
  select(date, wd, ws, t)


MetMod_Copiapo <- MetMod_Copiapo %>% 
  inner_join(select(MetObs_Copiapo, date))

MetObs_Copiapo <- MetObs_Copiapo %>% 
  inner_join(select(MetMod_Copiapo, date))

MetMod_SanFdo <- MetMod_SanFdo %>% 
  inner_join(select(MetObs_SanFdo, date))

MetObs_SanFdo <- MetObs_SanFdo %>% 
  inner_join(select(MetMod_SanFdo, date))

MetMod_Paipote <- MetMod_Paipote %>% 
  inner_join(select(MetObs_Paipote, date))

MetObs_Paipote <- MetObs_Paipote %>% 
  inner_join(select(MetMod_Paipote, date))

MetMod_TierraAmarilla <- MetMod_TierraAmarilla %>% 
  inner_join(select(MetObs_TierraAmarilla, date))

MetObs_TierraAmarilla <- MetObs_TierraAmarilla %>% 
  inner_join(select(MetMod_TierraAmarilla, date))

MetMod_Principal <- MetMod_Principal %>% 
  inner_join(select(MetObs_Principal, date))

MetObs_Principal <- MetObs_Principal %>% 
  inner_join(select(MetMod_Principal, date))


#########################################################################################################



WindSpeed_Copiapo <- MetObs_Copiapo[,c(1,3)] %>%
  mutate(ws_mod = MetMod_Copiapo$ws)

WindSpeed_Copiapo <- pivot_longer(WindSpeed_Copiapo, cols = c("ws", "ws_mod"))

WindSpeed_Copiapo %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "", y = "Velocidad de Viento (m/s)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "4 day")


#########################################################################################################

windRose(MetObs_Copiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

windRose(MetMod_Copiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

#########################################################################################################

Diurno_MetObsCopiapo <- selectByDate(MetObs_Copiapo, hour = 08:20)
Diurno_MetModCopiapo <- selectByDate(MetMod_Copiapo, hour = 08:20)

windRose(Diurno_MetObsCopiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

windRose(Diurno_MetModCopiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

#########################################################################################################

Nocturno_MetObsCopiapo <- selectByDate(MetObs_Copiapo, hour = c(00:07, 21:23))
Nocturno_MetModCopiapo <- selectByDate(MetMod_Copiapo, hour = c(00:07, 21:23))

windRose(Nocturno_MetObsCopiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

windRose(Nocturno_MetModCopiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

#########################################################################################################

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

HourAndWD_CopiapoObs <-  MetObs_Copiapo %>% 
  mutate(hora = hour(date)) %>%
  select(hora, wd)

hist2d(HourAndWD_CopiapoObs, nbins = 25, col = r, xlab = "Horas del día", ylab = "Dirección del Viento")

HourAndWD_CopiapoMod <-  MetMod_Copiapo %>% 
  mutate(hora = hour(date)) %>%
  select(hora, wd)

hist2d(HourAndWD_CopiapoMod, nbins = 25, col = r, xlab = "Horas del día", ylab = "Dirección del Viento")

#########################################################################################################

ObsModSO2_Copiapo <- AvgHourlySO2Obs_Copiapo %>% 
  mutate(VALOR_Mod = AvgHourlySO2Mod_Copiapo$VALOR)

ObsModSO2_Copiapo <- pivot_longer(ObsModSO2_Copiapo, cols = c("VALOR", "VALOR_Mod"))

ObsModSO2_Copiapo %>% 
  ggplot(aes(x = Hora, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  labs(x = "Hora del día", y = "SO2 (µg/m3N)") +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_continuous(breaks = seq(0,23,1))


#########################################################################################################

DateSO2Obs_Copiapo <- SO2Obs_Copiapo %>%
  mutate(DateAndTime = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H")) %>% 
  select(DateAndTime, VALOR)

DateSO2Mod_Copiapo <- SO2Mod_Copiapo %>%
  mutate(DateAndTime = as.POSIXct(paste(Año, mes, dia, hora), format = "%Y %m %d %H"))%>%
  rename(VALOR_Mod = VALOR) %>% 
  select(DateAndTime, VALOR_Mod)

ExactitudSO2_Copiapo <- DateSO2Obs_Copiapo %>%
  inner_join(DateSO2Mod_Copiapo) %>% 
  mutate(Bueno = case_when((between(VALOR, 0, 100) & between(VALOR_Mod, 0, 100)) | (!between(VALOR, 0, 100) & (!between(VALOR_Mod, 0, 100))) ~ 1,
                            TRUE ~ 0),
         Alerta1 = case_when((between(VALOR, 100, 150) & between(VALOR_Mod, 100, 150)) | (!between(VALOR, 100, 150) & (!between(VALOR_Mod, 100, 150))) ~ 1,
                              TRUE ~ 0),
         Alerta2 = case_when((VALOR > 150 & VALOR_Mod > 150) | (VALOR < 150 & VALOR_Mod < 150) ~ 1,
                              TRUE ~ 0))

pct_bueno = sum(ExactitudSO2_Copiapo$Bueno, na.rm = TRUE) / nrow(ExactitudSO2_Copiapo)
pct_alerta1 = sum(ExactitudSO2_Copiapo$Alerta1, na.rm = TRUE) / nrow(ExactitudSO2_Copiapo)
pct_alerta2 = sum(ExactitudSO2_Copiapo$Alerta2, na.rm = TRUE) / nrow(ExactitudSO2_Copiapo)
promedio = mean(c(pct_bueno, pct_alerta1, pct_alerta2))

#########################################################################################################
# Agregar Grafico Temperatura a Estacion Principal

Temperatura <- MetObs_Principal[,c(1,4)] %>% 
  mutate(t_Mod = MetMod_Principal$t)

Temperatura <- pivot_longer(Temperatura, cols = c("t", "t_Mod"))

Temperatura %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "", y = "Temperatura (°C)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "4 day")

#########################################################################################################

MFError = round(200 * mean(abs(MetMod_Copiapo$t - MetObs_Copiapo$t) / (MetMod_Copiapo$t + MetObs_Copiapo$t), na.rm = TRUE), 2)

MFBias = round(100 * compute.fb(MetMod_Copiapo$t, MetObs_Copiapo$t), 2)

IOAgreement = round(md(MetMod_Copiapo$t, MetObs_Copiapo$t), 2)

#########################################################################################################

WindSpeed_SanFdo <- MetObs_SanFdo[,c(1,3)] %>%
  inner_join(MetMod_SanFdo[,c(1,3)], by = c("date"))

WindSpeed_SanFdo <- WindSpeed_SanFdo %>% 
  rename(ws = ws.x, ws_mod = ws.y)

WindSpeed_SanFdo <- pivot_longer(WindSpeed_SanFdo, cols = c("ws", "ws_mod"))

WindSpeed_SanFdo %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "", y = "Velocidad de Viento (m/s)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "4 day")

#########################################################################################################

MetMod_SanFdo <- MetMod_SanFdo %>% 
  rename(date = FECHA, wd = WDIR, ws = WSPEED) %>% 
  select(date, wd, ws)

MetObs_SanFdo <- MetObs_SanFdo %>% 
  rename(wd = `Direccion viento (°)`, ws = `Velocidad del viento (m/s)`) %>% 
  mutate(date = as.POSIXct(paste(Fecha, Hora), format = "%d/%m/%Y %H")) %>% 
  select(date, wd, ws)

MetMod_SanFdo <- MetMod_SanFdo %>% 
  inner_join(select(MetObs_SanFdo, date))

MetObs_SanFdo <- MetObs_SanFdo %>% 
  inner_join(select(MetMod_SanFdo, date))














































































































































































