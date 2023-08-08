
library(openair)
library(pander) #
library(tidyverse)
library(dygraphs) #
library(xts) #
library(readxl) # 
library(RColorBrewer)
library(data.table) #
library(gplots)
library(scales)
library(hydroGOF) #
library(cvms) #
library(lubridate)
library(stringr)

MesDelReporte = "julio"

start_date <- '01-07-2023'
end_date <- '31-07-2023'

fecha_comienzo <- as.Date(start_date, format='%d-%m-%Y')
fecha_comienzo_dia <- format(fecha_comienzo, format = "%d")
fecha_termino <- as.Date(end_date, format='%d-%m-%Y')
fecha_termino_dia <- format(fecha_termino, format = "%d")
fecha_termino_mes <- format(fecha_termino, format = "%m")
fecha_termino_year <- format(fecha_termino, format = "%Y")

# Create a lookup vector for Spanish month names
spanish_months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", 
                    "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Use the month number to get the Spanish month name
fecha_termino_mes_nombre <- spanish_months[as.numeric(fecha_termino_mes)]

fecha_entrega <- fecha_termino %m+% months(1)
Mes_de_entrega <- format(fecha_entrega, format = "%m")
Year_de_entrega <- format(fecha_entrega, format = "%Y")

Mes_de_entrega_nombre <- spanish_months[as.numeric(Mes_de_entrega)]

# Fecha para ELABORACION, REVISION, APROBACION
first_day_next_month <- floor_date(fecha_termino %m+% months(1), "month")
first_day_next_month_weekday <- wday(first_day_next_month, week_start = getOption("lubridate.week.start", 1))

if(first_day_next_month_weekday > 5) {
  first_day_next_month <- first_day_next_month + (8 - first_day_next_month_weekday)
}

Primer_dia_habil_sgte_mes <- format(first_day_next_month, format='%d-%m-%Y')

##### COPIAPO #####

#SO2 MOD #

path_so2_mod_copiapo <- paste0('/Users/cristobal512/Desktop/geoaire/ENAMI/Modelados/Geologger/calidad-aire-dia-hora_Copiapo-PRO_', start_date, '_a_', end_date, '.csv')

SO2_Modelados_Copiapo <- read_csv(path_so2_mod_copiapo, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))

SO2_Modelados_Copiapo <- SO2_Modelados_Copiapo %>% 
  rename(SO2_Mod = `SO₂ (µg/m³N)`)

SO2_Modelados_Copiapo <- SO2_Modelados_Copiapo %>%
  mutate(Fecha = ymd_h(paste(Fecha, Hora)))

MeanSO2byHour_Modelados_Copiapo <- SO2_Modelados_Copiapo %>% 
  select(Hora, SO2_Mod) %>% 
  group_by(Hora) %>% 
  summarise(SO2_Mod = mean(SO2_Mod, na.rm = TRUE))


#SO2 OBS #

path_SO2_obs_Copiapo <- paste0('/Users/cristobal512/Desktop/geoaire/ENAMI/Observados/Geologger/calidad-aire-dia-hora_Copiapo_', start_date, '_a_', end_date, '.csv')

SO2_Observados_Copiapo <- read_csv(path_SO2_obs_Copiapo, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))

SO2_Observados_Copiapo <- SO2_Observados_Copiapo %>% 
  rename(SO2_Obs = `SO₂ (µg/m³N)`)

SO2_Observados_Copiapo <- SO2_Observados_Copiapo %>%
  mutate(Fecha = ymd_h(paste(Fecha, Hora)))

MeanSO2byHour_Observados_Copiapo <- SO2_Observados_Copiapo %>% 
  select(Hora, SO2_Obs) %>% 
  group_by(Hora) %>% 
  summarise(SO2_Obs = mean(SO2_Obs, na.rm = TRUE))


# MET MOD #

path_met_mod_Copiapo <- paste0('/Users/cristobal512/Desktop/geoaire/ENAMI/Modelados/Geologger/Copiapo-PRO_', start_date, '_a_', end_date, '.csv')

Met_Modelados_Copiapo <- read_csv(path_met_mod_Copiapo,
                          col_types = cols(Fecha = col_date(format = "%d/%m/%Y")),
                          locale = locale(decimal_mark = ","))

Met_Modelados_Copiapo <- Met_Modelados_Copiapo %>% 
  rename(date = Fecha,
         wd = `Direccion viento (°)`,
         ws = `Velocidad del viento (m/s)`)

Met_Modelados_Copiapo <- Met_Modelados_Copiapo %>%
  mutate(date = ymd_h(paste(date, Hora)))


# MET OBS #

path_met_obs_Copiapo <- paste0('/Users/cristobal512/Desktop/geoaire/ENAMI/Observados/Geologger/Copiapo_', start_date, '_a_', end_date, '.csv')

Met_Observados_Copiapo <- read_csv(path_met_obs_Copiapo, col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), locale = locale(decimal_mark = ","))

Met_Observados_Copiapo <- Met_Observados_Copiapo %>% 
  rename(date = Fecha,
         wd = `Direccion viento (°)`,
         ws = `Velocidad del viento (m/s)`)

Met_Observados_Copiapo <- Met_Observados_Copiapo %>%
  mutate(date = ymd_h(paste(date, Hora)))



Velocidad <- inner_join(Met_Observados_Copiapo[c('date', 'ws')], Met_Modelados_Copiapo[c('date', 'ws')], by = "date")

Velocidad_longer <- pivot_longer(Velocidad, cols = c("ws.x", "ws.y"))

Velocidad_longer %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = "", y = "Velocidad de Viento (m/s)") +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "4 day")


windRose(Met_Observados_Copiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

windRose(Met_Modelados_Copiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")


MetObs_Diurno_Copiapo <- selectByDate(Met_Observados_Copiapo, hour = 08:20)
MetMod_Diurno_Copiapo <- selectByDate(Met_Modelados_Copiapo, hour = 08:20)

windRose(MetObs_Diurno_Copiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

windRose(MetMod_Diurno_Copiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")



MetObs_Nocturno_Copiapo <- selectByDate(Met_Observados_Copiapo, hour = c(00:07, 21:23))
MetMod_Nocturno_Copiapo <- selectByDate(Met_Modelados_Copiapo, hour = c(00:07, 21:23))

windRose(MetObs_Nocturno_Copiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")

windRose(MetMod_Nocturno_Copiapo, key.position="right", auto.text = FALSE, paddle = FALSE, annotate = c("Promedio", "Calma"), key.header = "Wind Speed (m/s)",
         key.footer = "")


rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

HoraYdireccion_Obs_Copiapo <-  Met_Observados_Copiapo %>% 
  dplyr::select(Hora, wd)

hist2d(HoraYdireccion_Obs_Copiapo, nbins = 24, col = r, xlab = "Horas del día", ylab = "Dirección del Viento")

HoraYdireccion_Mod_Copiapo <-  Met_Modelados_Copiapo %>% 
  dplyr::select(Hora, wd)

hist2d(HoraYdireccion_Mod_Copiapo, nbins = 24, col = r, xlab = "Horas del día", ylab = "Dirección del Viento")

Direccion <- inner_join(Met_Observados_Copiapo[c('date', 'wd')], Met_Modelados_Copiapo[c('date', 'wd')], by = "date")


SO2 <- inner_join(MeanSO2byHour_Observados_Copiapo, MeanSO2byHour_Modelados_Copiapo, by = "Hora")

SO2_longer <- pivot_longer(SO2, cols = c("SO2_Obs", "SO2_Mod"))

SO2_longer %>% 
  ggplot(aes(x = Hora, y = value, color = name)) +
  geom_line() +
  theme_bw() +
  labs(x = "Hora del día", y = "SO2 (µg/m3N)") +
  theme(legend.position = "top", legend.title = element_blank()) +
  scale_color_discrete(labels = c("Observado", "Modelado")) +
  scale_x_continuous(breaks = seq(0,23,1))


ExactitudSO2_Copiapo <- inner_join(SO2_Modelados_Copiapo[c('Fecha', 'SO2_Mod')], SO2_Observados_Copiapo[c('Fecha', 'SO2_Obs')], by = 'Fecha')

ExactitudSO2_Copiapo <- ExactitudSO2_Copiapo %>% 
  mutate(Bueno = case_when((between(SO2_Obs, 0, 350) & between(SO2_Mod, 0, 350)) | (!between(SO2_Obs, 0, 350) & (!between(SO2_Mod, 0, 350))) ~ 1,
                           TRUE ~ 0),
         Regular = case_when((between(SO2_Obs, 350, 500) & between(SO2_Mod, 350, 500)) | (!between(SO2_Obs, 350, 500) & (!between(SO2_Mod, 350, 500))) ~ 1,
                             TRUE ~ 0),
         Alerta1 = case_when((between(SO2_Obs, 500, 650) & between(SO2_Mod, 500, 650)) | (!between(SO2_Obs, 500, 650) & (!between(SO2_Mod, 500, 650))) ~ 1,
                             TRUE ~ 0),
         Alerta2 = case_when((between(SO2_Obs, 650, 950) & between(SO2_Mod, 650, 950)) | (!between(SO2_Obs, 650, 950) & (!between(SO2_Mod, 650, 950))) ~ 1,
                             TRUE ~ 0),
         Alerta3 = case_when((SO2_Obs > 950 & SO2_Mod > 950) | (SO2_Obs < 950 & SO2_Mod < 950) ~ 1,
                             TRUE ~ 0)
  )

pct_bueno = sum(ExactitudSO2_Copiapo$Bueno, na.rm = TRUE) / nrow(ExactitudSO2_Copiapo)
pct_regular = sum(ExactitudSO2_Copiapo$Regular, na.rm = TRUE) / nrow(ExactitudSO2_Copiapo)
pct_alerta1 = sum(ExactitudSO2_Copiapo$Alerta1, na.rm = TRUE) / nrow(ExactitudSO2_Copiapo)
pct_alerta2 = sum(ExactitudSO2_Copiapo$Alerta2, na.rm = TRUE) / nrow(ExactitudSO2_Copiapo)
pct_alerta3 = sum(ExactitudSO2_Copiapo$Alerta3, na.rm = TRUE) / nrow(ExactitudSO2_Copiapo)
promedio_Copiapo = mean(c(pct_bueno, pct_regular, pct_alerta1, pct_alerta2, pct_alerta3))



































































































































































































































































































































































































































































































































































































































