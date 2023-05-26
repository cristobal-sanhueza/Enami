
suppressPackageStartupMessages(library(tidyverse))
library(readr)

# Importar Datos
Copiapo <- read_csv("ENAMI/Observados/2018-2022/SO2_Copiapo_01-01-2018_a_25-07-2022.csv")

LosVolcanes <- read_csv("ENAMI/Observados/2018-2022/SO2_Los_Volcanes_01-01-2018_a_25-07-2022.csv",
                        col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))

SanFdo <- read_csv("ENAMI/Observados/2018-2022/SO2_San_Fernando_01-01-2018_a_25-07-2022.csv",
                   col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))

TierraAmarilla <- read_csv("ENAMI/Observados/2018-2022/SO2_Tierra_Amarilla_01-01-2018_a_25-07-2022.csv",
                           col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))

Paipote <- read_csv("ENAMI/Observados/2018-2022/SO2_Paipote_01-01-2018_a_25-07-2022.csv",
                    col_types = cols(Fecha = col_date(format = "%d/%m/%Y")))

###### LOS PEAKS (SOBRE 350 ug/m3 de SO2)

# 1) En qué estaciones se dan

# 2) Frecuencia de ocurrencia

# 3) si se dan todas simultáneamente o en algunas o una sola a la vez

# 4) A que hora ocurren


# -------------------------------------- COPIAPO ------------------------------------ #

# Arreglar fechas
Copiapo <- Copiapo %>% 
  mutate(Fecha = case_when(nchar(Fecha) == 10 ~ as.Date(Fecha, "%d/%m/%Y"),
                           TRUE ~ as.Date(Fecha, "%d/%m/%y")))


Copiapo %>% 
  ggplot(aes(x = Fecha, y = `SO2 (µg/m³N)`)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0,600,50)) +
  geom_hline(yintercept =  350, color = "red") +
  theme_bw()


## CANTIDAD DE DATOS VÁLIDOS EN LA COLUMNA SO2 ========= 35.362
sum(!is.na(Copiapo$`SO2 (µg/m³N)`))

## CANTIDAD DE NAS EN LA COLUMNA SO2 =================== 1.308
sum(is.na(Copiapo$`SO2 (µg/m³N)`))

## CANTIDAD DE VECES QUE EL SO2 MARCÓ SOBRE 350 ======== 18
sum(Copiapo$`SO2 (µg/m³N)` >= 350, na.rm = T)

Copiapo %>% 
  filter(`SO2 (µg/m³N)` >= 350)
  


# -------------------------------------- LOS VOLCANES ------------------------------------ #


LosVolcanes %>% 
  ggplot(aes(x = Fecha, y = `SO₂ (µg/m³N)`)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0,600,50)) +
  geom_hline(yintercept =  350, color = "red") +
  theme_bw()


## CANTIDAD DE DATOS VÁLIDOS EN LA COLUMNA SO2 ========= 36.112
sum(!is.na(LosVolcanes$`SO₂ (µg/m³N)`))

## CANTIDAD DE NAS EN LA COLUMNA SO2 =================== 557
sum(is.na(LosVolcanes$`SO₂ (µg/m³N)`))

## CANTIDAD DE VECES QUE EL SO2 MARCÓ SOBRE 350 ======== 17
sum(LosVolcanes$`SO₂ (µg/m³N)` >= 350, na.rm = T)

LosVolcanes %>% 
  filter(`SO₂ (µg/m³N)`>= 350)


# -------------------------------------- SAN FERNANDO ------------------------------------ #

SanFdo %>% 
  ggplot(aes(x = Fecha, y = `SO₂ (µg/m³N)`)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0,600,50)) +
  geom_hline(yintercept =  350, color = "red") +
  theme_bw()


## CANTIDAD DE DATOS VÁLIDOS EN LA COLUMNA SO2 ========= 36.191
sum(!is.na(SanFdo$`SO₂ (µg/m³N)`))

## CANTIDAD DE NAS EN LA COLUMNA SO2 =================== 448
sum(is.na(SanFdo$`SO₂ (µg/m³N)`))

## CANTIDAD DE VECES QUE EL SO2 MARCÓ SOBRE 350 ======== 19
sum(SanFdo$`SO₂ (µg/m³N)` >= 350, na.rm = T)

SanFdo %>% 
  filter(`SO₂ (µg/m³N)`>= 350)



# -------------------------------------- TIERRA AMARILLA ------------------------------------ #

TierraAmarilla %>% 
  ggplot(aes(x = Fecha, y = `SO₂ (µg/m³N)`)) +
  geom_point() +
  scale_y_continuous(breaks = c(0, 350, 600, 900)) +
  geom_hline(yintercept =  350, color = "red") +
  theme_bw()


## CANTIDAD DE DATOS VÁLIDOS EN LA COLUMNA SO2 ========= 36.291
sum(!is.na(TierraAmarilla$`SO₂ (µg/m³N)`))

## CANTIDAD DE NAS EN LA COLUMNA SO2 =================== 374
sum(is.na(TierraAmarilla$`SO₂ (µg/m³N)`))

## CANTIDAD DE VECES QUE EL SO2 MARCÓ SOBRE 350 ======== 76
sum(TierraAmarilla$`SO₂ (µg/m³N)` >= 350, na.rm = T)

TierraAmarilla %>% 
  filter(`SO₂ (µg/m³N)`>= 350) %>% 
  group_by(Hora) %>% 
  summarise(Frequencia = n())

# -------------------------------------- PAIPOTE ------------------------------------ #

Paipote %>% 
  ggplot(aes(x = Fecha, y = `SO₂ (µg/m³N)`)) +
  geom_point() +
  scale_y_continuous(breaks = c(0, 350, 1000, 1500, 2000)) +
  geom_hline(yintercept =  350, color = "red") +
  theme_bw()


## CANTIDAD DE DATOS VÁLIDOS EN LA COLUMNA SO2 ========= 36.355
sum(!is.na(Paipote$`SO₂ (µg/m³N)`))

## CANTIDAD DE NAS EN LA COLUMNA SO2 =================== 319
sum(is.na(Paipote$`SO₂ (µg/m³N)`))

## CANTIDAD DE VECES QUE EL SO2 MARCÓ SOBRE 350 ======== 178
sum(Paipote$`SO₂ (µg/m³N)` >= 350, na.rm = T)

Paipote %>% 
  filter(`SO₂ (µg/m³N)`>= 350) %>% 
  group_by(Hora) %>% 
  summarise(Frequencia = n())



























































































































































































