library(readxl)
library(tree)
library(xlsx)
library(lubridate)
library(tree)
library(DMwR)
library(caTools)
library(rpart)
library(DMwR)

set.seed(123)

## Importar Tabla de Datos en formato Excel
So2Data <- read_excel("SO2/datoshorarios_red_FHVL_010118_240821.xlsx", skip = 2)

So2Data <- So2Data %>% 
  mutate(...2 = hour(...2))

## Crear dataframe solo de paipote
Paipote <- So2Data[c(1, 2, 11:13, 17, 19, 21)]
names(Paipote) <- c("Fecha", "Hora", "SO2", "WS", "WD", "TEMP", "HR", "RS")


## Convertir variables de respuesta en categorias
Paipote <- Paipote %>% 
  mutate(Norma = case_when(between(SO2, 0, 500) ~ "Bueno",
                           between(SO2, 500, 649) ~ "Alerta",
                           between(SO2, 650, 949) ~ "Preemergencia",
                           SO2 > 950 ~ "Emergencia"))


## Dividir datos en train y test
sample = sample.split(Paipote$Norma,SplitRatio = 0.75)

Paipote.Train =subset(Paipote, sample == TRUE)

Paipote.Test =subset(Paipote, sample == FALSE)

## No se que hice aqui
treeimb <- rpart(Norma ~ WS + WD + TEMP + HR + RS, data = Paipote.Train)

pred.treeimb <- predict(treeimb, newdata = Paipote.Test)

accuracy.meas(Paipote.Test$Norma, pred.treeimb[,2])








## usando CARET
set.seed(2969)
imbal_train <- twoClassSim(10000, intercept = -20, linearVars = 20)
imbal_test  <- twoClassSim(10000, intercept = -20, linearVars = 20)
table(imbal_train$Class)

set.seed(9560)
down_train <- downSample(x = imbal_train[, -ncol(imbal_train)],
                         y = imbal_train$Class)
table(down_train$Class)   

Paipote.Train$Norma <- as.factor(Paipote.Train$Norma)

menor.paipote <- downSample(x = Paipote.Train[,-ncol(Paipote.Train)],
                            y = Paipote.Train$Norma)

mayor.paipote <- upSample(x = Paipote.Train[,-ncol(Paipote.Train)],
                            y = Paipote.Train$Norma)

## usando DMWR - SMOTE


smote_train <- SMOTE(Norma ~ WS + WD + TEMP + HR + RS, data = Paipote.Train)
table(smote_train$Class) 

# paipote2 <- Paipote %>%
#   mutate(year = year(Fecha))
# 
#   
# paipote2018 <- paipote2 %>% 
#   filter(year == 2018)
# 
# paipote2019 <- paipote2 %>% 
#   filter(year == 2019)
# 
# paipote2020 <- paipote2 %>% 
#   filter(year == 2020)
# 
# quantile(paipote2018$SO2, 0.985, na.rm = TRUE)
# 
# quantile(paipote2019$SO2, 0.985, na.rm = TRUE)
# 
# quantile(paipote2020$SO2, 0.985, na.rm = TRUE)
# 
# sapply(paipote2018, function(x) sum(is.na(x)))
# 
# sapply(paipote2019, function(x) sum(is.na(x)))
# 
# sapply(paipote2020, function(x) sum(is.na(x)))
# 
# paipote2018_clean <- paipote2018 %>% drop_na(SO2)
# 
# paipote2019_clean <- paipote2019 %>% drop_na(SO2)
# 
# paipote2020_clean <- paipote2020 %>% drop_na(SO2)
# 
# quantile(paipote2018_clean$SO2, 0.985, na.rm = TRUE)
# 
# quantile(paipote2019_clean$SO2, 0.985, na.rm = TRUE)
# 
# quantile(paipote2020_clean$SO2, 0.985, na.rm = TRUE)
# 
# mean(paipote2018_clean$SO2)
# 
# mean(paipote2019_clean$SO2)
# 
# mean(paipote2020_clean$SO2)
# 
# paipote2018_sobre500 <- paipote2018 %>% 
#   mutate(limite = case_when(SO2 < 500 ~ "Normal",
#                             SO2 >= 500 ~ "Alerta"))
# 
# table(paipote2018_sobre500$limite)
# 
# paipote2019_sobre500 <- paipote2019 %>% 
#   mutate(limite = case_when(SO2 < 500 ~ "Normal",
#                             SO2 >= 500 ~ "Alerta"))
# 
# table(paipote2019_sobre500$limite)
# 
# paipote2020_sobre500 <- paipote2020 %>% 
#   mutate(limite = case_when(SO2 < 500 ~ "Normal",
#                             SO2 >= 500 ~ "Alerta"))
# 
# table(paipote2020_sobre500$limite)


x <- c(60,
       76,
       68,
       67,
       50,
       94,
       71,
       55,
       61,
       54)

y <- c(84,
       83,
       90,
       65,
       92,
       68,
       66,
       81,
       71,
       67)

t.test(x = x, y = y)














































































































































































































































































































































































































































































































































































































































































































































































































































































































