library("plyr")  
library("dplyr")                                                
library("readr")  
library("readxl")
library("tidyverse")



## PARA LOS KPI

### DATAFRAME MODELADOS

Modelados <- list.files(path = "Geoaire/ENAMI/Modelados/2022/11.Noviembre//Meteorologia",    
                        pattern = "*.xlsx",
                        full.names = TRUE) 


View(Modelados)

Modelados <- Modelados[2:28]

####################################################################################################################################

NumberOfDaysInMonth = 27

MyLogicVector <- rep(c(T,F,F), NumberOfDaysInMonth) # T,F,F    T,F,F    T,F,F   ... 31 times (93 items)

MyLogicVector_2 <- rep(MyLogicVector, each = 24)  # T,T,T... 24 times, F,F,F,... 24 times, F,F,F,... 24 times, ...(93*24) times = 2232 times

Mysequence <- seq(1:(NumberOfDaysInMonth*24*3))  # 1,2,3,4...2232 (744 * 3) = 744 hours in a period of 31 days and 3 because each excel file has 3 days.

MyNewIndex <- Mysequence[MyLogicVector_2]  # 1,2,3,...24, 73,74,75,...96, 145,146,147,...168, 217,218,219,... ... 2182,2183,2184 (only correct indeces)

####################################################################################################################################


all_sheets <- excel_sheets(Modelados[1])

first_sheet_contents <- map_df(.x = Modelados,
                               .f = ~ read_excel(path = .x,
                                                 sheet = all_sheets[1]))

second_sheet_contents <- map_df(.x = Modelados,
                                .f = ~ read_excel(path = .x,
                                                  sheet = all_sheets[2]))

third_sheet_contents <- map_df(.x = Modelados,
                                .f = ~ read_excel(path = .x,
                                                  sheet = all_sheets[3]))

fourth_sheet_contents <- map_df(.x = Modelados,
                               .f = ~ read_excel(path = .x,
                                                 sheet = all_sheets[4]))

fifth_sheet_contents <- map_df(.x = Modelados,
                               .f = ~ read_excel(path = .x,
                                                 sheet = all_sheets[5]))

sixth_sheet_contents <- map_df(.x = Modelados,
                               .f = ~ read_excel(path = .x,
                                                 sheet = all_sheets[6]))

seventh_sheet_contents <- map_df(.x = Modelados,
                               .f = ~ read_excel(path = .x,
                                                 sheet = all_sheets[7]))

####################################################################################################################################


first_sheet_contents <- first_sheet_contents[MyNewIndex,]  # correct dataframe.

second_sheet_contents <- second_sheet_contents[MyNewIndex,]  # correct dataframe.

third_sheet_contents <- third_sheet_contents[MyNewIndex,]  # correct dataframe.

fourth_sheet_contents <- fourth_sheet_contents[MyNewIndex,]  # correct dataframe.

fifth_sheet_contents <- fifth_sheet_contents[MyNewIndex,]  # correct dataframe.

sixth_sheet_contents <- sixth_sheet_contents[MyNewIndex,]  # correct dataframe.

seventh_sheet_contents <- seventh_sheet_contents[MyNewIndex,]  # correct dataframe.


####################################################################################################################################

write_excel_csv(first_sheet_contents, file = "Geoaire/ENAMI/Modelados/2022/11.Noviembre/Meteorologia/Combinados/Paipote.csv")

write_excel_csv(second_sheet_contents, file = "Geoaire/ENAMI/Modelados/2022/11.Noviembre/Meteorologia/Combinados/Copiapo.csv")

write_excel_csv(third_sheet_contents, file = "Geoaire/ENAMI/Modelados/2022/11.Noviembre/Meteorologia/Combinados/Pabellon.csv")

write_excel_csv(fourth_sheet_contents, file = "Geoaire/ENAMI/Modelados/2022/11.Noviembre/Meteorologia/Combinados/Volcanes.csv")

write_excel_csv(fifth_sheet_contents, file = "Geoaire/ENAMI/Modelados/2022/11.Noviembre/Meteorologia/Combinados/TierraAmarilla.csv")

write_excel_csv(sixth_sheet_contents, file = "Geoaire/ENAMI/Modelados/2022/11.Noviembre/Meteorologia/Combinados/SanFdo.csv")

write_excel_csv(seventh_sheet_contents, file = "Geoaire/ENAMI/Modelados/2022/11.Noviembre/Meteorologia/Combinados/Principal.csv")




















































































