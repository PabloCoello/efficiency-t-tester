library(readxl)


setwd("C:/Users/epiph/OneDrive - Universidade de Santiago de Compostela/Proyecto MyCoast/Datos procesados")
panel_data<- read_excel("panel_data_2.xlsx")
data = panel_data[,c(127,128,129,130,131)]
