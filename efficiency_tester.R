library(readxl)

rm(list=ls())
setwd("C:/Users/epiph/OneDrive - Universidade de Santiago de Compostela/Proyecto MyCoast/Datos procesados")
panel_data<- read_excel("panel_data_2.xlsx")
data = panel_data[,c(127,128,129,130,131)]

split_in_variables = function(df, periods, dmu){
    variable = df
    countercol = 0
    counterrow = 1
    toret=matrix(ncol = periods, nrow = dmu)
    for (j in 1:nrow(variable)){
      countercol=countercol+1
      if (countercol == periods+1){
        countercol = 1
        counterrow=counterrow+1
      }
      toret[counterrow,countercol]=as.numeric(variable[j,1])
    }
    return(toret)
  }

for (i in 1:length(data)){
  assign(names(data)[i], split_in_variables(df=data[,i], periods=120, dmu=28))
}

