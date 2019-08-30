library(readxl)
library(stats)

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


get_t_test_pvalue = function(reference, x, confidence){
  array=matrix(nrow=1, ncol=ncol(reference)+nrow(reference))
  for (i in 1:ncol(reference)){
    t=t.test(x=reference[,i], y=x[,i], conf.level = confidence)
    array[,i]=t$p.value
  }
  for (j in 1:nrow(reference)){
    t=t.test(x=reference[j,], y=x[j,], conf.level = confidence)
    array[,ncol(reference)+j]=t$p.value
  }
  return(array)
}


get_t_test_periods_match = function(pvalues_array, periods, confidence){
  periods = array(pvalues_array[,c(1:periods)])
  match = length(periods[which(periods<confidence)])/length(periods)
  return(match)
}


pvalues_array = get_t_test_pvalue(reference=teradial_prod_crs, x=SFA_c_c, confidence = 0.95)
periods_match = get_t_test_periods_match(pvalues_array, periods = 120, confidence = 0.95)


