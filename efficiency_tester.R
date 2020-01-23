library(readxl)
library(stats)
rm(list=ls())

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
  return(array(array))
}


get_t_test_periods_match = function(pvalues_array, periods, confidence){
  periods = pvalues_array[1:periods]
  match = length(periods[which(periods>(1-confidence))])/length(periods)
  return(match)
}


get_t_test_dmu_match = function(pvalues_array, dmu, confidence){
  dmu_pvalues = pvalues_array[c((length(pvalues_array)-dmu):length(pvalues_array))]
  match = length(dmu_pvalues[which(dmu_pvalues>(1-confidence))])/length(dmu_pvalues)
  return(match)
}

get_result_matrix = function(df, reference, confidence, periods, dmu){
  result_matrix = data.frame(matrix(ncol = ncol(data), nrow = 2), row.names = c("periods match", "dmu match"))
  colnames(result_matrix)=names(data)
  for (i in names(data)){
    pvalues_array = get_t_test_pvalue(reference, x=get(i), confidence)
    result_matrix[1,i] = get_t_test_periods_match(pvalues_array, periods, confidence)
    result_matrix[2,i] = get_t_test_dmu_match(pvalues_array, dmu, confidence)
  }
  return(result_matrix)
}

setwd("C:/Users/epiph/OneDrive - Universidade de Santiago de Compostela/Proyecto MyCoast/Datos procesados")
panel_data<- read_excel("panel_data_2.xlsx")
data = panel_data[,c(127,128,129,130,131,132,133,134,135)]


for (i in 1:length(data)){
  assign(names(data)[i], split_in_variables(df=data[,i], periods=120, dmu=28))
}
(result_matrix = get_result_matrix(df=data, reference=teradial_prod_crs, confidence = 0.95, periods = 120, dmu=28))






