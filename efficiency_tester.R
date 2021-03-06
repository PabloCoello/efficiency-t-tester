library(readxl)
library(readr)
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


get_test_periods_match = function(pvalues_array, periods, confidence){
  periods = pvalues_array[1:periods]
  match = length(periods[which(periods>(1-confidence))])/length(periods)
  return(match)
}


get_test_dmu_match = function(pvalues_array, dmu, confidence){
  dmu_pvalues = pvalues_array[c((length(pvalues_array)-dmu):length(pvalues_array))]
  match = length(dmu_pvalues[which(dmu_pvalues>(1-confidence))])/length(dmu_pvalues)
  return(match)
}

get_wilcox_test_pvalue = function(reference, x, confidence){
  array=matrix(nrow=1, ncol=ncol(reference)+nrow(reference))
  for (i in 1:ncol(reference)){
    wilcox=wilcox.test(x=reference[,i], y=x[,i], conf.level = confidence)
    array[,i]=wilcox$p.value
  }
  for (j in 1:nrow(reference)){
    wilcox=wilcox.test(x=reference[j,], y=x[j,], conf.level = confidence)
    array[,ncol(reference)+j]=wilcox$p.value
  }
  return(array(array))
}

get_result_matrix = function(df, reference, confidence, periods, dmu){
  result_matrix = data.frame(matrix(ncol = 4, nrow = ncol(data)))
  rownames(result_matrix)=names(data)
  colnames(result_matrix) = c("t.test periods match", "t.test dmu match",
                "wilcox.test periods match"," wilcox.test dmu match")
  for (i in names(data)){
    pvalues_array = get_t_test_pvalue(reference, x=get(paste("sum_",i,sep="")), confidence)
    result_matrix[i,1] = get_test_periods_match(pvalues_array, periods, confidence)
    result_matrix[i,2] = get_test_dmu_match(pvalues_array, dmu, confidence)
    pvalues_array = get_wilcox_test_pvalue(reference, x=get(paste("sum_",i,sep="")), confidence)
    result_matrix[i,3] = get_test_periods_match(pvalues_array, periods, confidence)
    result_matrix[i,4] = get_test_dmu_match(pvalues_array, dmu, confidence)
  }
  return(result_matrix)
}

get_annual_sum = function(x, periods, dmu){
  toret = matrix(ncol=periods/12, nrow=dmu)
  array = array(dim=12)
  for (i in 1:nrow(x)){
    counterrow = 0
    countercol = 0
    for (j in 1:ncol(x)){
      counterrow = counterrow+1
      array[counterrow] = x[i,j]
      if (counterrow==12){
        counterrow = 0
        countercol = countercol+1
        toret[i,countercol] = sum(array)/length(array)
      }
    }
  }
  return(toret)
}

setwd("C:/Users/epiph/OneDrive/Desktop/Reunion 17-09-2019")
data <- read_excel("Result_matrix17-09-2019.xlsx")

correlations = cor(data)
pairs(data, panel = panel.smooth)

for (i in 1:length(data)){
  assign(names(data)[i], split_in_variables(df=data[,i], periods=120, dmu=28))
  assign(paste("sum",names(data)[i],sep="_"), get_annual_sum(x=get(names(data)[i]), periods=120, dmu=28))
}

(result_matrix_total_traffic = get_result_matrix(df=data, reference=sum_teradial_vrs, confidence = 0.95, periods = 10, dmu=28))


#write.csv2(x=result_matrix_fish,file="matrix_liquid_fish.csv")
#t.test(x=COBB_c[,1],y=teradialbc_prod_crs_hetero[,1])
save.image("ttest.RData")
