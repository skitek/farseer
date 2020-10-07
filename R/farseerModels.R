#farseerModels - S3 class for storing trained models


farseer.models <- function(farseerDataSets){
        value = list()
        attr(value, "class") <- "farseerModels"
        value
}

farseer.models.create <- function(formula, farseerDataSet, additional_targets, test){
  return("dummy create")    
}

farseer.models.retrain <- function(formula, farseerDataSet, additional_targets, farseerModels, test){
  return("dummy retrain")
}

#new generic functions

#generic function implementation

plot.farseer.models <- function(obj){
        
}

print.farseer.models <- function(obj){
        
}

summary.farseer.models <- function(obj){
        
}