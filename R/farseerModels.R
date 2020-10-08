#farseerModels - S3 class for storing trained models

#' farseer.models (S3 class)
#' 
#' Container class for models trained with farseer-package.
#' It is a list of models
#'  \tabular{ll}{
#'  $linear \tab linear model \cr
#'  $partition \tab partition tree \cr
#'  $neural \tab neural networks
#'  }
#' @params farseerDataSets a farseer.data.set
#' 
farseer.models <- function(farseerDataFrame, mode = "create", ...){
  if(mode == "create"){
    value <- create(farseerDataFrame, ...)
    }
  else if(mode == "retrain"){
    value <- retrain(farseerDataFrame, ...)
  }
  else{
    stop("invalid mode argument")
  }
        attr(value, "class") <- "farseerModels"
        return(value)
}

#GENERIC FUNCTIONS
#' create (generic)
create <- function(obj, ...){
  UseMethod("create")
}

#' create (farseer.data.frame)
#' 
#' Call of the create function on an farseer.data.frame object creates new farseer.models
#' 
create.farseer.data.frame <- function(farseerDataFrame, formula, additional_targets, test){
  
  return("dummy create")    
}

retrain.farseer.data.frame <- function(farseerDataFrame, formula, additional_targets, farseerModels, test){
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