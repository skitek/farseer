#' farseer(function)
#' 
#' Wrapper for easy usage of the farseer package. 
#' 
#' It generates three models, a linear model, neural networks and partition tree 
#' wrapped in an \code{\link{farseer.models}}-object. Those models can be then plotted using the generic \code{plot} function. 
#' Additionally, the \code{\link{farseer.data.frames}} are also returned for further reference. 
#' 
#' 
#' Dependant and independant variables can be either provided as formula, or if all variables should be used
#' one can simply place the dependant variable as last variable in \code{dataFrame}. 
#' If models should be created for more than one variable simultaniously, additional dependand variables 
#' can be specified. They will be excluded from \code{dataFrame} prior to training.
#' 
#' @param formula a formula for building models. If it is ommited, a new formula will be created using
#' last variable in data.frame as dependant variable and all others as independent variables.
#' @param dataFrame a data.frame containing data to be used for training.
#' @param farseerModels optional parameter, when existing models are supossed to be retrained.
#' If it remains NULL, a new set of models will be returned.
#' @param test boolean, if TRUE the models will be tested. Default is TRUE.
#' @param additional_targets a list of dependant variables, other then the one specified in formula, 
#' if models for more then one dependand variable should be created. Note, that in current version separate models are 
#' created. If all dependant variables are situated as last variables in the data.frame, farseer can be used without 
#' providing formula, as it will be automatically created. 
#' @param ... additional parameters for plotting. See \code{\link{plot.farseer.models}} for details.
#' 
#' @return 
#' \strong{If \code{additional_targets = NULL}, a list:}
#' \describe{
#'   \item{$models}{\code{\link{farseer.models}}}
#'   \item{$data.frames}{\code{\link{farseer.data.frames}}}
#'  
#'  \strong{If \code{additional_targets} are provided, a list:}
#'  \describe{
#'    \item{\code{$<dependant variable name>}}{list of models and data.frames as described above}
#'    \item{\code{$<additional_targets[1]>}}{list of models and data.frames as described above, for first additional target}
#'    \item{...}
#'    \item{\code{$<additional_targets[n]>}}{list of models and data.frames as described above, for last additional target}
#'  }
#'  }
#' 
#' @usage 
#' ##Trains new models. Creates a formula using last variable in data.frame as dependant variable 
#' ##and all others as independent variables.
#' models <- farseer(dataFrame)
#' 
#' ##Retrains models. Creates a formula using last variable in data.frame as dependant variable 
#' ##and all others as independent variables.
#' models <- farseer(dataFrame, farseerModels)
#' 
#' ##Trains new models according to formula.
#' models <- farseer(formula, dataFrame)
#' 
#' ##Retrains the models using new data. 
#' models <- farseer(formula, dataFrame, farseerModels)
#' 
#' ##Trains new models according to formula, then repeats the process for each variable
#' ##specified in target_variables
#' models <- farseer(formula, dataFrame, additional_targetc = c(<additional dependant variable names>))
#' 
#' ##Trains new models, then repeats the process for each variable specified in target_variables.
#' ##Creates a formula using last variable in data.frame as dependant variable 
#' ##and all others as independent variables, excluding variables specified in target_variables
#' models <- farseer(dataFrame, additional_targetc = c(<additional dependant variable names>))
#'  
#' @export
#' 
farseer <- function(formula = NULL, dataFrame, additional_targets = NULL, farseerModels = NULL, test = TRUE, ...){
  
  #if no formula, create one
  if(is.null(formula)){
    if(is.null(additional_targets)){
      formula <- createFormula(colnames(dataFrame))
    }
    #if there are additional targets specified, exclude them before creating formula
    else{
      names <- colnames(dataFrame)
      for(i in 1:length(additional_targets)){
        names <- names[!(names == additional_targets[i])]
      }
      formula <- createFormula(names)
    }
  }
  
  if(is.null(farseerModels)){
    #farseerModels <- farseer.models(farseerDataFrame, test = test)
  }
  else{
    #farseerModels <- farseer.models(dataFrame, farseerModels, test)
  }
  return(farseerModels)
}

#'Help function: create formula
#'
#'Creates a formula using last entry in a char vector as dependant variable 
#'and all others as independent variables.
#'
#'@param names colnames of a data.frame
#'
#'@return formula
#'
createFormula <- function(names){
  len <- length(names)
  dependent <- names[len]
  independent <- names[1:len-1]
  formula <- as.formula(paste(dependent, paste(independent, collapse = " + "), sep = " ~ "))
  return(formula)
}