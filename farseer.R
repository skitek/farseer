#' farseer
#' 
#' Wrapper for easy user of the farseer package. 
#' 
#' It generates 3 models, wrapped in an \code{\link{farseer.models}}-object.
#' Those models can be then plotted using the generic \code{\link{plot.farseer.models}} function. 
#' Additionally, the farseer.data.frames are returned. It is a list containig:
#' 
#' \describe{
#'   \item{$original}{original \code{data.frame.}}
#'   \item{$factorised}{factors converted to numeric [0,1] values.}
#'   \item{$normalised}{all variables are converted to have range [0,1]}
#' }
#' 
#' see \code{\link{farseer.models}} or \code{\link{farseer.data.frames}} for more details.
#' 
#' @param formula a formula for building models. If it is ommited, a new formula will be created using
#' last variable in data.frame 
#' @param dataFrame a data.frame containing data to be used for training.
#' @param farseerModels optional parameter, when existing models are supossed to be retrained.
#' If it remains NULL, a new set of models will be returned.
#' @param test boolean, if TRUE the models will be tested. Default is TRUE.
#' 
#' @return a list: 
#' \describe{
#'   \item{$models}{\code{\link{farseer.models}}}
#'   \link{$data.frames}{\code{\link{farseer.data.frames}}}
#'  }
#' 
#' @usage models <- farseer(formula, dataFrame)
#' trains new models according to formula.
#' @usage models <- farseer(formula, dataFrame, farseerModels)
#' retrains the models using new data. For specifics please refer to \code{\link{farseer.models.retrain}}.
#' 
#' @export
#' 
farseer <- function(formula = NULL, dataFrame, additional_targets = NULL, farseerModels = NULL, test = TRUE){
  
  if(is.null(frormula)){
    
  }
  
  if(is.null(farseerModels)){
    farseerModels <- farseer.models.create(formula, dataFrame, additional_targets, test)
  }
  else{
    farseerModels <- farseer.models.retrain(formula, dataFrame, additional_targets, farseerModels, test)
  }
}