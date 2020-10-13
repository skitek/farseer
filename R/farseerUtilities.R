#General help functions

#' farseer.training.vector (function)
#' 
#' Gets a integer vector of case numbers for the training set
#' 
#' @param noOfCases integer, number of cases in a data.frame
#' @param trainingFraction how many cases should be a training set?
#' @param seed if it is not NULL, a seed for randomization is set
#' 
#' @usage 
#' ##gets a psedo-random vector
#' trainingVector <- farseer.training.vector(nrow(dataFrame))
#' 
#' ##smaller training fraction
#' trainingVector <- farseer.training.vector(nrow(dataFrame), trainingFraction = 0.7)
#' 
#' ##set a seed
#' trainingVector <- farseer.training.vector(nrow(dataFrame), seed = 123)
#' 
#' @examples
#' #create a farseer.data.frame
#' data <- farseer.data.frame(formula = formula, dataFrame)
#' #get the vector
#' training.vector <- farseer.training.vector(noOfCases = nrows(data$data))
#' 
#' #get the sets
#' training.set <- data$data[training.vector,]
#' testing.set <- data$data[-training.vector,]
#' 
#' @export
farseer.training.vector <- function(noOfCases, trainingFraction = 0.9, seed = NULL){
  if(!is.null(seed)){
    set.seed(seed = seed)
  }
  vector <- sort(sample(noOfCases,(trainingFraction*noOfCases)))
  return(vector)
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
#'@export
createFormula <- function(names){
  len <- length(names)
  dependent <- names[len]
  independent <- names[1:len-1]
  formula <- as.formula(paste(dependent, paste(independent, collapse = " + "), sep = " ~ "))
  return(formula)
}

#' Help function: draw multiple plots
#' 
#' @param plots a character vector of plots to be created from c("Correlation", "Bland-Altman", "ROC")
#' 
#' @export
multiplot <- function(plots, formula, dataFrame){
  
}
#BUG: use GGPLOT2!!!

#'rocplot (S3 function)
#'
#'creates a roc-plot for models
#'
#'@param predictions a data.frame containig predictions of the models
#'@param trueValue a vector of the measured value
#'@param cutoff numeric value for labeling, all trueValues < cuttoff will be labeled 0, otherwise 1
#'
#'@return a roc plot
#'
#'@export
rocplot <- function(predictions, trueValue, cuttoff = 1, predictionNames = c("linear", "partition", "neural"), title = NULL){
  #create labels
  predictions$labels <- ifelse(trueValue < cuttoff, 0, 1)
  #create predictions
  predictionsList <- list()
  for(i in 1:length(predictionNames)){
    predictionsList[[predictionNames[i]]] <- ROCR::prediction(predictions = predictions[,predictionNames[i]], labels = predictions$labels)
  }
  
  #create performance
  performanceList <- list()
  for(i in 1: length(predictionNames)){
    performanceList[[predictionNames[i]]] <- ROCR::performance(predictionsList[[predictionNames[i]]], "tpr", "fpr")
    auc <- ROCR::performance(predictionsList[[predictionNames[i]]], "auc")
    print(paste(predictionNames[i], ": ", as.character(auc@y.values[[1]])))
  }
  
  #plot
  if(is.null(title)){
    title <- paste("ROC plot for ", names(trueValue))
  }
  ROCR::plot(performanceList[[1]], lty = 1, main = title)
  for(i in 2:length(performanceList)){
    ROCR::plot(performanceList[[i]], lty = i, add = TRUE)
  }
  legend(0.8, 0.5, predictionNames, lty = c(1:length(performanceList)))
  #return(rocpl)
}