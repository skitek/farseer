#farseerModels - S3 class for storing trained models

#' farseer.models (S3 class)
#' 
#' Container class for models trained with farseer-package.
#' 
#' Using this constructor will result in training/retraining for all targets 
#' specified in \code{\link{farseer.data.frame}}
#' 
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
    value <- models(farseerDataFrame, ...)
    }
  else if(mode == "retrain"){
    value <- retrain(farseerDataFrame)
  }
  else{
    stop("invalid mode argument")
  }
}

#' create (farseer.models)
#' 
#' creates a single set of farseer models
#' 
#' @param farseerDataFrame a data for training (must be a farseer.data.frame)
#' @param target an index of target from farseer.data.frame$target.variables
#' 
#' @returns a farseer.models object
#' 
create.farseer.models <- function(farseerDataFrame, target, ...){
  if(!is.farseer.data.frame(farseerDataFrame) | !is.numeric(target)){
    stop("invalid parameters")
  }
  #create selection vectors for machine learning models
  vector.normalized <- c(farseerDataFrame$normalized, farseerDataFrame$factorized)
  #vector.part <- c(setdiff(farseerDataFrame$model.variables, farseerDataFrame$factors), farseerDataFrame$factorized)
  # 
  # formula_linear <- createFormula(c(farseerDataFrame$model.variables, farseerDataFrame$target.variables[target]))
   formula <- createFormula(c(vector.normalized, farseerDataFrame$target.variables[target]))
  # formula_part <- createFormula(c(vector.part, farseerDataFrame$target.variables[target]))
  
  trainingVector <- farseer.training.vector(nrow(farseerDataFrame$data), ...)
  trainingSet <- farseerDataFrame$data[trainingVector,]
  partition_tree <- rpart::rpart(formula = formula, data = trainingSet) #partition tree is created
  neural <- neuralnet::neuralnet(formula = formula, data = trainingSet, hidden = 5) #neural network is created
  linear <- lm(formula = formula, data = trainingSet)
  
  value <- list(target = farseerDataFrame$target.variables[target], linear = linear, partition = partition_tree, neural = neural,
                call = formula, trainingVector = trainingVector)
  attr(value, "class") <- "farseer.models"
  return(value)
}


#Performance measurement
#'performance.classification (S3 function)
#'
#'calculates performance measurements of models.
#'
#'
#'@param predictions a data.frame containig predictions of the models
#'@param trueValue a vector of the measured value
#'@param cutoff numeric: value for labeling, all trueValues < cuttoff will be labeled 0, otherwise 1; 
#'non-numeric: if it is equal to cutoff, will be labeled 0, otherwise 1
#'
#'@return 
#'list \describe{
#'\item{predictions}{a list of \link[ROCR]{prediction-class} objects for every model}
#'\item{performance}{a list of \link[ROCR]{performance-class} objects for every model; performance is called with "tpr", "fpr" options}
#'\item{auc}{a list of \link[ROCR]{performance-class} objects for every model;performance is called with "auc" option}
#'}
#'
#'@export
performance.classification <- function(predictions, trueValue, cuttoff = 1, predictionNames = c("linear", "partition", "neural")){
  #create labels
  if(is.numeric(cuttoff)){
      predictions$labels <- ifelse(trueValue < cuttoff, 0, 1)
  }
  else {
      predictions$labels <- ifelse(trueValue == cuttoff, 0, 1)
  }
  #create predictions
  predictionsList <- list()
  for(i in 1:length(predictionNames)){
    predictionsList[[predictionNames[i]]] <- ROCR::prediction(predictions = predictions[,predictionNames[i]], labels = predictions$labels)
  }
  
  #create performance
  performanceList <- list()
  for(i in 1: length(predictionNames)){
    performanceList[[predictionNames[i]]] <- ROCR::performance(predictionsList[[predictionNames[i]]], "tpr", "fpr")
    aucList[[predictionNames[i]]] <- ROCR::performance(predictionsList[[predictionNames[i]]], "auc")
  }
  
  return(list(predictions = predictionsList, performance = performanceList, auc = aucList))
}

#'performance.numeric (S3 function)
#'
#'Calculates the absolute and relative errors of predictions, as well as their means and SDs.
#'Additionally calculates the correlations.
#'
#'@param predictions predicted values
#'@param trueValue observed values
#'@param predictionNames names of the columns, where predictions can be found
#'
#'@return 
#'list: \describe{
#'\item{errors}{data.frame with measured, predicted, absolute and relative errors for all models}
#'\item{performance}{data.frame with correlation, mean error and SD of mean error}
#'}
performance.numeric <- function(predictions, trueValue, predictionNames = c("linear", "partition", "neural")){
  errors <- as.data.frame(cbind(predictions, trueValue))
  performanceDataFrame <- data.frame(correlation = numeric(), mean_error = numeric(), error_SD = numeric())
  for(i in 1:length(predictionNames)){
    errors[, paste(predictionNames[i], "_error")] <- errors[, predictionNames[i]] - trueValue
    errors[, paste(predictionNames[i], "_relative_error")] <- (errors[, paste(predictionNames[i], "_error")]/errors[, predictionNames[i]])*100
    correlation <- cor(errors[, predictionNames[i]], trueValue)
    mean_error <- mean(errors[, paste(predictionNames[i], "_error")])
    error_SD <- sd(errors[, paste(predictionNames[i], "_error")])
    performanceDataFrame[predictionNames[i],] <- c(correlation, mean_error, error_SD)
  }
  return(list(errors = errors, performance = performanceDataFrame))
}
#GENERIC FUNCTIONS

#'test (generic)
predict <- function(x, y){
  UseMethod("predict")
}


retrain.farseer.data.frame <- function(farseerDataFrame, formula, additional_targets, farseerModels){
  return("dummy retrain")
}


#new generic functions

#'predict(farseer.models)
#'
#'predicts data using trained models
#'
#'@param models a farseer.models object with the trained models
#'@param newData data to be used for predictions
#'
#'@return data.frame with predictions for every model
#'
predict.farseer.models <- function(models, newData){
  linearPrediction <- predict.lm(models$linear, newData)
  partitionPrediction <- rpart.plot::rpart.predict(models$partition, newData)
  neuralPrediction <- neuralnet::compute(models$neural, newData)
  
  pred <- data.frame(original = newData[,models$target], linear = linearPrediction, partition = partitionPrediction, 
                      neural = neuralPrediction$net.result)
  
  #calculate performance
  trueValue <- newData[,models$target, drop = FALSE]
  
  if(is.numeric(trueValue)){
    perf <- performance.numeric(predictions = pred, trueValue = trueValue)
  }
  else{
    if(is.factor(trueValue)){
      levels <- levels(trueValue)
      perf <- performance.classification(predictions = pred, trueValue = trueValue, cuttoff = levels[1])
    }
    else{
      perf <- performance.classification(predictions = pred, trueValue = trueValue)
    }
  }
  
  value <- list(predictions = pred, performance = perf)
  return(value)
}

#generic function implementation

plot.farseer.models <- function(obj){
        
}

print.farseer.models <- function(obj){
        
}

summary.farseer.models <- function(obj){
        
}