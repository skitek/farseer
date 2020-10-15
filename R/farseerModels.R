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
  partition_tree <- tryCatch(rpart::rpart(formula = formula, data = trainingSet), error = function(e) e)
  neural <- tryCatch(neuralnet::neuralnet(formula = formula, data = trainingSet, hidden = 5, lifesign = 'full', stepmax = 5e+05), error = function(e) e, warning = function(w) w) #neural network is created
  linear <- tryCatch(lm(formula = formula, data = trainingSet), error = function(e) e)
  
  if(checkModels(class_partition = class(partition_tree), class_neural = class(neural), class_linear = class(linear))){
    return(NULL)
  }
  
  value <- list(target = farseerDataFrame$target.variables[target], linear = linear, partition = partition_tree, neural = neural,
                call = formula, trainingVector = trainingVector)
  attr(value, "class") <- "farseer.models"
  return(value)
}

#'checkModels(S3 function)
#'
#'help function for checking, if models were created
#'
#'@return boolean
checkModels <- function(class_partition, class_neural, class_linear){
  if((class_partition[1] != "rpart") | (class_neural[1] != "nn") | (class_linear[1] != "lm")){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

#Performance measurement
#'performance.classification (S3 function)
#'
#'calculates performance measurements of models.
#'
#'
#'@param predictions a data.frame containing predictions of the models and original data
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
performance.classification <- function(predictions, cuttoff = 1, predictionNames = c("linear", "partition", "neural"), observed = "original"){
  #create labels
  if(is.numeric(cuttoff)){
      predictions$labels <- ifelse(predictions$original < cuttoff, 0, 1)
  }
  else {
      predictions$labels <- ifelse(predictions$original == cuttoff, 0, 1)
  }
  #create predictions
  predictionsList <- list()
  for(i in 1:length(predictionNames)){
    predictionsList[[predictionNames[i]]] <- ROCR::prediction(predictions = predictions[,predictionNames[i]], labels = predictions$labels)
  }
  
  #create performance
  performanceList <- list()
  aucList <- list()
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
#'@param predictions data.frame with predictied and observed values
#'@param predictionNames names of the columns, where predictions can be found
#'
#'@return 
#'list: \describe{
#'\item{errors}{data.frame with measured, predicted, absolute and relative errors for all models}
#'\item{performance}{data.frame with correlation, mean error and SD of mean error}
#'}
performance.numeric <- function(predictions, predictionNames = c("linear", "partition", "neural"), observed = "original"){
  value <- predictions
  performanceDataFrame <- data.frame(correlation = numeric(), mean_error = numeric(), error_SD = numeric())
  for(i in 1:length(predictionNames)){
    value[, paste(predictionNames[i], "_error")] <- value[, predictionNames[i]] - value[, observed]
    value[, paste(predictionNames[i], "_relative_error")] <- (value[, paste(predictionNames[i], "_error")]/value[, predictionNames[i]])*100
    correlation <- cor(value[, predictionNames[i]], value[, observed])
    mean_error <- mean(value[, paste(predictionNames[i], "_error")])
    error_SD <- sd(value[, paste(predictionNames[i], "_error")])
    performanceDataFrame[predictionNames[i],] <- c(correlation, mean_error, error_SD)
  }
  return(list(errors = value, performance = performanceDataFrame))
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
  neuralPrediction <- neuralPrediction$net.result
  
  if(is.factor(newData[,models$target])){
    #levels <- levels(newData[,models$target])
    #the second response column corresponds to values for the "higher" factor probability
    partitionPrediction <- partitionPrediction[,2]
    neuralPrediction <- neuralPrediction[,2]
    #partitionPrediction <- levels[max.col(partitionPrediction)]
    #neuralPrediction <- levels[max.col(neuralPrediction)]
  }
  
  pred <- data.frame(original = newData[,models$target], linear = linearPrediction, partition = partitionPrediction, 
                      neural = neuralPrediction)
  
  #calculate performance
#  trueValue <- newData[,models$target, drop = FALSE]
  
  if(is.numeric(pred$original)){
    perf <- performance.numeric(predictions = pred)
    perf[["plots"]] <- farseer.bland.altmann(predictions = pred, title = models$target)
    pred_type <- "numeric"
  }
  else{
    if(is.factor(pred$original)){
      levels <- levels(pred$original)
      perf <- performance.classification(predictions = pred,  cuttoff = levels[1])
      farseer.rocplot(perf$performance, targetName = models$target)
    }
    else{
      perf <- performance.classification(predictions = pred)
      farseer.rocplot(perf$performance, targetName = models$target)
    }
    pred_type <- "classification"
  }
  
  value <- list(predictions = pred, performance = perf, prediction_type = pred_type)
  return(value)
}

#generic function implementation

plot.farseer.models <- function(obj){
        
}

print.farseer.models <- function(obj){
        
}

summary.farseer.models <- function(obj){
        
}