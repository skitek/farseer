#' farseer(S3-class)
#' 
#' Wrapper for easy usage of the farseer package. 
#' 
#' It generates three models, a linear model, neural networks and partition tree 
#' wrapped in an \code{\link{farseer.models}}-object. Those models can be then plotted using the generic \code{plot} function. 
#' Additionally, the \code{\link{farseer.data.frames}} are also returned for further reference. 
#' 
#' 
#' Dependent and independent variables can be either provided as formula, or if all variables should be used
#' one can simply place the dependent variable as last variable in \code{dataFrame}. 
#' If models should be created for more than one variable simultaneously, additional dependednt variables 
#' can be specified. They will be excluded from \code{dataFrame} prior to training.
#' 
#' @param formula a formula for building models. If it is omitted, a new formula will be created using
#' last variable in data.frame as dependent variable and all others as independent variables.
#' @param dataFrame a data.frame containing data to be used for training.
#' @param farseerModels optional parameter, when existing models are supposed to be retrained.
#' If it remains NULL, a new set of models will be returned.
#' @param test boolean, if TRUE the models will be tested. Default is TRUE.
#' @param additional_targets a list of dependent variables, other then the one specified in formula, 
#' if models for more then one dependand variable should be created. Note, that in current version separate models are 
#' created. If all dependent variables are situated as last variables in the data.frame, farseer can be used without 
#' providing formula, as it will be automatically created. 
#' @param ... additional parameters for plotting. See \code{\link{plot.farseer.models}} for details.
#' 
#' @return 
#' \strong{If \code{additional_targets = NULL}, a list:}
#' \describe{
#'   \item{$models}{\code{\link{farseer.models}}}
#'   \item{$data.frames}{\code{\link{farseer.data.frames}}}
#'   \item{$test}{if tested, predictions, performance and plots are provided}
#'  
#'  \strong{If \code{additional_targets} are provided, a list:}
#'  \describe{
#'    \item{\code{$<dependant variable name>}}{list of models, data.frames and test as described above}
#'    \item{\code{$<additional_targets[1]>}}{list of models, data.frames and test as described above, for first additional target}
#'    \item{...}
#'    \item{\code{$<additional_targets[n]>}}{list of models, data.frames and test as described above, for last additional target}
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
    predictions <- NULL
    farseerDataFrame <- farseer.data.frame(dataFrame = dataFrame, formula = formula, additional_targets)
    farseerModels <- farseer.models(farseerDataFrame = farseerDataFrame, ...)
    predictions <- NULL
    if((test) & !is.null(farseerModels)){
      for(i in 1:length(farseerModels)){
         testingSet <-  farseerDataFrame$data[-farseerModels[[i]]$trainingVector, ]
         prediction <- predict(farseerModels[[i]], testingSet)
         predictions[[farseerModels[[i]]$target]] <- prediction
      }
    }
    value <- list(models = farseerModels, data.frame = farseerDataFrame, predictions = predictions)
    attr(value, "class") <- "farseer"
    if(is.null(value$predictions)){
      plot.farseer(value)
    }
    return(value)
  }
  else{
    #farseerModels <- farseer.models(dataFrame, farseerModels, test)
  }

}

plot.farseer <- function(obj){
  roc_plot_list <- list()
  #roc_counter <- 1
  ba_plot_list <- list()
  #ba_counter <- 1
  targetNames <- names(obj$predictions)
  for(i in 1:length(obj$predictions)){
    if(obj$predictions[[i]]$prediction_type == "numeric"){
      grobs = obj$predictions[[i]]$performance$plots
      grobs$table <- gridExtra::tableGrob(round(obj$predictions[[i]]$performance$performance, 2))
      finalPlot <- gridExtra::arrangeGrob(grobs = grobs, nrow = 2, ncol = 2, heights = c(1,1), widths = c(1,1))
      finalPlot <- ggpubr::annotate_figure(finalPlot, top = ggpubr::text_grob(paste("Bland-Altman plots for ", names(obj$predictions[i]))))
      ba_plot_list[[targetNames[i]]] <- gridExtra::grid.arrange(finalPlot)
      #ba_counter <- ba_counter+1
    }
    else if(obj$predictions[[i]]$prediction_type == "classification"){
      finalPlot <- obj$predictions[[i]]$performance$plots
      annotation <- "AUC: "
      annotationNames <- names(obj$predictions[[i]]$performance$auc)
      for(j in 1:length(obj$predictions[[i]]$performance$auc)){
        annotation = paste(annotation, annotationNames[j], 
                           as.character(round(obj$predictions[[i]]$performance$auc[[j]]@y.values[[1]], 2)))
      }
      finalPlot <- ggpubr::annotate_figure(finalPlot, bottom = ggpubr::text_grob(annotation))
      roc_plot_list[[targetNames[i]]] <- finalPlot
      print(roc_plot_list[[targetNames[i]]])
      #roc_counter <- roc_counter + 1
    } 
  }
  value <- list();
  value$performance_plots <- c(ba_plot_list, roc_plot_list)
  plot.partition.models.farseer(obj)
  #value <- gridExtra::arrangeGrob(grobs = c(roc_plot_list, ba_plot_list), nrow = round(no_plots/2), ncol = round(no_plots/3))
  #ba_plots <- gridExtra::arrangeGrob(grobs = ba_plot_list)
  #ba_plots <- ggpubr::annotate_figure(plots, top = ggpubr::text_grob(paste("Bland-Altman plot for: ", title), face = "bold", size = 14))
  #value <- gridExtra::grid.arrange(roc_plots, ba_plots)
  return(value)
}

plot.partition.models.farseer <- function(fars){
  modelNames <- names(fars$models)
  value <- list()
  for(i in 1:length(fars$models)){
    value[[i]] <- rpart.plot::rpart.plot(fars$models[[i]]$partition, main = paste("Decision tree for ", modelNames[i]), roundint = FALSE)
  }
  names(value) <- modelNames
  return(value)
}

#'farseer.simulate
#'
#'simulates all possible factor levels, and/or numerical data. In case of numerical data, values 0:1 with a difference of 0.1 will be generated,
#'as to simulate wide range of normalized data. This behavior can be changed by setting range and difference parameter.
#'
#'@param fars a farseer object.
#'@param newData a data.frame with data of the patients to be simulated. All variables have to be named exactly the same as in training set, 
#'but newData can contain more variables. It is subsetted prior to evaluation.
#'@param simulatFor a character vector of columns, for which values should be simulated
#'@param models character vector which models should be used for prediction. If left NULL, all available models will be used.
#'@param range range of values to be simulated for numeric values.
#'@param difference <- difference between values in range
#'
#'@return data.frame with simulated and predicted values
#'
#'@export
farseer.simulate <- function(fars, newData, simulateFor, modelNames = NULL, range = c(0,1), difference = 0.1){
  modelVariables <- fars$data.frame$model.variables
  newData <- newData[,modelVariables]
  #prepare the dataset, normalizes numerical values using max and min values from training set
  normalizeVector <- names(fars$data.frame$maxmin$max)
  newData[,normalizeVector]   <- sweep(newData[,normalizeVector], 2, as.numeric(fars$data.frame$maxmin$min))
  newData[,normalizeVector]   <- sweep(newData[,normalizeVector], 2, (as.numeric(fars$data.frame$maxmin$max) - as.numeric(fars$data.frame$maxmin$min)), 
                                       FUN = "/")
  #generate all possible combinations of simulated values
  valuesList <- list()
    for(j in 1:length(simulateFor)){
      if(is.factor(newData[,simulateFor[j]])){
        valuesList[[simulateFor[j]]] <- levels(newData[,simulateFor[j]])
      }
      else{
        valuesList[[simulateFor[j]]] <- seq(range[1], range[2], difference)
      }
    }
  dataset <- expand.grid(valuesList)
  simnames <- do.call(paste, c(colnames(dataset), dataset, sep = "_"))
  finalData <- data.frame()
  caseNames <- rownames(newData)
  for(i in 1:nrow(newData)){
    columns <- setdiff(colnames(newData), colnames(dataset))
    tempData <- dataset
    tempData[,columns] <- newData[i, columns]
    rownames(tempData) <- paste("Patient", caseNames[i], "sim", simnames, sep = "_")
    finalData <- rbind(finalData, tempData)
  }
  rowna <- rownames(finalData)
  finalData <- factorize.data.frame(finalData)
  #finalData[,names(fars$data.frame$maxmin$max)]
  if(is.null(modelNames)){
      modelNames <- names(fars$models)
  }
  for(i in 1:length(modelNames)){
    predictions <- predict(fars$models[[modelNames[i]]], finalData, test = FALSE)
    predictions <- predictions$predictions
    colnames(predictions) <- paste(colnames(predictions), modelNames[i], sep = "_")
    finalData <- cbind(finalData, predictions)
  }
  rownames(finalData) <- rowna
  return(finalData)
}