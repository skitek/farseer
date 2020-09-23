#'Farseer - a simple machine learning model creation program
#'Creates, tests and predicts data from a given dataset
#'This program is ditrubuted AS IS WITH ABSOLUTE NO WARRANTY under GNU GPL v. 3.0
#'Please feel free to modify and distribute
#'Copyright Kornel Skitek 2017 - 2020


#'main functions

################
#'farseer.create#
################

#function creates a training and testing datasets.75% of cases are randomly assigned to training set, 25% to testing
#a list is returned, the first element is training dataset, the second is testing dataset
#attributes: modelVariables - the columns to be selected from the dataset BUG ALERT: include the target variable, data - the source dataset
#targetVariable is only used if the target variable is a factor
#factorize - if set to true, the set is normalized and factorised
#ratio - sets the amount of data for testing set (base 0.75)
#returns a list ret with $TrainingSet (ratio of cases), $TestingSet (1-ratio of cases)
#IN CASE OF NORMALIZATION THE MAXMIN OF THE TARGETVARIABLE IS NOT SAVED; HAS TO BE SAVED PRIOOR (VIDE FUNCTION CREATE_MODELS)
#!!!BUG ALERT - the neuralnet function does not work for factor target variables, use nnet instead. And add target_variable to the sourceDatabase

createDatasets <- function (data, modelVariables, factorize = FALSE, targetVariable = NULL, ratio = 0.75){
  
  ret <- list(TrainingSet = NULL, TestingSet = NULL) #init, maxmin is used to defactorize the outcome variable
  
  sourceDatabase <- data[, modelVariables] #selects only variables for modelvariables, this step is required for complete.cases to work in the next line
  sourceDatabase <- sourceDatabase[complete.cases(sourceDatabase), ] #creates a clean (without NAs) database
  
  if(factorize == TRUE){                                    #factorization and normalization occurs only if factorize == TRUE
    sourceDatabase <- prepare_dataset(sourceDatabase)       #factorises the dataset
    #ret$maxmin$max <- max(sourceDatabase[, targetVariable]) #saves the max value of the targetVariable
    #ret$maxmin$min <- min(sourceDatabase[, targetVariable]) #saves the min value of the targetVariable
    
    sourceDatabase <- as.data.frame(lapply(sourceDatabase, normalize)) #normalizes the database
  }
  
  cases <- length(sourceDatabase[, 1]) #number of cases is stored
  n <- as.integer((ratio)*cases) #number of cases for training set
  
  #set.seed(123)
  trainSample <- sample(cases, n) #a random sample of cases numbers is created
  
  ret$TrainingSet <- sourceDatabase[trainSample, ] #selects the cases for training
  ret$TestingSet <-  sourceDatabase[-trainSample, ] #selects the cases for testing (due to - operator)
  return(ret)
}

#function createModels create a decision tree, a partition tree, a naive bayes and linear model for target_variables
#returns a list with those models
#attributes: model_variables - a vector with variables to be included in the model
#target_variable - a variable to be modeled
#data - source data
#if test is TRUE, the models are also tested
createModels <- function(data, model_variables, target_variable, test = FALSE, mass = FALSE, ratio = 0.75){
  
  library("rpart")
  library("neuralnet")
  model_variables_new <- NULL #init
  
  ret <- list(partition_tree = NULL, linear = NULL, neural = NULL)
  database <- data[ , c(model_variables, target_variable)] #a subset of the source database is created for the model 
  denormalize_data <- list(max = max(database[, target_variable], na.rm = TRUE), min = min(database[, target_variable], na.rm = TRUE))
  #Sets <- createDatasets(database, colnames(database))
  SetsNormalized <- createDatasets(database, colnames(database), factorize = TRUE, ratio = ratio)
  names <- colnames(SetsNormalized$TrainingSet)
  
  for(x in 1:length(names)){  #a new model variables, after factorization, is created, the target_variable has to be excluded form the colnames of the set
    if(!identical(target_variable, names[x]))
      model_variables_new <- c(model_variables_new, names[x])
  }
  
  formula <- createFormula(target_variable, model_variables_new)
  formula_neural <- createFormula(target_variable, model_variables)
  
  #set.seed(123) #a seed is set to keep the models constant
  ret$partition_tree <- rpart(formula, data = SetsNormalized$TrainingSet) #partition tree is created
  ret$neural <- neuralnet(formula = formula, data = SetsNormalized$TrainingSet, hidden = 5) #neural network is created
  ret$linear <- lm(formula = formula, data = SetsNormalized$TrainingSet)
  
  if(test == TRUE){
    #prop_data = rbind(SetsNormalized$TestingSet, SetsNormalized$TrainingSet)
    modelsPredict(models = ret, data = SetsNormalized$TestingSet, target_variable = target_variable, mass = mass, maxmin = denormalize_data)
    return(ret)
  }
  return(ret)
}

#farseer.create is a function for creating models
#parameters dataSet - DataFrame with all collected data, 
#model_variables - predictors, can be given as a numerical or character vector
#target_variables - targets for the model, can be given as a numerical or character vector
#ratio sets the ratio for training set
farseer.create <- function(dataSet, model_variables, target_variables, ratio = 0.75){
  returnModels <- NULL
  for(x in 1:length(target_variables)){
    models <- createModels(data = data, model_variables = model_variables, target_variable = target_variables[x], test = TRUE, mass = TRUE, ratio = ratio)
    returnModels <- rbind(returnModels, models)
  }
  dimnames(returnModels)[[1]] <- as.list(target_variables)
  returnModelsurn(returnModels)
}

####################
#farseer.create END#
####################

#farseer.test is a function for testing models
#parameters:
#models - models, supported by farseer, to be tested
#dataSet - a DataFrame with data to be used for testing
#blandAltman - boolean, if true, blandAltman plots will be created
farseer.test <- function(models, dataSet, blandAltman = TRUE){
  
}

#farseer.plot plots the supported models, using default plotting functions
#models has to be a onedimensional list
farseer.plot <- function(models){
  for(x in length(models)){
    if(isClass("rpart",models[x])){
      plot.rpart(models[x])
    }
    else{
      plot(models[x])
    }
  }
}

farseer.predict <- function(models, dataSet){
  
}


