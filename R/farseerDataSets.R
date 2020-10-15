#'farseerDataSet - S3 class for creating and storing factorized and normalized datasets
#'
#'
#' @param dataFrame a data.frame containing data for modeling
#'
#' functions: \link{factorize.data.set}
#' @return list
#' $data: resulting data.frame
#' $factorised: character vector of columns containing vectors converted to numerical values 0,1 See \code{\link{factorize.data.frame}}
#' $normalised: character vector of columns containing normalized numerical values See \code{\link{normalize.data.frame}}
#' $timestamp: date and time of creation
#' $denormalization: used for denormalization of data, if needed
#' $model.variables: character vector of model varaibles
#' $target.variables: character vector of target variables
#' @author Kornel Skitek 2020
#' @export
farseer.data.frame <- function(dataFrame, formula, additional_targets = NULL){
        #deparsing formula
        primary.target <- attr(terms(formula), "variables")
        primary.target <- as.character(primary.target[[2]])
        target.variables <- c(primary.target, additional_targets)
        model.variables <- attr(terms(formula), "term.labels")
        
        #prepare model data, create a logical vector for selecting factors, non-factors
        originalData <- dataFrame[, c(model.variables, target.variables)]
        factors <- sapply(originalData[, model.variables], is.factor)
        #check, if data is either numeric or factors
        bad_columns <- colnames(originalData)
        bad_columns <- bad_columns[!((sapply(originalData, is.factor) | sapply(originalData, is.numeric)))]
        if(length(bad_columns) != 0){
          stop("Data has to be either factors, logical, or numerical values, column/s [", bad_columns, "] is/are not")
        }
        #check, if target.variables defined as factors are binary
        target.factors <- target.variables[sapply(originalData[, target.variables, drop = FALSE], is.factor)]
        if(length(target.factors) != 0){
        for(i in 1:length(target.factors)){
          if(!length(levels(originalData[,target.factors[i]])) == 2){
            stop(paste("Targets, if they aare factors, can only be binary;", target.factors[i], "is not"))
          }
        }
        }
        #select complete cases only
        originalData <- originalData[complete.cases(originalData),]
        
        #change all factors to 0,1 integers
        factorizedData <- factorize(originalData[, model.variables[factors]]) 
                                
        #normalize all numeric data
        normalizedData <- normalize(originalData[, model.variables[!factors]])
        
        creationDate <- Sys.Date()
        value = list(data = cbind(factorizedData, normalizedData, originalData[,target.variables, drop = FALSE]), 
                     factorized = colnames(factorizedData), 
                     normalized = colnames(normalizedData), 
                     timestamp = creationDate, 
                     model.variables = model.variables, 
                     factor.variables = factors,
                     target.variables = target.variables)
        attr(value, "class") <- "farseer.data.frame"
        return(value)
}


#generic functions

#' generic function for factorizing - changind factor levels into numerical 0 or 1 values
#'
#' @param obj - object to be factorized
#'
#' see factorize.data.frame for details
#' @export
factorize <- function(obj){
        UseMethod("factorize")
}

#' generic function for normalizing
#'
#' @param obj
#'
#' see normalize.data.frame for details
#' @export
normalize <- function(obj){
        UseMethod("normalize")
}


#' factorize.data.frame
#'
#' This function converts all factors to numeric values 0 and 1.
#'
#' For every factor level save one a new variable is created {name_factor_name}
#' last factor level does not have to be included, as if all the values are 0 it means this case belongs to the group
#' @param dataFrame a dataFrame object with data to be factorized. Non-Factor data will be left unchanged.
#' @examples
#' \preformatted{
#'  example <- data.frame(Patient_Number = c(1:5), Sex = c("male", "female", "diverse", NA , "male"))
#'  factorize.data.frame(example)
#'  >example
#'     Patient_Number     Sex
#'     1              1    male
#'     2              2  female
#'     3              3 diverse
#'     4              4    <NA>
#'     5              5    male
#'  > factorize.data.frame(example)
#'     Patient_Number Sex_diverse Sex_female
#'     1              1           0          0
#'     2              2           0          1
#'     3              3           1          0
#'     4              4          NA         NA
#'     5              5           0          0
#'}
#'@author Kornel Skitek 2020
#'@export
factorize.data.frame <- function(dataFrame){
        ret <- data.frame()
        names <- colnames(dataFrame)
        for (x in 1:length(colnames(dataFrame))){
                if(is.factor(dataFrame[, x])){
                        factor_levels <- levels(dataFrame[, x])             #saves the factor names for the second for loop
                        for(a in 1:(length(factor_levels) - 1)){ #this loop binarises a factor to levels() - 1 variables
                                for (b in 1:length(dataFrame[, x])){
                                        #if (is.na(dataFrame[b, x])) { ret[b, paste(names[x], factor_levels[a], sep = "_")] <- NA} else  #if the value is missing, an NA is saved in a new dataframe
                                                ret[b, paste(names[x], factor_levels[a], sep = "_")] <- ifelse(identical(as.character(dataFrame[b, x]), factor_levels[a]), 1 , 0) #if the value for a case is the same as the checked factor level in 1 is saved in the appropriate variable, if it belongs to the last factor level - 0 in all new variables
                                }
                        }
                }
                else if(is.numeric(dataFrame[ , x])){
                        for (b in 1:length(dataFrame[, x])) { ret[b, names[x]] <- dataFrame[b, names[x]] }
                }
        }
        return(ret)
}

#normalization

#' normalize converts all the values into values of range(0,1)
#'
#' This allows for correct training, if the variables have very different ranges
#' for example:
#' if a antihypertension drug has a dosage range of 2 to 10 mg
#' and initial mean blood pressure of our patients varies from 100 to 200
#' the initial blood pressure will be overfitted, compared to drug dose.
#'
#' @param dataFrame a data.frame factorized using \code{\link{factorize.data.frame}}
#'
#' @return data.frame
#' @export
normalize.data.frame <- function(dataFrame){
        dataFrame <- as.data.frame(lapply(dataFrame, normalize.vector))
        colnames(dataFrame) <- paste(colnames(dataFrame), "normalized", sep = "_")
        return(dataFrame)
}

#help function for normalization

#'normalizes the given vector
#'
#' @param x numerical vector to be normalized.
#' @export
normalize.vector <- function(x){
        return((x - min(x))/(max(x)-min(x)))
}

#generic functions

models <- function(obj, ...){
  UseMethod("models")
}

#' create (farseer.data.frame)
#' 
#' Call of the create function on an farseer.data.frame object creates new farseer.models
#' 
#' @param farseerDataFrame prepared data frame for training
#' 
#' @return list of models
#' 
models.farseer.data.frame <- function(farseerDataFrame, ...){
  i <- 1
  value <- list()
  while(i <= length(farseerDataFrame$target.variables)){
    print(paste("Training start for: ", farseerDataFrame$target.variables[i], "target ", as.character(i), " from ", 
                as.character(length(farseerDataFrame$target.variables))))
    entry <- create.farseer.models(farseerDataFrame = farseerDataFrame, target = i, ...)
    if(!is.null(entry)){
      value[[farseerDataFrame$target.variables[i]]] <- entry
      print("OK")
    }
    else
    {
      print("failed")
    }
    i <- i+1
  }
  if(length(value) != 0)
    return(value)    
  else
    return(NULL)
}



is.farseer.data.frame <- function(obj){
  if(class(obj) == "farseer.data.frame"){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

print.farseer.data.frame <- function(obj){
  print(summary(obj[[1]]))
  for(i in 2:length(obj)){
    print(obj[i])
  }
}