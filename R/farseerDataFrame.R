# Copyright (C) 2020 Kornel Skitek
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#'farseer.data.frame - S3 class for creating and storing factorized and normalized datasets
#'
#'
#' @param dataFrame a data.frame containing data for modeling
#' @param formula formula for training of \link{farseer.models}
#' @param additional_targets additional dependand variables, for which models should be
#' created using same dependand variables as in original formula.
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
        numerical_columns <- bad_columns[sapply(originalData, is.numeric)]
        numerical_columns <- intersect(model.variables, numerical_columns)
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
        
        #save min/max values for each numerical column (used for simulation)
        min <- lapply(originalData[, numerical_columns], min)
        max <- lapply(originalData[, numerical_columns], max)
        
        
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
                     target.variables = target.variables,
                     maxmin = list(min = min, max = max))
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
#' @param obj a dataFrame object with data to be factorized. Non-Factor data will be left unchanged.
#'@author Kornel Skitek 2020
#'@export
factorize.data.frame <- function(obj){
        ret <- data.frame()
        names <- colnames(obj)
        for (x in 1:length(colnames(obj))){
                if(is.factor(obj[, x])){
                        factor_levels <- levels(obj[, x])             #saves the factor names for the second for loop
                        for(a in 1:(length(factor_levels) - 1)){ #this loop binarises a factor to levels() - 1 variables
                                for (b in 1:length(obj[, x])){
                                        #if (is.na(obj[b, x])) { ret[b, paste(names[x], factor_levels[a], sep = "_")] <- NA} else  #if the value is missing, an NA is saved in a new obj
                                                ret[b, paste(names[x], factor_levels[a], sep = "_")] <- ifelse(identical(as.character(obj[b, x]), factor_levels[a]), 1 , 0) #if the value for a case is the same as the checked factor level in 1 is saved in the appropriate variable, if it belongs to the last factor level - 0 in all new variables
                                }
                        }
                }
                else if(is.numeric(obj[ , x])){
                        for (b in 1:length(obj[, x])) { ret[b, names[x]] <- obj[b, names[x]] }
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
#' @param obj data.frame factorized using \code{\link{factorize.data.frame}}
#'
#' @return data.frame
#' @export
normalize.data.frame <- function(obj){
        obj <- as.data.frame(lapply(obj, normalize.vector))
        #colnames(obj) <- paste(colnames(obj), "normalized", sep = "_")
        return(obj)
}

#help function for normalization

#'normalizes the given vector
#'
#' @param obj numerical vector to be normalized.
#' @export
normalize.vector <- function(obj){
        return((obj - min(obj))/(max(obj)-min(obj)))
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
#' @param ... trainingFraction from \link{farseer.training.vector} how many cases should be a training set?
#' 
#' @return list of models
#' 
models.farseer.data.frame <- function(farseerDataFrame, ...){
  i <- 1
  value <- list()
  trainingVector <- NULL
  while(i <= length(farseerDataFrame$target.variables)){
    print(paste("Training start for: ", farseerDataFrame$target.variables[i], "target ", as.character(i), " from ", 
                as.character(length(farseerDataFrame$target.variables))))
    entry <- create.farseer.models(farseerDataFrame = farseerDataFrame, target = i, trainingVector = trainingVector, ...)
    if(!is.null(entry)){
      value[[farseerDataFrame$target.variables[i]]] <- entry
      trainingVector <- entry$trainingVector
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