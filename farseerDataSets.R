#'farseerDataSet - S3 class for creating and storing factorized and normalized datasets

#' constructor for a farseerDataSet
#' @param dataFrame a data.frame containing data for modeling, without target variables
#' @return list
#' $original: original data.frame
#' $factorized: all factors converted to 0 and 1
#' $normalized: all values are ranged 0-1. See @method factorize.data.frame
#' 
#' @author Kornel Skitek 2020
farseerDataSet <- function(dataFrame){
        originalData <- dataFrame
        factorizedData <- factorize(dataFrame)
        normalizedData <- normalize(factorizedData)
        
        #create denormalization vector
        minVector <- sapply(factorizedData, min)
        maxVector <- sapply(factorizedData, max)
        denormalizeVector <- (maxVector - minVector) + minVector
        
        creationDate <- Sys.Date()
        value = list(original = originalData, factorized = factorizedData, normalized = normalizedData, timestamp = creationDate, denormalization = denormalizeVector)
        attr(value, "class") <- "farseerDataSet"
        value
}

#generic functions

#' factorize
#' 
#' generic function for factorizing - changind factor levels into numerical 0 or 1 values
#'
#' @param obj - object to be factorized
#' 
#' see @method factorize.data.frame for details
#' @export
factorize <- function(obj){
        UseMethod("factorize")
}

#' normalize
#' 
#' generic function for normalizing
#'
#' @param obj 
#'
#' see @method normalize.data.frame for details
#' @export
normalize <- function(obj){
        UseMethod("normalize")
}


#' @method 
#' This function converts all factors to numeric values 0,1.
#' For every factor level save one a new variable is created {name_factor_name}
#' last factor level does not have to be included, as if all the values are 0 it means this case belongs to the group
#' @example 
#' 
#'  example <- data.frame(Patient_Number = c(1:5), Sex = c("male", "female", "diverse", NA , "male"))
#'  factorize.data.frame(example)
#'  
#'   >example
#'     Patient_Number     Sex
#'     1              1    male
#'     2              2  female
#'     3              3 diverse
#'     4              4    <NA>
#'     5              5    male
#'   > factorize.data.frame(example)
#'     Patient_Number Sex_diverse Sex_female
#'     1              1           0          0
#'     2              2           0          1
#'     3              3           1          0
#'     4              4          NA         NA
#'     5              5           0          0
#'     
#'@author Kornel Skitek 2020
#'@export
factorize.data.frame <- function(dataFrame){
        ret <- data.frame()
        names <- colnames(dataFrame)
        for (x in 1:length(colnames(dataFrame))){
                if(is.factor(dataFrame[, x])){
                        factor_levels <- levels(dataFrame[, x])             #saves the factor names for the second for loop
                        for(a in 1:(length(factor_levels) - 1)){ #this loop binarises a factor to levels() - 1 variables
                                for (b in 1:length(dataFrame[, x]))
                                        if (is.na(dataFrame[b, x])) { ret[b, paste(names[x], factor_levels[a], sep = "_")] <- NA} else  #if the value is missing, an NA is saved in a new dataframe
                                                ret[b, paste(names[x], factor_levels[a], sep = "_")] <- ifelse(identical(as.character(dataFrame[b, x]), factor_levels[a]), 1 , 0) #if the value for a case is the same as the checked factor level in 1 is saved in the appropriate variable, if it belongs to the last factor level - 0 in all new variables
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
#' This allows for correct training, if the variables have very different ranges
#' for example:
#' if a antihypertension drug has a dosage range of 2 to 10 mg
#' and initial mean blood pressure of our patients varies from 100 to 200
#' the initial blood pressure will be overfitted, compared to drug dose.
#'
#' @param factorizedDataFrame - a data.frame factorized using @method factorize.data.frame
#'
#' @return data.frame
#' @export
normalize.data.frame <- function(dataFrame){
        dataFrame <- as.data.frame(lapply(dataFrame, normalize.vector))     
        return(dataFrame)
}

#help function for normalization

#'normalizes the given vector
#'
#' @param x numerical vector to be normalised.
#' @export
normalize.vector <- function(x){
        return((x - min(x))/(range(x)))
}
