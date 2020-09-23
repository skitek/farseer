#farseerDataSet - S3 class for creating and storing factorized and normalized datasets


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

factorize <- function(obj){
        UseMethod("factorize")
}

normalize <- function(obj){
        UseMethod("normalize")
}


#implementation
factorize.data.frame <- function(obj){
        ret <- data.frame()
        names <- colnames(obj)
        for (x in 1:length(colnames(obj))){
                if(is.factor(obj[, x])){
                        factor_levels <- levels(obj[, x])             #saves the factor names for the second for loop
                        for(a in 1:(length(factor_levels) - 1)){ #this loop binarises a factor to levels() - 1 variables
                                for (b in 1:length(obj[, x]))
                                        if (is.na(obj[b, x])) { ret[b, paste(names[x], factor_levels[a], sep = "_")] <- NA} else  #if the value is missing, an NA is saved in a new dataframe
                                                ret[b, paste(names[x], factor_levels[a], sep = "_")] <- ifelse(identical(as.character(obj[b, x]), factor_levels[a]), 1 , 0) #if the value for a case is the same as the checked factor level in 1 is saved in the appropriate variable, if it belongs to the last factor level - 0 in all new variables
                        }
                }
                else if(is.numeric(obj[ , x])){
                        for (b in 1:length(obj[, x])) { ret[b, names[x]] <- obj[b, names[x]] }
                }
        }
        return(ret)
}

normalize.data.frame <- function(obj){
        obj <- as.data.frame(lapply(obj, normalize.vector))     
        return(obj)
}

#help function for normalization

#function normalize
#normalizes the given vector
normalize.vector <- function(x){
        return((x - min(x))/(max(x) - min(x)))
}
