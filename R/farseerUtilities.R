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
farseer.training.vector <- function(noOfCases, trainingFraction = 0.9, seed = NULL){
  if(!is.null(seed)){
    set.seed(seed = seed)
  }
  vector <- sort(sample(noOfCases,(trainingFraction*noOfCases)))
  return(vector)
}