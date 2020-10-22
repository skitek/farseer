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

#General help functions

#' farseer.training.vector (function)
#' 
#' Gets a integer vector of case numbers for the training set
#' 
#' ##gets a psedo-random vector
#' trainingVector <- farseer.training.vector(nrow(dataFrame))
#' 
#' ##smaller training fraction
#' trainingVector <- farseer.training.vector(nrow(dataFrame), trainingFraction = 0.7)
#' 
#' ##set a seed
#' trainingVector <- farseer.training.vector(nrow(dataFrame), seed = 123)
#' 
#' @param noOfCases integer, number of cases in a data.frame
#' @param trainingFraction how many cases should be a training set?
#' @param seed if it is not NULL, a seed for randomization is set
#' 
#' 
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


#'farseer.rocplot (S3 function)
#'
#'creates a roc-plot for models
#'
#'@param performanceList a \link[ROCR]{performance-class} object containing "frp" and "tpr" performance of the models
#'@param targetName name of the dependent variable
#'@param title optional plot title
#'@return a roc plot
#'
farseer.rocplot <- function(performanceList, targetName = NULL, title = NULL){
  #plot
  if(is.null(title)){
    title <- paste("ROC plot for ", targetName)
  }
  #create ggplots
  groupNames <-  names(performanceList)
  frame <- data.frame()
  for(i in 1:length(performanceList)){
     pf <- data.frame(FPR = performanceList[[i]]@x.values[[1]], TPR = performanceList[[i]]@y.values[[1]], Group = groupNames[i])
     frame <- rbind(frame, pf)
  }
  ggplot <- ggplot2::ggplot() + ggplot2::geom_line(data = frame, ggplot2::aes(x = FPR, y = TPR,colour = Group, group = Group)) +
    # ggplot2::annotate("text", label = result, x = 0.875, y = 0.05, size = 5, colour = "red") +
    ggplot2::xlab("False Positive Rate (1-Specificity)")+ggplot2::ylab("True Positive Rate (Sensitivity)")+ggplot2::ggtitle(title)
  return(ggplot)
}

#' farseer.bland.altmann (S3 function)
#' 
#' creates a bland-altmann plot for numeric predictions
#' 
#' @param predictions a data.frame containing predictions of the models and the target variable
#' @param targetName name of the column with measured data
#' @param title title of plot
#' @export
farseer.bland.altmann <- function(predictions, targetName = "original", title = NULL){
  if(is.null(title)){
    title <- targetName
  }
  
  labels <- colnames(predictions)
  labels <- labels[labels != targetName]
  dataset <- predictions[,labels]
  measured <- predictions[,targetName, drop = FALSE]
  plots <- list()
  yMAX <- 0
  yMIN <- 0
  for(x in 1:length(labels)){
    avg <- (dataset[x] + measured)/2 #a mean measure is created
    diff <- (dataset[x] - measured)  #a difference between measurements is created
    plot_dataset <- as.data.frame(cbind(avg, diff))
    colnames(plot_dataset) <- c("avg", "diff")
    plot <- ggplot2::ggplot(plot_dataset, ggplot2::aes(x = avg, y = diff))
    plots[[labels[x]]] <- plot + ggplot2::geom_point(alpha = 0.5) + ggplot2::geom_hline(yintercept = mean(plot_dataset$diff), colour = "blue", size = 0.5) + 
            ggplot2::annotate("text", y = mean(plot_dataset$diff), x = 0.9*max(plot_dataset$avg), label = round(mean(plot_dataset$diff), digits = 2), color = "blue", vjust = 1.2) + #adds the mean difference
            ggplot2::geom_hline(yintercept = (mean(plot_dataset$diff) - (1.96 * sd(plot_dataset$diff))), colour = "red", size = 0.5) + #adds the -1.96SD line
            ggplot2::geom_hline(yintercept = (mean(plot_dataset$diff) + (1.96 * sd(plot_dataset$diff))), colour = "red", size = 0.5) + #adds the -1.96SD line
            ggplot2::ylab("Real - Predicted") + ggplot2::xlab("Average") #+ ggplot2::ggtitle(labels[x])
    yMAX <- max(yMAX, max(plot_dataset$diff), (1.96 * sd(plot_dataset$diff)))
    yMIN <- min(yMIN, min(plot_dataset$diff), (-1*(1.96 * sd(plot_dataset$diff))))
  }
  
  for(i in 1:length(plots)){
    plots[[i]] <- plots[[i]] + ggplot2::ylim(c(yMIN-(0.05*abs(yMIN)), yMAX+(0.05*yMAX)))
  }
  return(plots)
}