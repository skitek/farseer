#test s3 class farseer

farseer <- function(name, dataSet = NULL){
        
        value <- list(Name = name, dataSet = dataSet)
        attr(value, "class") <- "farseer"
        value
}

model <- function(obj){
        UseMethod("model")
}

model.farseer <- function(obj){
        if(class(obj) != "farseer"){
                stop("Modeling defined only for farseer objects")
        }
        
        print("Here are your models")
}