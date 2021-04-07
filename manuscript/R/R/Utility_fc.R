### Utility Functions

## Catch warnings and errors of functions
catchToList <- function(expr) {
  val <- NULL
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, w$message)
    invokeRestart("muffleWarning")
  }
  myError <- NULL
  eHandler <- function(e) {
    myError <<- e$message
    NULL
  }
  val <- tryCatch(withCallingHandlers(expr, warning = wHandler), error = eHandler)
  list(value = val, warnings = myWarnings, error=myError)
} 

## pairwise sccaterplots Spearman's correlation
Spearman_scatters <- function(data){
  for(x in 1:ncol(data)){
    for(y in 1:ncol(data)){
      if(x==y){
        next
      }
      plot(data[,x], data[,y])
      legend("top", legend = round(cor(data[,x], data[,y], method = "spearman"), 2), 
             bty = "n", text.col = "red")
      title(paste0("x=", colnames(data)[x], "; y=", colnames(data)[y]), cex.main = 0.8)
      if(round(cor(data[,x], data[,y], method = "spearman"), 2)>.7){
        print(paste(colnames(data)[x], colnames(data)[y]))
        print(round(cor(data[,x], data[,y], method = "spearman"), 2))
      }
    }
  }
}