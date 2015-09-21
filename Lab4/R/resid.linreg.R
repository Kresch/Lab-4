#method: resid

residus.linreg <- function(X)
  UseMethod("residus")

residus.linreg<-function(X){
        return(X$resid)
}

