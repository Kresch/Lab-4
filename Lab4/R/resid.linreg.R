#method: resid

resid<-function(X){
        UseMethod("resid")
}


resid.linreg<-function(X){
        return(c(X$resid))
}


