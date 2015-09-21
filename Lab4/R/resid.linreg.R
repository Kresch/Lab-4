#method: resid

resid<-function(X){
        UseMethod("resid")
}


resid.linreg<-function(X){
        return(X$resid)
}

#ask måns about this.
