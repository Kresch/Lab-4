#methods to linreg

print.linreg<-function(X){
        cat("Call:","\n")
        print(X$call)
        cat("\n")
        cat("Coefficients:","\n")
        cat(rownames(X$coefficients),"\n")
        cat(X$coefficients)
}