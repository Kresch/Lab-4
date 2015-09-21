#methods to linreg
impression.linreg<-function(X){
        cat("Call:","\n")
        print(X$call)
        cat("\n")
        cat("Coefficients:","\n")
        cat(rownames(X$coefficients),"\n")
        cat(X$coefficients)
        cat("Bonjour!")
        #when using "print", whatever we change in the function, it works
        #but if we change the name of the function, it does not work anymore
}
