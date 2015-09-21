#method: Pred


pred<-function(result,x){
        UseMethod("pred")
}

pred.linreg<<-function(result,x){
                c(1,x)%*%result$coefficients
        }