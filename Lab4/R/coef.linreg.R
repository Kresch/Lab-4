#method: coef

coef.linreg<-function(result){
                        vect<-c(result$coefficients)
                        names(vect)<-rownames(result$coefficients)
                        return(vect)
                }