###
#' @title linreg
#' @param a "formula" of the form y ~ x+... and a dataframe
#' @description mimics the lm-function in R, i.e. makes linear regression.
#' @return an object with class "linreg", i.e. that has attributes according to linear regression.
#' @references https://en.wikipedia.org/wiki/Linear_regression
#' @author Niclas Lovsjö & Maxime Bonneau


linreg<-function(formula,data){
        #error handling, might wanna add more.
        if(!(class(formula)=="formula"))
                {stop(cat(formula, "is not an formula!"))}
        if(!is.data.frame(data)){
                stop(cat(data, "is not a data.frame!"))
        }
        
        
        
        #design matrix X
        X<-model.matrix(formula,data)
        
        #all.vars(formula)[2] will always be the response
        y<-as.matrix(data[all.vars(formula)[2]])
        
        #now we can use regular lin.alg. to find the needed values
        
        n<-ncol(X)
        
        beta_hat<-solve(t(X)%*%X)%*%(t(X)%*%y)
        y_hat<-X%*%beta_hat
        eps_hat<-y-y_hat
        df<-length(y)-(n-1)
        sigma_sq<-as.numeric(t(eps_hat)%*%eps_hat)/df
        var_beta_hat<-sigma_sq*solve(t(X)%*%X)
        t_beta<-beta_hat/sqrt(diag(var_beta_hat))
        
        #To Maxime: it was just to use "diag" to get the var-values.
        
        
        result<-list(coefficients=beta_hat, resid=eps_hat, 
                     df.residual=df,rank=ncol(X)-1,call=call("linreg",formula))
        
        #build class
        class(result)<-"linreg"
        
        #this is our new class "linreg"
        #which we can set attr to, such as "coefficients" etc
        #set methods
        
        #I am not sure if we can define methods inside the
        #function like this.
#         resid.linreg<<-function(result){
#                 result$resid
#         }
#         coefficients.linreg<<-function(result){
#                 result$coefficients
#         }
#         pred.linreg<<-function(result,x){
#                 x%*%result$coefficients
#         }
        
        #use those later 
        
        
        #Cant get it to print "call...blabalba"
        #and perhaps tab it
        cat("\n")
        cat("Coefficients:","\n")
        cat(colnames(X),"\n")
        cat(result$coefficients)
        
        return(result)
}