#Ridgereg

#' @name ridgereg
#' @author Niclas Lovsjö & Maxime Bonneau
#' @description Uses ridge regression to fit a model.
#' @param lambda - hyperparameter to be tuned. if not specified, we set it to 0.
#' @param a "formula" of the form y ~ x+... and a dataframe
#' @references ESLII: http://web.stanford.edu/~hastie/local.ftp/Springer/OLD/ESLII_print4.pdf


#say:
formula<-Sepal.Length~Sepal.Width+Petal.Width
data<-iris


ridgereg<-function(formula,data,lambda=0){
        norm_data<-function(vect){
                res<-(vect-mean(vect))/(sd(vect))
                return(res)
        }
        #data will come as a data.frame with one y-vector and covariates in X.
        #formula defines which are which.
        
        #we will use same as in linreg to define this:
        data1 <- deparse(substitute(data))
        formula1 <- c()
        formula1[2] <- paste(data1,"$",as.character(formula[2]), sep = "")
        vect <- unlist(strsplit(as.character(formula[3]), "[+]"))
        for(i in 1:length(vect)){
                vect[i]<- paste(data1, "$", vect[i], sep = "")
        }
        formula1 <- paste(formula1[2], formula[1])
        formula1 <- paste(formula1, vect[1])
        j <- 2
        while(j <= length(vect)){
                formula1 <- paste(formula1, "+", vect[j])
                j <- j+1
        }
        formula1 <- as.formula(formula1)
        
        #error handling, might wanna add more.
        if(!(class(formula1)=="formula"))
        {stop(cat(formula, "is not an formula!"))}
        if(!is.data.frame(data)){
                stop(cat(data, "is not a data.frame!"))
        }
        
        #design matrix X
        X<-model.matrix(formula,data)
        
        #all.vars(formula)[2] will always be the response
        y<-as.matrix(data[all.vars(formula1)[2]])
        
        #now we can use regular lin.alg. to find the needed values
        
        p<-ncol(X)-1
        n<-length(y)
        
        norm_X<-apply(X[,2:ncol(X)],2,norm_data)
        norm_X<-cbind(rep(1,ncol(X)),norm_X)
        #normalizes X and takes out intercept before and then enter it after normalizing.
        #i.e. we dont wanna normalize the intercept.
        
        B_ridge<-function(lambda){
                res<-solve(t(norm_X)%*%norm_X+lambda*diag(ncol(norm_X)))%*%(t(norm_X)%*%y)
                return(res)
        }
        # I dont know how to handle lambda here
        beta_hat<-B_ridge(1)
        fitted.values<-norm_X%*%beta_hat
        result<-list(coefficients=beta_hat,fitted.values=fitted.values)
        class(result)<-"ridgereg"
        
        return(result)
}