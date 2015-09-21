library(ggplot2)

plot.linreg <- function(X){
  resid <- as.vector(X$resid)
  fitted <- X$fitted.values
  ggplot(data = sqrt(abs(resid))) + 
           aes(x="fitted values", y="sqrt(abs(Standarized residuals))") + 
           geom_point()
}

y <- linreg(formula,iris)
resid <- as.vector(y$resid)
fitted <- as.vector(y$fitted.values)
dat <- data.frame(resid,fitted)
ggplot(data=dat)
