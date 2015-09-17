plot.linreg <- function(X){
  resid <- X$resid
  fitted <- X$fitted.values
  ggplot(data = sqrt(abs(resid)) + 
           aes(, x="fitted values", y="sqrt(abs(Standarized residuals))") + 
           geom_point())
}