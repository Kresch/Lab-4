####ggplot2

plot.linreg <- function(X){
        resid <- as.vector(X$resid)
        fitted <- as.vector(X$fitted.values)
        data<-data.frame(resid,fitted)
        res_plot<-ggplot(data,aes(fitted,resid)) + 
                geom_point(aes(color=abs(resid)),size=4)+
                scale_color_gradient(high="red")+
                theme_minimal() +
                geom_smooth(method="loess")
        plot(res_plot)
        
}