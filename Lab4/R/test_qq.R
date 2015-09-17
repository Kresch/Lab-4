data(mpg)
test_gg<-ggplot(data,aes(fitted,sqrt(resid^2))) +
        geom_point() +
        coord_cartesian()+
        theme_bw()

outl<-
gg_2<-ggplot(data,aes(fitted,resid)) + 
        geom_point(aes(color=abs(resid)),size=4)+
        scale_color_gradient(high="red")+
        theme_minimal() +
        geom_smooth()

res_lm<-lm(resid~fitted)
plot(res_lm$fitted.values,type="l")
