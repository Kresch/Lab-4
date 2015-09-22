## ------------------------------------------------------------------------
X<-linreg(iris$Petal.Length~iris$Sepal.Length,data=iris)
plot(X)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

