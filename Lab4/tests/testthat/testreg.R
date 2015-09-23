library(stringr)
data("iris")
my_reg<-linreg(Sepal.Length ~ Sepal.Width, iris)
R_reg<-lm(iris$Sepal.Length ~ iris$Sepal.Width)
test_that("Output of linreg() mimics lm()", {
        expect_less_than(abs(coefficients(my_reg)-R_reg$coefficients), 0.001)
        expect_less_than(abs(resid(my_reg)-R_reg$residuals),0.001)
        expect_less_than(abs(fitted.values(my_reg)-R_reg$fitted.values), 0.001)
})

my_reg<-linreg(eruptions ~ waiting, faithful)
R_reg<-lm(faithful$eruptions~faithful$waiting)
test_that("Output of linreg() mimics lm()", {
  expect_less_than(abs(coefficients(my_reg)-R_reg$coefficients), 0.001)
  expect_less_than(abs(resid(my_reg)-R_reg$residuals),0.001)
  expect_less_than(abs(fitted.values(my_reg)-R_reg$fitted.values), 0.001)
})

my_reg_mult<-linreg(Sepal.Length ~ Sepal.Width+Petal.Length, iris)
R_reg_mult<-lm(iris$Sepal.Length ~ iris$Sepal.Width+iris$Petal.Length)

test_that("Output of linreg() mimics lm() for multiple reg", {
        expect_less_than(abs(coefficients(my_reg_mult)-R_reg_mult$coefficients), 0.001)
        expect_less_than(abs(resid(my_reg_mult)-R_reg_mult$residuals), 0.001)
        expect_less_than(abs(fitted.values(my_reg_mult)-R_reg_mult$fitted.values), 0.001)
})

