library(stringr)
data("iris")
my_reg<-linreg(iris$Sepal.Length ~ iris$Sepal.Width)
R_reg<-lm(iris$Sepal.Length ~ iris$Sepal.Width)
test_that("Output of linreg() mimics lm()", {
        expect_equal(coefficients(my_reg),coefficients(R_reg))
        expect_equal(resid(my_reg),resid(R_reg))
})

my_reg_mult<-linreg(iris$Sepal.Length ~ iris$Sepal.Width+iris$Petal.Length)
R_reg_mult<-lm(iris$Sepal.Length ~ iris$Sepal.Width+iris$Petal.Length)

test_that("Output of linreg() mimics lm() for multiple reg", {
        expect_equal(coefficients(my_reg_mult),coefficients(R_reg_mult))
        expect_equal(resid(my_reg_mult),resid(R_reg_mult))
})

no_data_reg<-linreg(Sepal.Length~Sepal.Width)

test_that("We can both input with and without data before $",{
        expect_equal(no_data_reg,my_reg)
}
          )