testthat::test_that("lenreg rejects errounous input", {
  testthat::expect_error(linreg_mod <- linreg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  testthat::expect_error(linreg_mod <- linreg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})

testthat::test_that("class is correct", {
  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

  testthat::expect_true(class(linreg_mod)[1] == "linreg")
})

testthat::test_that("print() method works", {
  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

  testthat::expect_output(linreg_mod$print(),"linreg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris\\)")
  testthat::expect_output(linreg_mod$print(),"( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length")
})

testthat::test_that("pred() method works", {
  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

  testthat::expect_equal(round(unname(linreg_mod$pred()[c(1,5,7)]),2), c(1.85, 1.53, 1.09))
})

testthat::test_that("resid() method works", {
  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

  testthat::expect_equal(round(unname(linreg_mod$resid()[c(7,13,27)]),2), c(0.31, -0.58, -0.20))
})

testthat::test_that("coef() method works", {
  linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

  testthat::expect_true(all(round(unname(linreg_mod$coef()),2) %in% c(-2.52, -1.34, 1.78)))
})
