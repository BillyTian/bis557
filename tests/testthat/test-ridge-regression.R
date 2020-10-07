library(testthat)
library(MASS)

context("Test the output of ridge_regression().")

test_that("Your ridge_regression() function works in common cases.", {

  data(iris)
  fit_lm_ridge <- lm.ridge(Sepal.Length ~ ., iris, lambda = 0.01)
  fit_ridge_regression <- ridge_regression(Sepal.Length  ~ ., iris, lambda = 0.01)

  expect_equivalent(coef(fit_lm_ridge), fit_ridge_regression$coefficients, 0.1)
})

test_that("Your ridge_regression() function can deal with collinearity.", {

  data(iris)
  modified.iris <- iris
  modified.iris$collinear.var <- 2*modified.iris$Petal.Width

  ridge_fit <- lm.ridge(Sepal.Length ~ ., modified.iris, lambda = 0.01)
  my_fit <- ridge_regression(Sepal.Length ~ ., modified.iris, lambda = 0.01)

  expect_equivalent(coef(ridge_fit), my_fit$coefficients, 0.1)

})
