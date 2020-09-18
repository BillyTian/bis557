library(testthat)

context("Test the output of homework 1: gradient_descent().")

test_that("Your gradient_descent() function returns approximately the same coefficients as lm.", {

  data(iris)
  fit_gd <- gradient_descent(Sepal.Length ~ ., iris)
  fit_lm <- lm(Sepal.Length  ~ ., iris)
  expect_equivalent(fit_lm$coefficients, fit_gd$coefficients, tolerance = 0.001)
})

test_that("Your gradient_descent() function works with contrasts.", {

  data(iris)
  fit_gd <- gradient_descent(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"))
  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))
  expect_equivalent(fit_lm$coefficients, fit_gd$coefficients, tolerance = 0.001)
})

test_that("Your gradient_descent() function works in a special case with collinearity.", {

  data(lm_patho)
  fit_gd <- gradient_descent(y ~., lm_patho)
  fit_lm <- lm(y ~., lm_patho)
  expect_equivalent(fit_lm$coefficients, fit_gd$coefficients, tolerance = 0.001)
})
