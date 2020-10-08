
library(testthat)

context("Test the function oos_gradient_descent()")

test_that("You oos_gradient_descent() function works in an easy case.", {
  
  data(iris)
  
  fit_ogd <- oos_gradient_descent(Sepal.Length ~ ., iris)
  
  fit_lm <- lm(Sepal.Length  ~ ., iris)
  
  expect_equivalent(fit_ogd$coefficients, fit_lm$coefficients, 1e-3)
})


test_that("Your oos_gradient_descent() function works in a tough case.", {
  
  data(lm_patho)
  
  fit_ogd <- oos_gradient_descent(y ~., lm_patho)
  
  fit_lm <- lm(y ~., lm_patho)
  
  expect_equivalent(fit_ogd$coefficients, fit_lm$coefficients, 1e-3)
})