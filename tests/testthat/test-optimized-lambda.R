library(testthat)
library(glmnet)

context("Test the output of optimized_lambda()")

test_that("Your optimized_lambda() function works.", {
  
  data(iris)
  try_optimized_lambda <- optimized_lambda(Sepal.Length ~ ., iris, lambdas = seq(0,2,0.01))
  try_glmnet <- cv.glmnet(model.matrix(Sepal.Length ~ ., iris), as.matrix(iris[,1]), lambda = seq(0,2,0.01))
  
  expect_equivalent(try_optimized_lambda$lambda, try_glmnet$lambda.min, 0.1)
})

