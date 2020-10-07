#' @title Optimizing ridge parameter based on MSE
#' @description This is a function that attempts the best lambda parameter.
#' @param form a formula with the legal format.
#' @param data a dataframe provided by the user.
#' @param lambdas a set of ridge parameter from which the function searches the best one.
#' @param fold.num number of folds divided for the cross validation (default=10).
#' @return a list including the MSE vector, the optimized ridge parameter, and the minimimal MSE which indicates the best lambda.
#' @examples
#' data(iris)
#' best.lambda <- optimized_lambda(Sepal.Length ~ ., iris, lambdas=seq(0, 2, by=0.01))
#' @export

optimized_lambda <- function(form, data, lambdas, fold.num=10) {

  #Identify the name of dependent variable
  y_name <- as.character(form)[2]

  folds <- vfold_cv(data, v=fold.num)

  resids <- foreach(i = seq_along(lambdas)) %do% {
    foreach(fold = folds$splits, .combine = c) %do% {
      fit <- ridge_regression(form, analysis(fold), lambda=lambdas[i])
      as.vector(assessment(fold)[,y_name] - as.vector(predict_ridge(fit, assessment(fold))))
    }
  }

  #Calculate the vector of MSE based on the residuals
  MSE<-NULL
  for (i in 1:length(lambdas)) {
    MSE<-c(MSE, mean(resids[[i]]^2))
  }
  optimized.lambda <- lambdas[which.min(MSE)]
  ret <- list(MSE=MSE, lambda=optimized.lambda, min.MSE=min(MSE))
  return(ret)
}
