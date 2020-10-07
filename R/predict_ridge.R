#' @title Predict function for ridge regression
#' @description Predicted values based on the linear model object
#' @param object object of class inheriting from "ridge_regression"
#' @param ... other arguments
#' @examples
#' data(iris)
#' fit_ridge <- ridge_regression(form = Sepal.Length ~ ., data = iris, lambda = 0.001)
#' predict_ridge(fit_ridge)
#' @export

predict_ridge <- function(object, ...){
  dots <- list(...)
  data <- dots[[1]]
  if(!inherits(data, "data.frame")){
    stop("The second argument must be a data frame.")
  }
  X <- model.matrix(object$formula, data)
  X %*% object$coefficients
}
