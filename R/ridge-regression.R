#' @title Ridge Regression Considering Collinearity
#' @description This is a ridge regression function taking into account collinear or nearly collinear regression variables.
#' @param form a formula with the legal format.
#' @param data a dataframe provided by the user.
#' @param lambda a penalty parameter specified by the user.
#' @return a list including the coefficient estimates.
#' @examples
#' data(iris)
#' fit <- ridge_regression(Sepal.Length ~ ., iris)
#' fit$coefficients
#' @export
#'
#'
ridge_regression <- function(form, data, lambda) {

  #Extract the independent variables
  X <- model.matrix(form, data)
  #Create a dataset dropping NAs in regressors, preparing for extracting a Y vector with proper length
  data_no_na <- model.frame(form, data)
  #Identify the name of dependent variable
  y_name <- as.character(form)[2]
  #Extract a vector of response variable
  Y <- as.matrix(data_no_na[,y_name], ncol = 1)

  #Singular value decomposition
  svd_x <- svd(X)

  #Truncate the components of decomposition, get rid of the variables induce collinearity problem
  #cond.num <- svd_x$d / svd_x$d[1]
  #truncate <- max(which(tol < cond.num))
  #svd_x$d <- svd_x$d[seq_len(truncate)]
  #svd_x$u <- svd_x$u[, seq_len(truncate)]
  #svd_x$v <- svd_x$v[, seq_len(truncate)]

  #Compute the estimate, specify the size in case of 1x1 matrices
  #Sigma <- diag(svd_x$d, ncol = length(svd_x$d), nrow = length(svd_x$d))
  #lambda_I <-  diag(rep(lambda, length(svd_x$d)), ncol = length(svd_x$d), nrow = length(svd_x$d))
  #beta <- svd_x$v %*% solve(Sigma^2 + lambda_I) %*% Sigma %*% t(svd_x$u) %*% Y

  Sigma <- diag(svd_x$d)
  lambda_I <-  diag(rep(lambda, length(svd_x$d)))
  beta <- svd_x$v %*% solve(Sigma^2 + lambda_I) %*% Sigma %*% t(svd_x$u) %*% Y

  beta <- as.vector(beta)
  names(beta)<-colnames(X)
  ret <- list(coefficients = beta, formula = form)
  class(ret) <- "ridge_regression"
  return(ret)
}
