#' @title Ridge Regression Considering Collinearity
#' @description This is a ridge regression function taking into account collinear or nearly collinear regression variables.
#' @param X a X model matrix created by function "model_matrices".
#' @param Y a Y vector created by function "model_matrices".
#' @param lambda a penalty parameter specified by the user (default=0.01).
#' @return a list including the coefficient estimates.
#' @examples
#' data(iris)
#' fit <- ridge_regression(X, Y)
#' fit$coefficients
#' @export
#'
#'
ridge_R <- function(X, Y, lambda=0.01) {

  #Singular value decomposition
  svd_x <- svd(X)

  Sigma <- diag(svd_x$d)
  lambda_I <-  diag(rep(lambda, length(svd_x$d)))
  beta <- svd_x$v %*% solve(Sigma^2 + lambda_I) %*% Sigma %*% t(svd_x$u) %*% Y

  beta <- as.vector(beta)
  names(beta) <- colnames(X)
  ret <- list(coefficients = beta)
  return(ret)
}
