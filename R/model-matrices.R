#' @title Extract X and Y from data
#' @description Make model matrices
#' @param form a formula with the legal format.
#' @param d a dataframe provided by the user.
#' @examples
#' data(iris)
#' fit <- model_matrices(Sepal.Length ~ ., iris)
#' fit$X
#' @export
#'

model_matrices <- function(form, d){
  d_no_na <- model.frame(form, d)
  X <- model.matrix(form, d)
  y_name <- as.character(form)[2]
  Y <- matrix(d_no_na[,y_name], ncol = 1)
  names <- colnames(X)
  list(X=X, Y=Y, names=names)
}
