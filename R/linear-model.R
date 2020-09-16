#' @title Fit a linear model
#' @description This is a function implement the linear regression 
#' and approximately reproduce the coefficients in the linear model 
#' object that can be created by 'lm()'.
#' @param formula a formula with the legal format.
#' @param data a dataframe provided by the user.
#' @param contrasts a list of contrasts of interest (default=NULL).
#' @examples
#' data(iris)
#' linear_model(Sepal.Length ~ ., iris)
#' @export


# define linear model function with contrasts and basic collinearity handling

linear_model <- function(formula, data, contrasts = NULL){
  
  
  
  # define model matrix and response variable
  
  X <- model.matrix(formula, data, contrasts.arg = contrasts)
  
  Y <- as.matrix(subset(data, select = as.character(formula[[2]])), ncol = 1)
  
  
  
  # solve with qr decomposition
  
  beta <- qr.solve(qr(X), Y)
  
  
  
  # convert 0s to NAs
  
  beta[beta==0] <- NA
  
  
  
  # return same type as lm()
  
  list(coefficients = beta)
  
}