#' @title Fit a linear model
#' @description This is a function to approximately reproduce the coefficients
#' in the linear model object that can be created by 'lm()'. The method being
#' used to compute the OLS estimates is QR decomposition.
#' @param formula.input a formula with the legal format.
#' @param data.input a dataframe provided by the user.
#' @param contrasts.input a list of contrasts of interest (default=NULL).
#' @return a list including the OLS estimates, simulating what will be produced by 'lm()'.
#' @examples
#' data(iris)
#' fit <- linear_model(Sepal.Length ~ ., iris)
#' fit$coefficients
#' @export


#A function simulating 'lm()', which computes OLS estimate using orthogonal projection
linear_model <- function(formula.input, data.input, contrasts.input=NULL){
  #Extract the independent variables
  X <- model.matrix(formula.input, data.input, contrasts.arg = contrasts.input)

  #Create a dataset dropping NAs in regressors, preparing for extracting a Y vector with proper length
  data_no_na <- model.frame(formula.input, data.input)

  #Identify the name of dependent variable
  y_name <- as.character(formula.input)[2]

  #Extract a vector of response variable
  Y <- as.matrix(data_no_na[,y_name], ncol = 1)

  #Derive OLS estimates via QR decomposition, which subtly deals with strong collinearity (automatic rank adjustment)
  beta <- as.vector(qr.solve(X, Y))

  #Simulate the behavior of 'lm()', display NA instead of 0 when encountering collinearity
  beta[beta==0] <- NA

  #Extract the names of independent variables and match them to the estimates
  names(beta) <- colnames(X)

  #Return a list where the user can call the coefficient estimates
  return(list(coefficients=beta))
}
