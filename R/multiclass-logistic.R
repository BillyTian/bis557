#' @title Multi-Class Model Generalizing Logistic Regression
#' @description This function implement a classification model
#' generalizing logistic regression to accommodate more than two classes.
#' @param form a formula with the legal format.
#' @param data a dataframe provided by the user.
#' @param maxit the number of iteration for the second-order gradient descent.
#' @return a list including the dependent variable of interest, coefficient estimates, and membership propensities.
#' @examples
#' q3.data <- penguinsi
#' form <- species ~ bill_length_mm + bill_depth_mm
#' q3_fit <- multiclass_logistic(form, q3.data, maxit = 50)
#' @export

multiclass_logistic <- function(form, data, maxit){
  #Extract the independent variables
  X <- model.matrix(form, data)
  #Create a dataset dropping NAs in regressors, preparing for extracting a Y vector with proper length
  data_no_na <- model.frame(form, data)
  #Identify the name of dependent variable
  y_name <- as.character(form)[2]
  #Extract a vector of response variable
  Y <- as.matrix(data_no_na[,y_name], ncol = 1)

  #Levels of Y
  multiclass <- length(unique(Y))
  #Initialize coefficient estimates
  beta <- matrix(0, nrow=multiclass, ncol = ncol(X))
  beta_old <- matrix(0, nrow=multiclass, ncol = ncol(X))

  for(i in 1:maxit){
    for (j in 1:multiclass){
      beta_old[j,] <- beta[j,]
      p <- 1 / (1 + exp(- X %*% beta[j,]))
      D <- diag(as.numeric(p*(1-p)))
      XtDX <- crossprod(X, D %*% X)
      grad <- t(X) %*% (1*(Y==unique(Y)[j]) - p)
      beta[j,] <- beta[j,] + solve(XtDX, grad)
    }
  }

  #Probabilities to enter each class for each individual
  probs <- matrix(0, nrow=nrow(X), ncol=multiclass)
  denom <- 0
  #Use the softmax formula to calculate propensities
  for (k in 1:multiclass) {
    denom <- denom + exp(X %*% beta[k,])
  }
  for (l in 1:multiclass){
    probs[,l] <- exp(X %*% beta[l,])/denom
  }
  return(list(Y=Y, beta=beta, probs=probs))
}
