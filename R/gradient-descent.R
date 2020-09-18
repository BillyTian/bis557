#' @title Implement Gradient Descent for OLS
#' @description This is a function to numerically compute OLS estimates via
#' implementing the gradient descent method. Its reception of inputs and the
#' coefficients grabbed are all similar to 'lm()' function.
#' @param formula.input a formula with the legal format.
#' @param data.input a dataframe provided by the user.
#' @param contrasts.input a list of contrasts of interest (default=NULL).
#' @param gamma.input a factor defined by user as a pace or speed of gradient descent (default=0.0001).
#' @param beta0 an initializer for the beta vector that can be changed by user (default=1).
#' @param stop.diff an absolute difference between SSR and the updated one set by user
#' to stop the iteration at some satisfying point (default=1e-12).
#' @param max.itera a maximum number of iterations set by user,
#' which is an alternative criterion to stop the iterations (default=1e6).
#' @return a list including the OLS estimates, simulating what will be produced by 'lm()'.
#' @examples
#' data(iris)
#' fit <- linear_model(Sepal.Length ~ ., iris)
#' fit$coefficients
#' @export


gradient_descent <- function(formula.input, data.input, contrasts.input = NULL, gamma.input = 0.0001, beta0 = 1, stop.diff = 1e-12, max.itera = 1e6){

  #Extract the independent variables
  X <- model.matrix(formula.input, data.input, contrasts.arg = contrasts.input)

  #Create a dataset dropping NAs in regressors, preparing for extracting a Y vector with proper length
  data_no_na <- model.frame(formula.input, data.input)

  #Identify the name of dependent variable
  y_name <- as.character(formula.input)[2]

  #Extract a vector of response variable
  Y <- as.matrix(data_no_na[,y_name], ncol = 1)

  #Initialize beta vector
  beta <- matrix(beta0, ncol = 1, nrow = ncol(X))

  #Compute the current sum of squared residuals
  ssr <- function(beta.input, X, Y){
    return(as.numeric(t(Y) %*% Y - t(Y) %*% X %*% beta.input - t(beta.input) %*% t(X) %*% Y + t(beta.input) %*% t(X) %*% X %*% beta.input))
  }

  #Compute the gradient based on some updated beta
  gradient <- function(beta.input, X, Y){
    return(-2*t(X) %*% Y + 2*t(X) %*% X %*% beta.input)
  }

  if (dim(X)[2] != qr(X)$rank){
    print("The chosen independent variables involve collinearity. The results will be based on 'linear_model()'.")
    #Use alternative method in the same package to tackle the special case.
    linear_model(formula.input, data.input, contrasts.input = NULL)

  } else{

    #Sum of squared residuals computed with initial beta
    ssr0 <- ssr(beta, X, Y)

    #Initialize some absolute difference updated ssr and the old and a count of iterations
    ss.diff <- 1
    count <- 0

    #To control the unpredictable running time, set a maximum number of iteration
    while ((ss.diff > stop.diff) & (count < max.itera)){
      #Update beta by implement the gradient descent in a rate of gamma
      beta <- beta - gamma.input*gradient(beta, X, Y)

      #Check the absolute difference of ssr at the current iteration
      ss.diff <- abs(ssr(beta, X, Y) - ssr0)

      #Before closing the current iteration, update ssr0 (the reference) and the counter
      ssr0 <- ssr(beta, X, Y)
      count <- count + 1
    }

    #Prepare for the true behavior of coefficients grabbing
    beta <- as.vector(beta)

    #Extract the names of independent variables and match them to the estimates
    names(beta) <- colnames(X)

    if (count==max.itera){
      print("The result has not achieved the difference standard! Iterations have been stopped by the current maximum number setting.")
    }

    #Return a list where the user can call the coefficient estimates
    return(list(coefficients=beta))
  }
}
