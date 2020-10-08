#' @title Gradient Descent based on out-of-sample accuracy
#' @description This is a function fitting the OLS model using gradient descent
#' which calculate the loss based on the out-of-sample accuracy.
#' @param form a formula with the legal format.
#' @param data a dataframe provided by the user.
#' @param gamma a factor defined by user as a pace or speed of gradient descent (default=0.0001).
#' @param beta0 an initializer for the beta vector that can be changed by user (default=1).
#' @param stop.diff an absolute difference between loss and the updated one set by user
#' to stop the iteration at some satisfying point (default=1e-8).
#' @param max.itera a maximum number of iterations set by user,
#' which is an alternative criterion to stop the iterations (default=1e6).
#' @return a list including the coefficient estimates.
#' @examples
#' data(iris)
#' fit <- oos_gradient_descent(Sepal.Length ~ ., iris)
#' fit$coefficients
#' @export

oos_gradient_descent <- function(form, data, gamma = 0.0001, beta0 = 1, stop.diff = 1e-15, max.itera = 5e5){

  #Extract the independent variables
  X <- model.matrix(form, data)
  #Create a dataset dropping NAs in regressors, preparing for extracting a Y vector with proper length
  data_no_na <- model.frame(form, data)
  #Identify the name of dependent variable
  y_name <- as.character(form)[2]
  #Extract a vector of response variable
  Y <- as.matrix(data_no_na[,y_name], ncol = 1)

  #Initialize beta vector
  beta.old <- matrix(beta0, ncol = 1, nrow = ncol(X))

  #Compute the gradient based on some updated beta
  gradient <- function(beta.input, X, Y){
    return(-2*t(X) %*% Y + 2*t(X) %*% X %*% beta.input)
  }

  if (dim(X)[2] != qr(X)$rank){
    print("The chosen independent variables involve collinearity. The results will be based on 'linear_model()'.")
    #Use alternative method in the same package to tackle the special case.
    linear_model(form, data)

  } else{

    #Initialize the difference measuring the loss and a counter of iterations
    os.diff <- 1
    count <- 0

    #Create training and testing data set
    train <- round(nrow(X) * 0.75)
    ind <- sample(c(1:nrow(X)), train)

    set.seed(17319)
    X_train <- X[ind, ]
    Y_train <- Y[ind, ]
    X_test <- X[-ind, ]
    Y_test <- Y[-ind, ]

    oos.old <- sqrt(sum((X_test %*% beta.old- Y_test)^2))/length(Y_test)

    #To control the unpredictable running time, set a maximum number of iteration
    while ((os.diff > stop.diff) & (count < max.itera)){
      #Update beta by implement the gradient descent in a rate of gamma
      beta.new <- beta.old - gamma*gradient(beta.old, X, Y)

      oos.new <- sqrt(sum((X_test %*% beta.new - Y_test)^2))/length(Y_test)

      #Check the absolute difference of ssr at the current iteration
      os.diff <- abs(oos.new - oos.old)

      #Before closing the current iteration, update the loss and the counter
      oos.old <- oos.new
      beta.old <- beta.new
      count <- count + 1
    }

    #Prepare for the true behavior of coefficients grabbing
    beta <- as.vector(beta.old)
    #Extract the names of independent variables and match them to the estimates
    names(beta) <- colnames(X)

    if (count==max.itera){
      print("The result has not achieved the difference standard! Iterations have been stopped by the current maximum number setting.")
    }

    #Return a list where the user can call the coefficient estimates
    return(list(coefficients=beta, formula=form))
  }
}

