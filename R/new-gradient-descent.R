#' @title New Gradient Descent
#' @description This is a function fitting the OLS model using gradient descent 
#' which calculate the loss based on the out-of-sample accuracy.
#' @param form a formula with the legal format.
#' @param data a dataframe provided by the user.
#' @param gamma a factor defined by user as a pace or speed of gradient descent (default=0.0001).
#' @param beta0 an initializer for the beta vector that can be changed by user (default=1).
#' @param fold.num a fold number of k-fold cv.
#' @param stop.diff an absolute difference between loss and the updated one set by user
#' to stop the iteration at some satisfying point (default=1e-8).
#' @param max.itera a maximum number of iterations set by user,
#' which is an alternative criterion to stop the iterations (default=1e6).
#' @return a list including the coefficient estimates.
#' @examples
#' data(iris)
#' fit <- new_gradient_descent(Sepal.Length ~ ., iris)
#' fit$coefficients
#' @export

new_gradient_descent <- function(form, data, gamma = 0.0001, beta0 = 1, fold.num=10, stop.diff = 1e-8, max.itera = 5e5){
  
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
    
    set.seed(101390)
    
    folds <- vfold_cv(data, fold.num)
    resids <- NULL
    for (i in 1:fold.num){
      resids <- c(resids, 
                  as.vector(assessment(folds$splits[[i]])[,y_name]-(model.matrix(form, assessment(folds$splits[[i]])) %*% beta.old)))
    }
    mse.old <- mean(resids^2)
    
    #To control the unpredictable running time, set a maximum number of iteration
    while ((os.diff > stop.diff) & (count < max.itera)){
      #Update beta by implement the gradient descent in a rate of gamma
      beta.new <- beta.old - gamma*gradient(beta.old, X, Y)
      
      resids.new <- NULL
      for (j in 1:fold.num){
        resids.new <- c(resids.new, 
                        as.vector(assessment(folds$splits[[j]])[,y_name]-(model.matrix(form, assessment(folds$splits[[j]])) %*% beta.new)))
      }
      mse.new <- mean(resids.new^2)
      
      #Check the absolute difference of ssr at the current iteration
      os.diff <- abs(mse.new - mse.old)
      
      #Before closing the current iteration, update the loss and the counter
      mse.old <- mse.new
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
