#' @title First-Order Method of Generalized Linear Model
#' @description This function gives a first-order solution for the GLM maximum likelihood problem using only gradient information,
#' avoiding the Hessian matrix. A comparison of a constant step size and an adaptive one is also included.
#' @param form a formula with the legal format.
#' @param data a dataframe provided by the user.
#' @param mu_fun a specified link function inverse.
#' @param step indicate method, constant or momentum step.
#' @param data a dataframe provided by the user.
#' @param gamma the learning rate.
#' @param maxit the maximum number of iterations.
#' @param tol the tolerance of the squared difference of beta to show convergence.
#' @return a list including the coefficient estimates.
#' @examples
#' set.seed(1713948)
#' n <- 1000
#' truth <- c(4, 0.5, 0.3, 1)
#' X <- cbind(rep(1,n), matrix(rnorm(3*n), ncol=3))
#' lambda <- exp(X %*% truth)
#' Y <- rpois(n, lambda=lambda)
#' covs <- X[,-1]
#' q2.data <- data.frame(cbind(Y, covs))
#' form <- Y ~ .
#' glm_firstorder(form, q2.data, step="momentum", mu_fun=function(eta) exp(eta))$beta
#' @export

glm_firstorder <- function(form, data, mu_fun, step="constant", gamma=1e-6, maxit = 1e3, tol = 1e-6){

  #Extract the independent variables
  X <- model.matrix(form, data)
  #Create a dataset dropping NAs in regressors, preparing for extracting a Y vector with proper length
  data_no_na <- model.frame(form, data)
  #Identify the name of dependent variable
  y_name <- as.character(form)[2]
  #Extract a vector of response variable
  Y <- as.matrix(data_no_na[,y_name], ncol = 1)

  beta <- rep(0, ncol(X))
  iter <- 0
  if (step=="constant"){
    for (i in 1:maxit){
      beta_old <- beta
      eta <- X %*% beta
      mu <- mu_fun(eta)
      grad <- t(X) %*% (Y-mu)
      beta <- beta_old + gamma*grad
      iter <- iter + 1
      if (crossprod(beta - beta_old)<tol){
        break
      }
    }
  }
  else if (step=="momentum"){
    v_old <- beta
    prop <- 0.8
    for (i in seq_len(maxit)){
      beta_old <- beta
      eta <- X %*% beta_old
      mu <- mu_fun(eta)
      grad <- t(X) %*% (Y-mu)
      v_new <- prop*v_old + gamma*grad
      beta <- beta_old + v_new
      v_old <- v_new
      iter <- iter + 1
      if (crossprod(beta - beta_old)<tol){
        break
      }
    }
  }
  list(beta=beta, iter=iter)
}

