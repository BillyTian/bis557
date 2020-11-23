import numpy as np 


def py_ridge_regression(X, Y, names, lamb):
  """
  Args:
        X: matrix with independent variables and 1's
        Y: outcome vector
        names: names of independent variables
        lam: a tuning parameter in ridge regression
  Returns:
        coefficient estimates of ridge regression
  """
  
  u = np.linalg.svd(X, full_matrices = False)[0]
  d = np.linalg.svd(X, full_matrices = False)[1]
  #Take transpose to be consistent with v in respective R coding
  v = np.linalg.svd(X, full_matrices = False)[2].T
  Sigma = np.diag(d)
  lambda_I = np.diag(np.repeat(lamb, len(d)))
  beta = v @ np.linalg.inv(Sigma @ Sigma + lambda_I) @ Sigma @ u.T @ Y
  var_names = names
  coef = [var_names, beta.T]
  return coef



def ooc_fit(X, Y, step=0.01):
  """ 
  Args:
      X: matrix with independent variables and 1's
      Y: outcome vector
      step: learning rate of stochastic gradient descent
  Returns:
      coefficient estimates from out-of-core implementation
  """
  
  beta_old = np.ones(np.shape(X)[1]).reshape(np.shape(X)[1],1)
  for i in range(np.shape(Y)[0]):
    temp_X = X[i,:].reshape(np.shape(X)[1],1)
    temp_Y = Y[i].reshape(np.shape(Y)[1],1)
    grad = -2*temp_X*temp_Y + 2*temp_X @ temp_X.T @ beta_old
    beta_new = beta_old - step*grad
    beta_old = beta_new
  return(beta_old)


def py_lasso_regression(X, Y, lam, maxit=10000, step=0.001, tol=1e-10):
  """ 
  Args:
      X: matrix with independent variables and 1's
      Y: outcome vector
      lamb: a tuning parameter in Lasso regression
      maxit: maximum iterations
      step: learning rate of Lasso regression
      tol: tolerance to close the iterations
        
  Returns:
      coefficient estimates from Lasso regression
  """
  beta = (np.zeros(X.shape[1])).reshape(X.shape[1],1)
  for i in range(maxit):
    beta_old = beta
    grad = X.T @ (X @ beta-Y) + len(Y)*lam*(np.sign(beta))
    beta = beta-step*grad
    if sum(abs(beta-beta_old)) <= tol:
      break
    for j in range(X.shape[1]):
      if abs(X[:,j].T @ Y) <= len(Y)*lam:
        beta[j] = 0
    
  return(beta)

def soft_thresh(a, b):
  for i in range(len(a)):
    if abs(a[i]) <= b:
      a[i] = 0
    if a[i] > 0:
      a[i] = a[i] - b
    if a[i] < 0:
      a[i] = a[i] + b
  return a


def update_beta(y, X, lam, alpha, beta, W):
  WX = W.reshape(len(y),1) * X
  WX2 = W.reshape(len(y),1) * (X**2)
  Xb = X @ beta.reshape(len(beta),1)
  for i in range(len(beta)):
    Xi = X[:,i]
    Xb = Xb - Xi.reshape((len(Xi),1)) * float(beta[i])
    WXi = WX[:,i].reshape(len(y),1)
    sum1 = sum(WXi*(y-Xb))
    beta[i] = soft_thresh(sum1, alpha*lam)[0]
    sum2 = sum(WX2[:,i])
    beta[i] = beta[i]/(sum2 + lam*(1-alpha))
    Xb = Xb + Xi*beta[i]
  return beta

def lasso_py(y, X, lam, alpha=1, beta = np.ones(X.shape[1]), tol=1e-5, maxit=1000):
  W=np.repeat(1/len(y), len(y))
  for j in range(maxit):
    beta_old = beta.copy()
    beta = update_beta(y, X, lam, alpha, beta, W)
    abs_diff = abs(beta-beta_old)
    #id of betas
    idv = len(abs_diff)
    abs_diff = abs_diff.reshape(len(abs_diff),1)
    tolv = np.repeat(tol,idv)
    tolv = tolv.reshape(len(abs_diff),1)
    exit = True
    for k in range(idv):
      if abs_diff[k,0] >= tolv[k,0]:
        exit = False
    if exit == True:
      print(j)
      break
    elif j == maxit:
      print("Do not converge after maximum number of iterations")
  return beta
