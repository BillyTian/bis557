% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oos-gradient-descent.R
\name{oos_gradient_descent}
\alias{oos_gradient_descent}
\title{Gradient Descent based on out-of-sample accuracy}
\usage{
oos_gradient_descent(form, data, gamma = 1e-04, beta0 = 1,
  stop.diff = 1e-15, max.itera = 5e+05)
}
\arguments{
\item{form}{a formula with the legal format.}

\item{data}{a dataframe provided by the user.}

\item{gamma}{a factor defined by user as a pace or speed of gradient descent (default=0.0001).}

\item{beta0}{an initializer for the beta vector that can be changed by user (default=1).}

\item{stop.diff}{an absolute difference between loss and the updated one set by user
to stop the iteration at some satisfying point (default=1e-8).}

\item{max.itera}{a maximum number of iterations set by user,
which is an alternative criterion to stop the iterations (default=1e6).}
}
\value{
a list including the coefficient estimates.
}
\description{
This is a function fitting the OLS model using gradient descent
which calculate the loss based on the out-of-sample accuracy.
}
\examples{
data(iris)
fit <- oos_gradient_descent(Sepal.Length ~ ., iris)
fit$coefficients
}
