
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis557

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/BillyTian/bis557.svg?branch=master)](https://travis-ci.com/BillyTian/bis557)
<!-- badges: end -->

The goal of bis557 is to encapsulate homework assignments and other
products for BIS557 in Fall 2020.

## Installation

The development version of the packages can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BillyTian/bis557")
```

## Example

``` r
data(iris)
# Simulate 'lm()'
fit_lm <- linear_model(Sepal.Length ~ ., iris)
fit_lm$coefficients
```
