# Coursera JHPH Data Science
# 007 - Regression Models
# Week 1 | Quiz
#
# Joe Nguyen | 04 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/07-regression"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## Question 1
# Consider the data set given below
x <- c(0.18, -1.54, 0.42, 0.95)
# And weights given by
w <- c(2, 1, 3, 1)
# Give the value of μ that minimizes the least squares equation ∑ni=1wi(xi−μ)2
#
# Solve using optimiser
wlsq <- function(x, w) {
    
    p <- list(x, w)
    obj <- function(mu, p) { sum(p[[2]] * (p[[1]] - mu)^2) }
    
    # Brent method (golden section search + parabolic interp)
    optPar <- optimise(obj, range(x), p = p)
    return(optPar$minimum)
}
wlsq(x,w)

# Alternative:
lm(x ~ rep(1, length(x)) - 1, weights = w)


## Question 2
# Consider the following data set
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
# Fit the regression through the origin and get the slope treating y as the outcome and x as the regressor. (Hint, do not center the data since we want regression through the origin, not through the means of the data.)
beta1 <- lm(y~x - 1)$coef; beta1

# Sum of residuals sqaured
sum(resid(lm(y~x - 1))^2)

# manually:
yhat <- beta1 * x
e <- y - yhat; e
sum(e^2)


## Question 3
# Do data(mtcars) from the datasets package and fit the regression model with mpg as the outcome and weight as the predictor. Give the slope coefficient.
data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg
lm(y~x)


## Question 4
# Consider data with an outcome (Y) and a predictor (X). The standard deviation of the predictor is one half that of the outcome. The correlation between the two variables is .5. What value would the slope coefficient for the regression model with Y as the outcome and X as the predictor?
#
# 1. Cor(Y,X) = 0.5 = Cov(Y,X) / (Sy * Sx)
# 2. Sx = Sy/2 <-> 2Sx = Sy
# Combining 1. and 2.:
# Cov(Y,X) = Sx^2
# Now,
# 3. ov(Y,X) = 1/(n-1) \sum_{i=1}^n (Xi - Xbar) (Yi - Ybar)
# 4. Sx^2 = 1/(n-1) \sum_{i=1}^n (Xi - Xbar)^2
# Combining 3. and 4.:
# \sum_{i=1}^n (Xi - Xbar) = \sum_{i=1}^n (Yi - Ybar)
# i.e. moment of inertia in x-axis == moment of inertia in y-axis
#
# slope = 1 q.e.d.


## Question 5
# Students were given two hard tests and scores were normalized to have empirical mean 0 and variance 1. The correlation between the scores on the two tests was 0.4. What would be the expected score on Quiz 2 for a student who had a normalized score of 1.5 on Quiz 1?
#
# Correlation == slope of regression line. So y = mx = 0.4 * 1.5 = 0.6


## Question 6
# Consider the data given by the following
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
# What is the value of the first measurement if x were normalized (to have mean 0 and variance 1)?
xn <- (x - mean(x))/sd(x)
xn[1]


## Question 7
# Consider the following data set (used above as well). What is the intercept for fitting the model with x as the predictor and y as the outcome?
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y~x)


## Question 8
# You know that both the predictor and response have mean 0. What can be said about the intercept when you fit a linear regression?
#
# It must be identically 0.


## Question 9
# Consider the data given by
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
# What value minimizes the sum of the squared distances between these points and itself?
w <- rep(1, length(x))
wlsq(x,w)

# Alternative:
lm(x ~ rep(1, length(x)) - 1)

# Alternative 2:
cor(x, rep(1, length(x)))

## Question 10
# Let the slope having fit Y as the outcome and X as the predictor be denoted as β1. Let the slope from fitting X as the outcome and Y as the predictor be denoted as γ1. Suppose that you divide β1 by γ1; in other words consider β1/γ1. What is this ratio always equal to?
library(UsingR); data(galton)
y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
# y <- galton$child
# x <- galton$parent

lm(y~x)
lm(x~y)


# Models:
# Y = β0 + β1X
# X = γ0 + γ1Y
#
# Then from lectures 03-ols & 05-stat-linear-reg,
# β1 = Cor(Y,X) * Sd(Y) / Sd(X)
# γ1 = Cor(X,Y) * Sd(X) / Sd(Y)
#
# So,
# β1/γ1 = Sd(Y)^2 / Sd(X)^2
# since Cor(Y,X) = Cor(X,Y)
#
# β1/γ1 = Var(Y) / Var(X)
