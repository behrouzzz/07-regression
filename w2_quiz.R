# Coursera JHPH Data Science
# 007 - Regression Models
# Week 2 | Quiz
#
# Joe Nguyen | 05 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/07-regression"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## Question 1
# Consider the following data with x as the predictor and y as as the outcome.
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
# Give a P-value for the two sided hypothesis test of whether β1 from a linear regression model is 0 or not.
fit <- lm(y ~ x)
beta1 <- coef(fit)[2]; beta1
# beta1 <- cor(y, x) * sd(y) / sd(x)

# Manual statistical inference:
n <- length(x)
# sigma <- sqrt( sum(resid(fit)^2) / (n - 2) ); sigma
ssx <- sum((x - mean(x))^2)
seBeta1 <- summary(fit)$sigma / sqrt(ssx)
ts <- (beta1 - 0) / seBeta1

pBeta1 <- 2 * pt(abs(ts), df = n-2, lower.tail = FALSE); pBeta1

# R-function statistical inference:
summary(fit)$coefficients
# ^ rows are for coeffs: {beta0, beta1, ...}


## Question 2
# Consider the previous problem, give the estimate of the residual standard deviation.
summary(fit)$sigma
# sigma <- sqrt( sum(resid(fit)^2) / (n - 2) ); sigma
# sigma <- sqrt( sum(resid(fit)^2) / fit$df ); sigma


## Question 3 ?? ANSWER = 18.991 ??
# In the mtcars data set, fit a linear regression model of weight (predictor) on mpg (outcome). Get a 95% confidence interval for the expected mpg at the average weight. What is the lower endpoint?
data("mtcars")
y <- mtcars$mpg
x <- mtcars$wt
n <- length(x)
fit <- lm(y ~ x)

# predict(fit, newdata = data.frame(3.21725), interval = ("prediction"))
# yEst <- fit$coefficients[1] + fit$coefficients[2] * mean(x); yEst
sumCoef <- summary(fit)$coefficients; sumCoef
x0 <- mean(x)
yEst <- sumCoef[1,1] + sumCoef[2,1] * x0; yEst

# # 95% lower confidence bound
# yEst - qt(.975, df = fit$df) * sumCoef[2,2] - 
#     qt(.975, df = fit$df) * sumCoef[1,2]

# SOLUTION:
# 1) predict function
predict(fit, newdata = data.frame(x = x0), interval = "confidence")

# 2) manual (lecture 07-inference-regression)
sigma <- summary(fit)$sigma   # sqrt( sum(resid(fit)^2) / fit$df )
ssx <- sum((x - mean(x))^2)

# confidence interval se at x0
se <- sigma * sqrt(1/n + (x0 - mean(x))^2 / ssx)

# confidence interval
yEst + c(-1,1) * qt(0.975, df = fit$df) * se

#####################
library(ggplot2)
newx <- data.frame(x = seq(min(x), max(x), length = 100))
p1 <- data.frame(predict(fit, newdata = newx, interval = "confidence"))
p1$x = newx$x
names(p1)[1] <- "y"

g <- ggplot(p1, aes(x,y)) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) + 
    geom_line() + 
    geom_point(data = data.frame(x=x, y=y), aes(x = x, y = y), size = 4)
g
#####################


## Question 4
# Refer to the previous question. Read the help file for mtcars. What is the weight coefficient interpreted as?
#
# wt -> lb/1000
#
# mpg <- b0 + b1 * wt
# -> b1 = mpg / wt = (1000 * mpg / lb)
#
# The estimated expected change in mpg per 1,000 lb increase in weight.


## Question 5 ?? ANSWER = 27.57 ??
# Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (1,000 lbs). A new car is coming weighing 3000 pounds. Construct a 95% prediction interval for its mpg. What is the upper endpoint?
x0 <- 3
yEst <- sumCoef[1,1] + sumCoef[2,1] * x0; yEst

# SOLUTION:
# 1) predict function
predict(fit, newdata = data.frame(x = x0), interval = "prediction")

# 2) manual (lecture 07-inference-regression)
# prediction interval se at x0
se <- sigma * sqrt(1 + 1/n + (x0 - mean(x))^2 / ssx)

# prediction interval
yEst + c(-1,1) * qt(0.975, df = fit$df) * se


## Question 6
# Consider again the mtcars data set and a linear regression model with mpg as predicted by weight (in 1,000 lbs). A “short” ton is defined as 2,000 lbs. Construct a 95% confidence interval for the expected change in mpg per 1 short ton increase in weight. Give the lower endpoint.
(sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]) * 2


## Question 7
# If my X from a linear regression is measured in centimeters and I convert it to meters what would happen to the slope coefficient?
#
# It would get multiplied by 100.


## Question 8
# I have an outcome, Y, and a predictor, X and fit a linear regression model with Y=β0+β1X+ϵ to obtain β^0 and β^1. What would be the consequence to the subsequent slope and intercept if I were to refit the model with a new regressor, X+c for some constant, c?
#
# The new intercept would be β^0 − cβ^1
#
# Reason:
# Y = β0 + β1(X + c) + ϵ
# Y = (β0 + β1c) + β1X + ϵ
#
# ... use graphical intepretation: for +ve β1, intercept moves down, for -ve β1, intercept moves up


## Question 9
# Refer back to the mtcars data set with mpg as an outcome and weight (wt) as the predictor. About what is the ratio of the sum of the squared errors, ∑ni=1(Yi−Y^i)2 when comparing a model with just an intercept (denominator) to the model with the intercept and slope (numerator)?
#
# Model with just intercept returns average of outcome (on y-axis). So this model has more variability than model that includes slope; ratio must be < 1
sseMean <- sum((y - mean(y))^2)
sseFit <- sum(fit$residuals^2)
sseFit / sseMean
# Ratio = 0.25


## Question 10
# Do the residuals always have to sum to 0 in linear regression?
sum(fit$residuals)
# The residuals must always sum to zero. -> No, E[e] = 0; only if an intercept is included, then they will sum to 0. (Not sure why, but from lecture 06-residual-variation)

