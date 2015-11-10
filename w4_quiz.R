# Coursera JHPH Data Science
# 007 - Regression Models
# Week 4 | Quiz
#
# Joe Nguyen | 10 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/07-regression"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## Question 1
# Consider the space shuttle data ?shuttle in the MASS library. Consider modeling the use of the autolander as the outcome (variable name use). Fit a logistic regression model with autolander (variable auto) use (labeled as "auto" 1) versus not (0) as predicted by wind sign (variable wind). Give the estimated odds ratio for autolander use comparing head winds, labeled as "head" in the variable headwind (numerator) to tail winds (denominator).
require(MASS); data("shuttle")
head(shuttle,3)
tail(shuttle,4)
sapply(shuttle, class)

# Logistic regression
logRegUse <- glm(use ~ wind, family = "binomial", data = shuttle)

require(ggplot2)
g <- ggplot(shuttle, aes(x = wind, y = use)) +
    geom_bar(stat = "identity", position = position_dodge())
g

summary(logRegUse)

plot(shuttle$wind, logRegUse$fitted, xlab = "Wind Type", ylab = "Autolanding Use")

# Log of odds: call the logit g = logit(p) = log(p/(1-p))
# beta1 is log odds ratio -> exp(beta1) is odds ratio
OR <- exp(logRegUse$coeff[2]); OR
# Odds ratio is of using autolanding given change from head to tail wind.


