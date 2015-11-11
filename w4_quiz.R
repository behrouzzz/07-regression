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

# Change use labels (auto = 1, noauto = 0)
shuttle$use <- factor(shuttle$use == "auto")
head(shuttle,3)


# Logistic regression
fitUse <- glm(use ~ wind - 1, family = "binomial", data = shuttle)

require(ggplot2)
g <- ggplot(shuttle, aes(x = wind, y = use, fill = use)) +
    geom_bar(stat = "identity", position = position_dodge())
    # geom_bar(stat = "identity")
g

summary(fitUse)

plot(shuttle$wind, fitUse$fitted, xlab = "Wind Type", ylab = "Autolanding Use")

# Log of odds: call the logit g = logit(p) = log(p/(1-p))
# beta1 is log odds ratio -> exp(beta1) is odds ratio
OR <- exp(fitUse$coef[2]); OR
# Odds ratio is of using autolanding given change from head to tail wind.
exp(fitUse$coef)
# Or is intercept the OR of using autopilot for head wind, slope is OR of using autopilot for tail wind??
#
# OR close to 1 means p = 0.5 (use either autopilot or not)
OR <- exp(fitUse$coef)
OR[1]/OR[2]


## Question 2
# Consider the previous problem. Give the estimated odds ratio for autolander use comparing head winds (numerator) to tail winds (denominator) adjusting for wind strength from the variable magn.
fitUseMagn <- glm(use ~ wind + magn - 1, family = "binomial", data = shuttle)
OR <- exp(fitUseMagn$coef); OR
OR[1]/OR[2]


## Question 3
# If you fit a logistic regression model to a binary variable, for example use of the autolander, then fit a logistic regression model for one minus the outcome (not using the autolander) what happens to the coefficients?
# Invert
shuttle$use <- factor(shuttle$use == "FALSE")
head(shuttle,3)
fitNouse <- glm(use ~ wind - 1, family = "binomial", data = shuttle)

fitUse$coef
fitNouse$coef
exp(fitNouse$coef)

# The coefficients reverse their signs.



