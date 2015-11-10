# Coursera JHPH Data Science
# 007 - Regression Models
# Week 3 | Quiz
#
# Joe Nguyen | 09 Nov, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/01-data-science"
dirWorking <- "/07-regression"
setwd(file.path(dirBase, dirWorking))
rm(list = ls())


## Question 1
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as confounder. Give the adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.
data("mtcars")
summary(lm(mtcars$mpg ~ factor(mtcars$cyl) + mtcars$wt))
#
# 4 cyl -> intercept
# expected change 8 to 4 cyl -> -6.0709


## Question 2
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight as a possible confounding variable. Compare the effect of 8 versus 4 cylinders on mpg for the adjusted and unadjusted by weight models. Here, adjusted means including the weight variable as a term in the regression model and unadjusted means the model without weight included. What can be said about the effect comparing 8 and 4 cylinders after looking at models with and without weight included?.

# Adjusted
summary(lm(mtcars$mpg ~ factor(mtcars$cyl) + mtcars$wt - 1))$coef

# Unadjusted
summary(lm(mtcars$mpg ~ factor(mtcars$cyl) - 1))$coef

######################################
## INCORRECT (1ST ATTEMPT)
# Holding weight constant, cylinder appears to have more of an impact on mpg than if weight is disregarded.
# -> Expected slopes for cyl higher in adjusted case.
######################################
## CORRECT (2ND ATTEMPT)??
# Holding weight constant, cylinder appears to have less of an impact on mpg than if weight is disregarded.
# EXPLANATION:
# It is both true and sensible that including weight would attenuate the effect of number of cylinders on mpg.
######################################
summary(lm(mtcars$mpg ~ factor(mtcars$cyl) + mtcars$wt))$coef
summary(lm(mtcars$mpg ~ factor(mtcars$cyl)))$coef
# Look at the change in effect on slope/coefficient attenuation, not the actually slope/coefficient values


## Question 3
# Consider the mtcars data set. Fit a model with mpg as the outcome that considers number of cylinders as a factor variable and weight as confounder.
fit1 <- lm(mtcars$mpg ~ factor(mtcars$cyl) + mtcars$wt)
# Now fit a second model with mpg as the outcome model that considers the interaction between number of cylinders (as a factor variable) and weight.
fit2 <- lm(mtcars$mpg ~ factor(mtcars$cyl) * mtcars$wt)

fit3 <- lm(mtcars$mpg ~ factor(mtcars$cyl) : mtcars$wt)

# Give the P-value for the likelihood ratio test comparing the two models and suggest a model using 0.05 as a type I error rate significance benchmark.
anova(fit1, fit2)
anova(fit1, fit3)
anova(fit1, fit2, fit3)

# Significance test for anova(fit1, fit2), since testing fit1 and fit3 doesn't give F statistic (same degrees of freedom DF). This is because ANOVA can only test nested models (model n where n > 1) must include variates/terms in model n-1.
#
# The P-value is larger than 0.05. So, according to our criterion, we would fail to reject, which suggests that the interaction terms may not be necessary.


## Question 4
# Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable and weight inlcuded in the model as
summary(lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars))
summary(lm(mpg ~ wt + factor(cyl), data = mtcars))
# How is the wt coefficient interpretted?
#
######################################
## INCORRECT (1ST ATTEMPT)
# The estimated expected change in MPG per half ton increase in weight for for a specific number of cylinders (4, 6, 8).
######################################
## INCORRECT (2ND ATTEMPT)
# The estimated expected change in MPG per half ton increase in weight for the average number of cylinders.
######################################
## INCORRECT (3RD ATTEMPT)
# The estimated expected change in MPG per half ton increase in weight.
######################################
## CORRECT (4TH ATTEMPT)
# The estimated expected change in MPG per one ton increase in weight for a specific number of cylinders (4, 6, 8).
#
# This is because 1000lbs is HALF A TON. Larger gradient I(wt * 0.5) = -6.411 (vs wt = -3.2056) means change in MPG per DOUBLE 1000lbs -> 2000lbs -> TON.
######################################


## Question 5
# Consider the following data set
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the hat diagonal for the most influential point
influence(lm(y ~ x))$hat
hatvalues(lm(y ~ x))

# The diagonals measure leverage of observations (y). The higher the value (leverage), the more influential the observation point. So hat diagonal of most influential point is: 0.9945734 (point 5)
plot(x,y)

# Note the diagonals are in [0,1] and their summation = number of coefficients
sum(influence(lm(y ~ x))$hat)


## Question 6
# Consider the following data set
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
# Give the slope dfbeta for the point with the highest hat value.
#
# dfbeta: change in individual coefficients when the ith point is deleted in fitting the model.
dfbeta(lm(y ~ x))

# Scaled version: i.e. /s.e.(beta)
dfbetas(lm(y ~ x))


## Question 7
# Consider a regression relationship between Y and X with and without adjustment for a third variable Z. Which of the following is true about comparing the regression coefficient between Y and X with and without adjustment for Z.
#
# It is possible for the coefficient to reverse sign after adjustment. For example, it can be strongly significant and positive before adjustment and strongly significant and negative after adjustment.
