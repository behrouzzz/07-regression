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
shuttle$use <- factor(shuttle$use, levels = c("noauto","auto"))

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
shuttle$use <- factor(shuttle$use, levels = c("auto","noauto"))
head(shuttle,3)
fitNouse <- glm(use ~ wind - 1, family = "binomial", data = shuttle)

fitUse$coef
fitNouse$coef
exp(fitNouse$coef)

# The coefficients reverse their signs.


## Question 4
# Consider the insect spray data InsectSprays. Fit a Poisson model using spray as a factor level. Report the estimated relative rate comparing spray A (numerator) to spray B (denominator). Rate is count per spray; so rate = count for each spray.
data("InsectSprays")
str(InsectSprays)

spray2 <- relevel(InsectSprays$spray, "B")

fitCount <- glm(count ~ spray2, family = "poisson", data = InsectSprays)
summary(fitCount)

############################################################
# CORRECT ATTEMPT 2
# Estimated relative rate comparing spray A (num) to B(den)
exp(fitCount$coef[2])
############################################################

# rate <- fitCount$coef[1] / fitCount$coef[2]; exp(rate)
# 
# fitCount2 <- glm(count ~ spray, family = "poisson", data = InsectSprays)
# rate2 <- exp(fitCount2$coef[2])

g <- ggplot(InsectSprays, aes(x = spray, y = count, fill = spray)) + 
    geom_histogram(stat = "identity")
g


## Question 5
# Consider a Poisson glm with an offset, t. So, for example, a model of the form glm(count ~ x + offset(t), family = poisson) where x is a factor variable comparing a treatment (1) to a control (0) and t is the natural log of a monitoring time. What is impact of the coefficient for x if we fit the model glm(count ~ x + offset(t2), family = poisson) where t2 <- log(10) + t? In other words, what happens to the coefficients if we change the units of the offset variable. (Note, adding log(10) on the log scale is multiplying by 10 on the original scale.)
set.seed(123)
n <- 100
count <- sample(50, n, replace = TRUE)
x <- factor(sample(2, n, replace = TRUE) - 1)
t <- log(seq(1,100, length.out = n))

fit1 <- glm(count ~ x + offset(t), family = "poisson")
fit1$coef

t2 <- log(10) + t
fit2 <- glm(count ~ x + offset(t2), family = "poisson")
fit2$coef

# Intercept is subtracted by log(10), coef of x1 is unchanged.


## Question 6
# Consider the data
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
# Using a knot point at 0, fit a linear model that looks like a hockey stick with two lines meeting at x=0. Include an intercept term, x and the knot point term. What is the estimated slope of the line after 0?
plot(x,y)
knot <- 0
splineTerm <- (x > knot) * (x - knot)

# add intercept and linear term
xMat <- cbind(1, x, splineTerm)
fit <- lm(y ~ xMat - 1)
fit$coef

# slope before 0 = -1.0241584
# slope after 0 = 2.0372258 + (-1.0241584) = 1.013067

