#enter data
setwd("~/Documents/Academic/Teaching/DePaul/DSC423/Alvin/DSC423/Week 3/Examples")
myd=read.table("movies.txt", header=F)
opening=myd[,2]
budget=myd[,3]
star=myd[,4]
release=myd[,5]

#plot variables
plot(budget, opening)
#plot boxplots
boxplot(opening~star)
boxplot(opening~release)

# OPTION 1:
#fit simple regression model using as.factor to create dummy variables 
fit=lm(opening~budget+as.factor(release)+as.factor(star))
summary(fit)

# OPTION 2: fit simple regression model 
# using dummy variables 

#dummy variables
numstar=(star=="Star")*1
numsum=(release=="Summer")*1
fit=lm(opening~budget+numstar+numsum)
fit
summary(fit)

# simple statistics, what happens?
describe(fit)

# get coefficients or Betas (parameter estimates)
coefficients(fit)

# compute new matrix including dummy variables using data frame
dummy_myd <- data.frame(opening, budget, numstar, numsum)
dummy_myd

# simple statistics on this new matrix
library(psych)
describe(dummy_myd)

#compute correlation
cor(dummy_myd, method="pearson")

#anova for analysis of variance
anova(fit)

summary(fit)

#Diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)
layout(matrix(c(1),1,1)) # reset

#standardized residuals vs fitted values plot
plot( fitted(fit), rstandard(fit), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

#standardized residuals vs independent variables
plot( budget, rstandard(fit), main="Budget vs residuals plot")
abline(a=0, b=0,col='red')

#normal probability plot of residuals
qqnorm(rstandard(fit))
qqline(rstandard(fit), col = 2)

#scatterplot of sqrt(opening) vs budget
plot(budget, sqrt(opening))

#apply regression model on transformed Y (sqrt(opening))
fit=lm(sqrt(opening)~budget+numstar+numsum)
summary(fit)

#analysis of variance
anova(fit)

#final model
fit=lm(sqrt(opening)~budget)
anova(fit)
summary(fit)

confint(fit, level=0.95) # CIs for model parameters

error <- qnorm(0.975) * sd(sqrt(opening))/sqrt(32)

# 95% confidence interval
# lower bound is average - error, upper bound is average + error
lower_interval <- mean(sqrt(opening)) - error
upper_interval <- mean(sqrt(opening)) + error

#Diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)
layout(matrix(c(1),1,1)) # reset

#standardized residuals vs fitted values plot
plot( fitted(fit), rstandard(fit), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

#standardized residuals vs independent variables
plot( budget, rstandard(fit), main="Budget vs residuals plot")
abline(a=0, b=0,col='red')

#normal probability plot of residuals
qqnorm(rstandard(fit))
qqline(rstandard(fit), col = 2)

#scatterplot of log(opening) vs budget
plot(budget, log(opening))           

#apply regression model on transformed Y (log(opening))
fit=lm(log(opening)~budget+d_star+d_summer)
summary(fit)
