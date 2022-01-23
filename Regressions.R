# set working directory 


# get data: for this excercise we will use a data shared on Mendeley by researchers: 
# they investigate the relationship between servant leadership and workplace deviance. 
# we will try to predict workplace deviance from servant leadership as well. 
# please use this website to access the dataset: https://data.mendeley.com/datasets/jz4cpkxm38/1/files/5247e1d8-ab8f-4a65-843a-7c986a0f72be

# load library to load the dataset
library(haven)

# load the dataset
dat <- read_sav("EXPERIMENT 1.sav")

# view the dataset 
View(dat)

# check exploratory statistics 
library(psych)
describe(dat[, c('Servant_leadership', 'Organizational_deviance')])
# we dont know how they measured these variables but seems to be fine

# check if there are any missing values in the variables we want to use
sum(is.na(dat$Organizational_deviance))
sum(is.na(dat$Servant_leadership))
sum(is.na(dat$Gender))

# plot the relationship between the two
plot(dat$Servant_leadership, dat$Organizational_deviance, xlab = 'Servant Leadership', ylab = 'Organizational Deviance', frame.plot = F, pch=20)
abline(lm(Organizational_deviance ~ Servant_leadership, data = dat), col = "blue")

# simple regression
reg1 <- lm(Organizational_deviance ~ Servant_leadership, data = dat)

# check the results
summary(reg1)
# report: b = -0.28, t(162) = -2.94, p > .05


# check the assumptions
# multiple plots on the same figure
par(mfrow=c(2,2)) # this specifies two rows and two columns (meaning four figures will appear)
plot(reg1)

# if you want to make it one plot one figure, you can do so by changing these 
# numbers back to 1, as follows: 
par(mfrow=c(1,1))
plot(reg1)



# another way to check the outliers
library(car)
influencePlot(reg1)
# run the same regression without these observations
reg2 <- lm(Organizational_deviance ~ Servant_leadership, data = dat[-52,])
# check the results
summary(reg2)

# re-check the assumptions
plot(reg2,3)


# get standardized estimates
install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(reg2)#std.estimate: b=-0.25

# 2.Build up the MLR(Forced Entry) model ----------------------------------
# unstandardized betas
# function template: lm(Y ~ X1 + X2 + ..., data = ...)
MLR <- lm(Organizational_deviance ~ Servant_leadership + Transformational_leadership, data = dat)
# see details of the model, using summary()
summary(MLR)

# check the assumptions
plot(MLR)

influencePlot(MLR)
# Hierarchical MLR
# unstandardized betas
# function template: lm(Y ~ X1 + X2 + ..., data = ...)
MLR1 <- lm(Organizational_deviance ~ Servant_leadership  + Gender, data = dat)
# see details of the model, using summary()
summary(MLR1)


# function template: lm(Y ~ X1 + X2 + ..., data = ...)
MLR2 <- lm(Organizational_deviance ~ Servant_leadership + Transformational_leadership    + Gender, data = dat)
# see details of the model, using summary()
summary(MLR2)

# comparison between the two models (if adding trans significantly improves model)
anova(MLR1, MLR2)


# function template: lm(Y ~ X1 + X2 + ..., data = ...)
MLR3 <- lm(Organizational_deviance ~ Servant_leadership + Transformational_leadership + Age  + Gender, data = dat)
# see details of the model, using summary()
summary(MLR3)

# compare all of the models to each other
anova(MLR1, MLR2, MLR3)

# standardized betas
# function template: lm.beta(model)
QuantPsyc::lm.beta(MLR)

# 3.Check assumptions -----------------------------------------------------
#Add residuals into the original data frame
dat$fitted <- fitted(MLR)
dat$residuals <- resid(MLR)
dat$standardized.residuals <- rstandard(MLR)
dat$studentized.residuals <- rstudent(MLR) 
#compared to the standardized, the studentized can reduce the influence of outliers
View(dat)

# 1) Normally distributed errors ------------------------------------------
qqplot.resid <- ggplot2::qplot(sample = dat$studentized.residuals, stat="qq") + 
  geom_abline(slope = 1, intercept = 0, color = "Red", linetype = "longdash") +
  labs(x = "Theoretical Values", y = "Observed Values", title = "Q-Q plot of residuals")
# If points are close to the diagonal line, errors are normally distributed
qqplot.resid

# 2)Linearity and 3)Homoscedasticity --------------------------------------
scatter <- ggplot2::ggplot(dat, aes(fitted, studentized.residuals)) +
  geom_point() + 
  geom_smooth(method = "lm", colour = "Blue") +
  labs(x = "Fitted Values", y = "Studentized Residuals", title = "Scatterplot of residuals against fitted values")
# Residuals should be randomly distributed around the "0" horizontal line
# If residuals' distribution funnels out, then heteroscedasticity exists
# If the fitted line is a curve, then the linearity isn't confirmed
scatter

# 


###########################Categorical Variables ############
# In addition to continous variables 
# we can have categorical variables as predictors as well. 
# For example, we could check if there are any gender differences in implicit motives
# However, here we are using number of cylinders a car has a categorical variable predictor
# what we do here is basically the same thing as t-test, where we compare two groups to each other
# we interpret the results as for example, what is the values of 4 cylinders as compared on 6 cylinders 
# in terms of the mpg (miles per gallon)
# We have to convert our cyl variable into a factor variable as it is numeric
# lets save in a different variable

# organizational deviance for males and females 
t.test(dat$Organizational_deviance ~ dat$Gender)

# regression with gender
lmgender = lm(Organizational_deviance ~ factor(Gender), dat)

summary(lmgender)

# regressions with mtcars
mtc<- lm(mpg~drat+hp, data=mtcars)
View(mtc)
summary(mtc)
mtcars$cyl_Factor <- factor(mtcars$cyl)

# check if if actually is factor
is.factor(mtcars$cyl_Factor)

# build the regression model 
factorLM <- lm(mpg ~ wt + cyl_Factor, data = mtcars)

# check the reults
summary(factorLM)

# PLEASE NOTE: that the lm function, in cases of categorical variables, 
# select a level of the factor variable, to compare the other levels to
# for example in the above example, 4 cylinders level is picked as a reference category
# the results of the rest are compared to this 4 cylinders level. 
# when one interpret the results, one should do so like this: 
# as compared to 4 cylinders, 6 cylinders in a car is negatively associated with miles per gallon.
# specifically, as compared to 4 cylinders, 6 cylinders is associated with
# 6.92 units in miles per gallon. 

# however you could select a reference level manually as well. 
# for example if we want to check the difference between 4 and 8 and 6 and 8 
# in terms of miles per gallon, we could have 8 cylinders as reference level
# and then re-run the model as below. 

# relevel 
mtcars$cyl_Factor <- relevel(mtcars$cyl_Factor, ref = "6")

# now run the model. 
relLM <- lm(mpg ~ cyl_Factor, data = mtcars)
# check the results
summary(relLM)
# now the results show estimates of 4 and 6 cylinders as compared to 8 cylinders
# create functions in to demonstrate a and b
b <- function(formul, dat){
  if (is.data.frame(dat)){
    stringF <- unlist(strsplit(gsub(" ", "", formul, fixed = TRUE), split='~', fixed=TRUE))
    y <- dat[, stringF[1]]
    x <- dat[, stringF[2]]
    xdiff <- (x - mean(x))
    ydiff <- (y - mean(y))
    summ <- sum(xdiff*ydiff)
    ssquareddifx <- sum(xdiff^2)
    b <- summ/ssquareddifx
    b
  }
  else{
    print('Please provide a dataframe')
  }
}
x <- 5
# test it
b("mpg ~ cyl", mtcars)

# try
lma <- lm(mpg ~ cyl, data = mtcars)
summary(lma)

# create a function for R-Squared (yourself)




