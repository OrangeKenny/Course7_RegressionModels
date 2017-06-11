## Firstly, load the dataset

library(datasets)
data(mtcars)

?mtcars # check if you need see the info on mtcars dataset

boxplot(mpg ~ am, data = mtcars, xlab = "Transmission type", ylab = "Miles per gallon")
pairs(mtcars)

##We need to be sure that gas consumption between automatic and manual cars is statistcally significant (not only via boxplot). 
##In order to that we use Student’s t-Test. If the value of p < 0.05, the difference is significant and if p >= 0.05 we can’t provide any suggestions that there is a difference.
t.test(mtcars$mpg ~ mtcars$am)

##Let’s make a model with only MPG and transmission type.
clearModel <- lm(mpg ~ am, data = mtcars)
summary(clearModel)

## We see that the model explains (adjusted R-square) only 33,85% of varience. 
## This is not appropriate. Firstly, lets start with simple way of defining the final model. 
## The method is called “Backward stepwise regression”. 
## It’s a a semi-automated process of building a model by successively adding or 
## removing variables based solely on the t-statistics of their estimated coefficients.
full.model <- lm(mpg ~ ., data = mtcars)
reduced.model <- step(full.model, direction="backward", k=2, trace=0)
## summary(reduced.model)

second.model <- lm(mpg ~  qsec + wt + am, data = mtcars)
anova(clearModel, second.model)

## Test the model
second.model <- lm(mpg ~  qsec + wt + am, data = mtcars)
anova(clearModel, second.model)

## Let’s make a test of our previous model. 
## We now grab manually the predictor variables that can impact on our outcome.
analysis <- aov(mpg ~ ., data = mtcars)
summary(analysis)

## We see that everyting that has p-value below 0.05 is significant. 
## Let’e make a new model with these results and take as predictor variables: cylinders, displacement, quatermiletime, weight and transmission.
fit1 <- lm(mpg ~ cyl + disp + qsec + wt + am, data = mtcars)
summary(fit1)

fit2 <- lm(mpg ~ qsec + wt + am, data = mtcars)
summary(fit2)

## We end up with the same results as with automatic model with adjusted R-squared 0.834.
## Now we can see the plot of our model (“fit2” is the same as “second.model”):
  
par(mfrow = c(2, 2))
plot(fit2)
