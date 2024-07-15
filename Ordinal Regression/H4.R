library(MASS)
library(psych)
library(nnet)
library(tidyverse)
library(tinytex)
library(rmarkdown)
library(skimr)
library(reshape2)

data <-read.csv("ORCouponApply.csv", stringsAsFactors = T)

#1.Check the structure of the data using str
str(data)

#2. Change purchase probability to an ordered factor 
data$rpurchase <- factor(data$rpurchase, levels = c ("Unlikely","Somewhat Likely", "Very Likely"))

#3. Recheck the structure of the data to make sure it is now ordered 
str(data)
levels(data$rpurchase)

#4. Describe the data using any descriptive technique you would like 
describe(data)

#5. Using the ORCoupon.csv data set to determine if your IV’s coupon, peers, and quality has any effect on the probability of purchasing (DV)
model <- polr(rpurchase~coupon+peers+quality, data=data, Hess= T)
model
#It tells us proportional odds logistic regression. It tells us the odds ratio, the change in the log odds of being in a higher category as the predictor increases. It also tells the intercepts, the threshold for each comparison.

#6.Exponentiate the coefficients 
exp(coef(model))
#Customers with a coupon are approximately 2.85 times more likely than those without coupon to be in a higher category of purchase probability (from 'Unlikely' to 'Somewhat Likely' or from 'Somewhat Likely' to 'Very Likely').
#The odds of moving to a higher category of purchase probability decrease by an odds of about 0.94 for products that are peer-recommended, compared to products without peer recommendation
#For every one-unit increase in the quality score of the product, the odds of moving to a higher category of purchase probability (from 'Unlikely' to 'Somewhat Likely' or from 'Somewhat Likely' to 'Very Likely') increase by an odd of 1.85.

#7.
summary(model)
fit <- deviance(multinom(rpurchase~1, data=data)) - deviance(model) 
pchisq(fit, 2, lower.tail=FALSE)
confint(model)

#8. Create a new dataframe with peers, coupon and quality and call it newdat and use the code below:
newdat <- data.frame (coupon = rep(0:1, 200), peers = rep(0:1, each = 200), quality = rep(seq(from = 1.9, to = 4, length.out = 100), 4))

#9. CBind the probabilities from the ordinal model called model to the newdat dataframe
newdat <- cbind(newdat, predict(model, newdat, type = "probs"))

#10. Use the reshape2 package to “melt” the data from the newdat dataframe for the levels of rpurchase (unlikely, somewhat likely, very unlikely) into one column and assign it to the object name lnewdat.
lnewdat <- melt(newdat, id.vars = c("coupon", "peers", "quality"),
                variable.name = "Level", value.name="Probability")

#11. Create a ggplot using the aes as x = quality, y = Probability, and colour = Level from the variable.name from the lnewdat dataframe you created. aes(x = quality, y = Probability, colour = Level)) +  geom_line() + facet_grid(coupon ~ peers, labeller="label_both"). 
ggplot(lnewdat, aes(x = quality, y = Probability, colour = Level)) +
  geom_line() +
  facet_grid(coupon ~ peers, labeller = "label_both")
# Peers Recommendation is not a significant factor in predicting the repurchase behavior since the charts of with recommendations or not are very similar. 
# When having coupons, the repurchase behavior generally increases. As the quality improves, the repurchase behavior increases.

