#Moderation categorical 

library(psych)
library(tidyverse)
library(car)
library(emmeans)

mathmod <- read.csv("mathmod.csv")
summary(mathmod)
describe(mathmod)
str(mathmod)
# compute interaction  whether gender is a moderator of the effect of training sessions on math score performance
#gender 0 male 1 female


XZinteraction <- lm(math~training*gender, data =mathmod)
summary(XZinteraction)
options(scipen =999)

cbind(coef(XZinteraction), confint(XZinteraction, level = 0.95))
#for every one unit increase in training, math score decreases by .34 #hmmmm!!!
#for every one unit increase in gender(for females), mathmod decreases by 2.8 #HMMM!!

#But the interaction is significant!
#significant interaction, for every one unit increase in gender, training increases by .50 (t x g)

## create an empty frame for the plot for the abline the first value is the intercept and 
#the second value is the slope so 5 is rounded from 4.98999 (the intercept)for male and 
#for female it is 4.98999+ -2.75688 = 2.23

#Since the regression coefficient (0.50427) for the interaction term XZ is significant at the alpha level 0.05 with a?p-value=5.8e-11, there is a significant moderation effect.

#In other words, the effect of training intensity on math performance 
#significantly depends on gender.

#For every one unit increase or change in gender, the effect of training on math performance increases by .504

library(emmeans)
# find mean and find std assigned to objects
m_training<- mean(mathmod$training, na.rm = TRUE)
sd_training<- sd(mathmod$training, na.rm = TRUE)
m_training
sd_training

#estimated marginal means for levels on gender 0 and 1 by training on math score (emmean column)
emm <- emmeans(XZinteraction,  ~ training*gender,
               cov.keep = 3, at = list(
                 gender = c(0,1),
                 training = c(m_training-sd_training, m_training, m_training+sd_training)), level = 0.95)
summary(emm)
#cpecifies that only three covariance parameters will be kept when computing standard errors and confidence intervals for the estimated marginal means.

#The decision to set cov.keep = 3 is arbitrary and context-dependent.
#It depends on various factors, including the complexity of your model, 
#the number of covariates or interactions involved, and considerations such as model interpretability and computational efficiency.
#resting the trends for gender to see differences in coefficients by male and female

simpleSlope <- emtrends(XZinteraction, pairwise~gender, var='training')
summary(simpleSlope)
#shows the slopes of training at each level of gender male and female 
#SHOWS THE CONTRAST DIFFERENCE AFTER 

#Plot by the two levels
#basic R plot first 
attach(mathmod)
plot(training, math, type='n') 
#We plug in gender to get the intercept at each level of gender 
#For Male:
  #5 is rounded from 4.98999 for male intercept and 
#For Female:
  #intercept it is 4.98999+ -2.75688 = 2.23
#slope is on the previous slide

#To get the slope at each level of training:
  #We plug in gender to get the slope at each level of gender just two levels so take the coefficient
#When Z=0 (male students), the estimated effect of training intensity on math performance 
#is-.3394 or -.34
#This is the SLOPE for males 

#When Z=1 (female students), the estimated effect of training intensity on math performance 
#-.34+.50=.16 (.50 is the interaction term for training * gender)
#This is the SLOPE for females

#The moderation analysis tells us that the effects of training
#intensity on math performance for males (-.34) and females (.16) 
#are significantly different for this example.

abline(5, -.34, lty=1,col="black") ## for male intercept and the value for males, (lty means line type)
abline(2.25, .16, lty=2, col='red') ## for female  the intercept is 2.23 .5 + -.2,75 = 2.25 from gender below
legend('topright', c('Male', 'Female'), lty=c(1,2), 
        col = c('black', 'red')) 
## add scatter plot 
points(training[gender==0], math[gender==0])  
points(training[gender==1], math[gender==1], col='red') 
detach(mathmod)

#second way is the est, marginal means interaction plot (emmip) below: this 
#you do not have to plug in the y intercept and x values manually as with basic R
emmip(XZinteraction, gender ~ training,
      cov.keep = 3, at = list(
        training = c(m_training-sd_training, m_training, m_training+sd_training),
        gender = c(0,1)),
      CIs = TRUE, level = 0.95, position = "jitter")+ ylab("math")+ scale_color_manual(labels = c("male","female"), values = c( "blue", "green"))
#position jitter adds some variance around lines 


#Setting zero to FALSE in the setCor function indicates that zero-order correlations 
#will not be included in the moderation analysis. 
#means that only the interaction term between the gender variable, 
#the training variable (moderator), 
#and the outcome variable (math) will be used to assess moderation