#MODERATED MEDIATION the moderation can occur on any and all paths in the mediation model (e.g., a path, b path, c path, or any combination of the three)

moderation <- read.csv("meddata2.csv", stringsAsFactors = T)

library(mediation)
library(psych)
library(car)
library(tidyverse)
View(moderation)
str(moderation)
levels(moderation$gender)
describeBy(moderation, group = moderation$gender)
# The interaction needs to happen with both "treatment" and mediating variables. 
#In this case, grades is our "treatment" and self-esteem is the mediator.

#Notice this is just like the code in the mediation analysis except we've added 
#an interaction for gender in both models. gender is the moderator 
#The formula notation grades*gender is a short cut for writing grades + gender + grades:gender,
#where ":" is an interaction operator in R's formula syntax. 

#An interaction allows the effect of grades and self-esteem to vary according to gender.INTX not sig here

#grades (treatment) is significant in Model.M, self-esteem (mediator) is significant in model.Y

model.M <- lm(self.esteem ~ grades*gender, moderation) #IV's as a function of the mediator
summary(model.M)
#grades is significant only nothing else

model.Y <- lm(happiness ~ grades*gender + self.esteem*gender, moderation)
summary(model.Y)
# dv happiness as a function of grades * gender (like in Model.M)
#and add in the mediator self.esteem *gender = total effect of mediator + moderator 
# Now see that mediation self-esteem is significant, but not gender is no longer significant(it was in Model.M)

#This is the mediation like before 
results =mediation::mediate(model.M, model.Y, treat="grades", mediator="self.esteem", sims=500)
summary(results)
str(moderation)
#ACME - indirect effect of IV of grades on DV of happiness thru mediator of
#self esteem is significant 

#x to mediator Average Casual Mediated Effect =.356
#(x to moderator of gender was not sig in model m) a x b product 
#ADE - average direct effect of x grades on Y happiness not sig 
#and total effect significant is  
#thus mediation has occurred, indirect significant and ade not significant 

#testing the moderation -- The second and third arguments BELOW  
#are the different levels of the moderators. 
#Notice they each need to be a list object. 
#The last argument specifies the number of simulations. We use 500, but you may want to do as many as 1000.

#this is new!!!!!
test.modmed(results, covariates.1 = list(gender = "M"), covariates.2 = list(gender = "F"), sims = 500) # list all levels of category or can be continuous as well with different values

#The first section is a test of difference between the average causal mediation effects(ACME), 
#i.e., the indirect effect of grades through self-esteem on happiness by gender.
#The estimated difference is about - 0.05, but the 95% confidence interval spans 
#from -0.38 to 0.33(THIS WILL VARY! due to bootstraps). 
#Using traditional hypothesis testing we might conclude we 
#cannot rule out 0 as the true difference between the mediation effects (0 in interval)
#not significant)
#Another conclusion might be that the true difference appears to be small, 
#but we don't have enough evidence to determine if that
#difference is positive or negative.


#The second section is a test of difference between the average direct effects (ADE), 
#i.e., the direct effect of grades on happiness by gender . As with the indirect effect, 
#we don't have enough evidence to conclude if the difference in direct effects between genders is positive or negative.
#not significant but do see crossover in graph

#plot change togender2 for scatterplot
moderation$gender2 <- as.numeric(moderation$gender)# to plot them
str(moderation$gender2)
moderation$gender2

plot(happiness ~ grades, pch = 23- gender2, bg = c("black","red")[gender2], 
     data=moderation, main = "Happiness as a Function of Gender")
by(moderation,moderation$gender2, function(x) abline(lm(happiness ~ grades,data =x),lty=c("solid","dashed")[x$gender2])) 
#text(6.5,5.5,"Male")
#text(3,3.9,"Female")
legend("topleft", c("Male", "Female"), col=c("red", "black"), lty=c("dashed", "solid"), pch = c(16,15))


#same graph in ggplot
library(tidyverse)
#quick plot or qplot with separate facets
qplot(x = grades, y = happiness, facets = ~gender2, data = moderation) +
  geom_smooth(method = "lm", se=F)

moderation$gender2 <- as.factor(moderation$gender2) 
qplot(x = grades, y = happiness, data = moderation, color = gender2) +
  geom_smooth(method = "lm", se=F) 

#OR
#as a factor 
ggplot(moderation, aes(x = grades, y = happiness, color = gender)) + geom_point() +geom_smooth(method="lm", se=FALSE)

#test multicollinearity
library(car)
vif(model.Y, type ="predictor")
#he "grades" predictor has a GVIF of approximately 14.26, suggesting moderate multicollinearity.
#The "gender" predictor has a GVIF of 1.00, indicating no multicollinearity (which makes sense, as it's a categorical variable).
#The "self-esteem" predictor has a GVIF of approximately 24.89, indicating significant multicollinearity.
#The "Interacts With Other Predictors" column indicates potential interactions (meaning contributing to multicollinearit with each other) between predictor variables. For example, "grades, self-esteem" suggests that the "grades" and "self-esteem" variables interact with each other in the model.

moderation$gender2 <- as.numeric(moderation$gender2)
setCor(happiness ~ grades*gender2 ,data=moderation,zero=FALSE,main="Moderation",std=FALSE) 
