# Load required libraries
library(tidyverse)
library(psych)
library(nnet)
library(tinytex)

#1.	Assign the name DR to the dataset and read the data file DRUGTEST.csv into R Studio. 
DR <- read.csv('DRUGTEST.csv')

#2. Check the structure of the dataset variables using the str command.
str(DR)

#3.	Are there any characters? - Yes! A lot!
DR <- read.csv('DRUGTEST.csv',stringsAsFactors = TRUE)

#4.	Check the structure of the data again
str(DR)

#5.	Confirm levels 
levels(DR$mjuser)
levels(DR$gender)
levels(DR$drugtest)

#6.	Change the reference group 
DR$drugtest <- relevel(DR$drugtest, ref = "random testing program")

#7.	Build a multinomial logistic regression model 
fitmn <- multinom(drugtest ~ mjuser + gender, data = DR)

#8.	Test the fit. Compare the null deviance to the residual deviance. Assign it to the name fit and get the chisquare and p value to determine significance. This will give you the chi square value with df = 1, number of predictors is 2.
#chi
fit <- deviance(multinom(drugtest~1, data=DR)) - deviance(fitmn) 
fit
#p
fit_p_value <- pchisq(fit, df = 2, lower.tail = FALSE)
fit_p_value #Significant


#9.	Test the statistical significance 
summary_fitmn <- summary(fitmn)
z_values <- summary_fitmn$coefficients / summary_fitmn$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
p_values

#10.	Report and interpret the odds ratios 
(ctable <- coef(summary(fitmn))) 
exp((ctable <- coef(summary(fitmn))))

#Interpretation
#The odds of being in a workplace with no testing program for marijuana users increase by a factor of about 1.159 compared to non-users, for males decrease by a factor of 0.921 compared to females
#The odds of being in a workplace with both preemployment and random testing for marijuana users decrease by a factor of about 0.756 compared to non-users, and for males increase by a factor of 1.635 compared to females
#The odds of being in a workplace with preemployment testing for marijuana users decrease by a factor of about 0.9197475 compared to non-users, and for males increase by a factor of about 1.0574139 compared to females



