library(mediation)
library(psych)
library(nnet)
library(tidyverse)
library(tinytex)
library(rmarkdown)
library(skimr)
library(diagram)

meddata <-read.csv("meddata2.csv", stringsAsFactors = T)

#1.Check the structure of the data using str
str(meddata)

#2. Get descriptive statistics 
describe(meddata)
summary(meddata)
#On the math assessment, the 50 female and 50 male students had a mean score of 5.99 (SD = 1.74). Scores of 5.000, 6.000, and 7.000 represented the 25th, 50th, and 75th percentiles respectively.
#For their self esteem estimate, they had a mean score of 4.86 (SD = 1.90). Scores of 3, 5, and 6 represented the 25th, 50th, and 75th percentiles respectively.
#For their happiness level, they had a mean score of 5.23 (SD = 2.04). Scores of 4, 5, and 7 represented the 25th, 50th, and 75th percentiles respectively.

# 3.four steps for mediation analysis
#a.	Step 1: Make sure you assign the model for this to a name called Totaleffect. 
#i.	Is this step necessary? If so, explain why, if not explain why.
Totaleffect = lm(happiness~grades,meddata)
summary(Totaleffect)
# This Step is NOT necessary, total effect equals to direct effect + indirect effect, which can be calculated. Because the mediation effects, which we are interested here, can be present in the absence of a total effect.
# Interpretation: The total effect (p<0.001) describes that there is a total effect that happiness has on grades, regardless of the mediation.

# b.	Step 2
fit.mediator=lm(self.esteem~grades,meddata)
summary(fit.mediator)
# Interpretation: Grades have a significant effect on students' self esteem (p<0.001).

#c.	Step 3 and Step 4 
fit.dv=lm(happiness~grades+self.esteem,meddata)
summary(fit.dv)
options(scipen = 999)
# Interpretation: The effect of grades is no longer significant while the mediator self esteem has a significant effect on happiness. Put the mediation in place, the total effect of grades on happiness is explained by the mediator - self esteem. 

#4.	Run the mediation using the mediation
results = mediation::mediate(fit.mediator,fit.dv,boot = T, treat = "grades", mediator = "self.esteem"  )
summary(results)
# ACME: The average causal mediation effect, which is path a x path b (step 2 times step 3), is the indirect effect. Here, (0.63 *0.56) = 0.3565
# ADE: Average Direct Effect says the direct effect of the IV on the DV, the path c' in step 3+4, which is 0.0396.
# Total effect is ACME+ADE, which is also step 1.

#5.	Using the diagram 
data <- c(0, "'0.561*'", 0,
          0, 0, 0,
          "'.636*'", "'.396* (.040)'", 0)
M<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data)
plot<- plotmat (M, pos=c(1,2),
                name= c( "self.esteem","grades", "happiness"),
                box.type = "rect", box.size = 0.13, box.prop=0.5,  curve=0)




