library(tidyverse)
library(emmeans)
library(psych)

#1.	Read in the data and assign it to an object called depress. 
depress <- read.csv("depress.csv")        

#2.	Describe the data
support_mean <- mean(depress$support)
support_sd <- sd(depress$support)
support_upper <- support_mean + support_sd
support_down <- support_mean - support_sd
support_mean 
support_sd
support_upper
support_down

#4.Configure the linear regression 
XZinteraction <- lm(depress ~  stress*support,data=depress)

#5.	Get the summary of XZinteraction
summary(XZinteraction)

options(scipen =999)

#6.Bind the coefficients with the confidence interval at the 95% level and report them
cbind(coef(XZinteraction), confint(XZinteraction, level = 0.95))

#7a. create three levels - have got them! create new for stress
m_Stress<- mean(depress$stress, na.rm = TRUE)
sd_Stress<- sd(depress$stress, na.rm = TRUE)
m_Stress
sd_Stress
#7b.
emm <- emmeans(XZinteraction,  ~ stress*support,
               cov.keep = 3, at = list(
                 stress = c(m_Stress-sd_Stress, m_Stress, m_Stress+sd_Stress),
                 support = c(support_down, support_mean, support_upper)), level = 0.95)
summary(emm)
#It assesses the levels of the DV (depression) at 3 levels of the moderator (support) and 3 levels of IV (stress)

#7c.
simpleSlope <- emtrends(XZinteraction, pairwise~support, var='stress',cov.keep=3, at = list(support = c(support_down, support_mean, support_upper)),level = 0.95)
summary(simpleSlope)

#Simple slopes show the slopes for each level of the Moderator (low, medium, high) It shows change in depression for each unit increase in stress when social support is low/medium/high
# t tests tells if the observed slopes for a given level of the support is different from zero significantly.

#8.	Plot /visualize the interaction
emmip(XZinteraction, support ~ stress,
      cov.keep = 3,  at = list(
        stress = c(m_Stress-sd_Stress, m_Stress, m_Stress+sd_Stress),
        support = c(support_down, support_mean, support_upper)), 
      CIs = TRUE, level = 0.95, position = "jitter")+ ylab("despression")+ scale_color_manual(labels = c("Low","Medium","High"), values = c( "blue", "green","red"))

#9. Explain the results for the moderation

#Social Support was examined as a moderator of the relation between depression (DV) and stress (IV). Stress and social support were entered in the first step of the regression analysis. In the second step of the regression analysis, the interaction term between stress and social support was entered, and it explained a significant increase in variance in depression, R squared = .96, p < .001. Thus, social support was a significant moderator of the relationship between stress and depression. The unstandardized simple slope for individuals 1 SD below the mean of social support was .996, the unstandardized simple slope for individuals with a mean level of social support was -.10, and the unstandardized simple slope for individuals 1 SD above the mean of social support was -.1.196."


