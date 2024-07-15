library(psych)
library(tidyverse)
library(emmeans)#this is new 
#CONTINUOUS iv AND DV USING WELLBEING DATA
#In this example, we will look at the relationship between stress and psychological
#wellbeing among adolescents, and test if this relationship is moderated by social support.
wellbeing <- read.csv('wellbeing.csv')
str(wellbeing)
#change sex to factor 
wellbeing <- read.csv("wellbeing.csv", stringsAsFactors = T)
str(wellbeing)
levels(wellbeing$Sex)
describe(wellbeing)

#1) Run the interaction model and cbind the coefficients
Interaction <- lm(Wellbeing ~ Stress*SocialSupport, data= wellbeing)
summary(Interaction)
options(scipen =999)

cbind(coef(Interaction), confint(Interaction, level = 0.95))


#2)#create 3 levels for social support and three levels for stress 
#one standard deviation below the mean, at the mean, and above the mean for stress 
# one standard deviation below the mean, at the mean and above the mean for social support
m_Stress<- mean(wellbeing$Stress, na.rm = TRUE)
sd_Stress<- sd(wellbeing$Stress, na.rm = TRUE)
m_Stress
sd_Stress

m_SocialSupport<- mean(wellbeing$SocialSupport, na.rm = TRUE)
sd_SocialSupport<- sd(wellbeing$SocialSupport, na.rm = TRUE)
m_SocialSupport
sd_SocialSupport


emm <- emmeans(Interaction,  ~ Stress*SocialSupport,
               cov.keep = 3, at = list(
                 gender = c(0,1),
                 Stress = c(m_Stress-sd_Stress, m_Stress, m_Stress+sd_Stress),
                 SocialSupport = c(m_SocialSupport-sd_SocialSupport, m_SocialSupport, m_SocialSupport+sd_SocialSupport)), level = 0.95)
summary(emm)


#3) and for social support get simple slopes 


simpleSlope <- emtrends(Interaction, pairwise~SocialSupport, var='Stress',cov.keep=3, at = list(SocialSupport = c(m_SocialSupport-sd_SocialSupport, m_SocialSupport, m_SocialSupport+sd_SocialSupport)),level = 0.95)
summary(simpleSlope)

#4) visualize graph with #emmip using both social support and and stress 
emmip(Interaction, SocialSupport ~ Stress,
      cov.keep = 3, at = list(
        Stress = c(m_Stress-sd_Stress, m_Stress, m_Stress+sd_Stress),
        SocialSupport = c(m_SocialSupport-sd_SocialSupport, m_SocialSupport, m_SocialSupport+sd_SocialSupport)),
      CIs = TRUE, level = 0.95, position = "jitter")+ ylab("wellbing")+ scale_color_manual(labels = c("Low","Medium","High"), values = c( "blue", "green","red"))
#position jitter adds some variance around lines 
