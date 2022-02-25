#-Binomial-Logistic-Regression-End-to-End-#

#-Library-# 
library(GGally)
library(dummies)
library(DescTools)
library(LogisticDx)
library(lessR)
#-----Data------#
charity <- read.csv("http://peopleanalytics-regression-book.org/data/charity_donation.csv")
head(charity)
summary(charity)
charity <- charity[complete.cases(charity), ]

#---General-Plot----#
setwd("D:/Regression")
pdf("charity.pdf", height = 12, width = 16, paper = "USr")
GGally::ggpairs(charity)
dev.off()

#---EDA---# 
BoxPlot(n_donations, by1 = gender, vbs_mean = T, box_fill = "lightgoldenrod", data = charity)

BarChart(recent_donation, by = gender, fill=c("deepskyblue", "black"), data = charity)

BarChart(recent_donation, by = gender, stack100 = T, data = charity) #full_stack
#---Mod-1---# 
Mod_1 <- glm(recent_donation ~ n_donations, family = "binomial", data = charity)
summary(Mod_1)
exp(Mod_1$coefficients)

#---Mod-2---# 
reside_dummies <- dummies::dummy("reside", data = charity)
total_donation <- cbind(charity[c("n_donations", "recent_donation", "age")], reside_dummies)

Mod_2 <- glm(recent_donation ~ ., family  = "binomial", data = total_donation)
summary(Mod_2)
library(car)
car::vif(Mod_2)


Mod_2_cf <- summary(Mod_2)$coefficients

Mod_2_coefs <- cbind(Mod_2_cf[,c("Estimate", "Pr(>|z|)")], 
                     odds_ratio = exp(Mod_2$coefficients))
Mod_2_coefs

#---Mod-3---# 
gender_dummies <- dummies::dummy("gender", data = charity)
gender_dummy_coded <- cbind(charity[c("n_donations", "recent_donation", "age")], gender_dummies)

Mod_3 <- glm(recent_donation ~., family = "binomial", data = gender_dummy_coded)
summary(Mod_3)

Mod_3_cf <- summary(Mod_3)$coefficients

Mod_3_coefs <- cbind(Mod_3_cf[,c("Estimate", "Pr(>|z|)")], 
                     odds_ratio = exp(Mod_3$coefficients))
Mod_3_coefs

#--Pseudo-R2--# 
DescTools::PseudoR2(Mod_1, which = c("McFadden", 
                                     "CoxSnell", "Nagelkerke", "Tjur"))
DescTools::PseudoR2(Mod_2, which = c("McFadden", 
                                     "CoxSnell", "Nagelkerke", "Tjur"))
DescTools::PseudoR2(Mod_3, which = c("McFadden", 
                                     "CoxSnell", "Nagelkerke", "Tjur"))
#--Fitness-Indices--# 
Mod_1_dignostics <- LogisticDx::gof(Mod_1, plotRoc = F)
Mod_1_dignostics$gof

d <- density(residuals(Mod_1, "pearson"))
plot(d, main = "")

d2 <- density(residuals(Mod_2, "pearson"))
plot(d2, main = "")

d3 <- density(residuals(Mod_3, "pearson"))
plot(d3, main = "")
