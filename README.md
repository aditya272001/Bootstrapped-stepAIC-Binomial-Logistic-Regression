## Exploratory data analysis and bootstrapped stepAIC Binomial Logistic Regression 
The current project uses data available from Routledge's Handbook of Regression modeling in People analytics by Keith McNulty. The aim 
of project is to determine the determinant factos in recent donations received in a NGO. The code and results discussed below.  

## Demo code

```
#-Library-# 
library(GGally)
library(dummies)
library(DescTools)
library(LogisticDx)
library(lessR)
library(bootStepAIC)

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
```
![Screenshot 2022-02-25 160954](https://user-images.githubusercontent.com/96023170/155701724-f04d7aca-bb8b-4e80-ac47-47b5b510b850.png)
#---EDA---# 

```
pdf("Ttest_gender_donations.pdf", height =12, width = 16, paper = "USr")
ttest(n_donations ~ gender, data = charity)
dev.off()
```
![Screenshot 2022-02-25 161602](https://user-images.githubusercontent.com/96023170/155702055-89de9090-e20a-4e65-8c72-cc37b922d5c6.png)

```
pdf("Bar_chart_recent_prop_G.pdf", height =12, width = 16, paper = "USr")
BarChart(recent_donation, by = gender, fill=c("deepskyblue", "black"), data = charity)
dev.off()
```

```
pdf("Bar_chart_recent_stack_G.pdf", height =12, width = 16, paper = "USr")
BarChart(recent_donation, by = gender, fill=c("deepskyblue", "black"), stack100 = T, data = charity) #full_stack
dev.off()
```
![Screenshot 2022-02-25 161711](https://user-images.githubusercontent.com/96023170/155702249-069db2ce-facf-418a-be21-8c267f14ce4b.png)

```
pdf("Bar_chart_recent_prop_R.pdf", height =12, width = 16, paper = "USr")
BarChart(recent_donation, by = reside, fill = c("deepskyblue", "black", "magenta"), data = charity)
dev.off()
```

```
pdf("Bar_chart_recent_stack_R.pdf", height =12, width = 16, paper = "USr")
BarChart(recent_donation, by = reside, fill = c("deepskyblue", "black", "magenta"), stack100 = T, data = charity)
dev.off()
```
![Screenshot 2022-02-25 161803](https://user-images.githubusercontent.com/96023170/155702349-d6fa051c-74d2-41b1-8c6b-9b5b2a640a54.png)

```
pdf("ANOVA_residential.pdf", height =12, width = 16, paper = "USr")
ANOVA(n_donations ~ reside, data = charity)
dev.off()
```
![ANOVA](https://user-images.githubusercontent.com/96023170/155702504-31fc992c-92f7-4419-8a4f-659c26721645.png)

```
pdf("Gender_Comparison_Residenctial.pdf", height =12, width = 16, paper = "USr")
BarChart(reside, by = gender, stack100 = T, data = charity)
dev.off()
```

```
#--Step-Wise-AIC--# 
Full_Mod_Data <- cbind(total_donation, gender_dummies$genderF)

Mod_4 <- glm(recent_donation ~ ., data = Full_Mod_Data, 
             family = "binomial")

Mod_4_boot <- boot.stepAIC(Mod_4, data = Full_Mod_Data, B = 1000)
Mod_4_boot
summary(Mod_4)
```
![Final table](https://user-images.githubusercontent.com/96023170/155702813-8804e1f1-07ad-45cd-b496-0195b16b3e76.png)


