#DPM corr.
#30/06/2022
#Nicole Todd

setwd("")
# Load in data for all deployments
#Dep 3, 4, 5, 6
#Hi, HiMod and HiModLo
Dep3_H <- read.csv("DPM/Dep3/Dep3_DPM_H.csv")
Dep3_HM <- read.csv("DPM/Dep3/Dep3_DPM_HM.csv")
Dep3_HML <- read.csv("DPM/Dep3/Dep3_DPM_HML.csv")
Dep4_H <- read.csv("DPM/Dep4/Dep4_DPM_H.csv")
Dep4_HM <- read.csv("DPM/Dep4/Dep4_DPM_HM.csv")
Dep4_HML<- read.csv("DPM/Dep4/Dep4_DPM_HML.csv")
Dep5_H <- read.csv("DPM/Dep5/Dep5_DPM_H.csv")
Dep5_HM <- read.csv("DPM/Dep5/Dep5_DPM_HM.csv")
Dep5_HML <- read.csv("DPM/Dep5/Dep5_DPM_HML.csv")
Dep6_H <- read.csv("DPM/Dep6/Dep6_DPM_H.csv")
Dep6_HM <- read.csv("DPM/Dep6/Dep6_DPM_HM.csv")
Dep6_HML <- read.csv("DPM/Dep6/Dep6_DPM_HML.csv")
#libraries
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr) #adding stat to graph
library(gridExtra) #combining plots

###### Dep 3 #####
#hi

hist(Dep3_H$C_DPM)
hist(log(Dep3_H$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep3_hi <-ggplot(Dep3_H, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 3, filter= Hi",
       x= "F-POD DPM", y= "C-POD DPM")+ 
  stat_cor(method = "kendall", label.x = 0, label.y = 20)

Dep3_hi 
ggsave("Dep3_DPM_H.png")
#correlation test
cor.test(x=Dep3_H$F_DPM, y=Dep3_H$C_DPM, method = 'kendall')

#himod

hist(Dep3_HM$C_DPM)
hist(log(Dep3_HM$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep3_himod <-ggplot(Dep3_HM, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 3, filter= HiMod",
       x= "F-POD DPM", y= "C-POD DPM")+ 
  stat_cor(method = "kendall", label.x = 0, label.y = 20)

Dep3_himod 
ggsave("Dep3_DPM_HM.png")
#correlation test


#himodlo

hist(Dep3_HML$C_DPM)
hist(log(Dep3_HML$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep3_himodlo <-ggplot(Dep3_HML, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 3, filter= HiModLo",
       x= "F-POD DPM", y= "C-POD DPM")+ 
  stat_cor(method = "kendall", label.x = 0, label.y = 20)

Dep3_himodlo 
ggsave("Dep3_DPM_HML.png")
#correlation test
corr <- cor.test(x=Dep3_HML$F_DPM, y=Dep3_HML$C_DPM, method = 'kendall')
corr

Dep3 <- grid.arrange(Dep3_hi, Dep3_himod, Dep3_himodlo, ncol=2, nrow=2)
ggsave("Dep3_DPM.png")


##### Dep 4 ####

#hi

hist(Dep4_H$C_DPM)
hist(log(Dep4_H$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep4_hi <-ggplot(Dep4_H, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 4, filter= Hi",
       x= "F-POD DPM", y= "C-POD DPM")+ 
  stat_cor(method = "kendall", label.x = 0, label.y = 20)

Dep4_hi + stat_cor(method = "kendall", label.x = 0, label.y = 20)
ggsave("Dep4_DPM_H.png")
#correlation test
corr <- cor.test(x=Dep4_H$F_DPM, y=Dep4_H$C_DPM, method = 'kendall')
corr

#himod

hist(Dep4_HM$C_DPM)
hist(log(Dep4_HM$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep4_himod <-ggplot(Dep4_HM, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 4, filter= HiMod",
       x= "F-POD DPM", y= "C-POD DPM")+
  stat_cor(method = "kendall", label.x = 0, label.y = 30)

Dep4_himod  
ggsave("Dep4_DPM_HM.png")
#correlation test
corr <- cor.test(x=Dep4_HM$F_DPM, y=Dep4_HM$C_DPM, method = 'kendall')
corr

#himodlo

hist(Dep4_HML$C_DPM)
hist(log(Dep4_HML$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep4_himodlo <-ggplot(Dep4_HML, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 4, filter= HiModLo",
       x= "F-POD DPM", y= "C-POD DPM")+ 
  stat_cor(method = "kendall", label.x = 0, label.y = 40)

Dep4_himodlo + stat_cor(method = "kendall", label.x = 0, label.y = 20)
ggsave("Dep4_DPM_HML.png")
#correlation test
corr <- cor.test(x=Dep4_HML$F_DPM, y=Dep4_HML$C_DPM, method = 'kendall')
corr


Dep4 <- grid.arrange(Dep4_hi, Dep4_himod, Dep4_himodlo, ncol=2, nrow=2)
ggsave("Dep4_DPM.png")
#### Dep 5 ####

#hi

hist(Dep5_H$C_DPM)
hist(log(Dep5_H$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep5_hi <-ggplot(Dep5_H, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 5, filter= Hi",
       x= "F-POD DPM", y= "C-POD DPM")+ 
  stat_cor(method = "kendall", label.x = 0, label.y = 30)

Dep5_hi 
ggsave("Dep5_DPM_H.png")
#correlation test
corr <- cor.test(x=Dep5_H$F_DPM, y=Dep5_H$C_DPM, method = 'kendall')
corr

#himod

hist(Dep5_HM$C_DPM)
hist(log(Dep5_HM$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep5_himod <-ggplot(Dep5_HM, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 5, filter= HiMod",
       x= "F-POD DPM", y= "C-POD DPM")+ 
  stat_cor(method = "kendall", label.x = 0, label.y = 45)

Dep5_himod 
ggsave("Dep5_DPM_HM.png")
#correlation test
corr <- cor.test(x=Dep5_HM$F_DPM, y=Dep5_HM$C_DPM, method = 'kendall')
corr

#himodlo

hist(Dep5_HML$C_DPM)
hist(log(Dep5_HML$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep5_himodlo <-ggplot(Dep5_HML, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 5, filter= HiModLo",
       x= "F-POD DPM", y= "C-POD DPM")+ 
  stat_cor(method = "kendall", label.x = 0, label.y = 45)

Dep5_himodlo 
ggsave("Dep5_DPM_HML.png")
#correlation test
corr <- cor.test(x=Dep5_HML$F_DPM, y=Dep5_HML$C_DPM, method = 'kendall')
corr


Dep5 <- grid.arrange(Dep5_hi, Dep5_himod, Dep5_himodlo, ncol=2, nrow=2)
ggsave("Dep5_DPM.png")

#### Dep 6 ####

#hi

hist(Dep6_H$C_DPM)
hist(log(Dep6_H$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep6_hi <-ggplot(Dep6_H, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 6, filter= Hi",
       x= "F-POD DPM", y= "C-POD DPM")+ 
  stat_cor(method = "kendall", label.x = 0, label.y = 30)

Dep6_hi 
ggsave("Dep6_DPM_H.png")
#correlation test
corr <- cor.test(x=Dep6_H$F_DPM, y=Dep6_H$C_DPM, method = 'kendall')
corr

#himod

hist(Dep6_HM$C_DPM)
hist(log(Dep6_HM$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep6_himod <-ggplot(Dep6_HM, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 6, filter= HiMod",
       x= "F-POD DPM", y= "C-POD DPM")+ 
  stat_cor(method = "kendall", label.x = 0, label.y = 45)

Dep6_himod 
ggsave("Dep6_DPM_HM.png")
#correlation test
corr <- cor.test(x=Dep6_HM$F_DPM, y=Dep6_HM$C_DPM, method = 'kendall')
corr

#himodlo

hist(Dep6_HML$C_DPM)
hist(log(Dep6_HML$C_DPM))

#visually examine the relationship between the clicks per minute for pods
Dep6_himodlo <-ggplot(Dep6_HML, aes(x= F_DPM, y= C_DPM)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=TRUE, color='red')+
  labs(title= "DPM Deployment 6, filter= HiModLo",
       x= "F-POD DPM", y= "C-POD DPM")+ 
  stat_cor(method = "kendall", label.x = 0, label.y = 45)

Dep6_himodlo 
ggsave("Dep6_DPM_HML.png")
#correlation test
corr <- cor.test(x=Dep6_HML$F_DPM, y=Dep6_HML$C_DPM, method = 'kendall')
corr


Dep6 <- grid.arrange(Dep6_hi, Dep6_himod, Dep6_himodlo, ncol=2, nrow=2)
ggsave("Dep6_DPM.png")
