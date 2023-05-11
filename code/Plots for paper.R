#Plots for paper

#packages
library(tidyverse)
library(scales)
library(gridExtra)

#### Figure 1 ####
Nclx <- read.csv("data/Nclx_season.csv")
Nclx$Season <- factor(Nclx$Season, levels= c("Spring", "Summer", "Autumn", "Winter") )
Ncl <-ggplot(data = Nclx, mapping = aes(x = Filter, y = Nclx, fill = POD))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_manual(values = c( "cyan", "plum"))+
  scale_y_continuous(labels = label_comma())+
  labs(x = "Filter", y = "Number of clicks (Nclx) per hour")+
  facet_wrap(~ Season, ncol = 4)

Ncl

DPM <- read.csv("data/DPM_season.csv")
DPM$Season <- factor(DPM$Season, levels= c("Spring", "Summer", "Autumn", "Winter") )
dpm <-ggplot(data = DPM, mapping = aes(x = Filter, y = rate, fill = POD))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_manual(values = c( "cyan", "plum"))+
  # scale_y_continuous(labels = label_comma())+
  labs(x = "Filter", y = "DPM detection rate (%)")+
  facet_wrap(~ Season, ncol = 4)+
  theme(strip.text.y = element_text(size = 10))
dpm
dpm

DPH <- read.csv("data/DPH_season.csv")
DPH$Season <- factor(DPH$Season, levels= c("Spring", "Summer", "Autumn", "Winter") )
dph<-ggplot(data = DPH, mapping = aes(x = Filter, y = rate, fill = POD))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_manual(values = c( "cyan", "plum"))+
    labs(x = "Filter", y = "DPH detection rate (%)")+
  facet_wrap(~ Season, ncol = 4)+
  theme(strip.text.y = element_text(size = 10))

dph


DPD <- read.csv("data/DPD_season.csv")
DPD$Season <- factor(DPD$Season, levels= c("Spring", "Summer", "Autumn", "Winter") )
dpd<-ggplot(data = DPD, mapping = aes(x = Filter, y = rate, fill = POD))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_manual(values = c( "cyan", "plum"))+
  labs(x = "Filter", y = "DPD detection rate (%)")+
  facet_wrap(~ Season, ncol = 4)+
  theme(strip.text.y = element_text(size = 10))
dpd




tiff('Fig1_mod2.1.tiff', units="in", width=11, height=5, res=300)
combine <- grid.arrange(dpm, dph, dpd, ncol=3, nrow=1)
combine
ggsave("combine.png")


dev.off()

#### Figure 2 ####

DPH_HML <- read.csv("DPH_HML2.csv")
str(DPH_HML)

DPH_HML$datetime <- as.POSIXct(DPH_HML$datetime, format="%d/%m/%Y")
DPH_HML$month <- as.factor(DPH_HML$month)

HML<- ggplot(data = DPH_HML, mapping = aes(x= month, DPH, fill= POD))+
  geom_boxplot()+
  #facet_wrap(~year)+
  theme_classic()+
  labs(x = "Month of the year", y = "Detection positive hours (DPH) per day")+
  scale_fill_manual(values = c( "cyan", "plum"))

HML

BPH_all <- read.csv("BPH_all.csv")
str(BPH_all)

BPH_all$datetime <- as.POSIXct(BPH_all$datetime, format="%d/%m/%Y")
BPH_all$month <- as.factor(BPH_all$month)

BPH <- ggplot(data = BPH_all, mapping = aes(x= month, BPH, fill= POD))+
  geom_boxplot()+
  #facet_wrap(~year)+
  theme_classic()+
  labs(x = "Month of the year", y = "Buzz positive hours (BPH) per day")+
  scale_fill_manual(values = c( "cyan", "plum"))

BPH

library(patchwork)


tiff('Fig2.tiff', units="in", width=10, height=10, res=300)
fig2 <- HML/BPH & plot_annotation(tag_levels = 'A')
fig2

dev.off()

#### Figure 3 ####

##### 1 ####
# 
C_pgam1 <- 
  plot(sm(hp_bam_viz2, 1)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5,6,7,8,9,10,11,12), 
                     labels = c("1", "2", "3", "4", "5","6","7","8","9","10","11","12")) +
  # Make the labels right
  labs(x = "Month", y = "")+
  theme_grey()

C_pgam1

#### 2#####
C_pgam2 <- 
  plot(sm(hp_bam_viz2, 2)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Average Temperature (°C) ", y = "")+
  theme_grey()

C_pgam2

##### 3 #####
C_pgam3 <- 
  plot(sm(hp_bam_viz2, 3)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "All clicks (NAll)", y = "")+
  theme_grey()

C_pgam3

###4#####
C_pgam4 <- 
  plot(sm(hp_bam_viz2, 4)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Tidal range (m)", y = "P(Occurence)")+
  theme_grey()

C_pgam4

## 5 ########
C_pgam5 <- 
  plot(sm(hp_bam_viz2, 5)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Difference to high tide (hours)", y = "P(Occurence)")+
  theme_grey()

# Take a look
C_pgam5




#6########
newdata$diel_period <- factor(newdata$diel_period, levels= c("morning", "day", "evening", "night") )

new.dat.period <- data.frame(C_DPM = newdata$diel_period,
                             diel_period = newdata$diel_period,
                             month = median(newdata$month),
                             average_temp = median(newdata$average_temp),
                             C_Nall = median(newdata$C_Nall),
                             tidal.range = median(newdata$tidal.range),
                             difftoHT = median(newdata$difftoHT))



p.dat.period <- data.frame(prob= newdata$C_DPM,
                           diel_period= newdata$diel_period,
                           fit_prob= predict.gam(bamm_mod2, new.dat.period, type = "response"))


pa.period <- ggplot(p.dat.period, aes(x=diel_period, y= fit_prob)) +
  geom_boxplot(outlier.shape = NA)+
  labs(x = "Diel period",
       y = "P(Occurence)") 

C_pgam6 <- pa.period 
C_pgam6

library 
figure <- ggarrange(C_pgam1 + rremove("ylab") , C_pgam2 + rremove("ylab") , C_pgam3 + rremove("ylab"), C_pgam4 + rremove("ylab"), C_pgam5 + rremove("ylab"), C_pgam6 + rremove("ylab"),# remove axis labels from plots
                    labels = NULL,
                    ncol = 1, nrow = 6,
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "left"))

annotate_figure(figure, left = textGrob("Common y-axis", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Common x-axis", gp = gpar(cex = 1.3)))

###PLOT#####
hp_bam_viz <- mgcViz::getViz(bamm_mod1)
print(plot(hp_bam_viz, allTerms = T, type= "response"), pages = 1)
#try mgcViz



hpgam1
##### 1 ####

F_pgam1 <- 
  plot(sm(hp_bam_viz, 1)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5,6,7,8,9,10,11,12), 
                     labels = c("1", "2", "3", "4", "5","6","7","8","9","10","11","12")) +
  # Make the labels right
  labs(x = "Month", y = "P(Occurence)")+
  theme_grey()

F_pgam1

#### 2#####
F_pgam2 <- 
  plot(sm(hp_bam_viz, 2)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Average Temperature", y = "P(Occurence)")+
  theme_grey()

F_pgam2

##### 3 #####
F_pgam3 <- 
  plot(sm(hp_bam_viz, 3)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "All clicks (NAll)", y = "P(Occurence)")+
  theme_grey()

F_pgam3

###4#####
F_pgam4 <- 
  plot(sm(hp_bam_viz, 4)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Tidal range (m)", y = "P(Occurence)")+
  theme_grey()

F_pgam4

## 5 ########
F_pgam5 <- 
  plot(sm(hp_bam_viz, 5)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Difference to high tide (hours)", y = "P(Occurence)")+
  theme_grey()

# Take a look
F_pgam5




#6########
newdata$diel_period <- factor(newdata$diel_period, levels= c("morning", "day", "evening", "night") )

new.dat.period <- data.frame(F_DPM = newdata$diel_period,
                             diel_period = newdata$diel_period,
                             month = median(newdata$month),
                             average_temp = median(newdata$average_temp),
                             F_Nall = median(newdata$F_Nall),
                             tidal.range = median(newdata$tidal.range),
                             difftoHT = median(newdata$difftoHT))



p.dat.period <- data.frame(prob= newdata$F_DPM,
                           diel_period= newdata$diel_period,
                           fit_prob= predict.gam(bamm_mod1, new.dat.period, type = "response"))


pa.period <- ggplot(p.dat.period, aes(x=diel_period, y= fit_prob)) +
  geom_boxplot(outlier.shape = NA)+
  labs(x = "Diel period",
       y = "P(Occurence)") 

F_pgam6 <- pa.period 
F_pgam6


#######

gridPrint(F_pgam1, F_pgam2, F_pgam3, F_pgam4, F_pgam5, F_pgam6)



tiff('FigCF3.tiff', units="in", width=10, height=10, res=300)
gridPrint(C_pgam1, F_pgam1, C_pgam2, F_pgam2, C_pgam3, F_pgam3, C_pgam4, F_pgam4,
                     C_pgam5, F_pgam5,C_pgam6, F_pgam6, ncol = 2)
dev.off()




#### Figure 4 ####

C_pgam2 <- 
  plot(sm(hp_bam_viz, 1)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Average Temperature (°C)", y = "P(Buzzing)")+
  theme_grey()

C_pgam2

C_pgam3 <- 
  plot(sm(hp_bam_viz, 2)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "All clicks (NAll)", y = "P(Buzzing)")+
  theme_grey()

C_pgam3


##### 1 ####
F_pgam1 <- 
  plot(sm(hp_bam_viz, 1)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5,6,7,8,9,10,11,12), 
                     labels = c("1", "2", "3", "4", "5","6","7","8","9","10","11","12")) +
  # Make the labels right
  labs(x = "Month", y = "P(Buzzing)")+
  theme_grey()

F_pgam1

#### 2#####
F_pgam2 <- 
  plot(sm(hp_bam_viz, 2)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Average Temperature (°C)", y = "P(Buzzing)")+
  theme_grey()

F_pgam2


#3########
newdata2$diel_period <- factor(newdata2$diel_period, levels= c("morning", "day", "evening", "night") )

new.dat.period <- data.frame(F_fpp = newdata2$F_fpp,
                             diel_period = newdata2$diel_period,
                             month = median(newdata2$month),
                             average_temp = median(newdata2$average_temp))
                         

p.dat.period <- data.frame(prob= newdata2$F_fpp,
                           diel_period= newdata2$diel_period,
                           fit_prob= predict.gam(F_bamm_mod4, new.dat.period, type = "response"))


pa.period <- ggplot(p.dat.period, aes(x=diel_period, y= fit_prob)) +
  geom_boxplot(outlier.shape = NA)+
  labs(x = "Diel period",
       y = "P(Buzzing)") 

F_pgam3 <- pa.period 
F_pgam3


C <-gridPrint(C_pgam2, C_pgam3, ncol=1)



F2 <- gridPrint(F_pgam1, F_pgam2, F_pgam3, ncol=1)

CFPlot <- gridPrint(C, F2, ncol=2)



CF_B <- gridPrint(F_pgam2, C_pgam2,
                  F_pgam1, C_pgam3,
                  F_pgam3, ncol=2)




CF_B
ggsave(file= "CF_B2.png", CFPlot)



tiff('FigCFBuzz.tiff', units="in", width=10, height=10, res=300)
gridPrint(C, F2, ncol=2)
dev.off()
