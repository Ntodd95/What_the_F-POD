##Bams
# Load in these packages 
if(!require(EnvStats)) install.packages("EnvStats")
if(!require(mgcv)) install.packages("mgcv")
if(!require(mgcViz)) install.packages("mgcViz")
if(!require(dplyr)) install.packages("dplyr")
if(!require(stats)) install.packages("stats")
if(!require(stats)) install.packages("stats")
library(devtools)
# library(devtools)
devtools::install_github("samclifford/mgcv.helper")

##### C_ detections####
#Load in data
C_det <- read.csv("C_det.csv")




#### Dealing with Autocorrelation ####
#Define grouping variable for GEE autocorrelation estimation

#rename source df
bamm_mod_df <- C_det

#one method is to use AR start in this way
#ARStart vector defines the start of each discrete AR cluster,
#required for bamm autocorrelation definition

bamm_mod_df$ARStart <- rep(F, nrow(bamm_mod_df))

#assign to each hour?
# or yday?

#hour
for(i in 2:nrow(bamm_mod_df)){
  if(bamm_mod_df$datetime[i] != bamm_mod_df$datetime[i - 1]){
    bamm_mod_df$ARStart[i] <- T}}

rm(i)


#I decided to go for Ar start using start event 
#using start event from itsadug
# Structure the dataframe according to deployment and date
bamm_mod_df <- bamm_mod_df[with(bamm_mod_df, order(datetime)),]

#rename df
newdata <- bamm_mod_df
library(itsadug)


newdata <- start_event(newdata, column="hour", event=c("Deployment" ,"Date"), label.event="Event")


#need to define base model without autocorrelation
bamm_base <- bam(
  C_DPM ~
    s(month, bs= "cc", k=6)+
    s(average_temp, k=6)+
    s(C_Nall, k=6) +
    s(tidal.range)+
    as.factor(year)+
    as.factor(diel_period),
  discrete = T,
  rho = 0.329,
  gamma = 1.4,
  family = negbin(0.166),
  data = newdata)

summary(bamm_base)
#VIF
vif(bamm_base)
#ACF plots to determine autocorrelation
acf(residuals(bamm_base))[1]
pacf(residuals(bamm_base))[1]
r1hp <- start_value_rho(bamm_base, plot=TRUE)
acf(resid(bamm_base), plot=FALSE)$acf[2]


par(mfrow=c(1,3), cex=1.1)

# default ACF function:
acf(resid(bamm_base), main="acf(resid(m1))")
# resid_gam:
acf(resid_gam(bamm_base), main="acf(resid_gam(m1))")
# acf_resid:
acf_resid(bamm_base, main="acf_resid(m1)")
r1 <- start_value_rho(bamm_base, plot=TRUE)





#### VIF#####
#vif
#run regular GLM to check VIF as likely some multicolinearity
m1<-glm(C_DPM ~ month+average_temp+diel_period+
          tidal.range+C_Nall,
        family=poisson(),data=newdata)

summary(m1) 
vif(m1) #be careful with seasonal varaibles 

##GEE GLM code
m1<-geeglm(C_DPH ~ bs(month,knots= 4)+bs(average_temp, knots= 4)+as.factor(season)+as.factor(diel_period)+
             as.factor(tide_period)+bs(tidal.range, knots = 4)+bs(C_Nall, knots = 4),
           family=binomial(link = 'logit'), 
           corstr="ar1",id=datetime, data=newdata)


######GAM model with everything #######



#model 1
bamm_mod1 <- bam(
  C_DPM ~
    s(yday, bs= "ts")+
    s(average_temp, bs= "ts")+
    s(C_Nall,  bs= "ts") +
    tidal.range +
    as.factor(year)+
    as.factor(diel_period),
  discrete = T,
  rho = 0.329,
  gamma = 1,
  AR.start = newdata$start.event,
  family = negbin(0.166), #to get theta, run with nb(), and then change to negbin() with theta stated
  data = newdata)

qq.gam(bamm_mod1)
par(mfrow=c(2,2), cex=1)
gam.check(bamm_mod1)

res <- residuals(bamm_mod1)

AIC(bamm_mod1)

#summary and plot
summary(bamm_mod1)

plot(bamm_mod1,rug = TRUE, se = TRUE, all.terms = TRUE)



#issues with the third residual plot
#dont understand the 'lines' 

#get theta
bamm_mod1$family$getTheta()
#0.166


#Normal versus corrected residuals

par(mfrow=c(1,1), cex=1)
# normal residuals:
normal.res <- resid(bamm_mod1)
acf(normal.res, main="HP normal")
# corrected residuals:
corrected.res <- resid_gam(bamm_mod1)
acf(corrected.res,main="HP corrected")

#overdispersion
overdis <- sum(normal.res^2) / bamm_mod1$df.residual
overdis





#plots some rough patterns for the variables
ggplot(data= newdata, mapping = aes(x= tidal.range ,y= F_DPM))+
  geom_smooth(color = "darkcyan", method = "lm")

ggplot(data= newdata, mapping = aes(x= difftoLT ,y= F_DPM))+
  geom_smooth(color = "darkcyan")

ggplot(data= newdata, mapping = aes(x= average_temp ,y= F_DPM))+
  geom_smooth(color = "darkcyan")

ggplot(data= newdata, mapping = aes(x= yday ,y= F_DPM))+
  geom_smooth(color = "darkcyan")

ggplot(data= newdata, mapping = aes(x= month ,y= F_DPM))+
  geom_smooth(color = "darkcyan")+
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5,6,7,8,9,10,11,12), 
                     labels = c("1", "2", "3", "4", "5","6","7","8","9","10","11","12"))

ggplot(data= newdata, mapping = aes(x= hour2 ,y= F_DPM))+
  geom_smooth(color = "darkcyan")

ggplot(data= newdata, mapping = aes(x= julian ,y= F_DPM))+
  geom_smooth(color = "darkcyan")

ggplot(data= newdata, mapping = aes(x= F_Nall ,y= F_DPM))+
  geom_smooth(color = "darkcyan")

#plotting factors 

ggplot(data= newdata, mapping = aes(x= season ,y= C_DPM))+
  geom_boxplot(color = "darkcyan")

#so skewed with the zeros...
#mean DPM per factor an option 


##### model 1 #####
bamm_mod1 <- bam(
  C_DPM ~
    s(month, bs= "cc")+
    s(average_temp, bs= "ts")+
    s(C_Nall, bs= "cr") +
    #  as.factor(tide_period)+
    s(tidal.range) +
    s(difftoHT, bs ="cc")+
    as.factor(diel_period),
  discrete = T,
  rho = 0.329,
  gamma = 1.2,
  AR.start = newdata$start.event,
  family = negbin(0.166), #to get theta, run with nb(), and then change to negbin() with theta stated
  data = newdata)

qq.gam(bamm_mod1)
par(mfrow=c(2,2), cex=1)
gam.check(bamm_mod1)

res <- residuals(bamm_mod1)

AIC(bamm_mod1)

#summary and plot
summary(bamm_mod1)

plot(bamm_mod1,rug = TRUE, se = TRUE, all.terms = TRUE)


###PLOT#####
hp_bam_viz <- mgcViz::getViz(bamm_mod1)
print(plot(hp_bam_viz, allTerms = T, type= "response"), pages = 1)
#try mgcViz



hpgam1
#### 1 ###
# 
C_pgam1 <- 
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

C_pgam1

### 2###
C_pgam2 <- 
  plot(sm(hp_bam_viz, 2)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Average Temperature", y = "P(Occurence)")+
  theme_grey()

C_pgam2

### 3 ###
C_pgam3 <- 
  plot(sm(hp_bam_viz, 3)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "All clicks (NAll)", y = "P(Occurence)")+
  theme_grey()

C_pgam3

##4###
C_pgam4 <- 
  plot(sm(hp_bam_viz, 4)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Tidal range (m)", y = "P(Occurence)")+
  theme_grey()

C_pgam4

## 5 ###
C_pgam5 <- 
  plot(sm(hp_bam_viz, 5)) +
  l_fitLine(linetype = 1)  +
  #CI polygon
  l_ciPoly(fill = "light blue", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Difference to high tide (hours)", y = "P(Occurence)")+
  theme_grey()

# Take a look
C_pgam5


#6###
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
                           fit_prob= predict.gam(bamm_mod1, new.dat.period, type = "response"))


pa.period <- ggplot(p.dat.period, aes(x=diel_period, y= fit_prob)) +
  geom_boxplot(outlier.shape = NA)+
  labs(x = "Diel period",
       y = "P(Occurence)") 

C_pgam6 <- pa.period 
C_pgam6




#######  #F_ detections  ########
#### Dealing with Autocorrelation ####
#Define grouping variable for GEE autocorrelation estimation

#rename source df
F_det <- read.csv("F_det.csv")

bamm_mod_df <- F_det

#one method is to use AR start in this way
#ARStart vector defines the start of each discrete AR cluster,
#required for bamm autocorrelation definition

bamm_mod_df$ARStart <- rep(F, nrow(bamm_mod_df))

#assign to each hour?
# or yday?

#hour
for(i in 2:nrow(bamm_mod_df)){
  if(bamm_mod_df$datetime[i] != bamm_mod_df$datetime[i - 1]){
    bamm_mod_df$ARStart[i] <- T}}

rm(i)


#I decided to go for Ar start using start event 
#using start event from itsadug
# Structure the dataframe according to dep and date
bamm_mod_df$datetime <- as.POSIXct(bamm_mod_df$datetime, format="%d/%m/%Y %H:%M")


bamm_mod_df <- bamm_mod_df[with(bamm_mod_df, order(julian, datetime)),]
#rename df
newdata <- bamm_mod_df
library(itsadug)
newdata$Date <- as.Date(newdata$Date, format = "%d/%m/%Y")

newdata <- start_event(newdata, column="hour", event=c("Deployment" ,"julian"), label.event="Event")


#need to define base model without autocorrelation
bamm_base <- bam(
  F_DPM ~
    s(month, bs= "cc", k=6)+
    s(average_temp, k=6)+
    s(F_Nall, k=6) +
    tidal.range+
    s(difftoHT)+
    as.factor(diel_period),
  discrete = T,
  rho = 0,
  gamma = 1.4,
  family = nb(),
  data = newdata)

summary(bamm_base)
#VIF
vif(bamm_base)
#ACF plots to determine autocorrelation
acf(residuals(bamm_base))[1]
pacf(residuals(bamm_base))[1]
r1hp <- start_value_rho(bamm_base, plot=TRUE)
acf(resid(bamm_base), plot=FALSE)$acf[2]


par(mfrow=c(1,3), cex=1.1)

# default ACF function:
acf(resid(bamm_base), main="acf(resid(m1))")
# resid_gam:
acf(resid_gam(bamm_base), main="acf(resid_gam(m1))")
# acf_resid:
acf_resid(bamm_base, main="acf_resid(m1)")
r1 <- start_value_rho(bamm_base, plot=TRUE)

#### VIF#####
#vif
#run regular GLM to check VIF as likely some multicolinearity
m1<-glm(F_DPM ~ month+average_temp+diel_period+
          tidal.range+F_Nall,
        family=poisson(),data=newdata)

summary(m1) 
vif(m1) #be careful with seasonal varaibles 

##GEE GLM code
m1<-geeglm(C_DPH ~ bs(month,knots= 4)+bs(average_temp, knots= 4)+as.factor(season)+as.factor(diel_period)+
             as.factor(tide_period)+bs(tidal.range, knots = 4)+bs(C_Nall, knots = 4),
           family=binomial(link = 'logit'), 
           corstr="ar1",id=datetime, data=newdata)


######GAM model with everything #######
newdata$year<- as.factor(newdata$year)
#model 1
bamm_mod1 <- bam(
  F_DPM ~
    s(month, bs= "cc")+
    s(average_temp, bs= "ts")+
    s(F_Nall, bs= "cr") +
    #  as.factor(tide_period)+
    s(tidal.range) +
    s(difftoHT, bs ="cc")+
    as.factor(diel_period),
  discrete = T,
  rho = 0.423,
  gamma = 1.4,
  AR.start = newdata$start.event,
  family = negbin(0.17), #to get theta, run with nb(), and then change to negbin() with theta stated
  data = newdata)

AIC(bamm_mod1)

#summary and plot
summary(bamm_mod1)
anova(bamm_mod1)
plot(bamm_mod1,se = TRUE, all.terms = TRUE)

concurvity(bamm_mod1, full= FALSE)
par(mfrow=c(2,2), cex=1)
qq.gam(bamm_mod1)
gam.check(bamm_mod1)

res <- residuals(bamm_mod1)
#issues with the third residual plot
#dont understand the 'lines' 

#get theta
bamm_mod1$family$getTheta()
#0.17


#Normal versus corrected residuals

par(mfrow=c(1,1), cex=1)
# normal residuals:
normal.res <- resid(bamm_mod1)
acf(normal.res, main="HP normal")
# corrected residuals:
corrected.res <- resid_gam(bamm_mod1)
acf(corrected.res,main="HP corrected")

#overdispersion
overdis <- sum(normal.res^2) / bamm_mod1$df.residual
overdis


###PLOT#####
hp_bam_viz <- mgcViz::getViz(bamm_mod1)
print(plot(hp_bam_viz, allTerms = T, type= "response"), pages = 1)
#try mgcViz



hpgam1
### 1 ##
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

#### 2###
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

### 3 ###
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

###4###
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

## 5 ###
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




#6###
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




#### Foraging models  ####

#foraging redo 
#fpp new variable = bpm/dpm*100 

#selecting models... for full process run CF_GAMS first
#but change dataset to C_buzzV2.csv  and C_buzzV2.csv respectfully

#### C Buzz  #####
bamm_mod1 <- bam(
  fpp ~
    s(month, bs= "cc")+
    s(average_temp, bs= "ts")+
    s(C_Nall, bs= "cr") +
    s(tidal.range, bs ="cr") +
    s(difftoHT, bs ="cc")+
    as.factor(diel_period),
  discrete = T,
  rho = 0.17,
  gamma = 1.2,
  AR.start = newdata$start.event,
  family = negbin(0.008), #to get theta, run with nb(), and then change to negbin() with theta stated
  data = newdata)

qq.gam(bamm_mod1)
par(mfrow=c(2,2), cex=1)


gam.check(bamm_mod1)

res <- residuals(bamm_mod1)

AIC(bamm_mod1)


#summary and plot
summary(bamm_mod1)

plot(bamm_mod1,se = TRUE, all.terms = TRUE)



#issues with the third residual plot
#dont understand the 'lines' 

#get theta
bamm_mod1$family$getTheta()
#0.17


#Normal versus corrected residuals

par(mfrow=c(1,2), cex=1)
# normal residuals:
normal.res <- resid(bamm_mod1)
acf(normal.res, main="HP normal")
# corrected residuals:
corrected.res <- resid_gam(bamm_mod1)
acf(corrected.res,main="HP corrected")

#overdispersion
overdis <- sum(normal.res^2) / bamm_mod1$df.residual
overdis

bamm_mod1_d1 <- bam(
  fpp ~
    s(month, bs= "cc")+
    s(average_temp, bs= "ts")+
    s(C_Nall, bs= "cr") +
    s(tidal.range, bs= "cr") +
    # s(difftoHT, bs ="cc")+
    as.factor(diel_period),
  discrete = T,
  rho = 0.17,
  gamma = 1.2,
  AR.start = newdata$start.event,
  family = negbin(0.008), #to get theta, run with nb(), and then change to negbin() with theta stated
  data = newdata)

summary(bamm_mod1)
summary(bamm_mod1_d1)
AIC(bamm_mod1_d1)


bamm_mod1_d2 <- bam(
  fpp ~
    s(month, bs= "cc")+
    s(average_temp, bs= "ts")+
    s(C_Nall, bs= "cr") +
    # s(tidal.range, bs= "ts"),
    s(difftoHT, bs ="cc"),
  # as.factor(diel_period),
  discrete = T,
  rho = 0.17,
  gamma = 1.4,
  AR.start = newdata$start.event,
  family = negbin(0.008), #to get theta, run with nb(), and then change to negbin() with theta stated
  data = newdata)

summary(bamm_mod1_d2)
AIC(bamm_mod1_d2)


bamm_mod1_d3 <- bam(
  fpp ~
    s(month, bs= "cc")+
    s(average_temp, bs= "ts")+
    s(C_Nall, bs= "cr"),
  # s(tidal.range, bs= "ts") +
  #s(difftoHT, bs ="cc")+
  # as.factor(diel_period),
  discrete = T,
  rho = 0.17,
  gamma = 1.4,
  AR.start = newdata$start.event,
  family = negbin(0.008), #to get theta, run with nb(), and then change to negbin() with theta stated
  data = newdata)

summary(bamm_mod1_d3)
AIC(bamm_mod1_d3)

summary(bamm_mod1)

##### FPOD ####

bamm_mod1 <- bam(
  F_FPP ~
    s(month, bs= "cc")+
    s(average_temp, bs= "ts")+
    s(F_Nall, bs= "cr") +
    s(difftoHT, bs= "cc")+
    tidal.range +
    #   as.factor(tide_period)+
    as.factor(diel_period),
  discrete = T,
  rho = 0.283,
  gamma = 1.2,
  AR.start = newdata$start.event,
  family = nb(), #to get theta, run with nb(), and then change to negbin() with theta stated
  data = newdata)

summary(bamm_mod1)
anova(bamm_mod1)
AIC(bamm_mod1)

plot(bamm_mod1,se = TRUE, all.terms = TRUE)

bamm_mod1_d1 <- bam(
  F_FPP ~
    s(month, bs= "cc")+
    s(average_temp, bs= "ts")+
    s(F_Nall, bs= "cr") +
    s(difftoHT, bs= "cc")+
    #tidal.range +
    #   as.factor(tide_period)+
    as.factor(diel_period),
  discrete = T,
  rho = 0.283,
  gamma = 1.2,
  AR.start = newdata$start.event,
  family = nb(), #to get theta, run with nb(), and then change to negbin() with theta stated
  data = newdata)

summary(bamm_mod1_d1)
anova(bamm_mod1_d1)
AIC(bamm_mod1_d1)

bamm_mod1_d2 <- bam(
  F_FPP ~
    s(month, bs= "cc")+
    s(average_temp, bs= "ts")+
    s(F_Nall, bs= "cr") +
    # s(difftoHT, bs= "cc")+
    #tidal.range +
    #   as.factor(tide_period)+
    as.factor(diel_period),
  discrete = T,
  rho = 0.283,
  gamma = 1.2,
  AR.start = newdata$start.event,
  family = nb(), #to get theta, run with nb(), and then change to negbin() with theta stated
  data = newdata)

summary(bamm_mod1_d2)
anova(bamm_mod1_d2)
AIC(bamm_mod1_d2)


bamm_mod1_d3 <- bam(
  F_FPP ~
    s(month, bs= "cc")+
    s(average_temp, bs= "ts")+
    #s(F_Nall, bs= "cr") +
    # s(difftoHT, bs= "cc")+
    #tidal.range +
    #   as.factor(tide_period)+
    as.factor(diel_period),
  discrete = T,
  rho = 0.283,
  gamma = 1.2,
  AR.start = newdata$start.event,
  family = nb(), #to get theta, run with nb(), and then change to negbin() with theta stated
  data = newdata)

summary(bamm_mod1_d3)
anova(bamm_mod1_d3)
AIC(bamm_mod1_d3)


hp_bam_viz <- mgcViz::getViz(F_bamm_mod4)
print(plot(hp_bam_viz, allTerms = T, type= "response"), pages = 1)
