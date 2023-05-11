#########Code to fit a Gaussian mixture model to ln(ICI) calculated from CPOD detected clicks#################
#######################################Enrico Pirotta#########################################################
rm(list=ls())
library(mixtools)
library(lubridate)
library(plyr)
install.packages("devtools")
library(patchwork)

dat<-read.table("Data/Click details/file_export.txt",sep="\t",header=T)       #load the Click details file generated using CPOD.exe
dat$Minute <- as.POSIXct(dat$Minute, format="%d/%m/%Y %H:%M")
dat$Minute_num <- as.numeric(dat$Minute)/60
dat$minfrom2005<-dat$Minute_num-as.numeric(as.POSIXct("2005-01-01"))/60                          #minutes from 2005
dat$mincomplete<-dat$minfrom2005+(dat$microsec/(60*1000000)) #add seconds to the minutes
dat<-dat[order(dat$minfrom2005,dat$TrN,dat$microsec),]       #reorder clicks
dat$ICI[2:nrow(dat)]<-dat$mincomplete[2:nrow(dat)]-dat$mincomplete[1:(nrow(dat)-1)] #calculates ICIs (subtracting the time of consecutive clicks)
dat$ICI[1]<-"NA"
dat$ICI<-as.numeric(dat$ICI)
hist(dat$ICI)
dat$ICI[dat$ICI<0]<-NA                                       #remove negative ICIs (there should't be after reordering)
dat<-na.omit(dat)

dat$logICI<-log(dat$ICI)                                     #calculates logICI
dat$logICI<-as.numeric(dat$logICI)
hist(dat$logICI, breaks=100)

dat$Date<-as.POSIXlt(dat$minfrom2005*60,origin="2005-01-01") #the date given by the CPOD is in minutes. R read dates in seconds, so multiply by 60. There is a correction of 2 days, because Excel date (=POD date) is incorrect by two days (wrong leap years!)

mix<-normalmixEM(dat$logICI, k = 3)
plot(mix, 2)
summary(mix)
#expect a first component centred around -10 (buzz ICIs), a second centred around -7 (regular ICIs) and a third one representing pauses between click trains (or between encounters)

#if the model does not identify the three components correctly (e.g. two components in regular ICI region, because low occurrence of buzzes), run a mixture model with four components:
mix<-normalmixEM(dat$logICI, k = 4)
summary(mix)
plot(mix, 2)

#models with different number of components can be compared using AIC

###repnormmixmodel.sel



dat$clicktype <- apply(mix$posterior,1,which.max)  #assigns an ICI type to each ICI based on the final model
dat$buzz <- ifelse(dat$clicktype ==1, 1, 0)

dat$buzz == 1

#more details in Pirotta et al. 2014 Scale-dependent foraging ecology of a marine top predator modelled using passive acoustic data. Functional Ecology

dat$hour2 <- hour(dat$Minute)
dat$yday <- yday(dat$Minute) #yday = day of the year
dat$hr_day <- paste(dat$hour, dat$yday)


#tapply(dat$buzz, dat$yday, function(x) ifelse(any(x==1), 1, 0)) #buzzing activity per day? yes/no...

#tapply(dat$buzz, dat$yday, function(x) (table(x)/length(x))[2]) #proportion of buzzing activity per day

#prop<- tapply(dat$buzz, dat$yday, function(x) (table(x)/length(x))[2])
#View(prop)
write.csv(dat, "Results/buzz.csv", row.names=F)





