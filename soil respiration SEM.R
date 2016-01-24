library(lavaan)
library(semPlot)
library(tidyr)
library(dplyr)

setwd('C:\\Users\\Kim\\Dropbox\\Smith Lab\\Yu manuscripts\\soil respiration')

rawData <- read.csv('Yu_IMGERS_soil respiration_SEM.csv')

meanData <- rawData%>%
  group_by(plot, treatment)%>%
  summarise(soil_respiration=mean(soil_respiration), soil_moisture=mean(soil_moisture), soil_temperature=mean(soil_temperature), root_biomass=mean(root_biomass))

pathModel <- 
 'soil_respiration ~ treatment + soil_moisture + soil_temperature + root_biomass
  soil_moisture ~ treatment
  soil_temperature ~ treatment
  root_biomass ~ treatment'

fitModel <- sem(pathModel, data=meanData)
summary(fitModel, standardized=T, rsq=T)
coef(fitModel)
semPaths(fitModel, 'std')

###try running seperate models for low and high N additions, because divergent responses were seen

#low N addition model
lowData <- meanData%>%
  filter(treatment!=1.6)

lowModel <- sem(pathModel, data=lowData)
summary(lowModel, standardized=T, rsq=T)
coef(lowModel)
semPaths(lowModel, 'std')

#high N addition model
highData <- meanData%>%
  filter(treatment!=0.4)

highModel <- sem(pathModel, data=highData)
summary(highModel, standardized=T, rsq=T)
coef(highModel)
semPaths(highModel, 'std')
