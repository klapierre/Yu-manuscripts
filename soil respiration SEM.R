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


