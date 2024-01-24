# ======================================================================
# Title: Power Analyses
# Date: 24-1-2024
# Copyright: Edmund Lo, checked by Dominique Maciejewski
# ======================================================================

# Library
library(dplyr)
library(tidyverse)
library(lubridate)
library(esmpack) #if you don't have this package, run remotes::install_github("wviechtb/esmpack")
library(betapart)
library(emodiff)
library(nlme)

source("func_preprocessing.R") # load functions for pre-processing data (including calculation of momentary indices)

# Read in data
rawdata <- read.csv('dataPrimary/primaryPower.csv') # this file contains anonymized primary data
inputPA.power <- c("HAPPY","RELAXED","EXCITED")
inputNA.power <- c("DISTRESSED","DOWN","IRRITATED","EMBARRASSED")
inputER.power <- c("SUPPRESSION","SHARING","REAPPRAISAL","RUMMINATION","ACCEPATANCE",
             "WORRIED")

inputNeeded <- c("ppnr","triggerid","CREATED_TS.1",inputPA.power,inputNA.power,inputER.power)
dateformat<-"mdy HM"

dfPower <- rawdata
dfPower <- dfPower[,inputNeeded]

# change variable names
dfPower$PARTICIPANT_ID <- dfPower$ppnr
dfPower$BEEP <- dfPower$triggerid
dfPower$day <-day(parse_date_time(dfPower$CREATED_TS.1,orders=dateformat))
dfPower <- dfPower %>%
  group_by(PARTICIPANT_ID) %>%
  mutate(DAY = day - min(day) + 1)

dfPower[dfPower == "MISS"] <- NA
dfPower[dfPower == ""] <- NA
dfPower[dfPower == -999] <- NA

dfPower[,c(inputPA.power,inputNA.power,inputER.power)] <- sapply(dfPower[,c(inputPA.power,inputNA.power,inputER.power)], function(x) as.numeric(x))
# There were certain glitches that produced wrong values for ER strategies (i.e., there were some instances
# where the values for the ER strategies were above 10, sometimes e.g., 20000), so those were to recoded to NA
dfPower$SUPPRESSION <- ifelse(dfPower$SUPPRESSION > 10, NA, dfPower$SUPPRESSION)
dfPower$SHARING <- ifelse(dfPower$SHARING > 10, NA, dfPower$SHARING)
dfPower$REAPPRAISAL <- ifelse(dfPower$REAPPRAISAL > 10, NA, dfPower$REAPPRAISAL)
dfPower$RUMMINATION <- ifelse(dfPower$RUMMINATION > 10, NA, dfPower$RUMMINATION)
dfPower$ACCEPATANCE <- ifelse(dfPower$ACCEPATANCE > 10, NA, dfPower$ACCEPATANCE)
dfPower$WORRIED <- ifelse(dfPower$WORRIED > 10, NA, dfPower$WORRIED)

dfPower <- calcDynamics(dfPower,inputNA = inputNA.power, inputER = inputER.power, inputPA = inputPA.power)

# Run multilevel models in data

## Model 1: Negative emotion differentiation predicting Emotion regulation variability
model1.power <- lme(fixed=BrayCurtisFull.amm ~ m_EDcwL1D +timecw+m_NAcw+  m_ERcw+
                               m_NAcb + m_EDcb + m_ERcb, 
                             data=dfPower, 
                             random=~1+m_EDcwL1D+ m_NAcw+m_ERcw | ppnr, correlation = corAR1(), 
                             control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

## Model 2: Emotion regulation variability predicting Negative emotion differentiation  
model2.power <- lme(fixed=m_ED ~ BrayCurtisFull.ammcw +timecw+m_NAcw+  m_ERcw + m_EDL1D +
                      m_NAcb + BrayCurtisFull.ammcb + m_ERcb, 
                    data=dfPower, 
                    random=~1+ BrayCurtisFull.ammcw+m_NAcw+ m_ERcw | ppnr, correlation = corAR1(), 
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Extract input values for power analysis input in our pre-registration
#  Model 1: emotion differentiation predicts subsequent emotion regulation variability
# (controlling for negative emotion intensity and emotion regulation strategy use)
mean(dfPower[(!is.na(dfPower$BrayCurtisFull.amm) & !is.na(dfPower$m_EDL1D) & !is.na(dfPower$m_ED)),"BrayCurtisFull.amm"])
sd(dfPower[(!is.na(dfPower$BrayCurtisFull.amm) & !is.na(dfPower$m_EDL1D) & !is.na(dfPower$m_ED)),"BrayCurtisFull.amm"])
summary(model1.power)

# Model 2: emotion regulation variability predicts subsequent emotion differentiation
# (controlling for lagged emotion differentiation, negative emotion intensity and emotion regulation strategy use)
mean(dfPower[(!is.na(dfPower$BrayCurtisFull.amm) & !is.na(dfPower$m_EDL1D) & !is.na(dfPower$m_ED)),"m_ED"])
sd(dfPower[(!is.na(dfPower$BrayCurtisFull.amm) & !is.na(dfPower$m_EDL1D) & !is.na(dfPower$m_ED)),"m_ED"])
summary(model2.power)

# Using these estimates, the simulation models (Model 3) were run using the shiny-app from Ginette Lafit available via: https://github.com/ginettelafit/PowerAnalysisIL
# Plus assuming 13 observations per participant and 1000 repetitions (for details see pre-registration)
