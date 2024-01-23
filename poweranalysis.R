library(dplyr)
library(tidyverse)
library(lubridate)
library(esmpack) #if you don't have this package, run remotes::install_github("wviechtb/esmpack")
library(betapart)
library(emodiff)
library(nlme)
rawdata <- read.csv('dataPrimary/primaryPower.csv') # this file contains anonymized primary data
inputPA.power <- c("HAPPY","RELAXED","EXCITED")
inputNA.power <- c("DISTRESSED","DOWN","IRRITATED","EMBARRASSED")
inputER.power <- c("SUPPRESSION","SHARING","REAPPRAISAL","RUMMINATION","ACCEPATANCE",
             "WORRIED")

inputNeeded <- c("ppnr","triggerid","CREATED_TS.1",inputPA.power,inputNA.power,inputER.power)
dateformat<-"mdy HM"


dfPower <- rawdata
dfPower <- dfPower[,inputNeeded]
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
# There were certain glitches that produced wrong values for ER strategies:
dfPower$SUPPRESSION <- ifelse(dfPower$SUPPRESSION > 10, NA, dfPower$SUPPRESSION)
dfPower$SHARING <- ifelse(dfPower$SHARING > 10, NA, dfPower$SHARING)
dfPower$REAPPRAISAL <- ifelse(dfPower$REAPPRAISAL > 10, NA, dfPower$REAPPRAISAL)
dfPower$RUMMINATION <- ifelse(dfPower$RUMMINATION > 10, NA, dfPower$RUMMINATION)
dfPower$ACCEPATANCE <- ifelse(dfPower$ACCEPATANCE > 10, NA, dfPower$ACCEPATANCE)
dfPower$WORRIED <- ifelse(dfPower$WORRIED > 10, NA, dfPower$WORRIED)

dfPower <- calcDynamics(dfPower,inputNA = inputNA.power, inputER = inputER.power, inputPA = inputPA.power)

# calculate with LME

# control for state mean NA as in many studies of ED
model1.power <- lme(fixed=BrayCurtisFull.amm ~ m_EDcwL1D +timecw+m_NAcw+  m_ERcw+
                               m_NAcb + m_EDcb + m_ERcb, 
                             data=dfPower, 
                             random=~1+m_EDcwL1D+ m_NAcw+m_ERcw | ppnr, correlation = corAR1(), 
                             control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
model2.power <- lme(fixed=m_ED ~ BrayCurtisFull.ammcw +timecw+m_NAcw+  m_ERcw + m_EDL1D +
                      m_NAcb + BrayCurtisFull.ammcb + m_ERcb, 
                    data=dfPower, 
                    random=~1+ BrayCurtisFull.ammcw+m_NAcw+ m_ERcw | ppnr, correlation = corAR1(), 
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# below are the power analysis input values in our pre-registration
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


