#===========================================
# Title: Data preparation
# Date: 24-1-2024
# Copyright: Edmund Lo, checked by Dominique Maciejewski
# https://www.psychologicalscience.org/publications/psychological_science/ps-submissions#data
# "Primary data refers to the first digital (and if necessary, anonymized) version of the raw data, otherwise unaltered"
#===========================================

# INSTALL PACKAGES IF NEEDED
# install.packages("remotes")
# install.packages("devtools")
# remotes::install_github("wviechtb/esmpack")
# devtools::install_github("seanchrismurphy/emodiff")

# libraries
library(worcs)
library(dplyr)
library(tidyverse)
library(esmpack)
library(haven) # for reading sav files
library(readxl) # for reading xlsx files
library(lubridate) #for function day
library(betapart)
library(emodiff)

source("list_of_variables.R") # load all ESM measures variable names
source("func_preprocessing.R") # load functions for pre-processing data (including calculation of momentary indices)

#===========================================
# Curate primary data (Part 0)
# Load raw data, apply inclusion criteria (age), and prepare primary data
# https://www.psychologicalscience.org/publications/psychological_science/ps-submissions#data
# "Primary data refers to the first digital (and if necessary, anonymized) version of the raw data, otherwise unaltered"
#===========================================

# The following steps were taken to anonymize the data; otherwise they are closest to the raw data we are supplied with
# These steps were taken so that data do not contain or can't combine to form personal identifiers
# 0.1. recode participant IDs
# 0.2. recode nationality/ethnicity item into binary (majority = 1, others = 0)
# 0.3. round age to integer (i.e., no decimals)
# 0.4. merge ESM data file with demographic data file
# Note that before step 3, we applied the pre-registered inclusion criterion of age<=25 so that we do not include extra data

primaryGVE <- read.csv("dataRaw/rawGVE.csv")
primaryLeuven2011 <- read.csv("dataRaw/rawLeuven2011.csv")
primaryLeuven3W <- read.csv("dataRaw/rawLeuven3W.csv")

# Merging for Tilburg primary data
primaryTilburg <- read_sav("dataRaw/rawTilburg_ESM.sav")
rawdemo <- read_sav("dataRaw/rawTilburg_baseline.sav")
# only age, sex, and origin are needed for mega analysis
primaryTilburg <- merge(primaryTilburg, rawdemo[,c("Eth_ID","age_yr","sex", "origin")], by="Eth_ID",all = TRUE)

# Merging for Ghent primary data
rawlines <- readLines("dataRaw/rawGhent_ESM.csv")
rawlines <- gsub('(^"|"$)', "", rawlines)
rawtemp <- read.csv(textConnection(rawlines), quote = '""')
rawdemo <- read_excel("dataRaw/rawGhent_demo.xlsx")
# only age, sex, and ethnicity are needed for mega analysis
primaryGhent <- merge(rawtemp, rawdemo[,c("ID","Birth_year","Birth_month","Sex", "Ethnicity")], by="ID",all = TRUE)


# Group COB as either 1 (Dutch or Belgian) or 0 (others)
primaryGVE$COB <- ifelse(primaryGVE$COB==1,1,0)
primaryLeuven2011$NAT_BL <- ifelse(primaryLeuven2011$NAT_BL==1,1,0)
primaryLeuven3W$NAT_BL <- ifelse(primaryLeuven3W$NAT_BL==1,1,0)
primaryTilburg$origin <- ifelse(primaryTilburg$origin==1,1,0)
primaryGhent$Ethnicity <- ifelse(grepl("Belgisch", primaryGhent$Ethnicity, fixed = TRUE),1,0) # Belgian origin = 1
primaryGhent$Ethnicity <- ifelse(primaryGhent$Ethnicity==1,1,0)

# pseudonymize the participant IDs 
primaryGVE$PARTICIPANT_ID <- as.integer(factor(primaryGVE$PARTICIPANT_ID))+1000
primaryLeuven2011$UUID <- as.integer(factor(primaryLeuven2011$UUID))+2000
primaryLeuven3W$UUID <- as.integer(factor(primaryLeuven3W$UUID))+3000
primaryTilburg$Eth_ID <- as.integer(factor(primaryTilburg$Eth_ID))+4000
primaryGhent$ID <- as.integer(factor(primaryGhent$ID))+5000

# for the Ghent dataset, we calculate age from birth year/month and start year/month
primaryGhent$start_month <- ifelse(day(primaryGhent$experiment_start_date)/15>1,1,0) + month(primaryGhent$experiment_start_date)
primaryGhent$start_year <- year(primaryGhent$experiment_start_date)
primaryGhent$Birth_year <- as.numeric(primaryGhent$Birth_year) + 2006 

primaryGhent$AGE <- ((primaryGhent$start_year - primaryGhent$Birth_year)*12 +
                  primaryGhent$start_month - as.numeric(primaryGhent$Birth_month))/12


# include those at or below 25 years old in the Tilburg dataset
# also keep those with age = NA
primaryTilburg <- primaryTilburg[primaryTilburg$age_yr <=25 | is.na(primaryTilburg$age_yr),]


# round age to integer
primaryGVE$AGE <-  round(primaryGVE$AGE,0)
primaryLeuven2011$AGE <- round(primaryLeuven2011$AGE_BL,0) 
primaryLeuven3W$AGE <- round(primaryLeuven3W$AGE_BL,0)
primaryTilburg$AGE <- round(primaryTilburg$age_yr,0)
primaryGhent$AGE <- round(primaryGhent$AGE,0)

# remove the original age variables
primaryLeuven2011$AGE_BL <- NULL
primaryLeuven3W$AGE_BL <- NULL
primaryTilburg$age_yr <- NULL

# remove unrelated (un-analyzed) variables
  # potential personal identifiers to be removed
  primaryGhent$Birth_month <- NULL
  primaryGhent$Birth_year <- NULL
  primaryGhent$X <- NULL # remove the original pseudonymized participant ID
  # 2 items below are, in the original study, related to careless responding control
  # They do not match with our pre-registered exclusion criteria (not based on reaction time)
  # so they are not used in our current analysis 
  primaryGhent$check_correct <- NULL 
  primaryGhent$n_correct <- NULL 

# save primary data

write.csv(primaryGVE,"dataPrimary/primaryGVE.csv", row.names = FALSE)
write.csv(primaryLeuven2011,"dataPrimary/primaryLeuven2011.csv", row.names = FALSE)
write.csv(primaryLeuven3W,"dataPrimary/primaryLeuven3W.csv", row.names = FALSE)
write.csv(primaryTilburg,"dataPrimary/primaryTilburg.csv", row.names = FALSE)
write.csv(primaryGhent,"dataPrimary/primaryGhent.csv", row.names = FALSE)


#===========================================
# Data pre-processing (Part 1)
# Part 1 of pre-processing includes
# 1.1. rename variables so they are consistent across datasets
# 1.2. remove erroneous entries
# 1.3. harmonize variables across datasets
# Calculating momentary indices will be done in Part 2
#===========================================


masterscalemin <- 0
masterscalemax <- 10
masterRTthreshold <- 0.5

dataGVE <- read.csv("dataPrimary/primaryGVE.csv")
dataLeuven2011 <- read.csv("dataPrimary/primaryLeuven2011.csv")
dataLeuven3W <- read.csv("dataPrimary/primaryLeuven3W.csv")
dataTilburg <- read.csv("dataPrimary/primaryTilburg.csv")
dataGhent <- read.csv("dataPrimary/primaryGhent.csv")

# prepare Dataset 1: Goed voor elkaar (Radboud)
  dataGVE$study <- "GVE"
  inputRT.GVE <- paste0(c(inputPA.GVE,inputNA.GVE,inputER.GVE),"_RT")
  dataGVE$timeperquestion <- rowMeans(dataGVE[,inputRT.GVE],na.rm = TRUE)
  # divide reaction time by 1000 to obtain seconds (because it's in ms)
  dataGVE$timeperquestion <- dataGVE$timeperquestion/1000

# prepare Dataset 2: Emotions for daily life 2011 (Leuven)

  dataLeuven2011$study <- "Leuven2011"
  colnames(dataLeuven2011)[colnames(dataLeuven2011)== "UUID"] <- "PARTICIPANT_ID"
  colnames(dataLeuven2011)[colnames(dataLeuven2011)== "NAT_BL"] <- "COB"
  
  inputRT.Leuven2011 <- colnames(dataLeuven2011)[grep("_RT_", colnames(dataLeuven2011))]
  dataLeuven2011$timeperquestion <- rowMeans(dataLeuven2011[,inputRT.Leuven2011],na.rm = TRUE)
  # these are hundredth of a second, thus divide by 100 to obtain seconds
  dataLeuven2011$timeperquestion <- dataLeuven2011$timeperquestion/100
  
  # remove last 3 rows, because these rows have no timestamps, and the PARTICIPANT_IDs associated
  # only has 1 entry each
  dataLeuven2011 <- dataLeuven2011[-c(6476:6478),]
  # convert date into integer with lubridate::dmy 
  dataLeuven2011$intDate <- as.integer(dmy(dataLeuven2011$Date_Local))
  # minus the integer with the minimum value within each participant
  # and then +1, so that each participant starts from DAY=1
  dataLeuven2011 <- dataLeuven2011 %>%
    group_by(PARTICIPANT_ID) %>%
    mutate(DAY = intDate - min(intDate) + 1)
  # convert timestamp to integer (in seconds of a 24-hour day)
  # and plus DAY*86400 so that we can calculate beeps
  dataLeuven2011$intSec <- as.integer(hms(dataLeuven2011$Time_Local))+dataLeuven2011$DAY*86400
  dataLeuven2011 <- dataLeuven2011 %>%
    group_by(PARTICIPANT_ID) %>%
    mutate(BEEPTIME = intSec - min(intSec) + 1)
  dataLeuven2011 <- dataLeuven2011 %>%
    group_by(PARTICIPANT_ID) %>%
    mutate(BEEP = order(BEEPTIME))
  # female = 0, male = 1; see DOI: 10.1037/a0033579 (62 female participants)
  dataLeuven2011$FEMALE <- dataLeuven2011$GENDER_BL*(-1) + 1

# prepare Dataset 3: 3-wave study (Leuven)

  dataLeuven3W$study <- "Leuven3W"
  colnames(dataLeuven3W)[colnames(dataLeuven3W)== "UUID"] <- "PARTICIPANT_ID"
  colnames(dataLeuven3W)[colnames(dataLeuven3W)== "NAT_BL"] <- "COB"
  
  # use only data from wave 1
  dataLeuven3W <- dataLeuven3W[dataLeuven3W$WAVE_ES==1,]
  # convert date into integer with lubridate::dmy 
  dataLeuven3W$intDate <- as.integer(dmy(dataLeuven3W$Date_Local))
  # minus the integer with the minimum value within each participant
  # and then +1, so that each participant starts from DAY=1
  dataLeuven3W <- dataLeuven3W %>%
    group_by(PARTICIPANT_ID) %>%
    mutate(DAY = intDate - min(intDate) + 1)
  # convert timestamp to integer (in seconds of a 24-hour day)
  # and plus DAY*86400 so that we can calculate beeps
  dataLeuven3W$intSec <- as.integer(hms(dataLeuven3W$Time_Local))+dataLeuven3W$DAY*86400
  dataLeuven3W <- dataLeuven3W %>%
    group_by(PARTICIPANT_ID) %>%
    mutate(BEEPTIME = intSec - min(intSec) + 1)
  dataLeuven3W <- dataLeuven3W %>%
    group_by(PARTICIPANT_ID) %>%
    mutate(BEEP = order(BEEPTIME))
  # 1 = male, 2 = female; see http://dx.doi.org/10.1037/pspa0000126 (90 male participants)
  dataLeuven3W$FEMALE <- dataLeuven3W$GENDER_BL - 1 #recode sex

# prepare Dataset 4: Emotions in daily life (Tilburg)

  dataTilburg$study <- "Tilburg"
  colnames(dataTilburg)[colnames(dataTilburg)== "Eth_ID"] <- "PARTICIPANT_ID"
  colnames(dataTilburg)[colnames(dataTilburg)== "origin"] <- "COB" # Dutch = 1
  colnames(dataTilburg)[colnames(dataTilburg)== "Time"] <- "BEEP"

  # these two participants have no ESM entries; thus remove
  dataTilburg <- dataTilburg %>%
  filter(!PARTICIPANT_ID %in% c(4172,4180))

  dataTilburg$day <- as.integer(as.Date(dataTilburg$ResponseDateTime))
  dataTilburg <- dataTilburg %>%
    group_by(PARTICIPANT_ID) %>%
    mutate(DAY = day - min(day) + 1)
  dataTilburg$FEMALE <- dataTilburg$sex - 1

# prepare Dataset 5: Outside-in (Ghent)


  dataGhent$study <- "Ghent"
  colnames(dataGhent)[colnames(dataGhent)== "ID"] <- "PARTICIPANT_ID"
  colnames(dataGhent)[colnames(dataGhent)== "Ethnicity"] <- "COB" # male = 1 , female = 2
  dataGhent <- dataGhent %>%
    group_by(PARTICIPANT_ID) %>%
    mutate(BEEP = order(timeStampScheduled))
  dataGhent <- dataGhent %>%
    group_by(PARTICIPANT_ID) %>%
    mutate(BEEPTIME = timeStampScheduled - min(timeStampScheduled) + 1)
  dataGhent$DAY <- ceiling((dataGhent$BEEPTIME)/86400)


  dataGhent$FEMALE <- as.numeric(dataGhent$Sex) - 1 #recode sex
  dataGhent$timeused<- dataGhent$timeStampStop - dataGhent$timeStampStart #calculate reaction time
  dataGhent$n_question <- ifelse(dataGhent$DAY<5,54, 30)
  dataGhent$n_question <- dataGhent$n_question + ifelse(dataGhent$DAY>5 & dataGhent$BEEP%%5 ==0, 2,0) 
  dataGhent$timeperquestion<-dataGhent$timeused/dataGhent$n_question
  dataGhent$timeperquestion<- abs(dataGhent$timeperquestion)
  dataGhent <- dataGhent[order(dataGhent$PARTICIPANT_ID),]
  




# add prefix to participant id
dataGVE$PARTICIPANT_ID<-paste0("GVE_",dataGVE$PARTICIPANT_ID)
dataLeuven2011$PARTICIPANT_ID<-paste0("L11_",dataLeuven2011$PARTICIPANT_ID)
dataLeuven3W$PARTICIPANT_ID<-paste0("L3W_",dataLeuven3W$PARTICIPANT_ID)
dataTilburg$PARTICIPANT_ID<-paste0("Tilburg_",dataTilburg$PARTICIPANT_ID)
dataGhent$PARTICIPANT_ID<-paste0("Ghent_",dataGhent$PARTICIPANT_ID)

# add beep per day information
dataGVE$beepperday <- 10
dataLeuven2011$beepperday <- 10
dataLeuven3W$beepperday <- 10
dataTilburg$beepperday <- 5
dataGhent$beepperday <- 5

# add information on whether an ESM observation is completed

# for datasets with reaction time, available reaction time indicated completion
dataGVE$filledESM <- !is.na(dataGVE$timeperquestion)
dataLeuven2011$filledESM <- !is.na(dataLeuven2011$timeperquestion)
dataGhent$filledESM <- !is.na(dataGhent$timeperquestion)
# for datasets without reaction time, we counted all ESM observations with non NA responses in our ESM measures
dataLeuven3W$filledESM <- (rowMeans(sapply(dataLeuven3W[,c(inputNA.Leuven3W,inputPA.Leuven3W,inputER.Leuven3W)], is.na))) < 1
dataTilburg$filledESM <-(rowMeans(sapply(dataTilburg[,c(inputNA.Tilburg,inputPA.Tilburg,inputER.Tilburg)], is.na))) < 1

# calculate person-level ESM observation completion %
dataGVE <- dataGVE %>%
  group_by(PARTICIPANT_ID) %>%
  mutate(filledESMpc = mean(filledESM))
dataLeuven2011 <- dataLeuven2011 %>%
  group_by(PARTICIPANT_ID) %>%
  mutate(filledESMpc = mean(filledESM))
dataLeuven3W <- dataLeuven3W %>%
  group_by(PARTICIPANT_ID) %>%
  mutate(filledESMpc = mean(filledESM))
dataTilburg <- dataTilburg %>%
  group_by(PARTICIPANT_ID) %>%
  mutate(filledESMpc = mean(filledESM))
dataGhent <- dataGhent %>%
  group_by(PARTICIPANT_ID) %>%
  mutate(filledESMpc = mean(filledESM))


# Harmonize ESM measures to a range of 0 to 10
dataGVE <- harmonize(dataGVE,c(inputPA.GVE,inputNA.GVE,inputER.GVE),
                     datasetmin = 0, datasetmax = 10,
                     mastermin = masterscalemin,
                     mastermax = masterscalemax)
dataLeuven2011 <- harmonize(dataLeuven2011,c(inputPA.Leuven2011,inputNA.Leuven2011,inputER.Leuven2011),
                            datasetmin = 1, datasetmax = 100,
                            mastermin = masterscalemin,
                            mastermax = masterscalemax)
dataLeuven3W <- harmonize(dataLeuven3W,c(inputPA.Leuven3W,inputNA.Leuven3W,inputER.Leuven3W),
                          datasetmin = 0, datasetmax = 100,
                          mastermin = masterscalemin,
                          mastermax = masterscalemax)
dataTilburg <- harmonize(dataTilburg,c(inputPA.Tilburg,inputNA.Tilburg,inputER.Tilburg),
                         datasetmin = 0, datasetmax = 100,
                         mastermin = masterscalemin,
                         mastermax = masterscalemax)
dataGhent <- harmonize(dataGhent,c(inputPA.Ghent,inputNA.Ghent,inputER.Ghent),
                       datasetmin = 1, datasetmax = 7,
                       mastermin = masterscalemin,
                       mastermax = masterscalemax)


#===========================================
# Data pre-processing (Part 2)
# 2.1. Create a pooled dataset before applying exclusion criteria
# 2.2. Apply pre-registered exclusion criteria (based on reaction time and zero variance)
# 2.3. Create a pooled dataset after applying exclusion criteria
# 2.4. Calculating momentary indices in preparation of main analyses
#===========================================

# 2.1. create a dataframe of demographics for participants before exclusion
  dfB4Ex <- rbind(dataGVE[,poolinfo_demo],
                  dataLeuven2011[,poolinfo_demo],
                  dataLeuven3W[,poolinfo_demo],
                  dataTilburg[,poolinfo_demo],
                  dataGhent[,poolinfo_demo]
  )
  
  # CHECK: No participants had age>25
  sum(dfB4Ex$AGE>25 & !is.na(dfB4Ex$AGE))
  
  # create a person-level dataframe before exclusion
  dfPersonB4Ex <- dfB4Ex %>% 
    group_by(PARTICIPANT_ID) %>% 
    slice(1)
  

# 2.2. Apply pre-registered exclusion criteria (based on reaction time and zero variance)

  # Dataset 1: GVE
  # There were 2 observations with average reaction time lower than 0.5s
  sum(dataGVE$timeperquestion < masterRTthreshold ,na.rm=TRUE)
  # according to pre-registration, exclude observations when they had <500ms reaction time in average
  dataGVE <- dataGVE %>%
    mutate(across(all_of(c(inputPA.GVE,
                           inputNA.GVE,
                           inputER.GVE)), ~ifelse(timeperquestion < 0.5, NA, .)))
  #according to pre-registration, exclude participants with zero variance
  dataGVE <- dataGVE %>% filter(!PARTICIPANT_ID %in% zerovariance(dataGVE,inputPA.GVE))
  dataGVE <- dataGVE %>% filter(!PARTICIPANT_ID %in% zerovariance(dataGVE,inputNA.GVE))
  dataGVE <- dataGVE %>% filter(!PARTICIPANT_ID %in% zerovariance(dataGVE,inputER.GVE))

  # Dataset 2: Leuven 2011
  # no participant had average reaction time lower than 0.5s
  sum(dataLeuven2011$timeperquestion < masterRTthreshold ,na.rm=TRUE)
  # according to pre-registration, exclude observations when they had <500ms reaction time in average
  # so no entry amended in the following script
  dataLeuven2011 <- dataLeuven2011 %>%
    mutate(across(all_of(c(inputPA.Leuven2011,
                           inputNA.Leuven2011,
                           inputER.Leuven2011)), ~ifelse(timeperquestion < masterRTthreshold, NA, .)))
  
  # nobody showed zero variance, so no participants need to be excluded
  zerovariance(dataLeuven2011,inputPA.Leuven2011)
  zerovariance(dataLeuven2011,inputNA.Leuven2011)
  zerovariance(dataLeuven2011,inputER.Leuven2011)

  # Dataset 3: Leuven 2011
  # no reaction time information available dataset 3: Leuven 3-wave study
  # nobody showed zero variance, so no participants excluded
  zerovariance(dataLeuven3W,inputPA.Leuven3W)
  zerovariance(dataLeuven3W,inputNA.Leuven3W)
  zerovariance(dataLeuven3W,inputER.Leuven3W)
  
  
  # Dataset 4: Tilburg
  # no reaction time information available for this dataset
  # according to pre-registration, exclude participants with zero variance
  dataTilburg <- dataTilburg %>% filter(!PARTICIPANT_ID %in% zerovariance(dataTilburg,inputPA.Tilburg))
  dataTilburg <- dataTilburg %>% filter(!PARTICIPANT_ID %in% zerovariance(dataTilburg,inputNA.Tilburg))
  dataTilburg <- dataTilburg %>% filter(!PARTICIPANT_ID %in% zerovariance(dataTilburg,inputER.Tilburg))
  
  # Dataset 5: Ghent
  # according to pre-registration, surveys with item completed below 500ms are seen to be of poor quality
  dataGhent <- dataGhent %>%
    mutate(across(all_of(c(inputPA.Ghent,
                           inputNA.Ghent,
                           inputER.Ghent)), ~ifelse(timeperquestion < masterRTthreshold, NA, .)))
  # according to pre-registration, exclude participants with zero variance
  dataGhent <- dataGhent %>% filter(!PARTICIPANT_ID %in% zerovariance(dataGhent,inputNA.Ghent))
  dataGhent <- dataGhent %>% filter(!PARTICIPANT_ID %in% zerovariance(dataGhent,inputER.Ghent))
  dataGhent <- dataGhent %>% filter(!PARTICIPANT_ID %in% zerovariance(dataGhent,inputER.Ghent))

# 2.3. Calculate all momentary dynamics indices
  # Note, Bray-Curtis dissimilarity is multiplied by 10 so that it ranges from 0 - 10, like other ESM measures do 
  dataGVE<- calcDynamics(dataGVE,inputNA = inputNA.GVE, inputER = inputER.GVE, inputPA = inputPA.GVE)
  # Warning message: In log((1 + rho)/(1 - rho)) : NaNs produced
  # a few participants returned NA on the c_ED output because there were too few observations 
  # but moment-level ED (m_ED), the variable we use for modeling, was still successfully calculated
  # so these warnings can be ignored
  dataLeuven2011 <- calcDynamics(dataLeuven2011, inputNA.Leuven2011,inputER.Leuven2011,inputPA.Leuven2011)
  dataLeuven3W <- calcDynamics(dataLeuven3W, inputNA.Leuven3W,inputER.Leuven3W,inputPA.Leuven3W)
  dataTilburg <- calcDynamics(dataTilburg, inputNA.Tilburg,inputER.Tilburg,inputPA.Tilburg)
  # Warning message: In log((1 + rho)/(1 - rho)) : NaNs produced
  # a few participants returned NA on the c_ED output because there were too few observations 
  # but moment-level ED (m_ED), the variable we use for modeling, was still successfully calculated
  # so these warnings can be ignored
  dataGhent <- calcDynamics(dataGhent,inputNA = inputNA.Ghent, inputER.Ghent,inputPA.Ghent)
  # Warning messages:
  # 1: In log((1 + rho)/(1 - rho)) : NaNs produced
  # 2: In log((1 + rho)/(1 - rho)) : NaNs produced
  # a few participants returned NA on the c_ED output because there were too few observations 
  # but moment-level ED (m_ED), the variable we use for modeling, was still successfully calculated
  # so these warnings can be ignored
  
  
# 2.4. Create a pooled dataset after applying exclusion criteria
  
  poolinfo_all <- c(poolinfo_demo,poolinfo_BC,poolinfo_ED,poolinfo_person,poolinfo_ctrl)
  
  df <- rbind(dataGVE[,poolinfo_all],
              dataLeuven2011[,poolinfo_all],
              dataLeuven3W[,poolinfo_all],
              dataTilburg[,poolinfo_all],
              dataGhent[,poolinfo_all]
  )
  
  dfPerson <- df %>% 
    group_by(PARTICIPANT_ID) %>% 
    slice(1)
  
# save ready-to-analyze data in .csv format
write.csv(df,"dataProcessed/ReadyPooledESM.csv", row.names = FALSE)
write.csv(dfPerson,"dataProcessed/ReadyPooledPerson.csv", row.names = FALSE)
write.csv(dfPersonB4Ex,"dataProcessed/ReadyPooledPersonB4Ex.csv", row.names = FALSE)
