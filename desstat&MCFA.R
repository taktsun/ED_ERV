# ======================================================================
# Title: Descriptive and psychometric analyses
# Date: 24-1-2024
# Copyright: Edmund Lo, checked by Dominique Maciejewski
#
# Overview: After loading all ready-to-analyze data (part 0),
# we have 6 parts in this R script
# 1. Demographic information
# 2. ICC of ESM measures
# 3. OMEGA RELIABILITY for momentary indices
# 4. Descriptive statistics for momentary indices
# 5. Supplementary descriptive statistics
# 6. Multilevel Confirmatory Factor Analysis per Dataset (supplemental materials 3)
# ======================================================================

# In case you run into the following error code (happened with a new Matrix package release):
# "Error in initializePtr() : function 'cholmod_factor_ldetA' not provided by package 'Matrix'"
# please run these codes to reinstall the packages:
  # oo <- options(repos = "https://cran.r-project.org/")
  # install.packages("Matrix")
  # install.packages("lme4")
  # options(oo)

# Library
library(dplyr)
library(lme4) #for the iccs
library(misty) # for multilevel.cor and multilevel ICC
library(multilevelTools) # for omega reliability
library(psych) #needed for Statsby
library(lavaan) # for MCFA



# ======================================================================
# Part 0: load data
# ======================================================================

source("list_of_variables.R") # load all ESM measures variable names
df <- read.csv("dataProcessed/ReadyPooledESM.csv")
dfPerson <- read.csv("dataProcessed/ReadyPooledPerson.csv")
dfPersonB4Ex <- read.csv("dataProcessed/ReadyPooledPersonB4Ex.csv")
dataGVE<- read.csv("dataProcessed/ReadyGVE.csv")
dataLeuven2011 <- read.csv("dataProcessed/ReadyLeuven2011.csv")
dataLeuven3W <- read.csv("dataProcessed/ReadyLeuven3W.csv")
dataTilburg <- read.csv("dataProcessed/ReadyTilburg.csv")
dataGhent <- read.csv("dataProcessed/ReadyGhent.csv")

# ======================================================================
# Part 1: Demographic information
# Method => Participants and Procedures
# ======================================================================


outputTable1 <- dfPerson %>% group_by(study) %>% 
  summarise(N = length(AGE),
            mean_AGE = mean(AGE, na.rm = TRUE),
            SD_AGE = sd(AGE, na.rm = TRUE),
            lrange_AGE = range(AGE, na.rm = TRUE)[1], 
            urange_AGE = range(AGE, na.rm = TRUE)[2], 
            mean_FEMALE = mean(FEMALE, na.rm = TRUE),
            mean_COB = mean(COB, na.rm = TRUE),
            mean_t = mean(filledESMpc, na.rm=TRUE),
            SD_t = sd(filledESMpc, na.rm=TRUE)) %>% as.data.frame()
outputOrder <- c("GVE", "Leuven2011", "Leuven3W", "Tilburg","Ghent")
outputTable1<-outputTable1 %>% slice(match(outputOrder, study))
outputTable1<-rbind(outputTable1, 
                    data.frame(study = "Total", 
                               N = length(dfPerson$AGE),
                               mean_AGE = mean(dfPerson$AGE,na.rm=TRUE),
                               SD_AGE = sd(dfPerson$AGE,na.rm=TRUE),
                               lrange_AGE = range(dfPerson$AGE,na.rm=TRUE)[1],
                               urange_AGE = range(dfPerson$AGE,na.rm=TRUE)[2],
                               mean_FEMALE = mean(dfPerson$FEMALE,na.rm=TRUE),
                               mean_COB = mean(dfPerson$COB,na.rm=TRUE),
                               mean_t = mean(dfPerson$filledESMpc, na.rm=TRUE),
                               SD_t = sd(dfPerson$filledESMpc, na.rm=TRUE)
))
outputTable1<-rbind(outputTable1, 
                      data.frame(study = "BeforeExcl", 
                                 N = length(dfPersonB4Ex$AGE),
                                 mean_AGE = mean(dfPersonB4Ex$AGE,na.rm=TRUE),
                                 SD_AGE = sd(dfPersonB4Ex$AGE,na.rm=TRUE),
                                 lrange_AGE = range(dfPersonB4Ex$AGE,na.rm=TRUE)[1],
                                 urange_AGE = range(dfPersonB4Ex$AGE,na.rm=TRUE)[2],
                                 mean_FEMALE = mean(dfPersonB4Ex$FEMALE,na.rm=TRUE),
                                 mean_COB = mean(dfPersonB4Ex$COB,na.rm=TRUE),
                                 mean_t = mean(dfPersonB4Ex$filledESMpc, na.rm=TRUE),
                                 SD_t = sd(dfPersonB4Ex$filledESMpc, na.rm=TRUE)
))
write.csv(outputTable1, "manuscript/results/Table1.csv", row.names = FALSE)

# ===================================
# Part 2: ICC of ESM measures
# Method => Measures
# ===================================

summaryICC <- function(df, group, strlist){
  dfICC <- data.frame()
  for (i in 1:length(strlist)){
    dfICC <- rbind(dfICC,
                  data.frame(varname = strlist[i],
                             ICC = round(multilevel.icc(unlist(df[strlist[i]]), cluster = unlist(df[group])),2)))
  }
  dfICC
}
dfESMmeasuresICC <- rbind(summaryICC(dataGVE,"PARTICIPANT_ID",c(inputPA.GVE,inputNA.GVE,inputER.GVE)),
summaryICC(dataLeuven2011,"PARTICIPANT_ID",c(inputPA.Leuven2011,inputNA.Leuven2011,inputER.Leuven2011)),
summaryICC(dataLeuven3W,"PARTICIPANT_ID",c(inputPA.Leuven3W,inputNA.Leuven3W,inputER.Leuven3W)),
summaryICC(dataTilburg,"PARTICIPANT_ID",c(inputPA.Tilburg,inputNA.Tilburg,inputER.Tilburg)),
summaryICC(dataGhent,"PARTICIPANT_ID",c(inputPA.Ghent,inputNA.Ghent,inputER.Ghent)))

write.csv(dfESMmeasuresICC, "manuscript/results/ESMmeasuresICC.csv", row.names=FALSE)


# =====================================
# Part 3: OMEGA RELIABILITY
# Methods => Momentary indices => Intensity  of positive emotions, negative emotions, and emotion regulation
# =====================================

# There are certain warnings about zero variance in single ESM measures, 
# which is expected, because we only excluded participants who show zero variance
# in the same GROUP of variables (e.g., in all negative emotion items)
omega_PA_SM_1 <- omegaSEM(items = inputPA.GVE,  data = dataGVE,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_PA_SM_2 <- omegaSEM(items = inputPA.Leuven2011,  data = dataLeuven2011,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_PA_SM_3 <- omegaSEM(items = inputPA.Leuven3W,  data = dataLeuven3W,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_PA_SM_4 <- omegaSEM(items = inputPA.Tilburg,  data = dataTilburg,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_PA_SM_5 <- omegaSEM(items = inputPA.Ghent,  data = dataGhent,  id = "PARTICIPANT_ID",  savemodel = FALSE)

omega_NA_SM_1 <- omegaSEM(items = inputNA.GVE,  data = dataGVE,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_NA_SM_2 <- omegaSEM(items = inputNA.Leuven2011,  data = dataLeuven2011,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_NA_SM_3 <- omegaSEM(items = inputNA.Leuven3W,  data = dataLeuven3W,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_NA_SM_4 <- omegaSEM(items = inputNA.Tilburg,  data = dataTilburg,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_NA_SM_5 <- omegaSEM(items = inputNA.Ghent,  data = dataGhent,  id = "PARTICIPANT_ID",  savemodel = FALSE)

omega_ER_SM_1 <- omegaSEM(items = inputER.GVE,  data = dataGVE,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_ER_SM_2 <- omegaSEM(items = inputER.Leuven2011,  data = dataLeuven2011,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_ER_SM_3 <- omegaSEM(items = inputER.Leuven3W,  data = dataLeuven3W,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_ER_SM_4 <- omegaSEM(items = inputER.Tilburg,  data = dataTilburg,  id = "PARTICIPANT_ID",  savemodel = FALSE)
omega_ER_SM_5 <- omegaSEM(items = inputER.Ghent,  data = dataGhent,  id = "PARTICIPANT_ID",  savemodel = FALSE)

outputReliability <- rbind(cbind(var = "PA",rbind(omega_PA_SM_1$Results,
                       omega_PA_SM_2$Results,
                       omega_PA_SM_3$Results,
                       omega_PA_SM_4$Results,
                       omega_PA_SM_5$Results)),
cbind(var = "NA",rbind(omega_NA_SM_1$Results,
                       omega_NA_SM_2$Results,
                       omega_NA_SM_3$Results,
                       omega_NA_SM_4$Results,
                       omega_NA_SM_5$Results)),
cbind(var = "ER",rbind(omega_ER_SM_1$Results,
                       omega_ER_SM_2$Results,
                       omega_ER_SM_3$Results,
                       omega_ER_SM_4$Results,
                       omega_ER_SM_5$Results)))

write.csv(outputReliability, "manuscript/results/outputReliability.csv", row.names=FALSE)


# ===================================
# Part 4: Descriptive statistics
# Results => Descriptive Statistics
# ===================================

# function to calculate descriptive statistics
desstat <- function(x, varname, group, ndigit=2){
  c(
    # variable name
    var = varname,
    # n obs
    nobs = sum(!is.na(x)),
    # grand mean
    mean = round(mean(x, na.rm = TRUE),ndigit),
    # within person SD
    wSD = round(mean(unlist(aggregate(x, by=list(group), FUN=sd, na.rm = TRUE)[2]),na.rm=TRUE),ndigit),
    # between person SD
    bSD = round(sd(unlist(aggregate(x, by=list(group), FUN=mean, na.rm = TRUE)[2]),na.rm=TRUE),ndigit)
  )
}

# wrapper function to produce descriptive statistics for many variables
summarydesstat <- function(df, group, strlist){
  output <- desstat(unlist(df[strlist[1]]),strlist[1],unlist(df[group]))
  for (i in 2:length(strlist)){
    output<- rbind(output, desstat(unlist(df[strlist[i]]),strlist[i],unlist(df[group])))
  }
  output[,-1] <- as.numeric(output[,-1])
  output
}

# descriptive statistics of momentary indices of the pooled dataset
outputDesStat <- summarydesstat(df, "PARTICIPANT_ID", c(inputIndices))
row.names(outputDesStat) <- NULL
outputDesStat <- as.data.frame(outputDesStat)

write.csv(outputDesStat, "manuscript/results/Table2.csv", row.names=FALSE)

# ===================================
# Part 5: supplementary descriptive statistics
# Supplemental materials 4
# ===================================

# Descriptive Statistics
# 
# desstat1  <- as.data.frame(summarydesstat(dataGVE, "PARTICIPANT_ID", c(inputIndices)))
# desstat2  <- as.data.frame(summarydesstat(dataLeuven2011, "PARTICIPANT_ID", c(inputIndices)))
# desstat3  <- as.data.frame(summarydesstat(dataLeuven3W, "PARTICIPANT_ID", c(inputIndices)))
# desstat4  <- as.data.frame(summarydesstat(dataTilburg, "PARTICIPANT_ID", c(inputIndices)))
# desstat5  <- as.data.frame(summarydesstat(dataGhent, "PARTICIPANT_ID", c(inputIndices)))
# SMTable41 <- rbind(desstat1,
#                    desstat2,
#                    desstat3,
#                    desstat4,
#                    desstat5)
# rownames(SMTable41) <- NULL
# colnames(SMTable41) <- c("Dataset and indices", "n","M","SDw","SDb")
# write.csv(SMTable41, "manuscript/results/SMTable41.csv", row.names=FALSE)

# Multilevel correlation

# 3 functions for handling matrix triangles
combineTriangles <- function(matrix1, matrix2) {
  # Create a new matrix with the same dimensions as the input matrices
  new_matrix <- matrix(0, nrow = nrow(matrix1), ncol = ncol(matrix1))
  # Fill in the lower triangle with values from matrix1
  new_matrix[lower.tri(new_matrix)] <- matrix1[lower.tri(matrix1)]
  # Fill in the upper triangle with values from matrix2
  new_matrix[upper.tri(new_matrix)] <- matrix2[upper.tri(matrix2)]
  return(new_matrix)
}
matrixFromTriangles <- function(mlength, diagonal_values, lv, uv) {
  mat <- matrix(0, nrow = mlength, ncol = mlength)
  mat[lower.tri(mat, diag = FALSE)] <- uv
  mat <- t(mat)
  mat[lower.tri(mat, diag = FALSE)] <- lv
  return(mat)
}
cortablesummary <- function(df, inputIndices = inputIndices, labelIndices = labelIndices){
  objStatsby <- statsBy((df[, c("PARTICIPANT_ID",inputIndices)]),
                    "PARTICIPANT_ID", cors=TRUE)
  tmp.r <- combineTriangles(objStatsby$rwg, objStatsby$rbg) #lower, upper
  tmp.low.ci <- matrixFromTriangles(length(inputIndices),0,
                                  objStatsby$ci.wg$r.ci$lower,objStatsby$ci.bg$r.ci$lower)
  tmp.up.ci <- matrixFromTriangles(length(inputIndices),0,
                                 objStatsby$ci.wg$r.ci$upper,objStatsby$ci.bg$r.ci$upper)
  mat <- matrix(paste0(sub("0.", ".",format(round(tmp.r,2),nsmall=2)), " [",
                         sub("0.", ".",format(round(tmp.low.ci,2),nsmall=2)),",",
                             sub("0.", ".",format(round(tmp.up.ci,2),nsmall=2)),"]"), nrow = nrow(tmp.r),
       dimnames = list(paste0(c(1:length(labelIndices)),". ",inputIndices),
                       c(1:length(inputIndices)))
       )

  diag(mat) <- ""
  mat <- cbind(matrix(as.numeric(summarydesstat(df, "PARTICIPANT_ID", c(inputIndices))[,-1]), nrow = length(inputIndices)),
                mat)
  colnames(mat) <- c("n",
                        "M",
                        "SDw",
                        "SDb",
                        colnames(mat)[-c(1:4)])
  mat
}

# warnings are about participants who have too few ESM observations so that some had no within-person SD
write.csv(cortablesummary(df,inputIndices,labelIndices),"manuscript/results/SMTable41.csv")
write.csv(cortablesummary(dataGVE,inputIndices,labelIndices),"manuscript/results/SMTable421.csv")
write.csv(cortablesummary(dataLeuven2011,inputIndices,labelIndices),"manuscript/results/SMTable422.csv")
write.csv(cortablesummary(dataLeuven3W,inputIndices,labelIndices),"manuscript/results/SMTable423.csv")
write.csv(cortablesummary(dataTilburg,inputIndices,labelIndices),"manuscript/results/SMTable424.csv")
write.csv(cortablesummary(dataGhent,inputIndices,labelIndices),"manuscript/results/SMTable425.csv")

# ===================================
# Part 6: Multilevel Confirmatory Factor Analysis per Dataset
# Supplemental materials 3
# ===================================
# function that extract fit indices
summaryfit <- function(cfamodel, inputPA,inputNA){
  sf <- summary(cfamodel, standardized = TRUE, fit.measures = TRUE)
  sf$fit[c("rmsea","cfi","tli")]
  sfest <- sf$pe[sf$pe$lhs %in% c("pa","na") & sf$pe$rhs %in% c(inputPA,inputNA),"std.all"]
  c(
    c(min = min(sfest),
      max = max(sfest),
            sf$fit[c("chisq",
                     "rmsea",
                     "cfi",
                     "tli")] # round to 2 decimal places
          )
  )
}

# Within- RMSEA
RMSEAw = function(f1aw) {
  N <- Reduce("+", fitmeasures(f1aw)["ntotal"]) # extracts the number of measurement occasions. In multigroup models it adds them up
  K <- lavInspect(f1aw, what = "ngroups")       # extracts number of groups
  ChisqW <- fitmeasures(f1aw)["chisq"]          # extract chisquare from model
  dfW <- fitmeasures(f1aw)["df"]                # extract dfs from model
  sqrt(K) * sqrt((ChisqW - dfW) / (dfW * N)) #formula based on Ryu & West 2009 + adjustment for multigroup case
}

# Between- RMSEA
RMSEAb = function(f1ab) {
  N <- Reduce("+", lavInspect(f1ab, what = "nclusters"))# extracts the number of persons. In multigroup models it adds them up
  K <- lavInspect(f1ab, what = "ngroups")               # extracts number of groups
  Chisqb <- fitmeasures(f1ab)["chisq"]                  # extract chisquare from model
  dfb <- fitmeasures(f1ab)["df"]                        # extract dfs from model
  sqrt(K) * sqrt((Chisqb - dfb) / (dfb * N)) #formula based on Ryu & West 2009 + adjustment for multigroup case
}

# We followed procedures of Eisele et al. (2021) to conduct 
# Multilevel Confirmatory Factor analyses to check for factor structure 
# at the within-level and between-level for NA and PA

# Naming of variables, where X is the dataset number 
# (1 = GVE, 2 = Leuven2011, 3 = Leuven3W, 4 = Tilburg, 5 = Ghent)
# mXaw:  2-factor structure only within person, not between person
# mXab:  2-factor structure only between person, not within
# mXad:  2-factor structure within AND between persons
# mXbw:  1-factor structure
# mXcw:  2-factor structure (within) but forcing 2 factors not associated (i.e., =0) 
# baseXw: baseline model

# Different models were run and compared to see which fits ebst theoretically. 
# Note that we only report the within-person (mxaw) and the between-person (mxab) in the supplement.

##### 1.1 Factor structure short questionnaire/ Establish reference model #####
#### 1.1a Within level (between level saturated) ####
#specify the two-factor structure at the within level

# ======= Dataset 1: GVE==============

m1aw <- '
level: 1
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ PA_1 + PA_2 + PA_3 + PA_4
na =~ NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS

level: 2
PA_1 ~~ PA_2 + PA_3 + PA_4 + NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
PA_2 ~~ PA_3 + PA_4 + NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
PA_3 ~~ PA_4 + NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
PA_4 ~~ NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
NA_1 ~~ NA_2 + NA_3 + NA_4 + LONELINESS
NA_2 ~~ NA_3 + NA_4 + LONELINESS
NA_3 ~~ NA_4 + LONELINESS
NA_4 ~~ LONELINESS

'

m1ab <- '
level: 1
PA_1 ~~ PA_2 + PA_3 + PA_4 + NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
PA_2 ~~ PA_3 + PA_4 + NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
PA_3 ~~ PA_4 + NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
PA_4 ~~ NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
NA_1 ~~ NA_2 + NA_3 + NA_4 + LONELINESS
NA_2 ~~ NA_3 + NA_4 + LONELINESS
NA_3 ~~ NA_4 + LONELINESS
NA_4 ~~ LONELINESS

level: 2
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ PA_1 + PA_2 + PA_3 + PA_4
na =~ NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS

'


#to calculate level specific CFI, fit a model that is saturated between and worst fitting solution within

base1w <- '
level: 1
PA_1 ~~ PA_1
PA_2 ~~ PA_2
PA_3 ~~ PA_3
PA_4 ~~ PA_4
NA_1 ~~ NA_1
NA_2 ~~ NA_2
NA_3 ~~ NA_3
NA_4 ~~ NA_4
LONELINESS ~~ LONELINESS

level: 2
PA_1 ~~ PA_2 + PA_3 + PA_4 + NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
PA_2 ~~ PA_3 + PA_4 + NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
PA_3 ~~ PA_4 + NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
PA_4 ~~ NA_1 + NA_2 + NA_3 + NA_4 + LONELINESS
NA_1 ~~ NA_2 + NA_3 + NA_4 + LONELINESS
NA_2 ~~ NA_3 + NA_4 + LONELINESS
NA_3 ~~ NA_4 + LONELINESS
NA_4 ~~ LONELINESS
'


#fit the above models
#to calculate the level specific CFI, a different baseline model needs to be specified in the formula

f1aw <- lavaan::sem(
  model = m1aw,
  data = dataGVE,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base1w
)

f1ab <- lavaan::sem(
  model = m1ab,
  data = dataGVE,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base1w
)

summary(f1aw, standardized = TRUE, fit.measures = TRUE)
modificationindices(f1aw, sort = TRUE)
inspect(f1aw,what="std")
inspect(f1aw)$lambda

summary(f1ab, standardized = TRUE, fit.measures = TRUE)
inspect(f1ab,what="std")

# ============================
# ======= DATASET 2: Leuven2011
# ============================


m2aw <- '
level: 1
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ RLX_ES + HAP_ES
na =~ ANG_ES + ANX_ES + DEP_ES + SAD_ES

level: 2
RLX_ES ~~ HAP_ES + ANG_ES + ANX_ES + DEP_ES + SAD_ES
HAP_ES ~~ ANG_ES + ANX_ES + DEP_ES + SAD_ES
ANG_ES ~~ ANX_ES + DEP_ES + SAD_ES
ANX_ES ~~ DEP_ES + SAD_ES
DEP_ES ~~ SAD_ES

'

m2ab <- '
level: 1
RLX_ES ~~ HAP_ES + ANG_ES + ANX_ES + DEP_ES + SAD_ES
HAP_ES ~~ ANG_ES + ANX_ES + DEP_ES + SAD_ES
ANG_ES ~~ ANX_ES + DEP_ES + SAD_ES
ANX_ES ~~ DEP_ES + SAD_ES
DEP_ES ~~ SAD_ES

level: 2
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ RLX_ES + HAP_ES
na =~ ANG_ES + ANX_ES + DEP_ES + SAD_ES

'


#to calculate level specific CFI, fit a model that is saturated between and worst fitting solution within

base2w <- '
level: 1
RLX_ES ~~ RLX_ES
HAP_ES ~~ HAP_ES
ANG_ES ~~ ANG_ES
ANX_ES ~~ ANX_ES
DEP_ES ~~ DEP_ES
SAD_ES ~~ SAD_ES

level: 2
RLX_ES ~~ HAP_ES + ANG_ES + ANX_ES + DEP_ES + SAD_ES
HAP_ES ~~ ANG_ES + ANX_ES + DEP_ES + SAD_ES
ANG_ES ~~ ANX_ES + DEP_ES + SAD_ES
ANX_ES ~~ DEP_ES + SAD_ES
DEP_ES ~~ SAD_ES
'


#fit the above models
#to calculate the level specific CFI, a different baseline model needs to be specified in the formula

f2aw <- lavaan::sem(
  model = m2aw,
  data = dataLeuven2011,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base2w
)
f2ab <- lavaan::sem(
  model = m2ab,
  data = dataLeuven2011,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base2w
)


summary(f2aw, standardized = TRUE, fit.measures = TRUE)
modificationindices(f2aw, sort = TRUE)
inspect(f2aw,what="std")
inspect(f2aw)$lambda

summary(f2ab, standardized = TRUE, fit.measures = TRUE)
inspect(f2ab,what="std")

# ============================
# DATASET 3: Leuven 3-WAVE
# ============================


m3aw <- '
level: 1
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ RLX_ES + HAP_ES + CHEER_ES
na =~ ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES


level: 2
RLX_ES ~~ HAP_ES + CHEER_ES + ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
HAP_ES ~~ CHEER_ES + ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
CHEER_ES ~~ ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
ANG_ES ~~ DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
DEP_ES ~~ LONE_ES + FEAR_ES + SAD_ES + STR_ES
LONE_ES ~~ FEAR_ES + SAD_ES + STR_ES
FEAR_ES ~~ SAD_ES + STR_ES
SAD_ES ~~ STR_ES

'

m3ab <- '
level: 1
RLX_ES ~~ HAP_ES + CHEER_ES + ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
HAP_ES ~~ CHEER_ES + ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
CHEER_ES ~~ ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
ANG_ES ~~ DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
DEP_ES ~~ LONE_ES + FEAR_ES + SAD_ES + STR_ES
LONE_ES ~~ FEAR_ES + SAD_ES + STR_ES
FEAR_ES ~~ SAD_ES + STR_ES
SAD_ES ~~ STR_ES

level: 2
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ RLX_ES + HAP_ES + CHEER_ES
na =~ ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES

'

#to calculate level specific CFI, fit a model that is saturated between and worst fitting solution within

base3w <- '
level: 1
RLX_ES ~~ RLX_ES
HAP_ES ~~ HAP_ES
CHEER_ES ~~ CHEER_ES
ANG_ES ~~ ANG_ES
DEP_ES ~~ DEP_ES
LONE_ES ~~ LONE_ES
FEAR_ES ~~ FEAR_ES
SAD_ES ~~ SAD_ES
STR_ES ~~ STR_ES

level: 2
RLX_ES ~~ HAP_ES + CHEER_ES + ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
HAP_ES ~~ CHEER_ES + ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
CHEER_ES ~~ ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
ANG_ES ~~ DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
DEP_ES ~~ LONE_ES + FEAR_ES + SAD_ES + STR_ES
LONE_ES ~~ FEAR_ES + SAD_ES + STR_ES
FEAR_ES ~~ SAD_ES + STR_ES
SAD_ES ~~ STR_ES
'


#fit the above models
#to calculate the level specific CFI, a different baseline model needs to be specified in the formula

f3aw <- lavaan::sem(
  model = m3aw,
  data = dataLeuven3W,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base3w
)
f3ab <- lavaan::sem(
  model = m3ab,
  data = dataLeuven3W,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base3w
)

summary(f3aw, standardized = TRUE, fit.measures = TRUE)
modificationindices(f3aw, sort = TRUE)
inspect(f3aw,what="std")
inspect(f3aw)$lambda

summary(f3ab, standardized = TRUE, fit.measures = TRUE)
inspect(f3ab,what="std")

# Low TLI for the within-person level. Looking at modification indices, should include correlation between stressed and relaxed.
# Makes sense, since these are very overlapping items


m3aw_mod <- '
level: 1
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ RLX_ES + HAP_ES + CHEER_ES
na =~ ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
 
RLX_ES ~~ STR_ES

level: 2
RLX_ES ~~ HAP_ES + CHEER_ES + ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
HAP_ES ~~ CHEER_ES + ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
CHEER_ES ~~ ANG_ES + DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
ANG_ES ~~ DEP_ES + LONE_ES + FEAR_ES + SAD_ES + STR_ES
DEP_ES ~~ LONE_ES + FEAR_ES + SAD_ES + STR_ES
LONE_ES ~~ FEAR_ES + SAD_ES + STR_ES
FEAR_ES ~~ SAD_ES + STR_ES
SAD_ES ~~ STR_ES

'

f3aw_mod <- lavaan::sem(
  model = m3aw_mod,
  data = dataLeuven3W,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base3w
)

summary(f3aw_mod, standardized = TRUE, fit.measures = TRUE)

# ============================
# DATASET 4: Tilburg
# ============================


m4aw <- '
level: 1
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ PA_ener + PA_cont + PA_enth + PA_deter + PA_calm + PA_joy + PA_grat
na =~ NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low

level: 2
PA_ener ~~ PA_cont + PA_enth + PA_deter + PA_calm + PA_joy + PA_grat +NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_cont ~~ PA_enth + PA_deter + PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_enth ~~ PA_deter + PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_deter ~~ PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_calm ~~ PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_joy ~~ PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_grat ~~ NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
NA_irri ~~ NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
NA_bor ~~ NA_nerv + NA_sad + NA_ang + NA_low
NA_nerv ~~ NA_sad + NA_ang + NA_low
NA_sad ~~ NA_ang + NA_low
NA_ang ~~ NA_low

'

m4ab <- '
level: 1
PA_ener ~~ PA_cont + PA_enth + PA_deter + PA_calm + PA_joy + PA_grat +NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_cont ~~ PA_enth + PA_deter + PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_enth ~~ PA_deter + PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_deter ~~ PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_calm ~~ PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_joy ~~ PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_grat ~~ NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
NA_irri ~~ NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
NA_bor ~~ NA_nerv + NA_sad + NA_ang + NA_low
NA_nerv ~~ NA_sad + NA_ang + NA_low
NA_sad ~~ NA_ang + NA_low
NA_ang ~~ NA_low


level: 2
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ PA_ener + PA_cont + PA_enth + PA_deter + PA_calm + PA_joy + PA_grat
na =~ NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low


'

#to calculate level specific CFI, fit a model that is saturated between and worst fitting solution within

base4w <- '
level: 1
PA_ener ~~ PA_ener
PA_cont ~~ PA_cont
PA_enth ~~ PA_enth
PA_deter ~~ PA_deter
PA_calm ~~ PA_calm
PA_joy ~~ PA_joy
PA_grat ~~ PA_grat
NA_irri ~~ NA_irri
NA_bor ~~ NA_bor
NA_nerv ~~ NA_nerv
NA_sad ~~ NA_sad
NA_ang ~~ NA_ang
NA_low ~~ NA_low

level: 2
PA_ener ~~ PA_cont + PA_enth + PA_deter + PA_calm + PA_joy + PA_grat +NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_cont ~~ PA_enth + PA_deter + PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_enth ~~ PA_deter + PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_deter ~~ PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_calm ~~ PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_joy ~~ PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_grat ~~ NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
NA_irri ~~ NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
NA_bor ~~ NA_nerv + NA_sad + NA_ang + NA_low
NA_nerv ~~ NA_sad + NA_ang + NA_low
NA_sad ~~ NA_ang + NA_low
NA_ang ~~ NA_low

'


#fit the above models
#to calculate the level specific CFI, a different baseline model needs to be specified in the formula

f4aw <- lavaan::sem(
  model = m4aw,
  data = dataTilburg,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base4w
)
f4ab <- lavaan::sem(
  model = m4ab,
  data = dataTilburg,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base4w
)


summary(f4aw, standardized = TRUE, fit.measures = TRUE)
modificationindices(f4aw, sort = TRUE)
inspect(f4aw,what="std")
inspect(f4aw)$lambda

summary(f4ab, standardized = TRUE, fit.measures = TRUE)
inspect(f4ab,what="std")

# Low TLI for the within-person level. Looking at modification indices, should include correlation between angry and irritated
# Makes sense, since these are very overlapping items

m4aw_mod <- '
level: 1
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ PA_ener + PA_cont + PA_enth + PA_deter + PA_calm + PA_joy + PA_grat
na =~ NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low

NA_irri ~~ NA_ang
NA_sad ~~   NA_low

level: 2
PA_ener ~~ PA_cont + PA_enth + PA_deter + PA_calm + PA_joy + PA_grat +NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_cont ~~ PA_enth + PA_deter + PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_enth ~~ PA_deter + PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_deter ~~ PA_calm + PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_calm ~~ PA_joy + PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_joy ~~ PA_grat + NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
PA_grat ~~ NA_irri + NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
NA_irri ~~ NA_bor + NA_nerv + NA_sad + NA_ang + NA_low
NA_bor ~~ NA_nerv + NA_sad + NA_ang + NA_low
NA_nerv ~~ NA_sad + NA_ang + NA_low
NA_sad ~~ NA_ang + NA_low
NA_ang ~~ NA_low

'

f4aw_mod <- lavaan::sem(
  model = m4aw_mod,
  data = dataTilburg,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base4w
)

summary(f4aw_mod, standardized = TRUE, fit.measures = TRUE)
modificationindices(f4aw_mod, sort = TRUE)


# ============================
# DATASET 5: Ghent
# ============================


m5aw <- '
level: 1
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ Emow_happy + Emow_relaxed + Emow_energetic
na =~ Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain

level: 2
Emow_happy ~~ Emow_relaxed + Emow_energetic + Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_relaxed ~~ Emow_energetic + Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_energetic ~~ Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_angry ~~ Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_annoyed ~~ Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_anxious ~~ Emow_sad + Emow_stressed + Emow_uncertain
Emow_sad ~~ Emow_stressed + Emow_uncertain
Emow_stressed ~~ Emow_uncertain
'

m5ab <- '
level: 1
Emow_happy ~~ Emow_relaxed + Emow_energetic + Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_relaxed ~~ Emow_energetic + Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_energetic ~~ Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_angry ~~ Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_annoyed ~~ Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_anxious ~~ Emow_sad + Emow_stressed + Emow_uncertain
Emow_sad ~~ Emow_stressed + Emow_uncertain
Emow_stressed ~~ Emow_uncertain

level: 2
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ Emow_happy + Emow_relaxed + Emow_energetic
na =~ Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain

'



#to calculate level specific CFI, fit a model that is saturated between and worst fitting solution within

base5w <- '
level: 1
Emow_happy ~~ Emow_happy
Emow_relaxed ~~ Emow_relaxed
Emow_energetic ~~ Emow_energetic
Emow_angry ~~ Emow_angry
Emow_annoyed ~~ Emow_annoyed
Emow_anxious ~~ Emow_anxious
Emow_sad ~~ Emow_sad
Emow_stressed ~~ Emow_stressed
Emow_uncertain ~~ Emow_uncertain

level: 2
Emow_happy ~~ Emow_relaxed + Emow_energetic + Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_relaxed ~~ Emow_energetic + Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_energetic ~~ Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_angry ~~ Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_annoyed ~~ Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_anxious ~~ Emow_sad + Emow_stressed + Emow_uncertain
Emow_sad ~~ Emow_stressed + Emow_uncertain
Emow_stressed ~~ Emow_uncertain
'


#fit the above models
#to calculate the level specific CFI, a different baseline model needs to be specified in the formula

f5aw <- lavaan::sem(
  model = m5aw,
  data = dataGhent,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base5w
)
f5ab <- lavaan::sem(
  model = m5ab,
  data = dataGhent,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base5w
)

summary(f5aw, standardized = TRUE, fit.measures = TRUE)
modificationindices(f5aw, sort = TRUE)
inspect(f5aw,what="std")
inspect(f5aw)$lambda
summary(f5ab, standardized = TRUE, fit.measures = TRUE)
inspect(f5ab,what="std")

# Low TLI for the within-person level. Looking at modification indices, should include correlation between sad and angry
# Those are not super overlapping items, but they had by far the highest modification index.
# Also, it is an adolescent sample and irritability and depressive feelings are very often overlapping in that age group

m5aw_mod <- '
level: 1
pa ~~ pa
na ~~ na
pa ~~ na

pa =~ Emow_happy + Emow_relaxed + Emow_energetic
na =~ Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain

Emow_angry ~~ Emow_sad

level: 2
Emow_happy ~~ Emow_relaxed + Emow_energetic + Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_relaxed ~~ Emow_energetic + Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_energetic ~~ Emow_angry + Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_angry ~~ Emow_annoyed + Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_annoyed ~~ Emow_anxious + Emow_sad + Emow_stressed + Emow_uncertain
Emow_anxious ~~ Emow_sad + Emow_stressed + Emow_uncertain
Emow_sad ~~ Emow_stressed + Emow_uncertain
Emow_stressed ~~ Emow_uncertain
'

f5aw_mod <- lavaan::sem(
  model = m5aw_mod,
  data = dataGhent,
  cluster = "PARTICIPANT_ID",
  effect.coding = c("loadings", "intercepts"),
  baseline = base5w
)

summary(f5aw_mod, standardized = TRUE, fit.measures = TRUE)
modificationindices(f5aw_mod, sort = TRUE)

# ====================================
# summarize all the fitting
# ====================================
rescfa <- rbind(c(model = deparse(substitute(f1aw)), summaryfit(f1aw, inputPA.GVE,inputNA.GVE)),
      c(model = deparse(substitute(f2aw)), summaryfit(f2aw, inputPA.Leuven2011,inputNA.Leuven2011)),
      c(model = deparse(substitute(f3aw_mod)), summaryfit(f3aw_mod, inputPA.Leuven3W,inputNA.Leuven3W)),
      c(model = deparse(substitute(f4aw_mod)), summaryfit(f4aw_mod, inputPA.Tilburg,inputNA.Tilburg)),
      c(model = deparse(substitute(f5aw_mod)), summaryfit(f5aw_mod, inputPA.Ghent,inputNA.Ghent)),
      c(model = deparse(substitute(f1ab)), summaryfit(f1ab, inputPA.GVE,inputNA.GVE)),
      c(model = deparse(substitute(f2ab)), summaryfit(f2ab, inputPA.Leuven2011,inputNA.Leuven2011)),
      c(model = deparse(substitute(f3ab)), summaryfit(f3ab, inputPA.Leuven3W,inputNA.Leuven3W)),
      c(model = deparse(substitute(f4ab)), summaryfit(f4ab, inputPA.Tilburg,inputNA.Tilburg)),
      c(model = deparse(substitute(f5ab)), summaryfit(f5ab, inputPA.Ghent,inputNA.Ghent)))
colnames(rescfa) <- c("model","min","chisq","rmsea","cfi","tli")
write.csv(rescfa,"manuscript/results/SMTable3.csv", row.names = FALSE)

