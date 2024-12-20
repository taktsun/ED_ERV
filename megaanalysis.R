# ======================================================================
# Title: Multi-level models, mega-analysis (Main analyses of paper)
# Date: 09-02-2024
# Updated on 20-12-2024 to include additional analyses
# 
# Copyright: Edmund Lo, checked by Dominique Maciejewski on 24-01-2024
#
# Note! Run this either (1) line by line (windows: Ctrl+Enter), or
#                       (2) as source with echo (windows: Ctrl+Shift+Enter)
#       so that you will receive prompts on data availability.
# ======================================================================

# Library
library(nlme)
library(devtools)
library(reshape2)
library(bootmlm)
library(lme4)
library(esmpack)

source("list_of_variables.R") # load all ESM measures variable names

# check and read read ready-to-analyze datasets
source("checkdata_analysis.R")

# a wrapper to extract fixed effect of an MLM (nlme)
preparemmresult <- function (m){
  cbind(model = deparse(substitute(m)),
        FEest = summary(m)$tTable[,1],
        SE = summary(m)$tTable[,2],
        DF = summary(m)$tTable[,3],
        pvalue = summary(m)$tTable[,5],
        residual = m$sigma^2,
        phi = coef(m$modelStruct$corStruct, unconstrained = FALSE), # needs coef to make this work 
        nobs = nobs(m),
        N = m$dims$ngrps[1],
        u95CI = summary(m)$tTable[,1]+summary(m)$tTable[,2]*1.96,
        l95CI = summary(m)$tTable[,1]-summary(m)$tTable[,2]*1.96
  )
  
}

if (run.ready){
# ===========================================================================
# A few features of these multilevel models:
# 1. we separate within- and between-person components (cw = component within-person, cb = component between-person)
# 2. we included the autocorrelation of the residuals (correlation = corAR1())
# 3. we conducted a 3-level modeling, nesting momentary indices (level 1) within PARTICIPANT_ID (level 2) within study (level 3)
# 4. we used the quasi-Newton optimizer (opt = "optim")
# 5. we controlled for time-trends, age, gender, and mean levels of affect and emotion regulation use
# ===========================================================================
# 
# ===========================================================================
# Mega analysis - registered confirmatory hypothesis
# NEGATIVE emotion differentiation and emotion regulation variability
# ===========================================================================

# Model 1A: Negative emotion differentiation predicting Emotion regulation variability (Full index)
mm1A_NA <- lme(fixed = BrayCurtisFull.amm ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + timecw  +
                m_EDcb + m_NAcb + m_ERcb + AGE + FEMALE,
              data=df,
              random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
              control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1B: Negative emotion  differentiation predicting Emotion regulation variability (Strategy switching component)
mm1B_NA <- lme(fixed = BrayCurtisRepl.amm ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisNest.ammcw + timecw  +
                     m_EDcb + m_NAcb + m_ERcb +  BrayCurtisNest.ammcb + AGE + FEMALE,
                   data=df,
                   random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw +BrayCurtisNest.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
                   control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1C: Negative emotion  differentiation predicting Emotion regulation variability (Endorsement change component)
mm1C_NA <- lme(fixed = BrayCurtisNest.amm ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisRepl.ammcw + timecw  +
                     m_EDcb + m_NAcb + m_ERcb +BrayCurtisRepl.ammcb+ AGE + FEMALE,
                   data=df,
                   random=~1+m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisRepl.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
                   control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 2A: Emotion regulation variability (Full index) predicting Negative emotion  differentiation
mm2A_NA <- lme(fixed = m_ED ~ BrayCurtisFull.ammcw + m_EDcwL1D +  m_NAcw + m_ERcw + timecw+
                 BrayCurtisFull.ammcb + m_NAcb+m_ERcb + AGE + FEMALE,
               data=df,
               random=~1+m_EDcwL1D+BrayCurtisFull.ammcw + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
               control =lmeControl(opt='optim'),na.action = na.omit)

# Model 2B: Emotion regulation variability (Switching and endorsement change components separately) predicting Negative emotion differentiation
mm2B_NA <- lme(fixed = m_ED ~ BrayCurtisNest.ammcw + BrayCurtisRepl.ammcw +m_EDcwL1D +  m_NAcw+ m_ERcw+ timecw+
                     BrayCurtisNest.ammcb + BrayCurtisRepl.ammcb + m_NAcb+m_ERcb + AGE + FEMALE,
                   data=df,
                   random=~1+m_EDcwL1D+BrayCurtisNest.ammcw +BrayCurtisRepl.ammcw + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                   control =lmeControl(opt='optim'),na.action = na.omit)

# ===========================================================================
# Mega analysis - pre-registered exploratory analyses
# POSITIVE emotion differentiation and emotion regulation variability
# ===========================================================================

# Model 1A: Positive emotion differentiation predicting Emotion regulation variability (Full index)
mm1A_PA <- lme(fixed = BrayCurtisFull.amm ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + timecw  +
                 m_EDPAcb + m_PAcb + m_ERcb + AGE + FEMALE,
               data=df,
               random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
               control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1B: Positive emotion differentiation predicting Emotion regulation variability (Strategy switching component)
mm1B_PA <- lme(fixed = BrayCurtisRepl.amm ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + BrayCurtisNest.ammcw + timecw  +
                 m_EDPAcb + m_PAcb + m_ERcb + BrayCurtisNest.ammcb + AGE + FEMALE,
               data=df,
               random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw + BrayCurtisNest.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
               control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1C: Positive emotion  differentiation predicting Emotion regulation variability (Endorsement change component)
mm1C_PA <- lme(fixed = BrayCurtisNest.amm ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + BrayCurtisRepl.ammcw + timecw  +
                 m_EDPAcb + m_PAcb + m_ERcb + BrayCurtisRepl.ammcb + AGE + FEMALE,
               data=df,
               random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw + BrayCurtisRepl.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
               control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 2A: Emotion regulation variability (Full index) predicting Positive emotion differentiation
mm2A_PA <- lme(fixed = m_EDPA ~ BrayCurtisFull.ammcw +m_EDPAcwL1D +  m_PAcw+ m_ERcw+ timecw+
                 BrayCurtisFull.ammcb + m_PAcb+m_ERcb + AGE + FEMALE,
               data=df,
               random=~1+m_EDPAcwL1D+BrayCurtisFull.ammcw + m_PAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
               control =lmeControl(opt='optim'),na.action = na.omit)

# Model 2B: Emotion regulation variability (Switching and endorsement change components separately) predicting Positive emotion differentiation
mm2B_PA <- lme(fixed = m_EDPA ~ BrayCurtisRepl.ammcw + BrayCurtisNest.ammcw +m_EDPAcwL1D +  m_PAcw+ m_ERcw+ timecw+
                 BrayCurtisRepl.ammcb + BrayCurtisNest.ammcb + m_PAcb+m_ERcb + AGE + FEMALE,
               data=df,
               random=~1+m_EDPAcwL1D+BrayCurtisRepl.ammcw + BrayCurtisNest.ammcw + m_PAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
               control =lmeControl(opt='optim'),na.action = na.omit)


outputSMTable5 <- rbind(
  preparemmresult(mm1A_NA),
  preparemmresult(mm1B_NA),
  preparemmresult(mm1C_NA),
  preparemmresult(mm2A_NA),
  preparemmresult(mm2B_NA),
  preparemmresult(mm1A_PA),
  preparemmresult(mm1B_PA),
  preparemmresult(mm1C_PA),
  preparemmresult(mm2A_PA),
  preparemmresult(mm2B_PA)
)
write.csv(outputSMTable5, "manuscript/results/SMTable5.csv")

# ===========================================================================
# mega analysis - SENSITIVITY analysis (supplemental materials 6)
# emotion regulation variability calculated in a successive temporal comparison approach
#
# object names: mms... refers to multilevel model for SENSITIVITY analysis
# ===========================================================================

### Negative emotion differentiation
# Model 1A: Negative emotion differentiation predicting Emotion regulation variability (Full index, Successive difference temporal approach)
mms1A_NA <- lme(fixed = BrayCurtisFull.suc ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + timecw  +
                m_EDcb + m_NAcb + m_ERcb + AGE + FEMALE,
              data=df,
              random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
              control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1B: Negative emotion  differentiation predicting Emotion regulation variability (Strategy switching component, Successive difference temporal approach )
mms1B_NA <- lme(fixed = BrayCurtisRepl.suc ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisNest.succw + timecw  +
                     m_EDcb + m_NAcb + m_ERcb +  BrayCurtisNest.succb + AGE + FEMALE,
                   data=df,
                   random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw +BrayCurtisNest.succw | study/PARTICIPANT_ID, correlation = corAR1(),
                   control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1C: Negative emotion  differentiation predicting Emotion regulation variability (Endorsement change component, Successive difference temporal approach)
mms1C_NA <- lme(fixed = BrayCurtisNest.suc ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisRepl.succw + timecw  +
                     m_EDcb + m_NAcb + m_ERcb +BrayCurtisRepl.succb+ AGE + FEMALE,
                   data=df,
                   random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw + BrayCurtisRepl.succw | study/PARTICIPANT_ID, correlation = corAR1(),
                   control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 2A: Emotion regulation variability (Full index, Successive difference temporal approach) predicting negative emotion differentiation
mms2A_NA <- lme(fixed = m_ED ~ BrayCurtisFull.succw +m_EDcwL1D +  m_NAcw+ m_ERcw+ timecw+
                  BrayCurtisFull.succb + m_NAcb+m_ERcb + AGE + FEMALE,
                data=df,
                random=~1+m_EDcwL1D+BrayCurtisFull.succw + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                control =lmeControl(opt='optim'),na.action = na.omit)

# Model 2B: Emotion regulation variability (Switching and endorsement change components separately, Successive difference temporal approach) predicting negative emotion differentiation
mms2B_NA <- lme(fixed = m_ED ~ BrayCurtisNest.succw+ BrayCurtisRepl.succw +m_EDcwL1D +  m_NAcw+ m_ERcw+ timecw+
                  BrayCurtisNest.succb+ BrayCurtisRepl.succb  + m_NAcb+m_ERcb + AGE + FEMALE,
                data=df,
                random=~1+m_EDcwL1D+BrayCurtisNest.succw+ BrayCurtisRepl.succw + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                control =lmeControl(opt='optim'),na.action = na.omit)

### Positive emotion differentiation
# Model 1A: Positive emotion differentiation predicting Emotion regulation variability (Full index, Successive difference temporal approach)
mms1A_PA <- lme(fixed = BrayCurtisFull.suc ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + timecw  +
                      m_EDPAcb + m_PAcb + m_ERcb + AGE + FEMALE,
                    data=df,
                    random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1B: Positive emotion  differentiation predicting Emotion regulation variability (Strategy switching component, Successive difference temporal approach)
mms1B_PA <- lme(fixed = BrayCurtisRepl.suc ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + BrayCurtisNest.succw + timecw  +
                      m_EDPAcb + m_PAcb + m_ERcb + BrayCurtisNest.succb + AGE + FEMALE,
                    data=df,
                    random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw + BrayCurtisNest.succw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1C: Positive emotion  differentiation predicting Emotion regulation variability (Endorsement change component, Successive difference temporal approach)
mms1C_PA <- lme(fixed = BrayCurtisNest.suc ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + BrayCurtisRepl.succw + timecw  +
                  m_EDPAcb + m_PAcb + m_ERcb + BrayCurtisRepl.succb + AGE + FEMALE,
                data=df,
                random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw + BrayCurtisRepl.succw| study/PARTICIPANT_ID, correlation = corAR1(),
                control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 2A: Emotion regulation variability (Full index, Successive difference temporal approach) predicting positive emotion differentiation
mms2A_PA <- lme(fixed = m_EDPA ~ BrayCurtisFull.succw +m_EDPAcwL1D +  m_PAcw+ m_ERcw+ timecw+
                      BrayCurtisFull.succb + m_PAcb+m_ERcb + AGE + FEMALE,
                    data=df,
                    random=~1+m_EDPAcwL1D+BrayCurtisFull.ammcw + m_PAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =lmeControl(opt='optim'),na.action = na.omit)

# Model 2B: Emotion regulation variability (Switching and endorsement change components separately, Successive difference temporal approach) predicting positive emotion differentiation
mms2B_PA <- lme(fixed = m_EDPA ~ BrayCurtisRepl.succw + BrayCurtisNest.succw + m_EDPAcwL1D +  m_PAcw+ m_ERcw+ timecw+
                      BrayCurtisRepl.succb + BrayCurtisNest.succb + m_PAcb+m_ERcb + AGE + FEMALE,
                    data=df,
                    random=~1+m_EDPAcwL1D+BrayCurtisFull.ammcw + m_PAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =lmeControl(opt='optim'),na.action = na.omit)

outputSMTable6 <- rbind(
  preparemmresult(mms1A_NA),
  preparemmresult(mms1B_NA),
  preparemmresult(mms1C_NA),
  preparemmresult(mms2A_NA),
  preparemmresult(mms2B_NA),
  preparemmresult(mms1A_PA),
  preparemmresult(mms1B_PA),
  preparemmresult(mms1C_PA),
  preparemmresult(mms2A_PA),
  preparemmresult(mms2B_PA)
)
write.csv(outputSMTable6, "manuscript/results/SMTable6.csv")
# Extra data processing for extra analyses below

# Dataset-centered age
agetable <- data.frame("study" = c("GVE","Ghent","Leuven2011","Leuven3W","Tilburg"),
                       "AGEdataset" = c(mean(dfPerson[dfPerson$study=="GVE","AGE"]),
                                        mean(dfPerson[dfPerson$study=="Ghent","AGE"]),
                                        mean(dfPerson[dfPerson$study=="Leuven2011","AGE"], na.rm=TRUE),
                                        mean(dfPerson[dfPerson$study=="Leuven3W","AGE"]),
                                        mean(dfPerson[dfPerson$study=="Tilburg","AGE"], na.rm = TRUE))
)
df <- merge(agetable, df, by = 'study')
df$AGEcd <- df$AGE-df$AGEdataset

# Moment-level binary variable that indicates
df$b0NA <- ifelse(df$m_NA==0,1,0)
df$b0NAL1D <- lagvar(b0NA, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
df$b0PA <- ifelse(df$m_PA==0,1,0)
df$b0PAL1D <- lagvar(b0PA, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
df$b0ER <- ifelse(df$m_ER==0,1,0)
df$b0ERL1D <- lagvar(b0ER, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)



# ===========================================================================
# Dataset-specific fixed effects (supplemental materials 8: table S8.1)
# ===========================================================================
# a wrapper to extract random slopes
preparemmdatasetrandom <- function (m,column){
  cbind(model = deparse(substitute(m)),
        study = rownames(m$coefficients$random$study),
        coname = column,
        RanEf = m$coefficients$random$study[,column],
        dstFE = m$coefficients$random$study[,column]+m$coefficients$fixed[column]
  )
}

outputSMTable8 <- rbind(
  preparemmdatasetrandom(mm1A_NA,"m_EDcwL1D"),
  preparemmdatasetrandom(mm1B_NA,"m_EDcwL1D"),
  preparemmdatasetrandom(mm1C_NA,"m_EDcwL1D"),
  preparemmdatasetrandom(mm2A_NA,"BrayCurtisFull.ammcw"),
  preparemmdatasetrandom(mm2B_NA,"BrayCurtisNest.ammcw"),
  preparemmdatasetrandom(mm2B_NA,"BrayCurtisRepl.ammcw"),
  preparemmdatasetrandom(mm1A_PA,"m_EDPAcwL1D"),
  preparemmdatasetrandom(mm1B_PA,"m_EDPAcwL1D"),
  preparemmdatasetrandom(mm1C_PA,"m_EDPAcwL1D"),
  preparemmdatasetrandom(mm2A_PA,"BrayCurtisFull.ammcw"),
  preparemmdatasetrandom(mm2B_PA,"BrayCurtisNest.ammcw"),
  preparemmdatasetrandom(mm2B_PA,"BrayCurtisRepl.ammcw"),
  preparemmdatasetrandom(mediation1M_NA,"dm:m_EDcwL1D"),
  preparemmdatasetrandom(mediation1M_NA,"dy:BrayCurtisFull.ammcw"),
  preparemmdatasetrandom(mediation1M_NA,"m_EDcwL1D:dy"),
  preparemmdatasetrandom(mediation1M_PA,"dm:m_EDPAcwL1D"),
  preparemmdatasetrandom(mediation1M_PA,"dy:BrayCurtisFull.ammcw"),
  preparemmdatasetrandom(mediation1M_PA,"m_EDPAcwL1D:dy")
)
write.csv(outputSMTable8, "manuscript/results/SMTable81.csv")


# ===========================================================================
# Sensitivity analysis: Age as moderator (supplemental materials 8: table S8.2)
# ===========================================================================


# Model 1A: Negative emotion differentiation predicting Emotion regulation variability (Full index)
mm1A_NAagecd <- lme(fixed = BrayCurtisFull.amm ~ m_EDcwL1D*AGEcd + m_NAcwL1D + m_ERcw + timecw  +
                      m_EDcb + m_NAcb + m_ERcb + FEMALE,
                    data=df,
                    random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1B: Negative emotion  differentiation predicting Emotion regulation variability (Strategy switching component)
mm1B_NAagecd <- lme(fixed = BrayCurtisRepl.amm ~ m_EDcwL1D*AGEcd + m_NAcwL1D + m_ERcw + BrayCurtisNest.ammcw + timecw  +
                      m_EDcb + m_NAcb + m_ERcb +  BrayCurtisNest.ammcb +  FEMALE,
                    data=df,
                    random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw +BrayCurtisNest.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1C: Negative emotion  differentiation predicting Emotion regulation variability (Endorsement change component)
mm1C_NAagecd <- lme(fixed = BrayCurtisNest.amm ~ m_EDcwL1D*AGEcd + m_NAcwL1D + m_ERcw + BrayCurtisRepl.ammcw + timecw  +
                      m_EDcb + m_NAcb + m_ERcb +BrayCurtisRepl.ammcb+  FEMALE,
                    data=df,
                    random=~1+m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisRepl.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 2A: Emotion regulation variability (Full index) predicting Negative emotion  differentiation
mm2A_NAagecd <- lme(fixed = m_ED ~ BrayCurtisFull.ammcw*AGEcd + m_EDcwL1D +  m_NAcw + m_ERcw + timecw+
                      BrayCurtisFull.ammcb + m_NAcb+m_ERcb +  FEMALE,
                    data=df,
                    random=~1+m_EDcwL1D+BrayCurtisFull.ammcw + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =lmeControl(opt='optim'),na.action = na.omit)

# Model 2B: Emotion regulation variability (Switching and endorsement change components separately) predicting Negative emotion differentiation
mm2B_NAagecd <- lme(fixed = m_ED ~ BrayCurtisNest.ammcw*AGEcd + BrayCurtisRepl.ammcw*AGEcd +m_EDcwL1D +  m_NAcw+ m_ERcw+ timecw+
                      BrayCurtisNest.ammcb + BrayCurtisRepl.ammcb + m_NAcb+m_ERcb +  FEMALE,
                    data=df,
                    random=~1+m_EDcwL1D+BrayCurtisNest.ammcw +BrayCurtisRepl.ammcw + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =lmeControl(opt='optim'),na.action = na.omit)


# Model 1A: Positive emotion differentiation predicting Emotion regulation variability (Full index)
mm1A_PAagecd <- lme(fixed = BrayCurtisFull.amm ~ m_EDPAcwL1D*AGEcd + m_PAcwL1D + m_ERcw + timecw  +
                      m_EDPAcb + m_PAcb + m_ERcb + FEMALE,
                    data=df,
                    random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1B: Positive emotion differentiation predicting Emotion regulation variability (Strategy switching component)
mm1B_PAagecd <- lme(fixed = BrayCurtisRepl.amm ~ m_EDPAcwL1D*AGEcd + m_PAcwL1D + m_ERcw + BrayCurtisNest.ammcw + timecw  +
                      m_EDPAcb + m_PAcb + m_ERcb + BrayCurtisNest.ammcb +  FEMALE,
                    data=df,
                    random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw + BrayCurtisNest.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1C: Positive emotion  differentiation predicting Emotion regulation variability (Endorsement change component)
mm1C_PAagecd <- lme(fixed = BrayCurtisNest.amm ~ m_EDPAcwL1D*AGEcd + m_PAcwL1D + m_ERcw + BrayCurtisRepl.ammcw + timecw  +
                      m_EDPAcb + m_PAcb + m_ERcb + BrayCurtisRepl.ammcb + FEMALE,
                    data=df,
                    random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw + BrayCurtisRepl.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 2A: Emotion regulation variability (Full index) predicting Positive emotion differentiation
mm2A_PAagecd <- lme(fixed = m_EDPA ~ BrayCurtisFull.ammcw*AGEcd +m_EDPAcwL1D +  m_PAcw+ m_ERcw+ timecw+
                      BrayCurtisFull.ammcb + m_PAcb+m_ERcb + FEMALE,
                    data=df,
                    random=~1+m_EDPAcwL1D+BrayCurtisFull.ammcw + m_PAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =lmeControl(opt='optim'),na.action = na.omit)

# Model 2B: Emotion regulation variability (Switching and endorsement change components separately) predicting Positive emotion differentiation
mm2B_PAagecd <- lme(fixed = m_EDPA ~ BrayCurtisRepl.ammcw*AGEcd + BrayCurtisNest.ammcw*AGEcd +m_EDPAcwL1D +  m_PAcw+ m_ERcw+ timecw+
                      BrayCurtisRepl.ammcb + BrayCurtisNest.ammcb + m_PAcb+m_ERcb + FEMALE,
                    data=df,
                    random=~1+m_EDPAcwL1D+BrayCurtisRepl.ammcw + BrayCurtisNest.ammcw + m_PAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =lmeControl(opt='optim'),na.action = na.omit)


outputAgecd <- rbind(
  preparemmresult(mm1A_NAagecd),
  preparemmresult(mm1B_NAagecd),
  preparemmresult(mm1C_NAagecd),
  preparemmresult(mm2A_NAagecd),
  preparemmresult(mm2B_NAagecd),
  preparemmresult(mm1A_PAagecd),
  preparemmresult(mm1B_PAagecd),
  preparemmresult(mm1C_PAagecd),
  preparemmresult(mm2A_PAagecd),
  preparemmresult(mm2B_PAagecd)
)
write.csv(outputAgecd, "manuscript/results/SMTable82.csv")


# ===========================================================================
# mega analysis - Within-person mediation analysis (Table 3, Supplemental Materials 5)
#
# object names: mms... refers to multilevel model for SENSITIVITY analysis
# ===========================================================================

# Before running mediation models, we need to restructure the data with the stacking method
# New dataframe names

# df_mm1M_NA: dataframe for running negative emotion models
# df_mm1M_PA: dataframe for running positive emotion models

# df_mms1M_NA: dataframe for running negative emotion models 
#              with ER variability calculated with successive comparison approach
# df_mms1M_PA: dataframe for running positive emotion models
#              with ER variability calculated with successive comparison approach

namedf <- names(df)
# Within-person mediation Model 1M (negative emotion): 
df_mm1M_NA <- df
df_mm1M_NA$y <- df_mm1M_NA$m_NAcw
df_mm1M_NA$m <- df_mm1M_NA$BrayCurtisFull.ammcw
df_mm1M_NA <- melt(data=df_mm1M_NA,
                   id.vars=namedf,
                   na.rm=FALSE, variable.name="dv",
                   value.name="z")
df_mm1M_NA$dy <- ifelse(df_mm1M_NA$dv=="y", 1, 0)
df_mm1M_NA$dm <- ifelse(df_mm1M_NA$dv=="m", 1, 0)
df_mm1M_NA$dvnum <- ifelse(df_mm1M_NA$dv=="m", 1, 0)
df_mm1M_NA <- df_mm1M_NA[order(df_mm1M_NA$PARTICIPANT_ID,df_mm1M_NA$timecw,df_mm1M_NA$dm),]

# Within-person mediation Model 1M (positive emotion): 
df_mm1M_PA <- df
df_mm1M_PA$y <- df_mm1M_PA$m_PAcw
df_mm1M_PA$m <- df_mm1M_PA$BrayCurtisFull.ammcw
df_mm1M_PA <- melt(data=df_mm1M_PA,
                   id.vars=namedf,
                   na.rm=FALSE, variable.name="dv",
                   value.name="z")
df_mm1M_PA$dy <- ifelse(df_mm1M_PA$dv=="y", 1, 0)
df_mm1M_PA$dm <- ifelse(df_mm1M_PA$dv=="m", 1, 0)
df_mm1M_PA$dvnum <- ifelse(df_mm1M_PA$dv=="m", 1, 0)
df_mm1M_PA <- df_mm1M_PA[order(df_mm1M_PA$PARTICIPANT_ID,df_mm1M_PA$timecw,df_mm1M_PA$dm),]

# Within-person mediation Model 1M (negative emotion - successive difference): 
df_mms1M_NA <- df
df_mms1M_NA$y <- df_mms1M_NA$m_NAcw
df_mms1M_NA$m <- df_mms1M_NA$BrayCurtisFull.succw
df_mms1M_NA <- melt(data=df_mms1M_NA,
                    id.vars=namedf,
                    na.rm=FALSE, variable.name="dv",
                    value.name="z")
df_mms1M_NA$dy <- ifelse(df_mms1M_NA$dv=="y", 1, 0)
df_mms1M_NA$dm <- ifelse(df_mms1M_NA$dv=="m", 1, 0)
df_mms1M_NA$dvnum <- ifelse(df_mms1M_NA$dv=="m", 1, 0)
df_mms1M_NA <- df_mms1M_NA[order(df_mms1M_NA$PARTICIPANT_ID,df_mms1M_NA$timecw,df_mms1M_NA$dm),]
# Within-person mediation Model 1M (positive emotion - successive difference): 
df_mms1M_PA <- df
df_mms1M_PA$y <- df_mms1M_PA$m_PAcw
df_mms1M_PA$m <- df_mms1M_PA$BrayCurtisFull.succw
df_mms1M_PA <- melt(data=df_mms1M_PA,
                    id.vars=namedf,
                    na.rm=FALSE, variable.name="dv",
                    value.name="z")
df_mms1M_PA$dy <- ifelse(df_mms1M_PA$dv=="y", 1, 0)
df_mms1M_PA$dm <- ifelse(df_mms1M_PA$dv=="m", 1, 0)
df_mms1M_PA$dvnum <- ifelse(df_mms1M_PA$dv=="m", 1, 0)
df_mms1M_PA <- df_mms1M_PA[order(df_mms1M_PA$PARTICIPANT_ID,df_mms1M_PA$timecw,df_mms1M_PA$dm),]


# Evaluating within-person mediation models...

# Model 1M: Negative emotion differentiation predicting negative emotion intensity 
#           via emotion regulation variability (Full index)
#           (no other extra specifications)

mediation1M_NA <- lme(fixed = z ~ -1 +
                        dm:m_EDcwL1D + dm:m_NAcwL1D+ dm:timecw+
                        dy:BrayCurtisFull.ammcw+dy:m_ERcw + dy:m_NAcwL1D+ dy:m_EDcwL1D + dy:timecw,
                      data=df_mm1M_NA,
                      random = ~-1+dm:m_EDcwL1D+ dm:m_NAcwL1D+
                        dy:BrayCurtisFull.ammcw+dy:m_ERcw + dy:m_NAcwL1D + dy:m_EDcwL1D| study/PARTICIPANT_ID,
                      correlation = corAR1(),
                      weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                      control =lmeControl(opt="optim",maxIter = 200, msMaxIter = 200, niterEM = 50, msMaxEval = 400),na.action = na.omit)

# Model 1M: Negative emotion differentiation predicting negative emotion intensity 
#           via emotion regulation variability (Full index)
#           adding person-level baseline negative emotion differentiation as co-moderator to a- and b-paths
mediation1M_NAcomod <- lme(fixed = z ~ -1 +
                             dm:m_EDcwL1D + dm:m_EDcwL1D:m_EDcb + dm:m_NAcwL1D+ dm:timecw+
                             dy:BrayCurtisFull.ammcw + dy:BrayCurtisFull.ammcw:m_EDcb +dy:m_ERcw + dy:m_NAcwL1D+ dy:m_EDcwL1D + dy:timecw,
                           data=df_mm1M_NA,
                           random = ~-1+dm:m_EDcwL1D+ dm:m_NAcwL1D+
                             dy:BrayCurtisFull.ammcw+dy:m_ERcw + dy:m_NAcwL1D + dy:m_EDcwL1D| study/PARTICIPANT_ID,
                           correlation = corAR1(),
                           weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                           control =lmeControl(opt="optim",maxIter = 200, msMaxIter = 200, niterEM = 50, msMaxEval = 400),na.action = na.omit)

# Model 1M: Negative emotion differentiation predicting negative emotion intensity 
#           via emotion regulation variability (Full index, Successive difference temporal approach)
mediation1M_NA.suc <- lme(fixed = z ~ -1 +
                            dm:m_EDcwL1D + dm:m_NAcwL1D+ dm:timecw+
                            dy:BrayCurtisFull.succw+dy:m_ERcw + dy:m_NAcwL1D+ dy:m_EDcwL1D + dy:timecw,
                          data=df_mms1M_NA,
                          random = ~-1+dm:m_EDcwL1D+ dm:m_NAcwL1D+
                            dy:BrayCurtisFull.succw+dy:m_ERcw + dy:m_NAcwL1D + dy:m_EDcwL1D| study/PARTICIPANT_ID,
                          correlation = corAR1(),
                          weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                          control =lmeControl(opt="optim",maxIter = 200, msMaxIter = 200, niterEM = 50, msMaxEval = 400),na.action = na.omit)

# Model 1M: Negative emotion differentiation predicting negative emotion intensity 
#           via emotion regulation variability (Full index, Successive difference temporal approach)
#           adding person-level baseline negative emotion differentiation as co-moderator to a- and b-paths
mediation1M_NA.succomod <- lme(fixed = z ~ -1 +
                                 dm:m_EDcwL1D+dm:m_EDcwL1D:m_EDcb + dm:m_NAcwL1D+ dm:timecw+
                                 dy:BrayCurtisFull.succw+dy:BrayCurtisFull.succw:m_EDcb+dy:m_ERcw + dy:m_NAcwL1D+ dy:m_EDcwL1D + dy:timecw,
                               data=df_mms1M_NA,
                               random = ~-1+dm:m_EDcwL1D+ dm:m_NAcwL1D+
                                 dy:BrayCurtisFull.succw+dy:m_ERcw + dy:m_NAcwL1D + dy:m_EDcwL1D| study/PARTICIPANT_ID,
                               correlation = corAR1(),
                               weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                               control =lmeControl(opt="optim",maxIter = 200, msMaxIter = 200, niterEM = 50, msMaxEval = 400),na.action = na.omit)

# Model 1M: Negative emotion differentiation predicting negative emotion intensity 
#           via emotion regulation variability (Full index)
#           adding dataset-centered age as main effect and within-person moderator
mediation1M_NAagecd <- lme(fixed = z ~ -1 +
                             dm:m_EDcwL1D+dm:m_EDcwL1D:AGEcd + dm:m_NAcwL1D+ dm:timecw+
                             dy:BrayCurtisFull.ammcw+dy:BrayCurtisFull.ammcw:AGEcd+dy:m_ERcw + dy:m_NAcwL1D+ dy:m_EDcwL1D + dy:timecw,
                           data=df_mm1M_NA,
                           random = ~-1+dm:m_EDcwL1D+ dm:m_NAcwL1D+
                             dy:BrayCurtisFull.ammcw+dy:m_ERcw + dy:m_NAcwL1D + dy:m_EDcwL1D| study/PARTICIPANT_ID,
                           correlation = corAR1(),
                           weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                           control =lmeControl(opt="optim",maxIter = 200, msMaxIter = 200, niterEM = 50, msMaxEval = 400),na.action = na.omit)

# Model 1M: Negative emotion differentiation predicting negative emotion intensity 
#           via emotion regulation variability (Full index)
#           adding dataset-centered age as main effect and within-person moderator
#           adding person-level baseline negative emotion differentiation as co-moderator to a- and b-paths
mediation1M_NAagecdcomod <- lme(fixed = z ~ -1 +
                                  dm:m_EDcwL1D+dm:m_EDcwL1D:m_EDcb+dm:m_EDcwL1D:AGEcd + dm:m_NAcwL1D+ dm:timecw+
                                  dy:BrayCurtisFull.ammcw+dy:BrayCurtisFull.ammcw:m_EDcb+dy:BrayCurtisFull.ammcw:AGEcd+dy:m_ERcw + dy:m_NAcwL1D+ dy:m_EDcwL1D + dy:timecw,
                                data=df_mm1M_NA,
                                random = ~-1+dm:m_EDcwL1D+ dm:m_NAcwL1D+
                                  dy:BrayCurtisFull.ammcw+dy:m_ERcw + dy:m_NAcwL1D + dy:m_EDcwL1D| study/PARTICIPANT_ID,
                                correlation = corAR1(),
                                weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                                control =lmeControl(opt="optim",maxIter = 200, msMaxIter = 200, niterEM = 50, msMaxEval = 400),na.action = na.omit)


# Model 1M: Negative emotion differentiation predicting negative emotion intensity 
#           via emotion regulation variability (Full index)
#           examining influence of zero-ratings
mediation1M_NAb <- lme(fixed = z ~ -1 +
                         dm:m_EDcwL1D*b0NAL1D+ dm:m_NAcwL1D+ dm:timecw+
                         dy:BrayCurtisFull.ammcw*b0ER+dy:m_ERcw + dy:m_NAcwL1D+ dy:m_EDcwL1D + dy:timecw,
                       data=df_mm1M_NA,
                       random = ~-1+dm:m_EDcwL1D*b0NAL1D+ dm:m_NAcwL1D+
                         dy:BrayCurtisFull.ammcw*b0ER+dy:m_ERcw + dy:m_NAcwL1D + dy:m_EDcwL1D| study/PARTICIPANT_ID,
                       correlation = corAR1(),
                       weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                       control =lmeControl(opt="optim",maxIter = 200, msMaxIter = 200, niterEM = 50, msMaxEval = 400),na.action = na.omit)

# Model 1M: Negative emotion differentiation predicting negative emotion intensity 
#           via emotion regulation variability (Full index)
#           examining influence of zero-ratings
#           adding person-level baseline negative emotion differentiation as co-moderator to a- and b-paths
mediation1M_NAbcomod <- lme(fixed = z ~ -1 +
                              dm:m_EDcwL1D*b0NAL1D+dm:m_EDcwL1D:m_EDcb+ dm:m_NAcwL1D+ dm:timecw+
                              dy:BrayCurtisFull.ammcw*b0ER+dy:BrayCurtisFull.ammcw:m_EDcb+dy:m_ERcw + dy:m_NAcwL1D+ dy:m_EDcwL1D + dy:timecw,
                            data=df_mm1M_NA,
                            random = ~-1+dm:m_EDcwL1D*b0NAL1D+ dm:m_NAcwL1D+
                              dy:BrayCurtisFull.ammcw*b0ER+dy:m_ERcw + dy:m_NAcwL1D + dy:m_EDcwL1D| study/PARTICIPANT_ID,
                            correlation = corAR1(),
                            weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                            control =lmeControl(opt="optim",maxIter = 200, msMaxIter = 200, niterEM = 50, msMaxEval = 400),na.action = na.omit)

# Model 1M: Negative emotion differentiation predicting negative emotion intensity 
#           via emotion regulation variability (Full index)
#           examining influence of within-person moderators of emotion (regulation) intensity on paths
mediation1M_NAxNA <- lme(fixed = z ~ -1 +
                           dm:m_EDcwL1D*dm:m_NAcwL1D+ dm:timecw+
                           dy:BrayCurtisFull.ammcw*dy:m_ERcw + dy:m_NAcwL1D+ dy:m_EDcwL1D + dy:timecw,
                         data=df_mm1M_NA,
                         random = ~-1+dm:m_EDcwL1D*dm:m_NAcwL1D+
                           dy:BrayCurtisFull.ammcw*dy:m_ERcw + dy:m_NAcwL1D + dy:m_EDcwL1D| study/PARTICIPANT_ID,
                         correlation = corAR1(),
                         weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                         control =lmeControl(opt="optim",maxIter = 200, msMaxIter = 200, niterEM = 50, msMaxEval = 400),na.action = na.omit)
# Model 1M: Negative emotion differentiation predicting negative emotion intensity 
#           via emotion regulation variability (Full index)
#           examining influence of within-person moderators of emotion (regulation) intensity on paths
#           adding person-level baseline negative emotion differentiation as co-moderator to a- and b-paths
mediation1M_NAxNAcomod <- lme(fixed = z ~ -1 +
                                dm:m_EDcwL1D*dm:m_NAcwL1D+dm:m_EDcwL1D:m_EDcb+ dm:timecw+
                                dy:BrayCurtisFull.ammcw*dy:m_ERcw +dy:BrayCurtisFull.ammcw:m_EDcb+ dy:m_NAcwL1D+ dy:m_EDcwL1D + dy:timecw,
                              data=df_mm1M_NA,
                              random = ~-1+dm:m_EDcwL1D*dm:m_NAcwL1D+
                                dy:BrayCurtisFull.ammcw*dy:m_ERcw + dy:m_NAcwL1D + dy:m_EDcwL1D| study/PARTICIPANT_ID,
                              correlation = corAR1(),
                              weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                              control =lmeControl(opt="optim",maxIter = 200, msMaxIter = 200, niterEM = 50, msMaxEval = 400),na.action = na.omit)

# Supplemental Materials 5: see if positive emotion models are similar in direction and significance
# when the first-order autocorrelation term is removed
mm1A_PAw <- lme(fixed = BrayCurtisFull.ammcw ~ m_EDPAcwL1D + m_PAcwL1D + timecw,
                data=df,
                random=~1+m_EDPAcwL1D + m_PAcwL1D | study/PARTICIPANT_ID, correlation = corAR1(),
                control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mm3A_PAw <- lme(fixed = m_PAcw ~ BrayCurtisFull.ammcw +m_EDPAcwL1D +  m_PAcwL1D + m_ERcw+ timecw,
                data=df,
                random=~1+m_EDPAcwL1D+BrayCurtisFull.ammcw + m_PAcwL1D+ m_ERcw | study/PARTICIPANT_ID, 
                correlation = corAR1(),
                control =lmeControl(opt='optim'),na.action = na.omit)
mm1A_PAexAR1 <- lme(fixed = BrayCurtisFull.ammcw ~ m_EDPAcwL1D + m_PAcwL1D + timecw ,
                    data=df,
                    random=~1+m_EDPAcwL1D + m_PAcwL1D | study/PARTICIPANT_ID, 
                    #correlation = corAR1(),
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mm3A_PAexAR1 <- lme(fixed = m_PAcw ~ BrayCurtisFull.ammcw +m_EDPAcwL1D +  m_PAcwL1D + m_ERcw+ timecw,
                    data=df,
                    random=~1+m_EDPAcwL1D+BrayCurtisFull.ammcw + m_PAcwL1D+ m_ERcw | study/PARTICIPANT_ID, 
                    #correlation = corAR1(),
                    control =lmeControl(opt='optim'),na.action = na.omit)

# Inspecting the fixed effects of interests showed they remained in the same direction and significance
# Therefore, we proceed with running within-person mediation model without the corAR1 term
summary(mm1A_PAw)
summary(mm1A_PAexAR1)
summary(mm3A_PAw)
summary(mm3A_PAexAR1)


# Model 1M: Positive emotion differentiation predicting positive emotion intensity 
#           via emotion regulation variability (Full index)
#           (no other extra specifications)

mediation1M_PA <- lme(fixed = z ~ -1 +
                        dm:m_EDPAcwL1D + dm:m_PAcwL1D+ dm:timecw+
                        dy:BrayCurtisFull.ammcw+dy:m_ERcw + dy:m_PAcwL1D+ dy:m_EDPAcwL1D + dy:timecw,
                      data=df_mm1M_PA,
                      random = ~-1+dm:m_EDPAcwL1D+ dm:m_PAcwL1D+
                        dy:BrayCurtisFull.ammcw+dy:m_ERcw + dy:m_PAcwL1D + dy:m_EDPAcwL1D| study/PARTICIPANT_ID,
                      # correlation = corAR1(), # this is excluded because of evaluation error
                      weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                      control =lmeControl(opt="optim",maxIter = 400, msMaxIter = 400, niterEM = 200, msMaxEval = 400),na.action = na.omit)

# Model 1M: Positive emotion differentiation predicting positive emotion intensity 
#           via emotion regulation variability (Full index)
#           adding person-level baseline positive emotion differentiation as co-moderator to a- and b-paths
mediation1M_PAcomod <- lme(fixed = z ~ -1 +
                             dm:m_EDPAcwL1D +dm:m_EDPAcwL1D:m_EDPAcb + dm:m_PAcwL1D+ dm:timecw+
                             dy:BrayCurtisFull.ammcw+dy:BrayCurtisFull.ammcw:m_EDPAcb+dy:m_ERcw + dy:m_PAcwL1D+ dy:m_EDPAcwL1D + dy:timecw,
                           data=df_mm1M_PA,
                           random = ~-1+dm:m_EDPAcwL1D+ dm:m_PAcwL1D+
                             dy:BrayCurtisFull.ammcw+dy:m_ERcw + dy:m_PAcwL1D + dy:m_EDPAcwL1D| study/PARTICIPANT_ID,
                           # correlation = corAR1(), # this is excluded because of evaluation error
                           weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                           control =lmeControl(opt="optim",maxIter = 400, msMaxIter = 400, niterEM = 200, msMaxEval = 400),na.action = na.omit)


# Model 1M: Positive emotion differentiation predicting positive emotion intensity 
#           via emotion regulation variability (Full index, Successive difference temporal approach)
mediation1M_PA.suc <- lme(fixed = z ~ -1 +
                            dm:m_EDPAcwL1D + dm:m_PAcwL1D+ dm:timecw+
                            dy:BrayCurtisFull.succw+dy:m_ERcw + dy:m_PAcwL1D+ dy:m_EDPAcwL1D + dy:timecw,
                          data=df_mms1M_PA,
                          random = ~-1+dm:m_EDPAcwL1D+ dm:m_PAcwL1D+
                            dy:BrayCurtisFull.succw+dy:m_ERcw + dy:m_PAcwL1D + dy:m_EDPAcwL1D| study/PARTICIPANT_ID,
                          # correlation = corAR1(), # this is excluded because of evaluation error
                          weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                          control =lmeControl(opt="optim",maxIter = 400, msMaxIter = 400, niterEM = 200, msMaxEval = 400),na.action = na.omit)
# Model 1M: Positive emotion differentiation predicting positive emotion intensity 
#           via emotion regulation variability (Full index, Successive difference temporal approach)
#           adding person-level baseline positive emotion differentiation as co-moderator to a- and b-paths
mediation1M_PA.succomod <- lme(fixed = z ~ -1 +
                                 dm:m_EDPAcwL1D + dm:m_EDPAcwL1D:m_EDPAcb + dm:m_PAcwL1D+ dm:timecw+
                                 dy:BrayCurtisFull.succw+dy:BrayCurtisFull.succw:m_EDPAcb+dy:m_ERcw + dy:m_PAcwL1D+ dy:m_EDPAcwL1D + dy:timecw,
                               data=df_mms1M_PA,
                               random = ~-1+dm:m_EDPAcwL1D+ dm:m_PAcwL1D+
                                 dy:BrayCurtisFull.succw+dy:m_ERcw + dy:m_PAcwL1D + dy:m_EDPAcwL1D| study/PARTICIPANT_ID,
                               # correlation = corAR1(), # this is excluded because of evaluation error
                               weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                               control =lmeControl(opt="optim",maxIter = 400, msMaxIter = 400, niterEM = 200, msMaxEval = 400),na.action = na.omit)

# Model 1M: Positive emotion differentiation predicting positive emotion intensity 
#           via emotion regulation variability (Full index)
#           adding dataset-centered age as main effect and within-person moderator
mediation1M_PAagecd <- lme(fixed = z ~ -1 +
                             dm:m_EDPAcwL1D+dm:m_EDPAcwL1D:AGEcd + dm:m_PAcwL1D+ dm:timecw+
                             dy:BrayCurtisFull.ammcw+dy:BrayCurtisFull.ammcw:AGEcd+dy:m_ERcw + dy:m_PAcwL1D+ dy:m_EDPAcwL1D + dy:timecw,
                           data=df_mm1M_PA,
                           random = ~-1+dm:m_EDPAcwL1D+ dm:m_PAcwL1D+
                             dy:BrayCurtisFull.ammcw+dy:m_ERcw + dy:m_PAcwL1D + dy:m_EDPAcwL1D| study/PARTICIPANT_ID,
                           # correlation = corAR1(), # this is excluded because of evaluation error
                           weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                           control =lmeControl(opt="optim",maxIter = 400, msMaxIter = 400, niterEM = 200, msMaxEval = 400),na.action = na.omit)

# Model 1M: Positive emotion differentiation predicting positive emotion intensity 
#           via emotion regulation variability (Full index)
#           adding dataset-centered age as main effect and within-person moderator
#           adding person-level baseline positive emotion differentiation as co-moderator to a- and b-paths
mediation1M_PAagecdcomod <- lme(fixed = z ~ -1 +
                                  dm:m_EDPAcwL1D+dm:m_EDPAcwL1D:m_EDPAcb+dm:m_EDPAcwL1D:AGEcd + dm:m_PAcwL1D+ dm:timecw+
                                  dy:BrayCurtisFull.ammcw+dy:BrayCurtisFull.ammcw:m_EDPAcb+dy:BrayCurtisFull.ammcw:AGEcd+dy:m_ERcw + dy:m_PAcwL1D+ dy:m_EDPAcwL1D + dy:timecw,
                                data=df_mm1M_PA,
                                random = ~-1+dm:m_EDPAcwL1D+ dm:m_PAcwL1D+
                                  dy:BrayCurtisFull.ammcw+dy:m_ERcw + dy:m_PAcwL1D + dy:m_EDPAcwL1D| study/PARTICIPANT_ID,
                                # correlation = corAR1(), # this is excluded because of evaluation error
                                weights = varIdent(form = ~ 1 | dvnum), #this invokes separate sigma^{2}_{e} for each outcome
                                control =lmeControl(opt="optim",maxIter = 400, msMaxIter = 400, niterEM = 200, msMaxEval = 400),na.action = na.omit)


# a wrapper to extract fixed effect of an model1M results (nlme)
prepare1Mresult <- function (m){
  cbind(model = deparse(substitute(m)),
        FEest = summary(m)$tTable[,1],
        SE = summary(m)$tTable[,2],
        DF = summary(m)$tTable[,3],
        pvalue = summary(m)$tTable[,5],
        residual = m$sigma^2,
        nobs = nobs(m),
        N = m$dims$ngrps[1],
        u95CI = summary(m)$tTable[,1]+summary(m)$tTable[,2]*1.96,
        l95CI = summary(m)$tTable[,1]-summary(m)$tTable[,2]*1.96
  )
  
}

outputSMTable5_1M <- rbind(
  prepare1Mresult(mediation1M_NA),
  prepare1Mresult(mediation1M_PA),
  prepare1Mresult(mediation1M_NAcomod),
  prepare1Mresult(mediation1M_PAcomod),
  prepare1Mresult(mediation1M_NA.suc),
  prepare1Mresult(mediation1M_PA.suc),
  prepare1Mresult(mediation1M_NAagecd),
  prepare1Mresult(mediation1M_PAagecd),
  prepare1Mresult(mediation1M_NAb),
  prepare1Mresult(mediation1M_NAxNA),
  prepare1Mresult(mediation1M_NA.succomod),
  prepare1Mresult(mediation1M_PA.succomod),
  prepare1Mresult(mediation1M_NAagecdcomod),
  prepare1Mresult(mediation1M_PAagecdcomod),
  prepare1Mresult(mediation1M_NAbcomod),
  prepare1Mresult(mediation1M_NAxNAcomod)
)

# write direct effect results
write.csv(outputSMTable5_1M, "manuscript/results/SMTable5_1M.csv")
# --
# End of calculating direct effects
# --

# --
# Start of calculating indirect effect
# --

# We calculate indirect effect with the Monte Carlo script here:
# https://www.quantpsy.org/medmc/medmc111.htm

# The object that carries the resampled estimates of indirect effect is at "ab", given by:
#   cvec=rnorm(rep)*sqrt(varcovajbj)+covajbj
#   ab=avec*bvec+cvec
# varcovajbj is the asymptotic covariance of random effect between a- and b-path.
# In other words, the expected variability in such covariance over repeated sampling
# As shown by the above formula about cvec, setting varcovajbj=0 is not going to 
# shift the distribution of ab to more positive or more negative.
# rnorm(rep)*sqrt(varcovajbj) essentially brings in more "noise" to the distribution of ab
# if varcovajbj is large enough, the distrbution of ab becomes more dispersed, 
# so that it is more likely to cross zero.
# So, in cases where varcovajbj is unavailable, we can set it to 0 to get a more liberal (less conservative)
# estimation of the confidence interval of indirect effect.
# If such a liberal interval still crosses zero, the true interval will definitely cross zero.

indirectMCnlme <- function (m,sda,sdb,abrcor){
  a <- fixed.effects(m)[1]
  b <- fixed.effects(m)[4]
  cprime <- fixed.effects(m)[7]
  covajbj <-  sda*sdb*abrcor
  indirecteffect <- a*b + covajbj  
  indirecteffect
  totaleffect <- cprime + a*b + covajbj  
  indirecteffect/totaleffect
  (ACM_FE <- vcov(m))
  vara <- ACM_FE[1,1]
  varb <- ACM_FE[4,4]
  covab <- ACM_FE[1,4]
  varcovajbj=0 # unable to extract this from nlme
  
  rep=20000
  conf=95
  dvec=rnorm(rep)
  avec=dvec*sqrt(vara)+a
  bvec=dvec*covab/sqrt(vara)+sqrt(varb)*rnorm(rep,sd=sqrt(1-(covab^2)/(vara*varb)))+b
  cvec=rnorm(rep)*sqrt(varcovajbj)+covajbj
  ab=avec*bvec+cvec
  ab_liberal=avec*bvec+covajbj
  low=(1-conf/100)/2
  upp=((1-conf/100)/2)+(conf/100)
  LL=quantile(ab_liberal,low)
  UL=quantile(ab_liberal,upp)
  LL4=format(LL,digits=4)
  UL4=format(UL,digits=4)
  
  data.frame("model" = deparse(substitute(m)),
             "ind" = indirecteffect,
             "LB"= LL4,
             "UB" = UL4)
}
indirectMClme4 <- function (m, haserror = FALSE){
  
  resmed <- m
  (FEmatrix <- coef(summary(resmed)))
  #making parameter objects
  a <- FEmatrix[1]
  b <- FEmatrix[4]
  cprime <- FEmatrix[7]
  (REmatrix <- as.data.frame(VarCorr(resmed)))
  sig2_a <- REmatrix[1,4]
  sig2_b <- REmatrix[3,4]
  sig2_cprime <- REmatrix[6,4]
  covajbj <- REmatrix[8,4]
  indirecteffect <- a*b + covajbj  
  totaleffect <- cprime + a*b + covajbj  
  indirecteffect/totaleffect
  (ACM_FE <- vcov(resmed))
  vara <- ACM_FE[1,1]
  varb <- ACM_FE[4,4]
  covab <- ACM_FE[1,4]
  if (haserror){
    varcovajbj=0 # error for 1M_NA and 1M_PA
  }else{
    (ACM_RE <- vcov_vc(resmed, sd_cor = FALSE, print_names = TRUE))
    varcovajbj=  ACM_RE[3,3]
  }
  
  rep=20000
  conf=95
  dvec=rnorm(rep)
  avec=dvec*sqrt(vara)+a
  bvec=dvec*covab/sqrt(vara)+sqrt(varb)*rnorm(rep,sd=sqrt(1-(covab^2)/(vara*varb)))+b
  cvec=rnorm(rep)*sqrt(varcovajbj)+covajbj
  ab=avec*bvec+cvec
  low=(1-conf/100)/2
  upp=((1-conf/100)/2)+(conf/100)
  LL=quantile(ab,low)
  UL=quantile(ab,upp)
  LL4=format(LL,digits=4)
  UL4=format(UL,digits=4)
  
  
  data.frame("model" = deparse(substitute(m)),
             "ind" = indirecteffect,
             "LB"= LL4,
             "UB" = UL4)
}

# We only assessed CI of indirect effect of Model 1M and sensitivity analyses on 1M with dataset-centered age.
# Because for other sensitivity analyses specifications, lme4 model results deviated from those of nlme.
# See supplemental materials 6 and 7 for more information.

# There were errors extracting asymptotic covariance of random effect for Model 1M due to singularity issue.
# As a result, indout1.lme4 and indout2.lme4 give more liberal estimation of CI.
indout1.lme4 <-   indirectMClme4(mediation1M_NA.lmer, haserror = TRUE)
indout2.lme4 <-   indirectMClme4(mediation1M_PA.lmer, haserror = TRUE)
indout3.lme4 <-   indirectMClme4(mediation1M_NAagecd.lmer)
indout4.lme4 <-   indirectMClme4(mediation1M_PAagecd.lmer)

# For nlme, we have to hard-code the SD of a- and b-path person-level random effects, 
# and the correlation between them
# Using the values from summary(model)
indout1.nlme <- indirectMCnlme(mediation1M_NA,0.01791552 ,0.17316229  ,0.156  )
indout2.nlme <- indirectMCnlme(mediation1M_PA,0.02487349  ,0.17648254   ,-0.153  )
indout3.nlme <- indirectMCnlme(mediation1M_NAagecd,0.01819089  ,0.17276638    ,0.151)
indout4.nlme <- indirectMCnlme(mediation1M_PAagecd,0.02426128   ,0.17749774     ,-0.143)


# indirect effect CI using lme4 model estimates
rbind(indout1.lme4,
      indout2.lme4,
      indout3.lme4,
      indout4.lme4)
# indirect effect CI using nlme model estimates
rbind(indout1.nlme,
      indout2.nlme,
      indout3.nlme,
      indout4.nlme)
# To summarize the results, 
# Estimates derived from nlme models indicate that
#    the positive emotion mediation models have CIs of indirect effects crossing zero.
# Estimates derived from lme4 models indicate that
#    the negative emotion mediation models have CIs of indirect effects crossing zero.
# Overall, no model could reject the null hypothesis by having CIs not crossing zero.
# In other words, results did not support the presence of within-person mediation effects.


# ===========================================================================
# mega analysis - SENSITIVITY analysis (supplemental materials 7)
# Do all-zero ratings of emotion (regulation) intensity influence our results?
#
# object names: 
#   suffix of b refers to models with extra within-person moderator 
#     given by the product of binary variable (0=absence, 1=presence) of all zero
#     and within-person component of the independent variable of interest
#   suffix of xNA/xER refers to models with extra within-person moderator 
#     given by the product of the within-person component of respective covariate
#     and within-person component of the independent variable of interest
# ===========================================================================


# Model 1A: Negative emotion differentiation predicting Emotion regulation variability (Full index)
mm1A_NAb <- lme(fixed = BrayCurtisFull.amm ~ b0NAL1D*m_EDcwL1D + m_NAcwL1D + m_ERcw + timecw  +
                  m_EDcb + m_NAcb + m_ERcb + AGE + FEMALE,
                data=df,
                random=~1+b0NAL1D*m_EDcwL1D + m_NAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mm1A_NAxNA <- lme(fixed = BrayCurtisFull.amm ~ m_EDcwL1D*m_NAcwL1D + m_ERcw + timecw  +
                    m_EDcb + m_NAcb + m_ERcb + AGE + FEMALE,
                  data=df,
                  random=~1+m_EDcwL1D*m_NAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                  control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
# Model 1B: Negative emotion  differentiation predicting Emotion regulation variability (Strategy switching component)
mm1B_NAb <- lme(fixed = BrayCurtisRepl.amm ~ b0NAL1D*m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisNest.ammcw + timecw  +
                  m_EDcb + m_NAcb + m_ERcb +  BrayCurtisNest.ammcb + AGE + FEMALE,
                data=df,
                random=~1+b0NAL1D*m_EDcwL1D + m_NAcwL1D+ m_ERcw +BrayCurtisNest.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
                control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mm1B_NAxNA <- lme(fixed = BrayCurtisRepl.amm ~ m_EDcwL1D*m_NAcwL1D + m_ERcw + BrayCurtisNest.ammcw+ timecw  +
                    m_EDcb + m_NAcb + m_ERcb + AGE + FEMALE,
                  data=df,
                  random=~1+m_EDcwL1D*m_NAcwL1D+ m_ERcw + BrayCurtisNest.ammcw| study/PARTICIPANT_ID, correlation = corAR1(),
                  control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)

# Model 1C: Negative emotion  differentiation predicting Emotion regulation variability (Endorsement change component)
mm1C_NAb <- lme(fixed = BrayCurtisNest.amm ~ b0NAL1D*m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisRepl.ammcw + timecw  +
                  m_EDcb + m_NAcb + m_ERcb +BrayCurtisRepl.ammcb+ AGE + FEMALE,
                data=df,
                random=~1+b0NAL1D*m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisRepl.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
                control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mm1C_NAxNA <- lme(fixed = BrayCurtisNest.amm ~ m_EDcwL1D*m_NAcwL1D + m_ERcw + BrayCurtisRepl.ammcw + timecw  +
                    m_EDcb + m_NAcb + m_ERcb + AGE + FEMALE,
                  data=df,
                  random=~1+m_EDcwL1D*m_NAcwL1D+ m_ERcw+ BrayCurtisRepl.ammcw  | study/PARTICIPANT_ID, correlation = corAR1(),
                  control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
# Model 2A: Emotion regulation variability (Full index) predicting Negative emotion  differentiation
mm2A_NAb <- lme(fixed = m_ED ~ BrayCurtisFull.ammcw*b0ER + m_EDcwL1D +  m_NAcw + m_ERcw + timecw+
                  BrayCurtisFull.ammcb + m_NAcb+m_ERcb + AGE + FEMALE,
                data=df,
                random=~1+m_EDcwL1D+BrayCurtisFull.ammcw*b0ER + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                control =lmeControl(opt='optim'),na.action = na.omit)
mm2A_NAxER <- lme(fixed = m_ED ~ BrayCurtisFull.ammcw*m_ERcw + m_EDcwL1D +  m_NAcw  + timecw+
                    BrayCurtisFull.ammcb + m_NAcb+m_ERcb + AGE + FEMALE,
                  data=df,
                  random=~1+m_EDcwL1D+BrayCurtisFull.ammcw*m_ERcw + m_NAcw | study/PARTICIPANT_ID, correlation = corAR1(),
                  control =lmeControl(opt='optim'),na.action = na.omit)

# Model 2B: Emotion regulation variability (Switching and endorsement change components separately) predicting Negative emotion differentiation
mm2B_NAb <- lme(fixed = m_ED ~ BrayCurtisNest.ammcw*b0ER + BrayCurtisRepl.ammcw*b0ER +m_EDcwL1D +  m_NAcw+ m_ERcw+ timecw+
                  BrayCurtisNest.ammcb + BrayCurtisRepl.ammcb + m_NAcb+m_ERcb + AGE + FEMALE,
                data=df,
                random=~1+m_EDcwL1D+BrayCurtisNest.ammcw*b0ER +BrayCurtisRepl.ammcw*b0ER + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                control =lmeControl(opt='optim'),na.action = na.omit)
mm2B_NAxER <- lme(fixed = m_ED ~ BrayCurtisNest.ammcw*m_ERcw + BrayCurtisRepl.ammcw*m_ERcw +m_EDcwL1D +  m_NAcw+ timecw+
                    BrayCurtisNest.ammcb + BrayCurtisRepl.ammcb + m_NAcb+m_ERcb + AGE + FEMALE,
                  data=df,
                  random=~1+m_EDcwL1D+BrayCurtisNest.ammcw*m_ERcw +BrayCurtisRepl.ammcw*m_ERcw + m_NAcw | study/PARTICIPANT_ID, correlation = corAR1(),
                  control =lmeControl(opt='optim'),na.action = na.omit)

# positive emotions are not analyzed because positive emotions have no measurement with all-zero ratings


outputSMTable7 <- rbind(
  preparemmresult(mm1A_NAb),
  preparemmresult(mm1A_NAxNA),
  preparemmresult(mm1B_NAb),
  preparemmresult(mm1B_NAxNA),
  preparemmresult(mm1C_NAb),
  preparemmresult(mm1C_NAxNA),
  
  preparemmresult(mm2A_NAb),
  preparemmresult(mm2A_NAxER),
  preparemmresult(mm2B_NAb),
  preparemmresult(mm2B_NAxER))
write.csv(outputSMTable7, "manuscript/results/SMTable7.csv")

}