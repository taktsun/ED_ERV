library(nlme)

source("list_of_variables.R") # load all ESM measures variable names
# read ready-to-analyze datasets
df <- read.csv("dataProcessed/ReadyPooledESM.csv")

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

# ===========================================================================
# A few features of these multilevel models:
# 1. we separate within- and between-person components (cw = component within-person, cb = component between-person)
# 2. we included autocorrelation of residual (correlation = corAR1())
# 3. we conducted a 3-level modeling, nesting momentary indices (level 1) within PARTICIPANT_ID (level 2) within study (level 3)
# 4. we used the quasi-Newton optimizer (opt = "optim")
# ===========================================================================

# ===========================================================================
# mega analysis - registered confirmatory hypothesis
# about NEGATIVE emotion differentiation and emotion regulation variability
#
# object names: mm1A_NA refers to multilevel model 1A in NEGATIVE emotion, mm2A_NA refers to model 2A, etc...
# ===========================================================================
mm1A_NA <- lme(fixed = BrayCurtisFull.amm ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + timecw  + 
                m_EDcb + m_NAcb + m_ERcb + AGE + FEMALE,
              data=df,
              random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
              control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mm1B_NA <- lme(fixed = BrayCurtisRepl.amm ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisNest.ammcw + timecw  + 
                     m_EDcb + m_NAcb + m_ERcb +  BrayCurtisNest.ammcb + AGE + FEMALE,
                   data=df,
                   random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw +BrayCurtisNest.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
                   control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mm1C_NA <- lme(fixed = BrayCurtisNest.amm ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisRepl.ammcw + timecw  + 
                     m_EDcb + m_NAcb + m_ERcb +BrayCurtisRepl.ammcb+ AGE + FEMALE,
                   data=df,
                   random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw + BrayCurtisRepl.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
                   control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mm2A_NA <- lme(fixed = m_ED ~ BrayCurtisFull.ammcw +m_EDcwL1D +  m_NAcw+ m_ERcw+ timecw+ 
                 BrayCurtisFull.ammcb + m_NAcb+m_ERcb + AGE + FEMALE,
               data=df,
               random=~1+BrayCurtisFull.ammcw + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
               control =lmeControl(opt='optim'),na.action = na.omit)
mm2B_NA <- lme(fixed = m_ED ~ BrayCurtisNest.ammcw + BrayCurtisRepl.ammcw +m_EDcwL1D +  m_NAcw+ m_ERcw+ timecw+ 
                     BrayCurtisNest.ammcb + BrayCurtisRepl.ammcb + m_NAcb+m_ERcb + AGE + FEMALE,
                   data=df,
                   random=~1+BrayCurtisNest.ammcw +BrayCurtisRepl.ammcw + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                   control =lmeControl(opt='optim'),na.action = na.omit)
# ===========================================================================
# mega analysis - pre-registered exploratory analyses
# about POSITIVE emotion differentiation and emotion regulation variability
# 
# object names: mm1A_PA refers to multilevel model 1A in POSITIVE emotion, mm2A_PA refers to model 2A, etc...
# ===========================================================================
mm1A_PA <- lme(fixed = BrayCurtisFull.amm ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + timecw  + 
                 m_EDPAcb + m_PAcb + m_ERcb + AGE + FEMALE,
               data=df,
               random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
               control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mm1B_PA <- lme(fixed = BrayCurtisRepl.amm ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + BrayCurtisNest.ammcw + timecw  + 
                 m_EDPAcb + m_PAcb + m_ERcb + BrayCurtisNest.ammcb + AGE + FEMALE,
               data=df,
               random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw + BrayCurtisNest.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
               control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mm1C_PA <- lme(fixed = BrayCurtisNest.amm ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + BrayCurtisRepl.ammcw + timecw  + 
                 m_EDPAcb + m_PAcb + m_ERcb + BrayCurtisRepl.ammcb + AGE + FEMALE,
               data=df,
               random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw + BrayCurtisRepl.ammcw | study/PARTICIPANT_ID, correlation = corAR1(),
               control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mm2A_PA <- lme(fixed = m_EDPA ~ BrayCurtisFull.ammcw +m_EDPAcwL1D +  m_PAcw+ m_ERcw+ timecw+ 
                 BrayCurtisFull.ammcb + m_PAcb+m_ERcb + AGE + FEMALE,
               data=df,
               random=~1+BrayCurtisFull.ammcw + m_PAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
               control =lmeControl(opt='optim'),na.action = na.omit)
mm2B_PA <- lme(fixed = m_EDPA ~ BrayCurtisRepl.ammcw + BrayCurtisNest.ammcw +m_EDPAcwL1D +  m_PAcw+ m_ERcw+ timecw+ 
                 BrayCurtisRepl.ammcb + BrayCurtisNest.ammcb + m_PAcb+m_ERcb + AGE + FEMALE,
               data=df,
               random=~1+BrayCurtisFull.ammcw + m_PAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
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
# emotion regulation variability calculated in a succussive temporal comparison approach
#
# object names: mms... refers to multilevel model for SENSITIVITY analysis
# ===========================================================================

# negative emotion differentiation
mms1A_NA <- lme(fixed = BrayCurtisFull.suc ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + timecw  + 
                m_EDcb + m_NAcb + m_ERcb + AGE + FEMALE,
              data=df,
              random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
              control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mms1B_NA <- lme(fixed = BrayCurtisRepl.suc ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisNest.succw + timecw  + 
                     m_EDcb + m_NAcb + m_ERcb +  BrayCurtisNest.succb + AGE + FEMALE,
                   data=df,
                   random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw +BrayCurtisNest.succw | study/PARTICIPANT_ID, correlation = corAR1(),
                   control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mms1C_NA <- lme(fixed = BrayCurtisNest.suc ~ m_EDcwL1D + m_NAcwL1D + m_ERcw + BrayCurtisRepl.succw + timecw  + 
                     m_EDcb + m_NAcb + m_ERcb +BrayCurtisRepl.succb+ AGE + FEMALE,
                   data=df,
                   random=~1+m_EDcwL1D + m_NAcwL1D+ m_ERcw + BrayCurtisRepl.succw | study/PARTICIPANT_ID, correlation = corAR1(),
                   control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mms2A_NA <- lme(fixed = m_ED ~ BrayCurtisFull.succw +m_EDcwL1D +  m_NAcw+ m_ERcw+ timecw+ 
                  BrayCurtisFull.succb + m_NAcb+m_ERcb + AGE + FEMALE,
                data=df,
                random=~1+BrayCurtisFull.succw + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                control =lmeControl(opt='optim'),na.action = na.omit)
mms2B_NA <- lme(fixed = m_ED ~ BrayCurtisNest.succw+ BrayCurtisRepl.succw +m_EDcwL1D +  m_NAcw+ m_ERcw+ timecw+ 
                  BrayCurtisNest.succb+ BrayCurtisRepl.succb  + m_NAcb+m_ERcb + AGE + FEMALE,
                data=df,
                random=~1+BrayCurtisNest.succw+ BrayCurtisRepl.succw + m_NAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                control =lmeControl(opt='optim'),na.action = na.omit)

# positive emotion differentiation
mms1A_PA <- lme(fixed = BrayCurtisFull.suc ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + timecw  + 
                      m_EDPAcb + m_PAcb + m_ERcb + AGE + FEMALE,
                    data=df,
                    random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mms1B_PA <- lme(fixed = BrayCurtisRepl.suc ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + BrayCurtisNest.succw + timecw  + 
                      m_EDPAcb + m_PAcb + m_ERcb + BrayCurtisNest.succb + AGE + FEMALE,
                    data=df,
                    random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw + BrayCurtisNest.succw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mms1C_PA <- lme(fixed = BrayCurtisNest.suc ~ m_EDPAcwL1D + m_PAcwL1D + m_ERcw + BrayCurtisRepl.succw + timecw  + 
                  m_EDPAcb + m_PAcb + m_ERcb + BrayCurtisRepl.succb + AGE + FEMALE,
                data=df,
                random=~1+m_EDPAcwL1D + m_PAcwL1D+ m_ERcw + BrayCurtisRepl.succw| study/PARTICIPANT_ID, correlation = corAR1(),
                control =list(msMaxIter = 1000, msMaxEval = 1000, opt = "optim"),na.action = na.omit)
mms2A_PA <- lme(fixed = m_EDPA ~ BrayCurtisFull.succw +m_EDPAcwL1D +  m_PAcw+ m_ERcw+ timecw+ 
                      BrayCurtisFull.succb + m_PAcb+m_ERcb + AGE + FEMALE,
                    data=df,
                    random=~1+BrayCurtisFull.ammcw + m_PAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
                    control =lmeControl(opt='optim'),na.action = na.omit)
mms2B_PA <- lme(fixed = m_EDPA ~ BrayCurtisRepl.succw + BrayCurtisNest.succw + m_EDPAcwL1D +  m_PAcw+ m_ERcw+ timecw+ 
                      BrayCurtisRepl.succb + BrayCurtisNest.succb + m_PAcb+m_ERcb + AGE + FEMALE,
                    data=df,
                    random=~1+BrayCurtisFull.ammcw + m_PAcw+ m_ERcw | study/PARTICIPANT_ID, correlation = corAR1(),
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
write.csv(outputSMTable6, "manuscript/results/SMTable5.csv")