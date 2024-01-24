# =====================================================
# List of variables
# Date: 24-1-2024
# Copyright: Edmund Lo, checked by Dominique Maciejewski
# There are 3 parts:
# 1. variable names of each dataset
# 2. variable names of momentary indices
# 3. variable names of the pooled dataset
# =====================================================


# =====================================================
# 1. variable names of each dataset
# =====================================================

# Dataset 1: Radboud GVE (Gvoed voor elkaar)
inputPA.GVE <- c("PA_1", "PA_2", "PA_3", "PA_4")
inputNA.GVE <- c("NA_1", "NA_2", "NA_3", "NA_4", "LONELINESS")
inputER.GVE <- c("ACCEPTANCE_ESM", "COGAPPRAISAL_ESM", "EXPSUPPRESSION_ESM", 
                 "RUMINATION_ESM", "SHARING_ESM")
# Dataset 2: Leuven daily emotions 2011
inputPA.Leuven2011 <- c("RLX_ES","HAP_ES")
inputNA.Leuven2011 <- c("ANG_ES","ANX_ES","DEP_ES","SAD_ES")
inputER.Leuven2011 <- c("DIST_ES","REAP_ES","REFL_ES","RUM_ES","SOCSHR_ES", "SUPR_ES")
# Dataset 3: Leuven 3-wave study
inputPA.Leuven3W <- c("RLX_ES","HAP_ES","CHEER_ES")
inputNA.Leuven3W <- c("ANG_ES","DEP_ES","LONE_ES","FEAR_ES","SAD_ES","STR_ES")
inputER.Leuven3W <- c("DIST_ES","PERS_ES","SOCSHR_ES","RUM_1_ES","RUM_2_ES","SUPR_ES")
# Dataset 4: Tilburg
inputPA.Tilburg <- c("PA_ener","PA_cont","PA_enth","PA_deter","PA_calm","PA_joy","PA_grat")
inputNA.Tilburg <- c("NA_irri","NA_bor","NA_nerv","NA_sad","NA_ang","NA_low")
inputER.Tilburg <- c("NER_distr","NER_avoid","NER_rumin","NER_probls",
                     "NER_accep","NER_socsh","NER_cobrood")
# Dataset 5: Ghent
inputPA.Ghent <- c("Emow_happy","Emow_relaxed","Emow_energetic")
inputNA.Ghent <- c("Emow_angry","Emow_annoyed","Emow_anxious","Emow_sad","Emow_stressed","Emow_uncertain")
inputER.Ghent <- c("Emotionregulation_reappraisal","Emotionregulation_distraction",
                   "Emotionregulation_socialsupport","Emotionregulation_suppresion",
                   "Emotionregulation_rumination","Emotionregulation_selfcompassion1",
                   "Emotionregulation_selfcompassion2","Emotionregulation_engagement")

# =====================================================
# 2. variable names of momentary indices
# =====================================================

inputIndices <- c("m_PA",                   # momentary intensity of all positive emotions  
                  "m_EDPA",                 # momentary POSITIVE emotion differentiation
                  "m_NA",                   # momentary intensity of all negative emotions
                  "m_ED",                   # momentary NEGATIVE emotion differentiation
                  "m_ER",                   # momentary intensity of all ER strategies
                  "BrayCurtisFull.amm",     # momentary emotion regulation variability (full index)
                  "BrayCurtisNest.amm",     # momentary emotion regulation variability (endorsement change subcomponent)
                  "BrayCurtisRepl.amm")     # momentary emotion regulation variability (strategy switching subcomponent)
labelIndices <- c("Positive emotion intensity",
                  "Positive emotion differentiation",
                  "Negative emotion intensity",
                  "Negative emotion differentiation",
                  "Emotion regulation intensity",
                  "Emotion regulation variability",
                  "Endorsement change",
                  "Strategy switching")

# =====================================================
# 3. variable names of the pooled dataset
# =====================================================
    
poolinfo_demo <- c("PARTICIPANT_ID",    # anonymized participant ID
                   "BEEP",              # beep number
                   "DAY",               # day of ESM
                   "AGE",               # age (rounded to integer)
                   "FEMALE",            # 1 = female, 0 = male
                   "COB",               # country of birth or ethnicity; 1 = majority (Dutch or Belgian), 0 = other
                   "beepperday",        # number of beeps per day
                   "study",             # which dataset it is
                   "filledESMpc")       # % of ESM observations completed per adolescent

poolinfo_BC <- c(
  # Bray-curtis dissimilarity: full index, switching, endorsement change. All-moment comparison
   "BrayCurtisFull.amm","BrayCurtisRepl.amm","BrayCurtisNest.amm",
  # Bray-curtis dissimilarity: full index, switching, endorsement change. Successive comparison
   "BrayCurtisFull.suc","BrayCurtisRepl.suc","BrayCurtisNest.suc",
  # Within-adolescent component (cw) and between-adolescent component (cb)
  "BrayCurtisFull.ammcw","BrayCurtisRepl.ammcw","BrayCurtisNest.ammcw",
   "BrayCurtisFull.ammcb","BrayCurtisRepl.ammcb","BrayCurtisNest.ammcb",
   "BrayCurtisFull.succw","BrayCurtisRepl.succw","BrayCurtisNest.succw",
   "BrayCurtisFull.succb","BrayCurtisRepl.succb","BrayCurtisNest.succb")

# Emotion differentiation indices
# m_ED:   Negative emotion differentiation
# m_EDPA: Positive emotion differentiation
# cw & cb: Within-adolescent component (cw) and between-adolescent component (cb)
# L1D & L1N: lagged variable. L1D took into consideration of day change and L1N didn't
poolinfo_ED <- c("m_ED","m_EDL1D","m_EDcw","m_EDcb","m_EDcwL1D","m_EDcwL1N","m_EDL1N", "c_ED",
                 "m_EDPA","m_EDPAL1D","m_EDPAcw","m_EDPAcb","m_EDPAcwL1D","m_EDPAcwL1N","m_EDPAL1N", "c_EDPA")

# adolescent-level (not mean-centered) indices
poolinfo_person <- c("person_PA","person_NA","person_ER","person_ED","person_EDPA",
                     "person_BrayCurtisFull.amm","person_BrayCurtisRepl.amm", "person_BrayCurtisNest.amm",
                     "person_BrayCurtisFull.suc","person_BrayCurtisRepl.suc", "person_BrayCurtisNest.suc")

# Covariates.
# m_NA: momentary NEGATIVE emotion differentiation
# m_PA: momentary POSITIVE emotion differentiation
# timecw: time mean-centered
# cw & cb: Within-adolescent component (cw) and between-adolescent component (cb)
# L1D & L1N: lagged variable. L1D took into consideration of day change and L1N didn't
poolinfo_ctrl <- c("m_NA","m_NAcw","m_NAcb","m_NAcwL1N","m_NAcwL1D",
                   "m_PA","m_PAcw","m_PAcb","m_PAcwL1N","m_PAcwL1D",
                   "m_ER","m_ERcw","m_ERcb",
                   "timecw"
)