# =====================================================
# Title: List of variables
# Date: 24-1-2024
# Copyright: Edmund Lo, checked by Dominique Maciejewski
# There are 4 parts:
# 1. variable names of each dataset
# 2. variable names of momentary indices
# 3. variable names of the pooled dataset
# 4. variable names for producing figures
# =====================================================


# =====================================================
# 1. variable names of each dataset
# =====================================================

# Dataset 1: Radboud GVE (Gvoed voor elkaar)
inputPA.GVE <- c("PA_1", "PA_2", "PA_3", "PA_4")
inputNA.GVE <- c("NA_1", "NA_2", "NA_3", "NA_4", "LONELINESS")
inputER.GVE <- c("ACCEPTANCE_ESM", "COGAPPRAISAL_ESM", "EXPSUPPRESSION_ESM", 
                 "RUMINATION_ESM", "SHARING_ESM")
labelPA.GVE <- c("Content", "Relaxed", "Joyful", "Energetic")
labelNA.GVE <- c("Irritated", "Worried", "Depressed", "Insecure", "Lonely")
labelER.GVE <- c("Acceptance", "Reappraisal", "Suppression", 
                 "Rumination", "Social Sharing")
# Dataset 2: Leuven daily emotions 2011
inputPA.Leuven2011 <- c("RLX_ES","HAP_ES")
inputNA.Leuven2011 <- c("ANG_ES","ANX_ES","DEP_ES","SAD_ES")
inputER.Leuven2011 <- c("DIST_ES","REAP_ES","REFL_ES","RUM_ES","SOCSHR_ES", "SUPR_ES")

labelPA.Leuven2011 <- c("Relaxed","Happy")
labelNA.Leuven2011 <- c("Angry","Anxious","Depressed","Sad")
labelER.Leuven2011 <- c("Distraction","Reappraisal","Reflection","Rumination","Social Sharing", "Suppression")
# Dataset 3: Leuven 3-wave study
inputPA.Leuven3W <- c("RLX_ES","HAP_ES","CHEER_ES")
inputNA.Leuven3W <- c("ANG_ES","DEP_ES","LONE_ES","FEAR_ES","SAD_ES","STR_ES")
inputER.Leuven3W <- c("DIST_ES","PERS_ES","SOCSHR_ES","RUM_1_ES","RUM_2_ES","SUPR_ES")
labelPA.Leuven3W <- c("Relaxed","Happy","Cheerful")
labelNA.Leuven3W <- c("Angry","Depressed","Lonely","Anxious","Sad","Stressed")
labelER.Leuven3W <- c("Distraction","Reappraisal","Social Sharing","Rumination","Worry","Suppression")
# Dataset 4: Tilburg
inputPA.Tilburg <- c("PA_ener","PA_cont","PA_enth","PA_deter","PA_calm","PA_joy","PA_grat")
inputNA.Tilburg <- c("NA_irri","NA_bor","NA_nerv","NA_sad","NA_ang","NA_low")
inputER.Tilburg <- c("NER_distr","NER_avoid","NER_rumin","NER_probls",
                     "NER_accep","NER_socsh","NER_cobrood")
labelPA.Tilburg <- c("Energetic","Content","Enthusiastic","Deteremined","Calm","Joyful","Grateful")
labelNA.Tilburg <- c("Irritated","Bored","Nervous","Sad","Angry","Depressed")
labelER.Tilburg <- c("Distraction","Avoidance","Rumination","Problem Solving",
                     "Acceptance","Social Sharing","Co-Brooding")
# Dataset 5: Ghent
inputPA.Ghent <- c("Emow_happy","Emow_relaxed","Emow_energetic")
inputNA.Ghent <- c("Emow_angry","Emow_annoyed","Emow_anxious","Emow_sad","Emow_stressed","Emow_uncertain")
inputER.Ghent <- c("Emotionregulation_reappraisal","Emotionregulation_distraction",
                   "Emotionregulation_socialsupport","Emotionregulation_suppresion",
                   "Emotionregulation_rumination","Emotionregulation_selfcompassion1",
                   "Emotionregulation_selfcompassion2","Emotionregulation_engagement")
labelPA.Ghent <- c("Happy","Relaxed","Energetic")
labelNA.Ghent <- c("Angry","Annoyed","Anxious","Sad","Stressed","Uncertain")
labelER.Ghent <- c("Reappraisal","Distraction",
                   "Social Support","Suppression",
                   "Rumination","Self-compassion (Support)",
                   "Self-compssion (Cheer-up)","Expression")

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

# momentary indices suffix
# cw & cb: Within-adolescent component (cw) and between-adolescent component (cb)
# L1D is a lagged variable that assigns NA to overnight beeps

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
poolinfo_ED <- c("m_ED","m_EDL1D","m_EDcw","m_EDcb","m_EDcwL1D","c_ED",
                 "m_EDPA","m_EDPAL1D","m_EDPAcw","m_EDPAcb","m_EDPAcwL1D","c_EDPA")

# adolescent-level (not mean-centered) indices
poolinfo_person <- c( # mean
                     "person_PA","person_NA","person_ER","person_ED","person_EDPA",
                     "person_BrayCurtisFull.amm","person_BrayCurtisRepl.amm", "person_BrayCurtisNest.amm",
                     "person_BrayCurtisFull.suc","person_BrayCurtisRepl.suc", "person_BrayCurtisNest.suc",
                      # sd
                     "person_PA.sd","person_NA.sd","person_ER.sd","person_ED.sd","person_EDPA.sd",
                     "person_BrayCurtisFull.amm.sd","person_BrayCurtisRepl.amm.sd", "person_BrayCurtisNest.amm.sd",
                     "person_BrayCurtisFull.suc.sd","person_BrayCurtisRepl.suc.sd", "person_BrayCurtisNest.suc.sd",
                     # skewness
                     "person_PA.skew","person_NA.skew","person_ER.skew","person_ED.skew","person_EDPA.skew",
                     "person_BrayCurtisFull.amm.skew","person_BrayCurtisRepl.amm.skew", "person_BrayCurtisNest.amm.skew",
                     "person_BrayCurtisFull.suc.skew","person_BrayCurtisRepl.suc.skew", "person_BrayCurtisNest.suc.skew"
)

# Covariates.
# m_NA: momentary NEGATIVE emotion differentiation
# m_PA: momentary POSITIVE emotion differentiation
# timecw: time mean-centered
poolinfo_ctrl <- c("m_NA","m_NAcw","m_NAcb","m_NAcwL1D",
                   "m_PA","m_PAcw","m_PAcb","m_PAcwL1D",
                   "m_ER","m_ERcw","m_ERcb",
                   "timecw"
)

# =====================================================
# 4. variable names of for producing figures
# =====================================================

index_seed <- c("aperson_PA",
                "cperson_NA",
                "eperson_ER",
                "dperson_ED",
                "bperson_EDPA",
                "fperson_BrayCurtisFull.amm",
                "hperson_BrayCurtisRepl.amm",
                "gperson_BrayCurtisNest.amm",
                "iperson_BrayCurtisFull.amm",
                "kperson_BrayCurtisRepl.amm",
                "jperson_BrayCurtisNest.amm"
)
index_seed_label <- c("aperson_PA",
                      "bperson_EDPA",
                      "cperson_NA",
                      "dperson_ED",
                      "eperson_ER",
                      "fperson_BrayCurtisFull.amm",
                      "gperson_BrayCurtisNest.amm",
                      "hperson_BrayCurtisRepl.amm",
                      "iperson_BrayCurtisFull.amm",
                      "kperson_BrayCurtisRepl.amm",
                      "jperson_BrayCurtisNest.amm"
)
index_order <- c(paste0(index_seed,".m"),
                 paste0(index_seed,".sd"),
                 paste0(index_seed,".skew"))
colour_order <- c("#332288",
                  "#88CCEE",
                  "#882255",
                  "#CC6677",
                  "#DDCC77",
                  "#117733",
                  "#999933",
                  "#44AA99")
