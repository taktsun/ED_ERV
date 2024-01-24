# ===========================================================================
# Title: functions for data pre-processing
# Date: 24-1-2024
# Copyright: Edmund Lo, checked by Dominique Maciejewski
# ===========================================================================

# Harmonize a list of variables according to the input "master" min and max across all datasets (needed for ESM variables)
harmonize <- function(df,listvar,datasetmin,datasetmax,mastermin,mastermax){
  # set the minimum raw scores to 0
  df[, listvar] <- df[, listvar] - datasetmin
  datasetmax <- datasetmax - datasetmin
  # scale the raw scores to "master" min and max
  df[, listvar] <- df[, listvar]/datasetmax * mastermax + mastermin
  df
}

# detect whether there are zero variance across multiple variables across ESM observations (pre-registered exclusion criterion)
zerovariance <- function(dftemp,variables_to_check){
  # Group by PPID and check if the specified variables remain the same in all rows
  dftemp <- dftemp[complete.cases(dftemp[, variables_to_check]), ]
  same_values_df <- dftemp %>%
    group_by(PARTICIPANT_ID) %>%
    filter(all(across(all_of(variables_to_check), ~. == first(.))))
  # Extract the list of PPID values
  unique_ppid_list <- unique(same_values_df$PARTICIPANT_ID)
  # Print the list of PPID values
  unique_ppid_list
}

# ===============================================================
# Function that calculates Bray-Curtis dissimilarity for ESM data
# Version 2024-01-23
# https://github.com/taktsun/dissimilarity-for-ESM-data/blob/5f78208b23d6a4a781578156cff3ebb0677bee4f/BrayCurtisDissimilarity_Calculate.R
# ===============================================================
calcBrayCurtisESM <- function (d, vn, pid, tid, bSubnarm = TRUE, bPersonnarm = TRUE, multiplyby = 1, addby = 0.001){
  m<- d[,vn]
  # T/F values for each observation
  d$b_firstbeep <- as.logical(d[tid]==ave(d[tid], d[pid], FUN = min))
  d$b_completeER <- complete.cases(m)
  
  # Calculate Bray-Curtis dissimilarity (successive difference)
  d$BrayCurtisFull.suc <-NA
  d$BrayCurtisRepl.suc <-NA
  d$BrayCurtisNest.suc <-NA
  
  # calculating by loop on pairwise dissimilarity because calculating dissimilarity for large matrix takes a long time
  for (i in 1:nrow(d)){
    if(!d$b_firstbeep[i]){ # first beep of each person remains NA
      tempmat <- rbind(d[i,vn],setNames(d[i-1,vn],names(d[i,vn]))) + addby 
      # add a small constant so that two observations with zero in every variable will return value 0 (no dissimilarity)
      
      tempres <- bray.part(tempmat)
      d$BrayCurtisFull.suc[i] <- tempres$bray[1]
      d$BrayCurtisRepl.suc[i] <- tempres$bray.bal[1]
      d$BrayCurtisNest.suc[i] <- tempres$bray.gra[1]
    }
  }
  # Calculate Bray-Curtis dissimilarity (all-moment comparison)
  # exclude rows with all NA or only 1 available score, because bray.part needs at least 2 variables for calculation
  dTemp <- d[rowSums(is.na(d[,vn])) < (ncol(d[,vn])-1),
             append(c(pid,tid),vn)] #include only ER items and binding info
  
  vbray.full <- NULL
  vbray.repl <- NULL
  vbray.nest <- NULL
  for (i in 1:length(unique(dTemp[,pid]))){
    dPerson <- dTemp[dTemp[,pid]==unique(dTemp[,pid])[i],] # create a temp d for each person
    
    
    # add a small constant so that two observations with zero in every variable will return value 0 (no dissimilarity)
    matx <- dPerson[,vn]+ addby 
    
    nobs <- nrow(dPerson)
    resbray <- bray.part(matx)
    if (nobs ==1){
      vbray.full <- append(vbray.full,NA)
      vbray.repl <- append(vbray.repl,NA)
      vbray.nest <- append(vbray.nest,NA)
    }else{
      vtemp <- colMeans(as.matrix(resbray$bray),na.rm=bSubnarm)
      vtemp <- ifelse(bSubnarm & is.na(as.matrix(resbray$bray)[,1]),as.matrix(resbray$bray)[,1],vtemp)
      vbray.full <- append(vbray.full,vtemp*nobs/(nobs-1))
      vtemp <- colMeans(as.matrix(resbray$bray.bal),na.rm=bSubnarm)
      vtemp <- ifelse(bSubnarm & is.na(as.matrix(resbray$bray.bal)[,1]),as.matrix(resbray$bray.bal)[,1],vtemp)
      vbray.repl <- append(vbray.repl,vtemp*nobs/(nobs-1))
      vtemp <- colMeans(as.matrix(resbray$bray.gra),na.rm=bSubnarm)
      vtemp <- ifelse(bSubnarm & is.na(as.matrix(resbray$bray.gra)[,1]),as.matrix(resbray$bray.gra)[,1],vtemp)
      vbray.nest <- append(vbray.nest,vtemp*nobs/(nobs-1))
    }
  }
  
  dTemp$BrayCurtisFull.amm <- vbray.full * multiplyby
  dTemp$BrayCurtisRepl.amm <- vbray.repl * multiplyby
  dTemp$BrayCurtisNest.amm <- vbray.nest * multiplyby
  
  dTemp<- dTemp[,setdiff(names(dTemp), vn)] # remove vn cols
  d<- merge(d,dTemp, by=c(pid,tid),all=TRUE)
  
  d$BrayCurtisFull.suc <-d$BrayCurtisFull.suc * multiplyby
  d$BrayCurtisRepl.suc <- d$BrayCurtisRepl.suc* multiplyby
  d$BrayCurtisNest.suc <- d$BrayCurtisNest.suc* multiplyby
  
  
  # calculate person-mean of dissimilarity
  d$person_BrayCurtisFull.suc <- ave(d$BrayCurtisFull.suc, d[pid], FUN = function(x) mean(x, na.rm = bPersonnarm))
  d$person_BrayCurtisRepl.suc <- ave(d$BrayCurtisRepl.suc, d[pid], FUN = function(x) mean(x, na.rm = bPersonnarm))
  d$person_BrayCurtisNest.suc <- ave(d$BrayCurtisNest.suc, d[pid], FUN = function(x) mean(x, na.rm = bPersonnarm))
  d$person_BrayCurtisFull.amm <- ave(d$BrayCurtisFull.amm, d[pid], FUN = function(x) mean(x, na.rm = bPersonnarm))
  d$person_BrayCurtisRepl.amm <- ave(d$BrayCurtisRepl.amm, d[pid], FUN = function(x) mean(x, na.rm = bPersonnarm))
  d$person_BrayCurtisNest.amm <- ave(d$BrayCurtisNest.amm, d[pid], FUN = function(x) mean(x, na.rm = bPersonnarm))
  
  d
}


# ===============================================================
# Function to calculate all momentary indices
# including moment-level emotion differentiation and emotion regulation variability
# ===============================================================

calcDynamics <- function(df, inputNA, inputER, inputPA){
  
  # momentary positive/negative emotion intensity
    df$m_PA <- rowMeans(df[,inputPA])
    df$m_NA <- rowMeans(df[,inputNA])
  # momentary emotion regulation intensity
    df$m_ER <- rowMeans(df[,inputER])
  
  # within-adolescent component (person-mean-center) of negative and positive emotion intensity
    df$m_PAcw <- calc.mcent(m_PA, PARTICIPANT_ID, data=df)
    df$m_NAcw <- calc.mcent(m_NA, PARTICIPANT_ID, data=df)
  # within-adolescent component (person-mean-center) of emotion regulation intensity
    df$m_ERcw <- calc.mcent(m_ER, PARTICIPANT_ID, data=df)
  
  #person-mean-center time
    df$timecw <- calc.mcent(BEEP, PARTICIPANT_ID, data=df)
  
  # Calculate Negative emotion differentiation
    # remove NA because emodiff package cannot handle NA
    dfCalcICC <- df
    dfCalcICC <- dfCalcICC[complete.cases(dfCalcICC[, inputNA]), ]
    # We specified allow_neg_icc = TRUE (i.e., negative values to be allowed) to have the maximum number of available assessments
    dfCalcICC <- calculate_ed(dat = dfCalcICC, emotions = inputNA, PARTICIPANT_ID, allow_neg_icc = TRUE)
    # within-adolescent component (person-mean-center) of emotion emotion differentiation
    dfCalcICC$m_EDcw <- calc.mcent(m_ED,PARTICIPANT_ID,data=dfCalcICC)
    dfCalcICC <- dfCalcICC[,c("PARTICIPANT_ID","BEEP",tail(names(dfCalcICC), 7))]
  
  # Calculate Positive emotion differentiation
    # remove NA for ICC calculation from emodiff package
    dfCalcICCPA <- df
    dfCalcICCPA <- dfCalcICCPA[complete.cases(dfCalcICCPA[, inputPA]), ]
    # We specified allow_neg_icc = TRUE (i.e., negative values to be allowed) to have the maximum number of available assessments
    dfCalcICCPA <- calculate_ed(dat = dfCalcICCPA, emotions = inputPA, PARTICIPANT_ID, allow_neg_icc = TRUE)
    dfCalcICCPA <- dfCalcICCPA %>%
      rename_with(~paste0(., "PA"), all_of(tail(names(dfCalcICCPA), 6)))
    dfCalcICCPA$m_EDPAcw <- calc.mcent(m_EDPA,PARTICIPANT_ID,data=dfCalcICCPA)
    dfCalcICCPA <- dfCalcICCPA[,c("PARTICIPANT_ID","BEEP",tail(names(dfCalcICCPA), 7))]
    
    dfMerge <-merge(df,dfCalcICC,by = c("PARTICIPANT_ID","BEEP"), all=TRUE)
    dfMerge <-merge(dfMerge,dfCalcICCPA,by = c("PARTICIPANT_ID","BEEP"),all=TRUE)
  
  # calculate emotion regulation variability (dBC) 
    df <- calcBrayCurtisESM(dfMerge,inputER,"PARTICIPANT_ID","BEEP", multiplyby = 10, addby = 0.001)
    
  # Create lag(1) state ED, state NA. WITH consideration of DAY
    df$m_EDL1D <- lagvar(m_ED, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
    df$m_EDcwL1D <- lagvar(m_EDcw, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
    df$m_EDPAL1D <- lagvar(m_EDPA, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
    df$m_EDPAcwL1D <- lagvar(m_EDPAcw, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
    df$m_PAcwL1D <- lagvar(m_PAcw, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
    df$m_NAcwL1D <- lagvar(m_NAcw, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
    df$BrayCurtisFull.sucL1D <- lagvar(BrayCurtisFull.suc, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
    df$BrayCurtisRepl.sucL1D <- lagvar(BrayCurtisRepl.suc, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
    df$BrayCurtisNest.sucL1D <- lagvar(BrayCurtisNest.suc, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
  
  # calculate within-person component of Bray-Curtis dissimilarity
    df$BrayCurtisFull.ammcw <- calc.mcent(BrayCurtisFull.amm, PARTICIPANT_ID, data=df)
    df$BrayCurtisRepl.ammcw <- calc.mcent(BrayCurtisRepl.amm, PARTICIPANT_ID, data=df)
    df$BrayCurtisNest.ammcw <- calc.mcent(BrayCurtisNest.amm, PARTICIPANT_ID, data=df)
    df$BrayCurtisFull.succw <- calc.mcent(BrayCurtisFull.suc, PARTICIPANT_ID, data=df)
    df$BrayCurtisRepl.succw <- calc.mcent(BrayCurtisRepl.suc, PARTICIPANT_ID, data=df)
    df$BrayCurtisNest.succw <- calc.mcent(BrayCurtisNest.suc, PARTICIPANT_ID, data=df)
    df$BrayCurtisFull.succwL1D <- lagvar(BrayCurtisFull.succw, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
    df$BrayCurtisRepl.succwL1D <- lagvar(BrayCurtisRepl.succw, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
    df$BrayCurtisNest.succwL1D <- lagvar(BrayCurtisNest.succw, id=PARTICIPANT_ID, obs=BEEP, day=DAY, data=df)
  
  
  # calculate person and grand means and the between-person components of all variables
    df$person_PA <- calc.mean(m_PA, PARTICIPANT_ID, data=df,expand=TRUE)
    df$person_NA <- calc.mean(m_NA, PARTICIPANT_ID, data=df,expand=TRUE)
    df$person_ER <- calc.mean(m_ER, PARTICIPANT_ID, data=df,expand=TRUE)
    df$person_ED <- calc.mean(m_ED, PARTICIPANT_ID, data=df,expand=TRUE)
    df$person_EDPA <- calc.mean(m_EDPA, PARTICIPANT_ID, data=df,expand=TRUE)
    
    df$grand_ER <- mean(calc.mean(m_ER, PARTICIPANT_ID, data=df),na.rm=TRUE)
    df$grand_PA <- mean(calc.mean(m_PA, PARTICIPANT_ID, data=df),na.rm=TRUE)
    df$grand_NA <- mean(calc.mean(m_NA, PARTICIPANT_ID, data=df),na.rm=TRUE)
    df$grand_ED <-mean(calc.mean(m_ED, PARTICIPANT_ID, data=df),na.rm=TRUE)
    df$grand_EDPA <-mean(calc.mean(m_EDPA, PARTICIPANT_ID, data=df),na.rm=TRUE)
    df$grand_BrayCurtisFull.amm <-mean(calc.mean(BrayCurtisFull.amm, PARTICIPANT_ID, data=df),na.rm=TRUE)
    df$grand_BrayCurtisRepl.amm <-mean(calc.mean(BrayCurtisRepl.amm, PARTICIPANT_ID, data=df),na.rm=TRUE)
    df$grand_BrayCurtisNest.amm <-mean(calc.mean(BrayCurtisNest.amm, PARTICIPANT_ID, data=df),na.rm=TRUE)
    df$grand_BrayCurtisFull.suc <-mean(calc.mean(BrayCurtisFull.suc, PARTICIPANT_ID, data=df),na.rm=TRUE)
    df$grand_BrayCurtisRepl.suc <-mean(calc.mean(BrayCurtisRepl.suc, PARTICIPANT_ID, data=df),na.rm=TRUE)
    df$grand_BrayCurtisNest.suc <-mean(calc.mean(BrayCurtisNest.suc, PARTICIPANT_ID, data=df),na.rm=TRUE)
    
    df$m_ERcb <- df$person_ER - df$grand_ER
    df$m_PAcb <- df$person_PA - df$grand_PA
    df$m_NAcb <- df$person_NA - df$grand_NA
    df$m_EDcb <- df$person_ED - df$grand_ED
    df$m_EDPAcb <- df$person_EDPA - df$grand_EDPA
    df$BrayCurtisFull.ammcb <- df$person_BrayCurtisFull.amm - df$grand_BrayCurtisFull.amm
    df$BrayCurtisRepl.ammcb <- df$person_BrayCurtisRepl.amm - df$grand_BrayCurtisRepl.amm
    df$BrayCurtisNest.ammcb <- df$person_BrayCurtisNest.amm - df$grand_BrayCurtisNest.amm
    df$BrayCurtisFull.succb <- df$person_BrayCurtisFull.suc - df$grand_BrayCurtisFull.suc
    df$BrayCurtisRepl.succb <- df$person_BrayCurtisRepl.suc - df$grand_BrayCurtisRepl.suc
    df$BrayCurtisNest.succb <- df$person_BrayCurtisNest.suc - df$grand_BrayCurtisNest.suc
  
  # return the dataframe after calculation
    df
}
