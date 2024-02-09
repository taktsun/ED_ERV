#================================================
# Title: Check availability of data for analysis
# Date: 09-02-2024
# Copyright: Edmund Lo, checked by Dominique Maciejewski on 24-01-2024
#================================================

fileReady1 <- "dataProcessed/ReadyGVE.csv"
fileReady2 <- "dataProcessed/ReadyLeuven3W.csv"
fileReady3 <- "dataProcessed/ReadyLeuven2011.csv"
fileReady4 <- "dataProcessed/ReadyTilburg.csv"
fileReady5 <- "dataProcessed/ReadyGhent.csv"
fileReadyESM <- "dataProcessed/ReadyPooledESM.csv"
fileReadyPall <- "dataProcessed/ReadyPooledPerson.csv"
fileReadyPB4 <- "dataProcessed/ReadyPooledPersonB4Ex.csv"

sumready <- sum(file.exists(fileReady1)+
                file.exists(fileReady2)+
                file.exists(fileReady3)+
                file.exists(fileReady4)+
                file.exists(fileReady5))
sumperson <- sum(file.exists(fileReadyPall)+
                  file.exists(fileReadyPB4))
file.exists(fileReadyESM)
df <- read.csv(fileReadyESM)
dfPerson <- read.csv(fileReadyPall)
dfPersonB4Ex <- read.csv(fileReadyPB4)

if (length(unique(df$study)) != length(unique(dfPerson$study)) | 
    length(unique(dfPerson$study)) != length(unique(dfPersonB4Ex$study))){
  run.ready = FALSE
  stop("Number of datasets do not match between pooled ESM & pooled person-level datasets. Rerun prepare_data.R.")
  notexist <- TRUE
}

if (!(sumready==5 | sumready==3) | sumperson != 2 | !file.exists(fileReadyESM)){
  stop("You have an incomplete set of ready-to-analyze datasets. Run prepare_data.R first.")
  run.ready = FALSE
  notexist <- TRUE
}

if(sumready ==3 & length(unique(df$study)) ==3){
  invisible(readline(prompt="You have only 3 ready-to-analyze datasets out of the 5 datasets we analyzed. Press [enter] to continue"))
  run.ready = TRUE
  ready.5datasets = FALSE
}else if(sumready ==5 & length(unique(df$study)) ==5){
  ready.5datasets = TRUE
  run.ready = TRUE
}else{
  stop("Number of individual datasets does not match that in the pooled ESM. Run prepare_data.R first.")
  run.ready = FALSE
  notexist <- TRUE
}
