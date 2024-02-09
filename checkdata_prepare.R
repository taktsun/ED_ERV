#================================================
# Title: Check availability of data for data preparation
# Date: 09-02-2024
# Copyright: Edmund Lo, checked by Dominique Maciejewski on 24-01-2024
#================================================
fileRaw1 <- "dataRaw/rawGVE.csv"
fileRaw2 <- "dataRaw/data_downloads_1D13C1YC2Q_2023-11-02Leuven_emotions_in_daily_life_2011.csv"
fileRaw3 <- "dataRaw/data_downloads_1D13C1YC2Q_2023-11-02Leuven_3-wave_longitudinal_study.csv"
fileRaw4 <- "dataRaw/rawTilburg_ESM.sav"
fileRaw5 <- "dataRaw/rawGhent_ESM.csv"
demoRaw4 <- "dataRaw/rawTilburg_baseline.sav"
demoRaw5 <- "dataRaw/rawGhent_demo.xlsx"

filePrimary1 <- "dataPrimary/primaryGVE.csv"
filePrimary2 <- "dataPrimary/primaryLeuven3W.csv"
filePrimary3 <- "dataPrimary/primaryLeuven2011.csv"
filePrimary4 <- "dataPrimary/primaryTilburg.csv"
filePrimary5 <- "dataPrimary/primaryGhent.csv"

# if else brackets to determine how much Part 0 will be run

sumraw <- sum(file.exists(fileRaw1)+
                 file.exists(fileRaw2)+
                 file.exists(fileRaw3)+
                 file.exists(fileRaw4)+
                 file.exists(fileRaw5))
sumdemo <- file.exists(demoRaw4) + file.exists(demoRaw5)



if (file.exists(fileRaw2) + file.exists(fileRaw3) == 0){
  raw.5datasets <- FALSE
}else{
  raw.5datasets <- TRUE
}

if (sumraw == 0){
  # no raw data files, which is okay because you can run on primary data files
  invisible(readline(prompt="You have NO raw datasets. Part 0 of prepare_data.R will be skipped. Press [enter] to continue"))
  run.raw = FALSE
}else if (sumraw == 3 & sumdemo == 2 & !raw.5datasets){
  # missing datasets 2 & 3 (Leuven dataset), which is okay because you can run on a subset of 3 datasets on Github
  invisible(readline(prompt="You have only 3 raw datasets out of the 5 datasets we analyzed. Press [enter] to continue"))
  run.raw = TRUE
}else if (sumraw == 5 & sumdemo == 2){
  # you have all raw data files, which is okay
  run.raw = TRUE
}else{
  stop("You have an incomplete set of raw datasets. Check README to obtain the datasets.")
  notexist <- TRUE
  run.raw = FALSE
}

# if else brackets to determine how much the rest will be run

if (run.raw){
  # if Part 0: curate primary data has been run, the rest of the code can be run
  run.primary = TRUE
  primary.5datasets = raw.5datasets
}else{
  # if not, check availability of primary data
  if (sum(file.exists(filePrimary1)+
          file.exists(filePrimary2)+
          file.exists(filePrimary3)+
          file.exists(filePrimary4)+
          file.exists(filePrimary5)) == 5){
    primary.5datasets = TRUE
    run.primary = TRUE
  }else if (sum(file.exists(filePrimary2)+file.exists(filePrimary3)) == 0 &
            sum(file.exists(filePrimary1)+
                file.exists(filePrimary4)+
                file.exists(filePrimary5)) == 3){
    invisible(readline(prompt="You have only 3 primary datasets out of the 5 datasets we analyzed. Press [enter] to continue"))
    primary.5datasets = FALSE
    run.primary = TRUE
  }else{
    run.primary = FALSE
    stop("You have an incomplete set of primary datasets. Check README to obtain the datasets.")
  }
}

