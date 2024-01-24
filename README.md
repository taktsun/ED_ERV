# Readme <a href='https://osf.io/zcvbs/'><img src='worcs_icon.png' align="right" height="139" /></a>

This is the github of my project ["Naming before Taming? Emotion Differentiation and Emotion Regulation Variability Hinder Each Other within Adolescents."](https://osf.io/cq6n4/). You can reproduce the analysis results in the manuscript following this readme. 

<!-- Please add a brief introduction to explain what the project is about    -->

## Where do I start?

You can load this project in RStudio by opening the file called 'EDpredictsERV.Rproj'.

## Project structure

Tables of R scripts, other files, and folders.

### R scripts

Read "list_of_variables.R" to familiarize yourself with how we named variables.

File                      | Description                | Usage         
------------------------- | -------------------------- | --------------
prepare_data.R            | Curate primary data from raw data; pre-process primary data into ready-to-analyze data | Optional because ready-to-analyze data are already in .csv
desstat&MCFA.R                 | Demographic info, descriptive statistics, and psychometrics (e.g., reliability of measures) | Run to reproduce results
megaanalysis.R                 | Multilevel models (confirmatory & exploratory)  | Run to reproduce results
poweranalysis.R                | Power analysis in pre-registration| Run to reproduce results
list_of_variables.R                | List of variable names of ESM measures and momentary indices| Required; read only
func_preprocessing.R                 | Functions for pre-processing data (in prepare_data.R) | Required; read only




### Project Files

File                      | Description                | Usage         
------------------------- | -------------------------- | --------------
EDpredictsERV.Rproj      | Project file               | Loads project 
sim1_input.RData      | Simulation 1 parameter input with seed               | README.md                 | Description of project     | Read only
LICENSE                   | User permissions           | Read only     
.worcs                    | WORCS metadata YAML        | Read only     
renv.lock                 | Reproducible R environment | Read only     


### Folders
Folder| Description                | Usage         
------------------------- | -------------------------- | --------------
manuscript | Folder that holds manuscript markdown files and pdfs (empty at the moment)      | Read only
dataRaw| Raw data files. An empty folder because raw data with potential personal identifiers are not uploaded.| Read only     
dataPrimary| Primary data files, which means anonymized raw data without any further pre-processing | Read only     
dataProcessed| Ready-to-analyze data files | Read only     




<!--  You can consider adding the following to this file:                    -->
<!--  * A citation reference for your project                                -->
<!--  * Contact information for questions/comments                           -->
<!--  * How people can offer to contribute to the project                    -->
<!--  * A contributor code of conduct, https://www.contributor-covenant.org/ -->

# Reproducibility
Reproduce the results by these 5 steps.

 1. Install RStudio and R
 2. Install WORCS dependencies
		
		install.packages("worcs", dependencies = TRUE)
		tinytex::install_tinytex()
		renv::consent(provided = TRUE)
		
 3. [Clone](https://resources.github.com/github-and-rstudio/#:~:text=Clone%20the%20repository%20with%20RStudio&text=On%20GitHub%2C%20navigate%20to%20the,RStudio%20on%20your%20local%20environment.) this repo (https://github.com/taktsun/ED_ERV) to your RStudio
 4. Restore the package dependencies
	

	    renv::restore()
	    
	    
 5. Run 4 R scripts to reproduce the results. Start new R session (Ctrl+Shift+F10 in Windows) and clear all environment variables before you run each R script.
 
	- prepare_data.R -> that was quite slow, mostly due to zero_variance code
	- desstat&MCFA.R -> Added new MCFA models, because the model fit did not look super good the the within-level for dataset 3, 4 and 5. Modification indices suggested that including correlations between residual variances of like items should help and it did. 
	- megaanalysis.R -> works, see comments
	- poweranalysis.R -> works, see comments

*PLUS, SHOULDNT WE ALSO ADD THAT THEY CAN THEN RUN THE MANUSCRIPT RMD FILE TO KNIT THE MANUSCRIPT?*
*Plus, we need to add information about the data.*
*We definitely need to add the folders, cause now I could only run the script if I myself manually added the folders.* 
*I also had to add the results folder in the manuscript*

Step 1 to 4 are detailed in the vignette on [reproducing a WORCS project](https://cjvanlissa.github.io/worcs/articles/reproduce.html).


### Adherence to WORCS

This project uses the Workflow for Open Reproducible Code in Science (WORCS) to ensure transparency and reproducibility. The workflow is designed to meet the principles of Open Science throughout a research project.

### More about WORCS

To learn how WORCS helps researchers meet the TOP-guidelines and FAIR principles, read the preprint at https://osf.io/zcvbs/
