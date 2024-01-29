
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

### Manuscript

All these files are under the [manuscript folder](manuscript).

File                      | Description                | Usage         
------------------------- | -------------------------- | --------------
apa7.csl            | Citation Style Language (APA 7th edition) | Required; read only                 | Demographic info, descriptive statistics, and psychometrics (e.g., reliability of measures) | Run to reproduce results
EDERV_manuscript.Rmd                 | R Markdown file for dynamically producing our manuscript  | Run to reproduce our manuscript
mainreferences.bib                | BibTeX reference for our manuscript| Required; read only
datasets_and_OSF.bib                | BibTeX reference related to our datasets and supplemental materials| Required; read only
r-references.bib                 | BibTeX reference related to the R Markdown files | Required; read only





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
	    
	    
 5. Run 4 R scripts to reproduce the results. Start new R session (Ctrl+Shift+F10 in R studio on Windows) and clear all environment variables before you run each R script.
 
	- prepare_data.R 
	- desstat&MCFA.R 
	- megaanalysis.R 
	- poweranalysis.R 

 6. Knit EDERV_manuscript.Rmd (Ctrl+Shift+K in R studio on Windows) to generate our manuscript.

Step 1 to 4 are detailed in the vignette on [reproducing a WORCS project](https://cjvanlissa.github.io/worcs/articles/reproduce.html).


### Adherence to WORCS

This project uses the Workflow for Open Reproducible Code in Science (WORCS) to ensure transparency and reproducibility. The workflow is designed to meet the principles of Open Science throughout a research project.

### More about WORCS

To learn how WORCS helps researchers meet the TOP-guidelines and FAIR principles, read the preprint at https://osf.io/zcvbs/
