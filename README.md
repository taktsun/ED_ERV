

# Readme <a href='https://osf.io/zcvbs/'><img src='worcs_icon.png' align="right" height="139" /></a>

This is the github of my project ["Naming before Taming? Emotion Differentiation and Emotion Regulation Variability Hinder Each Other within Adolescents"](https://osf.io/cq6n4/). You can reproduce the analysis results in the manuscript following this readme. 

<!-- Please add a brief introduction to explain what the project is about    -->

## Where do I start?

You can load this project in RStudio by opening the file called 'EDpredictsERV.Rproj'.

## Project structure

Tables of R scripts, other files, and folders.

### R scripts

Read "list_of_variables.R" to familiarize yourself with how we named variables.

File                      | Description                | Usage         
------------------------- | -------------------------- | --------------
prepare_data.R            | Curate primary data from raw data; pre-process primary data into ready-to-analyze data | Run to reproduce results
desstat&MCFA.R                 | Demographic info, descriptive statistics, and psychometrics (e.g., reliability of measures) | Run to reproduce results
megaanalysis.R                 | Multilevel models (confirmatory & exploratory)  | Run to reproduce results
poweranalysis.R                | Produces estimates for running power analyses on [Lafit et al.'s Shiny app](https://github.com/ginettelafit/PowerAnalysisIL) | Read only
list_of_variables.R                | List of variable names of ESM measures and momentary indices| Read only
func_preprocessing.R                 | Functions for pre-processing data (in prepare_data.R) | Read only

### Manuscript

All these files are under the [manuscript folder](manuscript).

File                      | Description                | Usage         
------------------------- | -------------------------- | --------------
EDERV_manuscript.Rmd                 | R Markdown file for dynamically producing our manuscript  | Run to reproduce pdf
EDERV_supplementalmaterials.Rmd                 | R Markdown file for dynamically producing our supplemental materials  | Run to reproduce pdf
apa7.csl            | Citation Style Language (APA 7th edition) | Read only                 
mainreferences.bib                | BibTeX reference for our manuscript| Read only
datasets_and_OSF.bib                | BibTeX reference related to our datasets and supplemental materials| Read only
r-references.bib                 | BibTeX reference related to the R Markdown files | Read only
EDERV_manuscript.pdf                 | Manuscript | Read only
EDERV_supplementalmaterials.pdf                 | Supplemental materials | Read only
OSFprereg...Variability.pdf                 | Pre-registration: original version | Read only
OSFprereg...2023Oct.pdf                 | Pre-registration: updated version | Read only





### Project Files

File                      | Description                | Usage         
------------------------- | -------------------------- | --------------
EDpredictsERV.Rproj      | Project file               | Loads project 
 README.md                 | Description of project     | Read only
LICENSE                   | User permissions           | Read only     
.worcs                    | WORCS metadata YAML        | Read only     
renv.lock                 | Reproducible R environment | Read only     


### Folders
Folder| Description                | Usage         
------------------------- | -------------------------- | --------------
manuscript | Folder that holds markdown files for knitting pdfs    | Read only
dataRaw| Raw data files. An empty folder because raw data with potential personal identifiers are not uploaded. | Read only     
dataPrimary| Primary data files, which means anonymized raw data without any further pre-processing. Contains 3 datasets from Radboud, Tilburg, and Ghent. | Read only     
dataProcessed| An empty folder, but ready-to-analyze data files will be saved here after running prepare_data.R | Read only     




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
	    
	    
 5. Download 2 raw datasets from EMOTE (http://emotedatabase.com/requestid/1D13C1YC2Q) to folder dataRaw.  
 6. Run 3 R scripts to reproduce the results. Start new R session (Ctrl+Shift+F10 in R studio on Windows) and clear all environment variables before you run each R script.
 
	- prepare_data.R 
		- Expect to see error messages in "Curate primary data (Part 0)" because raw data from Radboud, Tilburg, and Ghent are not available. This part still needs to be run so that you can prepare primary data for the 2 EMOTE datasets.
		- We have uploaded primary data from Radboud Tilburg, and Ghent to Github. So, from "Data pre-processing (Part 1)" onwards the R script will run normally.
	- desstat&MCFA.R 
	- megaanalysis.R 


 7. Under the manuscript folder, knit (Ctrl+Shift+K in R studio on Windows)  EDERV_manuscript.Rmd and EDERV_supplementalmaterials.pdf to generate our manuscript and supplemental materials.
 8. With poweranalysis.R, you may inspect how we obtained estimates with a reference dataset to run power analyses on [Lafit et al.'s Shiny app](https://github.com/ginettelafit/PowerAnalysisIL). Power analysis results are detailed in Supplemental Materials 1. The reference dataset ("primaryPower.csv", to be put in folder dataPrimary) is required to run this R script. This primary dataset is temporarily unavailable but will be made available in a later stage when the related manuscript is published. 

Step 1 to 4 are detailed in the vignette on [reproducing a WORCS project](https://cjvanlissa.github.io/worcs/articles/reproduce.html).


### Adherence to WORCS

This project uses the Workflow for Open Reproducible Code in Science (WORCS) to ensure transparency and reproducibility. The workflow is designed to meet the principles of Open Science throughout a research project.

### More about WORCS

To learn how WORCS helps researchers meet the TOP-guidelines and FAIR principles, read the preprint at https://osf.io/zcvbs/
