# Bacterial Infections: temporal trends in the children population
Analysis of the bacterial infection trends in the children population.

## Data
Analysis developed with the aim of using the 2.2 4CE consortium data

## Repo organization
- `R/`: contains the files carrying out the analysis
- `public-data/ICD10codes.csv`: mapping of ICD codes to bacterial infection categories
- `output`: location of the files saved with by the analysis 

## How to run this code?
First, clone the repository: git clone https://github.com/covidclinical/Phase2.2.bacterial_infections 

Then open the file runAnalysis.Rmd and go to the section “Variables that need to be checked/modified by each site”:
- change the folder_4ce_files to the directory where your phase 2.2 AllAdm cohort data is located
- determine the obfuscation threshold: 
    - obfuscation = FALSE if no obfuscation
    - the numeric value of the obfuscation threshold if any; e.g. obfuscation = 3
    Make sure you comment the existing obfuscation line, set up as FALSE
- change the dateFormat to the one followed in your site (e.g., if your date looks like 03-AUG-20 follows the format "%d-%b-%y"). Further details about how to specificy the format below:

%d day as a number (0-31)	01-31
%a abbreviated weekday (e.g., Mon )
%A unabbreviated weekday (e.g, Monday)
%m	month (00-12)	00-12
%b abbreviated month (e.g, Jan)
%B unabbreviated month (e.g, January)
%y 2-digit year (e.g., 07)
%Y 4-digit year (e.g., 2007)


After all these changes are done, run the runAnalysis.Rmd. 
As an output the next files will be generated in the output folder:
- an html file named: runAnalysis.html 
- files with the counts/frequencies for meta-analysis
