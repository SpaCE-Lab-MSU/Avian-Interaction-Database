# Avian Interaction Database: Instructions for R Programming

`R/readme.md`

These are instructions for using the R code to check data files and build
the database.  

For an overview of the project, details about the database, it's structure, and a protocol 
for how the data is pulled from primary sources, see the [Project Readme file](../readme.md) 
in the root directory. 


---

This repository contains code and workflows for the The Avian Interaction Database project, 

This is a guide for using the R code to assist with validation, correction, 
aggregation, normalizing the taxonomy and connecting to other databases.  


## Location of data

See the main readme for the project, but the data that are in preparation 
are store in files in the L0 and L1 folders (see below).  Currently these are
only accessible by collaborators.  Once published, the North American Avian 
Interaction Database will be made open access and linked here. 

## Set-up and Configuration for R code

For those using the R scripts to build the database, you must set up the 
configuration for where to find the data, as each computer/execution environment 
has it's own folder paths.  
To allow for collaboration, this project uses 
a simple R script that only sets variables with the paths.  
Then there is a function you can call (or gets called when you source the scripts)
to check that they are set and the paths are found. 

1. find the file "filepaths_example.R" in the top folder
2. make a copy of this file as filepaths.R  (open and 'save as...' filepaths.R)
   the configuration functions expect the file to be named this
3. open the new filepaths.R and put the paths for your computer
   (this is an R script and uses R syntax, not a config or environment file)
   - DATA_FOLDER : the path to the folder that has the L0 and L1 subfolders
   - SYNC_DATA_FOLDER : the path to yoru google drive folder. 
   
   
   

Example filepaths.R contents

```
SYNC_DATA_FOLDER =  "/Users/USERID/Library/CloudStorage/GoogleDrive-billspat@msu.edu/Shared\ drives/Avian_MetaNetwork/"
DATA_FOLDER =  "/Users/USERID/Avian-Interaction-Database"
```

This project uses the widely used ['here' package](https://here.r-lib.org) to 
automatically identify the top folder for scripts to be able to find each other 
regardless of where they are run. 

### Google Drive

We use Google Sheets (see protocol) to facilitate data entry for each species. 
If as part of using the R code you'd like to view and access the intermediate files
in Google Drive, you must have google drive installed. 

To use Google drive on MacOS15, you may have to grant 'full file access' to 
Rstudio in the the MacOS System Settings - Privacy & Security - Full Disk Acces - 
select Rstudio and/or Positron.  

To usg Google drive on Windows (...to be written)

Older installations of Google Drive may have it in different locations and these
locations are not documented well.  You may be able to find this by dragging a 
file from an open google drive window into the terminal window

Note that you then must add the location of your google drive folder to the 
`filepaths.R` file described above.   The folder location of google drive on MacOS
for most recent installations is in the example file. 

A technique for finding the location of your Google drive folder on Mac is

1. open the Mac Finder and find the parent folder of where you sync your data
2. open the terminal.app utility application (the Rstudio terminal doesn't work for this)
3. drag the folder where your data is into the terminal.app window
   this will then show the full path to the google drive folder.  
4. highlight this folder and copy it (Command+c or rightclick and copy)
5. paste this folder into `filepaths.R`

For those who have an older install of Google drive, it is in the `/Volumes..` 
path but in newer installs the path is something like

`/Users/YOURUSERID/Library/CloudStorage/GoogleDrive-youremail@someplace.com/`
 



### Installing Packages

To install all of the packages used here quickly, try the `renv` package
from Rstudio/Posit as follows:

```
install.packages("renv")
renv::init()
```

Choose option 1. restore the project from the lockfile. 

If you need to update packages, choose option 2 to re-install everything. 

See the file `renv.lock` for both the R version and package versions with 
which the code was developed and tested. 

## Code Workflow Overview

See the main workflow document describing how data is collected, entered, 
checked-in and reviewed.   This describes how the code is used to build the final
database with updated taxa from the data as entered.  

The workflow for this repository follows the guidelines set out by the 
[Environmental Data Initiative (EDI)]((https://edirepository.org/)). 
Briefly, this involves aligning with FAIR data practices, and employing a 
workflow that uses different levels for harmonization and derived data products. 
The overall workflow aligns with this EDI diagram:

<img src="https://edirepository.org/static/images/thematic-standardization-workflow.png" class="inline"/>

First, Data is entered into individual files for logistics per the main workflow
above.

The data as entered by reading from primary or secondary sources (as 
a meta analysis) may have typos or inconsistencies from data entry personnel, but
we consider the "L0" data to be a reviewed, corrected, validated and 
combined table based on our data entry process.   The L0 data typically has 
common and scientific names as they appear in the literature we base the data on. 


1) **Review**
   Data submitted for review can be checked using scripts reviewing 
   Outcome: CSV files in the "species" folder with mostly cleaned and corrected data
   but with potentially outdated taxonomic designations
   
2) **clean and combine**
   Outcome: single file with all interactions that is even more cleaned and made 
   consistent
   
3) **build checklist**
   download latest checklists if necessary and build comprehensive checklist 
   with 
   
4) **build taxnomic reconciliation table**
   using checklists from step above and functions to examine / compare 
   taxonomy in data as entered ('raw') and checklist to create an 'edited' version
   of the scientific name or common name to correct for typos or taxonomic changes
   to match the current checklist
   Outcome:  L1_taxnomonic_edits.csv
   
5) **reconcile taxonomy**
  apply a final cleaning, then use the taxnomic edits file to create a file that 
  matches the final 
  database with the taxonomic 


### Order & description of scripts:

### For BBS analysis in North America:
/R/bbs/bbs_specieslist_L0.R = reads in current BBS species list and adds any species that was split
/R/bbs/bbs_specieslist_L1.R = cleans the BBS species list, e.g., combining subspecies into species


### For entire database:

Functions

- R/L0/L0_functions.R  functions called by quarto files in 
These are RMarkdown notebooks that can be run cell-by-cell to examine output, 
rendered to HTML with status output and saved to git with either knitr or Quarto

1. R/L0/repo_status.qmd = Quarto Notebook file to list status of each folder: checked, in_review, and temporary files
1. R/L0/L0_stitch.qmd = Quarto Notebook file to work through opening each stitches together all individual csvs in /L0/species and optionally /L0/species_in_review
   This called the function "read_and_amend()" for each file to be combined, which 
   does several cleaning steps (previously done in L1) to produce as clean a table
   for the L1 taxomomy 
2. R/L1/AvianInteractionData_specieslists_Canada_CONUS_L1.R creates checklists
4. R/L1/AvianInteractionData_L1.qmd = Quarto notebook to merge checklists and 
   species in interation database file from L0_stitch process
   

## Description of subdirectories

- **L0**: L0 (raw) data files = CSV files containing entries in the database from 2012-present; data entry procedure follows [/L0/AvianInteractionData_ENTRY_INSTRUCTIONS.md](L0/AvianInteractionData_ENTRY_INSTRUCTIONS.md)
- **L1**: L1 data; cleaned & edited L0 data.
- **L2**: Derived data from L1 data (e.g., pulling in & checking data from other sources including: Hurlbert Bird Diet Database for North America: https://www.nature.com/articles/s41597-021-01049-9 & https://github.com/hurlbertlab/dietdatabase)).
- **R**: Code to create L0 and L1 data.

### docs
- documents supporting the data and analysis.

## Funding
Funding is provided by Michigan State University (to P.L. Zarnetske), and by a 
MSU Ecology Evolution, and Behavior SEED Grant (to P.L. Zarnetske). 
Original work on a subset of species was funded by the Yale Climate and Energy 
Institute (to P.L. Zarnetske), Erasmus Mundus Fellowship (to S. Zonneveld).

## Authors of the code in this repository

* Phoebe L. Zarnetske, PI, [Spatial and Community Ecology Lab (SpaCE Lab)](https://www.communityecologylab.com)
* Patrick Bills, staff data scientist [Institute for Cyber-enabled Research (ICER)](https://icer.msu.edu)
* Emily Parker, staff data manager 2022-24


