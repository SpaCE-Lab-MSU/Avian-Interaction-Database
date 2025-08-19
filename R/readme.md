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

## Workflow

The workflow for this repository follows the guidelines set out by the [Environmental Data Initiative (EDI)]((https://edirepository.org/)). Briefly, this involves aligning with FAIR data practices, and employing a workflow that uses different levels for harmonization and derived data products. The overall workflow aligns with this EDI diagram:

<img src="https://edirepository.org/static/images/thematic-standardization-workflow.png" class="inline"/>

### Configuration for R code

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

#### Google Drive

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
 



#### Installing Packages

To install all of the packages used here quickly, try the `renv` package
from Rstudio/Posit as follows:

```
install.packages("renv")
renv::init()
```

Choose option 1. restore the project from the lockfile. 

If you need to update packages, choose option 2 to re-install everything. 



### Order & description of scripts:

Preparation of Data: See
### For BBS analysis in North America:
/R/bbs/bbs_specieslist_L0.R = reads in current BBS species list and adds any species that was split
/R/bbs/bbs_specieslist_L1.R = cleans the BBS species list, e.g., combining subspecies into species

### For entire database:

These are RMarkdown notebooks that can be run cell-by-cell to examine output, 
rendered to HTML with status output and saved to git with either knitr or Quarto

1. R/L0/repo_status.rmd = Notebook file to list status of each folder: checked, in_review, and temporary files
1. R/L0/L0_stitch.rmd = Notebook file to work through opening each stitches together all individual csvs in /L0/species and optionally /L0/species_in_review
2. R/L1/AvianInteractionData_L1.R = fixes species names, interaction codes, checks species name discrepencies based on current and past BOW names.

## Description of subdirectories

- **L0**: L0 (raw) data files = CSV files containing entries in the database from 2012-present; data entry procedure follows [/L0/AvianInteractionData_ENTRY_INSTRUCTIONS.md](L0/AvianInteractionData_ENTRY_INSTRUCTIONS.md)
- **L1**: L1 data; cleaned & edited L0 data.
- **L2**: Derived data from L1 data (e.g., pulling in & checking data from other sources including: Hurlbert Bird Diet Database for North America: https://www.nature.com/articles/s41597-021-01049-9 & https://github.com/hurlbertlab/dietdatabase)).
- **R**: Code to create L0 and L1 data.

### docs
- documents supporting the data and analysis.

## Funding
Funding is provided by Michigan State University (to P.L. Zarnetske), and by a MSU Ecology Evolution, and Behavior SEED Grant (to P.L. Zarnetske). Original work on a subset of species was funded by the Yale Climate and Energy Institute (to P.L. Zarnetske), Erasmus Mundus Fellowship (to S. Zonneveld).

## Authors of the code in this repository

* Phoebe L. Zarnetske, PI, [Spatial and Community Ecology Lab (SpaCE Lab)](https://www.communityecologylab.com)
* Patrick Bills, staff data scientist [Institute for Cyber-enabled Research (ICER)](https://icer.msu.edu)

## Collaborators

* Emily Parker
* Dr. Kelly Kapsar

## Student Research Assistants
* 2024-
  - India Hirschowitz
  - Giovanni DePasquale
  - Caroline Roche
  - Ava Fountain
  - Ann Joseph
  - Maddie Andreatta
* 2023
  - India Hirschowitz
  - Giovanni DePasquale
  - Caroline Roche
* 2022
  - India Hirschowitz
  - Jordan Zapata
  - Elaine Hammond
* 2021-2024
  - Emily Parker
* 2018-2020
  - Erik Ralston
  - Minali Bhatt

## References

Belmaker, J., P. Zarnetske, M.-N. Tuanmu, S. Zonneveld, S. Record, A. Strecker, and L. Beaudrot. 2015. Empirical evidence for the scale dependence of biotic interactions. Global Ecology and Biogeography 24:750–761. https://doi.org/10.1111/geb.12311

Birds of the World - Comprehensive life histories for all bird species and families. (Accessed January 13, 2022). http://birdsoftheworld.org/bow/home.

Hurlbert, A. H., A. M. Olsen, M. M. Sawyer, and P. M. Winner. 2021. The Avian Diet Database as a source of quantitative information on bird diets. Scientific Data 8:260. https://www.nature.com/articles/s41597-021-01049-9

