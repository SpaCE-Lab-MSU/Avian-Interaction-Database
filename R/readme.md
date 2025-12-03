# Avian Interaction Database: Instructions for R Programming

`R/readme.md`

These are instructions for using the R code to check data files and build
the database.  

For an overview of the project, details about the database, its structure, and a protocol 
for how the data are pulled from primary sources, see the [Project Readme file](../readme.md) 
in the root directory. 

---

This repository contains code and workflows for the The Avian Interaction Database project. 

This is a guide for using the R code to assist with validation, correction, 
aggregation, normalizing the taxonomy and connecting to other databases.  


## Location of data

See the main readme for the project. The data that are in preparation 
are stored in files in the L0 and L1 folders (see below).  Currently these are
only accessible by collaborators. Once published, the North American Avian 
Interaction Database will be made open access and linked here. 


## Quick-start 

Assumes the use of Rstudio 2025 version or above.

1. copy file `R/filepaths_example.R` to `R/filepaths.R` and set files paths 
   pointing to data on your computer (details below). 
1. install packages as needed (details below)
2. clear R environment (scripts do not do this automatically)
3. check file paths/repository state
   - open R/L0/L0_repo_status.qmd
   - in Rstudio, in the upper-right "run" button, select
     "restart R and run all chunks"
   - if there are errors, check if `dir.exists(DATA_FOLDER)` 
4. stitch raw data
   - open R/L0/L0_stitch.qmd
   - in the upper-right "run" button, select
     "restart R and run all chunks"
   - this will report the file that is saved
5. build taxonomy edits, running one chunk at time
   - open R/L1/AvianInteractionData_L1.qmd
   - edit the value for stitched_L0_file to match the L0 step above (near the top of file)
   - check the value for the main checklists 
   - position the cursor in the first block of code and run it
   - easily run all subsequent blocks, one a time, using key short cut Option+Command+N
     (see the "->Run" button at the top for more options)
   - edit or add new taxonomic fixes to the edit list by adding code chunks like:
   
```
int.raw.names <- add_name_edits(int.raw.names,
  scientific_name.raw = "Corvus caurinus",
  edit_notes = "CHANGE TO CLOSEST MATCH: checklist Northwestern Crow Corvus brachyrhynchos caurinus",
  scientific_name.edit = "Corvus brachyrhynchos caurinus"
  )
  ```
   - save the edit list as a file (Work in Progress)

6. update names in interaction database (*Work in progress*)
   - open "AvianInteractionData_L1_final_merge.qmd" 
   - update input and output file names
   - run to merge and create final CSV database

## Set-up and Configuration for R code

For those using the R scripts to build the database, you must set up the 
configuration for where to find the data, as each computer/execution environment 
has its own folder paths.  
To allow for collaboration, this project uses a simple R script that only sets 
variables with the paths.  
Then there is a function you can call (or gets called when you source the scripts)
to check that they are set and the paths are found. 

1. find the file `filepaths_example.R` in the top folder
2. make a copy of this file as filepaths.R  (open and 'save as...' `filepaths.R`)
   the configuration functions expect the file to be named this, and the file
   is included in the gitignore so it is not synced with this repository (each 
   user will need to create their own `filepaths.R`)
3. open the new `filepaths.R` and put the paths for your computer
   (this is an R script and uses R syntax, not a config or environment file)
   - DATA_FOLDER : the path to the folder that has the L0 and L1 subfolders.
   - CHECKLIST_FOLDER : location of various checklist files in the repository.
   
**TO-DO: we have two checklists L0 = Clements etc and L1= PLZ Curated and this
system does not accommodate for that**

   

Example `filepaths.R` contents

```
DATA_FOLDER =  "/Users/USERID/Avian-Interaction-Database-Working"
# no need to edit this to match your computer if the folder above is correct
CHECKLIST_FOLDER = file.path(DATA_FOLDER, "L1", "species_checklists")
```

This project uses the widely used ['here' package](https://here.r-lib.org) to 
automatically identify the top folder for scripts to be able to find each other 
regardless of where they are run. Install this package.

### Google Drive

We use Google Sheets to facilitate data entry for each species (see protocol in
[L0/AvianInteractionData_ENTRY_INSTRUCTIONS.md](https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database-Working/L0/AvianInteractionData_ENTRY_INSTRUCTIONS.md). If, as part of using the R code, you'd like to view and access the intermediate files in Google Drive, you must have Google Drive installed on your computer. If you are not reviewing those files, this is not necessary. 

To use Google Drive on MacOS15, you may have to grant 'full file access' to 
Rstudio in the the MacOS System Settings - Privacy & Security - Full Disk Access - 
select Rstudio and/or Positron.  

To use Google Drive on Windows (...to be written)

Older installations of Google Drive may have it in different locations and these
locations are not documented well. You may be able to find this by dragging a 
file from an open Google Drive window into the terminal window.

Note that you then must add the location of your Google Drive folder to the 
`filepaths.R` file described above. The folder location of Google Drive on MacOS
for most recent installations is in the `filepaths_example.R` file. 

A technique for finding the location of your Google Drive folder on Mac is:

1. open the Mac Finder and find the parent folder of where you sync your data
2. open the terminal.app utility application (the Rstudio terminal doesn't work for this)
3. drag the folder where your data is into the terminal.app window
   this will then show the full path to the Google Drive folder.  
4. highlight this folder and copy it (Command+c or rightclick and copy)
5. paste this folder into `filepaths.R`

For those who have an older install of Google Drive, it is in the `/Volumes..` 
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


## Code Workflow

See the main workflow document describing how data is collected, entered, 
checked-in and reviewed. This describes how the code is used to build the final
database with updated taxa from the data as entered.  

The workflow for this repository follows the guidelines set out by the 
[Environmental Data Initiative (EDI)]((https://edirepository.org/)). 
Briefly, this involves aligning with FAIR data practices, and employing a 
workflow that uses different levels for harmonization and derived data products. 
The overall workflow aligns with this EDI diagram:

<img src="https://edirepository.org/static/images/thematic-standardization-workflow.png" class="inline"/>

First, Data are entered into individual files for logistics per the main workflow
above.

The data as entered by reading from primary or secondary sources (as 
a meta analysis) may have typos or inconsistencies from data entry personnel, but
we consider the "L0" data to be a reviewed, corrected, validated and 
combined table based on our data entry process. The L0 data typically have 
common and scientific names as they appear in the literature, and thus may need
updating to current taxonomy (a step that occurs within a L1 step). 


1) **Review**

  - Data submitted for review can be checked using reviewing scripts  
  - Outcome: CSV files in the "species" folder with mostly cleaned and corrected data but with potentially outdated taxonomic designations.
  - Scripts/Notebooks:
    - L0/L0_functions.R = contains most cleaning/data processing code
    - L0/L0_repo_status.qmd = count numbers of files in various states
    - L0/L0_corrections_discovery.qmd = point out issues in files to be corrected
    - L0/L0_ammendments.qmd = work in progress fix issues and ...
  
2) **clean and combine**

   - Outcome: single file with all interactions that are even more cleaned and made 
   consistent for interaction names, effect values, etc.
   - Scripts/Notebooks:
      - L0/L0_functions.R = contains most cleaning/data processing code
      - L0_stitch.qmd : read each file, clean (as above), and stitch together
   
3) **build checklist**
   download latest checklists if necessary and build comprehensive checklist 
   with 
   - see working repository!
     R/L1/AvianInteractionData_specieslists_Canada_CONUS_L1.R 
   - 
   
4) **build taxonomic reconciliation table**
   using checklists from step above and functions to examine / compare 
   taxonomy in data as entered ('raw') and checklist to create an 'edited' version
   of the scientific name or common name to correct for typos or taxonomic changes
   to match the current checklist (typically Clements).
   
   - Outcome:  L1_taxonomonic_edits.csv
   - Notebook: L1/L1_

   
5) **reconcile taxonomy**

  - apply a final cleaning, then use the taxonomic edits file to merge with current create a file that ...
  - apply various methods matches the final 
  - database with the taxonomic 
  
  - TBD


## Additional scripts

**R/config.R** sourced by all scripts to set file paths, should not need editing or
sourcing.



**Previous BBS scripts (see working repository)**

- /R/bbs/bbs_specieslist_L0.R = reads in current BBS species list and adds any species that was split
- /R/bbs/bbs_specieslist_L1.R = cleans the BBS species list, e.g., combining subspecies into species



## Acknowledgements 

Funding is provided by Michigan State University (to P.L. Zarnetske), and by a 
MSU Ecology Evolution, and Behavior SEED Grant (to P.L. Zarnetske). 
Original work on a subset of species was funded by the Yale Climate and Energy 
Institute (to P.L. Zarnetske), Erasmus Mundus Fellowship (to S. Zonneveld).

Please see main readme for additional acknlowedgmens

## Authors (R code)

* Phoebe L. Zarnetske, PI, [Spatial and Community Ecology Lab (SpaCE Lab)](https://www.communityecologylab.com)
* Patrick Bills, staff data scientist [Institute for Cyber-enabled Research (ICER)](https://icer.msu.edu)
* Emily Parker, staff data manager 2022-24


