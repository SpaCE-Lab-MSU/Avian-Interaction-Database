# TITLE:          Avian Interaction Pairs L0 data checking & typo fixes
# AUTHORS:        Phoebe Zarnetske, Pat Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray, Sara Zonneveld, ...
# DATA INPUT:     Data imported as csv https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/AvianInteractionData_raw.csv
# DATA OUTPUT:    L1 data: AvianInteractionData_L0.csv
# PROJECT:        avian-meta-network
# DATE:           27 Oct 2022 - 
# NOTES:          Next script to run: /R/AvianInteractionData_L1.R
#               
#               
# Clear all existing data
rm(list=ls())

# Set working directory
L0_dir <- Sys.getenv("L0DIR")

# Above .Renviron not working for PLZ; hard-coding in here
L0_dir <- "/Users/plz/DATA/git/Avian-Interaction-Database/L0"

# Read in csv with avian interactions from primary, secondary cavity nesting birds in North America.
intxns.raw<-read.csv(file.path(L0_dir, "./AvianInteractionData_raw.csv"))


# ... checking code here

# Save as L0
write.csv(intxns.raw, file.path(L0_dir, "AvianInteractionData_L0.csv"), row.names=F) 

