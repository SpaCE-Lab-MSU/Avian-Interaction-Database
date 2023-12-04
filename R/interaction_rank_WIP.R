# TITLE:          Avian Interaction Network - assigning cavity interaction rank
# AUTHORS:        Emily Parker
# COLLABORATORS:  Phoebe Zarnetske, Pat Bills, undergrads, ...
# DATA INPUT:     Data imported as csv https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/AvianInteractionData_raw.csv
# DATA OUTPUT:    list of interaction rank
# PROJECT:        avian-meta-network
# DATE:           17 Apr 2023
# NOTES:          draft




# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse, dplyr)


# Set L0 directory
### change this to github
L0dir<- setwd("/Users/emilyparker/Documents/GitHub/Avian-Interaction-Database/L0")
Rdir <- setwd("/Users/emilyparker/Documents/R/AvianMetanetwork")

#read in BBS sp list
splist <- read.csv(file.path(L0dir, "AvianInteractionData_BBS_Species_2022.csv"))

#read in name changes
names <- read.csv(file.path(L0dir, "bbsbow_names.csv"))

#fills empty values & removes rows with 2+ values missing
names[names == ""] <- NA
names <- names[!(is.na(names$bow)&is.na(names$bbs2019) | is.na(names$bbs2019) & is.na(names$other_or_old_bow) | is.na(names$bow) & is.na(names$other_or_old_bow)),]

#create data frame
BBS_species <- trimws(splist$Spanish_Common_Name)
interactor <-trimws(splist$cavitynest_interactor)


df <- data.frame(BBS_species,interactor)
df['rank'] <- 0
df['is_in_BBS'] <- NA
df['uncertain'] <- "no"

#replace old BBS names with current BOW names ?????
oldBBSnames <- df$BBS_species[df$BBS_species %in% names$bbs2019 & !df$BBS_species %in% names$bow]
#newBOWnames <- names$bow[!names$bow %in% df$BBS_species]

BBSrownums <- which(df$BBS_species %in% oldBBSnames)
#namesrownums <- which(names$bbs2019 %in% oldBBSnames)

#update BBS names based on BOW
for (a in 1:length(BBSrownums)){
  cur_species = df$BBS_species[BBSrownums[a]]
  df$BBS_species[BBSrownums[a]] <- names$bow[which(cur_species == names$bbs2019)]
}

#check which species are in the BBS
##uses Seq value
for (k in 1:nrow(df)){
  if (is.na(splist$Seq[k]))
    df$is_in_BBS[k] <- 'no'
  else
    df$is_in_BBS[k] <- 'yes'
}


# replace interaction names & note uncertainty

df$uncertain[df$interactor == "tertiary (co-occur)"] <- "yes"
df$uncertain[df$interactor == "secondary (co-occur)"] <- "yes"
df$uncertain[df$interactor == "tertiary (hybrid)"] <- "yes"
df$uncertain[df$interactor == "secondary (species not in BBS)"] <- "yes"
df$uncertain[df$interactor == "unsure (specific subspecies)"]<- "yes"
df$uncertain[df$interactor == "unsure (interaction at artificial nest)"] <- "yes"
df$uncertain[df$interactor == "tertiary (potential)"] <- "yes"

df$interactor[df$interactor == "tertiary (co-occur)"] <- "tertiary"
df$interactor[df$interactor == "secondary (co-occur)"] <- "secondary"
df$interactor[df$interactor == "tertiary (hybrid)"] <- "tertiary"
df$interactor[df$interactor == "secondary (species not in BBS)"] <- "secondary"
df$interactor[df$interactor == "unsure (specific subspecies)"]<- ""
df$interactor[df$interactor == "unsure (interaction at artificial nest)"] <- ""
df$interactor[df$interactor == "tertiary (potential)"] <- "tertiary"


#assign rank based on CNI

for (i in 1:nrow(df)){

  cur_intx <- df$interactor[i]
   if (cur_intx == "primary")
      df$rank[i] <- 1
  if (cur_intx == "secondary")
      df$rank[i] <- 2
  if (cur_intx == "tertiary")
    df$rank[i] <- 3
  if (cur_intx == "quaternary")
    df$rank[i] <- 4
}
 
#remove duplicates
df <- df[!duplicated(df$BBS_species),]


# read in species file - L0 stitch
intxlist <- read.csv(file.path(Rdir, "AvianInteractionData_L0.csv"))


#remove rows with missing sp1 or sp2
intxlist <- intxlist[!(intxlist$species1_scientific == "" | is.na(intxlist$species1_scientific) | intxlist$species2_scientific == "" | is.na(intxlist$species2_scientific) ), ]


#editing (trim ws, match case, fix old names/misspelled)

#sp1
oldBBSnames1 <- intxlist$species1_scientific[intxlist$species1_scientific %in% names$bbs2019 & !intxlist$species1_scientific %in% names$bow]
intxBBSnums1 <- which(intxlist$species1_scientific %in% oldBBSnames1)
oldnames1 <-intxlist$species1_scientific[intxlist$species1_scientific %in% names$other_or_old_bow]
intxoldnums1 <- which(intxlist$species2_scientific %in% oldnames1)


for (b in 1:length(intxBBSnums1)){
  cur_species <- intxlist$species1_scientific[intxBBSnums1[b]]
  
  #checks for old BBS names
  ifelse(is.na(names$bow[which(cur_species == names$bbs2019)]), next, intxlist$species1_scientific[intxBBSnums1[b]] <- names$bow[which(cur_species == names$bbs2019)])
}

for (c in 1:length(intxoldnums1)){
  cur_species <- intxlist$species1_scientific[intxoldnums1[c]]
  
  #checks for old names
  ifelse(is.na(names$bow[which(cur_species == names$other_or_old_bow)]), next, intxlist$species1_scientific[intxoldnums1[c]] <- names$bow[which(cur_species == names$other_or_old_bow)])
}


#sp2
oldBBSnames2 <- intxlist$species2_scientific[intxlist$species2_scientific %in% names$bbs2019 & !intxlist$species2_scientific %in% names$bow]
intxBBSnums2 <- which(intxlist$species2_scientific %in% oldBBSnames2)
oldnames2 <-intxlist$species2_scientific[intxlist$species2_scientific %in% names$other_or_old_bow]
intxoldnums2 <- which(intxlist$species2_scientific %in% oldnames2)

for (d in 1:length(intxBBSnums2)){
  cur_species <- intxlist$species2_scientific[intxBBSnums2[d]]
  
  #checks for old BBS names
  ifelse(is.na(names$bow[which(cur_species == names$bbs2019)]), next, intxlist$species2_scientific[intxBBSnums2[d]] <- names$bow[which(cur_species == names$bbs2019)])
  
}

for (e in 1:length(intxoldnums2)){
  cur_species <- intxlist$species2_scientific[intxoldnums2[e]]

  #checks for old names
  ifelse(is.na(names$bow[which(cur_species == names$other_or_old_bow)]), next, intxlist$species2_scientific[intxoldnums2[e]] <- names$bow[which(cur_species == names$other_or_old_bow)])
}



for (l in 1:nrow(intxlist)){
  
  #trim ws
  intxlist$species1_scientific[l] <- trimws(intxlist$species1_scientific[l])
  intxlist$species2_scientific[l] <- trimws(intxlist$species2_scientific[l])
  
  #match case - this doesn't work?
  #intxlist$species1_scientific[l] <- str_to_sentence(intxlist$species1_scientific[l])
  #intxlist$species2_scientific[l] <- str_to_sentence(intxlist$species2_scientific[l])
  
}

#new variables & new dataframe
df2 <- df
species1 <-intxlist$species1_scientific
species2 <- intxlist$species2_scientific
change_log <-""
changes <- data.frame(change_log)


#rerun this section as needed until no changes return 
#add code about uncertain interactions?
for (j in 1:nrow(intxlist)){
  
  #assigns current species1 & 2 & trims ws
  cur_species1 <- species1[j]
  cur_species2 <- species2[j]
  cur_rank1 <- df2$rank[df2$BBS_species == cur_species1]
  cur_rank2 <- df2$rank[df2$BBS_species == cur_species2]

 
  #if no current rank     
  if (length(cur_rank2) == "0"){
    cur_rank2 <- 0
    df2[nrow(df2)+1,] <- list(cur_species2,"",0,"no","no")
  }
  
  if (length(cur_rank1) == "0"){
    cur_rank1 <-0
    df2[nrow(df2)+1,] <- list(cur_species1,"",0,'no',"no")
  }
  
  #if species1 is lower rank than species2 and changes species2 rank
    if ((cur_rank1 < cur_rank2-1 | cur_rank2 == "0") & cur_rank1 != "0"){
      #if species1 rank is uncertain 
      if (df2$uncertain[df2$BBS_species == cur_species1] == "yes")
      {df2$uncertain[df$BBS_species == cur_species2] <- "yes"}
      cur_rank2 <- cur_rank1+1
      df2$rank[df2$BBS_species == cur_species2] <- cur_rank2
      output <- paste(cur_species2, "interacts with ", cur_species1, "(", cur_rank1, ") and is now rank ", cur_rank2, sep = " ")
      changes[nrow(changes) +1,] <- list(output)
    }
  
  #if species2 is lower rank than species1 and changes species1 rank
    if ((cur_rank2 < cur_rank1-1 | cur_rank1 == "0") & cur_rank2 != "0"){
      #if species2 rank is uncertain
      if (df2$uncertain[df2$BBS_species == cur_species2] == "yes")
      {df2$uncertain[df$BBS_species == cur_species2] <- "yes"}
      cur_rank1 <- cur_rank2 + 1
      df2$rank[df2$BBS_species == cur_species1] <- cur_rank1
      output <- paste(cur_species1, "interacts with ", cur_species2, "(", cur_rank2, ") and is now rank ", cur_rank1, sep = " ")
      changes[nrow(changes) +1,] <- list(output)
      
    }
}

#reassign interaction names
for (z in 1:nrow(df2)){
  
  cur_intx <- df2$rank[z]
  
  if (cur_intx == 1)
    df2$interactor[z] <- "primary"
  if (cur_intx == 2)
    df2$interactor[z] <- "secondary"
  if (cur_intx == 3)
    df2$interactor[z] <- "tertiary"
  if (cur_intx == 4)
    df2$interactor[z] <- "quaternary"
  if (cur_intx == 5)
    df2$interactor[z] <- "quinary"
}

write.csv(df2,file="../BBSInteractions_updated.csv", row.names=FALSE)
write.csv(changes,file="../rank_changes.csv", row.names = FALSE)
