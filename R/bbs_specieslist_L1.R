# TITLE:          L1 BBS Species List Data: Reads in bbs_specieslist_2022_L0.csv 
#                 (L0 BBS SpeciesList from 2023 release (up to 2022 BBS data), 
#                 so it can be used in avian-meta-network (align with combination 
#                 AOUs for subspecies before using in avian-meta-network) 
# AUTHORS:        Phoebe Zarnetske
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     L0 data: bbs_specieslist_2022_L0.csv from bbs_specieslist_L0.R;
#                 a copy of the raw data, just omits the .txt version top lines 
#                 without data.  
#                 SpeciesList2.csv from Jeff Hostetler @ BBS contains combo AOUs 
#                 for subspecies.
# DATA OUTPUT:    L1 data: bbs_specieslist_2022_L1.csv  
# PROJECT:        Avian Interaction Database & avian-meta-network
# DATE:           17 January 2022 - 8 August 2024
# NOTES:          adds new column AOUcombo.index for use with BBS abundance index. 
#
#               Next script to run: for avian-meta-network: 

# Clear all existing data
rm(list=ls())

#Load packages
library(dplyr)
library(readr)

# Set working directory
L0_dir <- Sys.getenv("L0DIR")

# Above .Renviron not working for PLZ; hard-coding in here
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0"
L1_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L1"

# Read in species list: all species in BBS (the 2023 release which includes all
# species as of 2022)
splist<-read.csv(file.path(L0_dir,"bbs_splist_2022_L0.csv"))

#### Make some updates to the Species List. Read in modified Species List
#version with 12 extra entries for combined species (from Jeff Hostetler @ BBS:
#August 7, 2024; this version contains some different genus species as well,
#making 12 additions for combined species and 16 that are actually old outdated
#names as far as I can tell. We will first add the 12 combined species to our
#original 2022 BBS Species List (splist)).
splist2<-read.csv(file.path(L0_dir,"SpeciesList2.csv"))
# remove Seq before merging
splist$Seq<-NULL
splist2$Seq<-NULL
# Merge
splist3<-merge(splist2,splist, by=c("AOU","English_Common_Name","French_Common_Name","Spanish_Common_Name","ORDER","Family","Genus","Species"),all.x=T, all.y=T)

# Differences in SpeciesList2:
splist3.new<-splist3[!complete.cases(splist3), ]
splist3.new[,1:2]
#       AOU                              English_Common_Name
# 44    550                                         Mew Gull
# 82   1206 unid. Double-crested Cormorant / Great Cormorant
# 83   1207                       unid. west coast cormorant
# 86   1220                               Brandt's Cormorant
# 88   1230                                Pelagic Cormorant
# 90   1240                              Red-faced Cormorant
# 99   1326 hybrid Mallard x Mexican, Black, or Mottled Duck
# 254  2980                                    Spruce Grouse
# 316  3620                                 Crested Caracara
# 409  4391                       Violet-crowned Hummingbird
# 474  4882          unid. American Crow / Northwestern Crow
# 475  4890                (Northwestern Crow) American Crow
# 491  5012    unid. Eastern Meadowlark / Western Meadowlark
# 714  7240                                       Sedge Wren
# 741  7430                                          Bushtit
# 747  7490                             Ruby-crowned Kinglet
# 775 30010                          Western & Clark's Grebe
# 776 31320                              Mallard (all forms)
# 777 31940                     Great Blue Heron (all forms)
# 778 33370                      Red-tailed Hawk (all forms)
# 779 34120                     Northern Flicker (all forms)
# 780 34641           Cordilleran & Pacific-slope Flycatcher
# 781 34660                        Alder & Willow Flycatcher
# 782 34810               California & Woodhouse's Scrub-Jay
# 783 34880                                    American Crow
# 784 35670                      Dark-eyed Junco (all forms)
# 785 35740                       Sagebrush & Bell's Sparrow
# 786 36550                Yellow-rumped Warbler (all forms)
# Same as last 12 rows of splist2
splist2[c(763:774),c(1:2)]

combinedAOUs<-splist2[c(763:774),]
combinedAOUs$genus_species <- do.call(paste, c(combinedAOUs[c("Genus", "Species")], sep = " "))

# Add the 12 combined AOUs to the end of the current 2022 BBS Species List:
dim(splist)
splist<-rbind(splist, combinedAOUs)
dim(splist)
# Now create a new column for AOUs to be used in the BBS index analysis. This
# will contain re-assignments to the 12 combined AOUs, for their subspecies:
splist$AOUcombo.index<-splist$AOU

# Grouping Rules provided by Jeff Hostetler @ BBS. 
# Update the splist AOUcombo.index for each subspecies species (assign it to the
# combo AOU). Make a selection for each subspecies group:

# Mallard (all forms)
# 2 31320
# 01320 01331
splist$AOUcombo.index[splist$AOU == 1320] <- 31320
splist$AOUcombo.index[splist$AOU == 1331] <- 31320

# Great Blue Heron (all forms)
# 2 31940    
# 01920 01940
splist$AOUcombo.index[splist$AOU == 1320] <- 31320
splist$AOUcombo.index[splist$AOU == 1331] <- 31320

# Red-tailed Hawk (all forms)
# 2 33370
# 03370 03380
splist$AOUcombo.index[splist$AOU == 3370] <- 33370
splist$AOUcombo.index[splist$AOU == 3380] <- 33370

# Northern Flicker (all forms)
# 4 34120
# 04120 04130 04123 04125
splist$AOUcombo.index[splist$AOU == 4120] <- 34120
splist$AOUcombo.index[splist$AOU == 4130] <- 34120
splist$AOUcombo.index[splist$AOU == 4123] <- 34120
splist$AOUcombo.index[splist$AOU == 4125] <- 34120
# 6 35670
# 05660 05670 05671 05680 05690 05677
splist$AOUcombo.index[splist$AOU == 5660] <- 35670
splist$AOUcombo.index[splist$AOU == 5670] <- 35670
splist$AOUcombo.index[splist$AOU == 5671] <- 35670
splist$AOUcombo.index[splist$AOU == 5680] <- 35670
splist$AOUcombo.index[splist$AOU == 5690] <- 35670
splist$AOUcombo.index[splist$AOU == 5677] <- 35670
# 3 36550
# 06550 06560 06556
splist$AOUcombo.index[splist$AOU == 6550] <- 36550
splist$AOUcombo.index[splist$AOU == 6560] <- 36550
splist$AOUcombo.index[splist$AOU == 6556] <- 36550
# Western & Clark's Grebe
# 3 30010
# 00010 00011 00012
splist$AOUcombo.index[splist$AOU == 10] <- 30010
splist$AOUcombo.index[splist$AOU == 11] <- 30010
splist$AOUcombo.index[splist$AOU == 12] <- 30010
# 3 34641
# 04640 04641 04642
splist$AOUcombo.index[splist$AOU == 4640] <- 34641
splist$AOUcombo.index[splist$AOU == 4641] <- 34641
splist$AOUcombo.index[splist$AOU == 4642] <- 34641
# 3 34660
# 04660 04661 04665
splist$AOUcombo.index[splist$AOU == 4660] <- 34660
splist$AOUcombo.index[splist$AOU == 4661] <- 34660
splist$AOUcombo.index[splist$AOU == 4665] <- 34660
# 3 34810
# 04812 04813 04810
splist$AOUcombo.index[splist$AOU == 4812] <- 34810
splist$AOUcombo.index[splist$AOU == 4813] <- 34810
splist$AOUcombo.index[splist$AOU == 4810] <- 34810
# 3 35740
# 05738 05739 05740
splist$AOUcombo.index[splist$AOU == 5738] <- 35740
splist$AOUcombo.index[splist$AOU == 5739] <- 35740
splist$AOUcombo.index[splist$AOU == 5740] <- 35740
# 3 34880
# 04880 04880 04882
splist$AOUcombo.index[splist$AOU == 4880] <- 34880
splist$AOUcombo.index[splist$AOU == 4880] <- 34880
splist$AOUcombo.index[splist$AOU == 4882] <- 34880
# 2 35010
# 05010 05010
splist$AOUcombo.index[splist$AOU == 5010] <- 35010
splist$AOUcombo.index[splist$AOU == 5010] <- 35010

# Export the cleaned data (note the encoding to maintain special characters)
write.csv(splist,file.path(L1_dir,"bbs_splist_2022_L1.csv"), row.names=F)

# Next script to run if combining with bbs_obs data: AvianInteractionData_L1.R

