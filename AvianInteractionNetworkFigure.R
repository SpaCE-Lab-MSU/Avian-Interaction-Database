# Title:   Example Network figures for North
#          American avian interactions
# Authors: Lucas Mansfield
# Date:    11 December 2025 -

# -----------------------------------------------
# Loading packages and data
# -----------------------------------------------

rm(list=ls())

library(tidyverse)
library(igraph)
library(ggraph)

inter <- read.csv("R/L1/AvianInteractionData_CanadaAKCONUS_L1.csv") #Original data

