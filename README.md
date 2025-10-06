# Avian Interaction Database 

This repository contains code and workflows for the The Avian Interaction Database project, initially focusing on North America (Canada and conterminous United States. 

This is our public-facing repository for this project. As we complete data review and related publications, data will be published here.   
Our private, work-in-progress repository is https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database-Working (restricted, 
access for SpaCE Lab students and staff only).

## About 

This project is overseen by PI Phoebe Zarnetske, [MSU Spatial and Community Ecology Lab (SpaCE Lab)](https://www.communityecologylab.com). Starting in 2019, a new protocol was established and the previous records were updated and further bird-bird interactions were added in by Zarnetske and MSU SpaCE Lab undergraduates (2019-present). The database is in preparation for publication as an open access data paper. A smaller version of these data were compiled from 2012-2014, by Zarnetske and Zonneveld, including bird-bird interactions among cavity nesting birds and their interacting bird species, based on species accounts in the Birds of North America (now Birds of the World). This led to an analysis of the influence of biotic interactions on North American cavity nesting bird species distributions ([Belmaker and Zarnetske et al. 2015 GEB](https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12311)). Ongoing work involves entering and checking bird-bird interactions, based on [Birds of the World Online](http://birdsoftheworld.org/bow/home).  

MSU Undergraduates led the presentations of research posters at the [MSU Undergraduate University Research and Arts Forum](https://urca.msu.edu/uuraf):

Joseph, A., C. Roche, G. DePasquale, M. Andreatta, P. Bills, E. Parker, P.L. Zarnetske. April 2025. HOW DOES HUMAN FOOTPRINT IMPACT NORTH AMERICAN AVIAN SPECIES INTERACTIONS? Michigan State University Undergraduate Research and Arts Forum (UURAF). East Lansing, MI. Poster Presentation.

Hirschowitz, I., P. Bills, P.L. Zarnetske. April 2025. NORTH AMERICAN AVIAN RESPONSES TO GLOBAL CHANGE: EFFECTS OF NEGATIVE INTERSPECIES INTERACTIONS ON POPULATION TRENDS. Michigan State University Undergraduate Research and Arts Forum (UURAF). East Lansing, MI. Poster Presentation.

DePasquale, G., I. Hirschowitz, C. Roche, E.G. Parker, P. Bills, P.L. Zarnetske. April 2024. The North American Avian Interaction Database. Michigan State University Undergraduate Research and Arts Forum (UURAF). East Lansing, MI. Poster Presentation.

## Location of data

These data are stored in this repository, accessible by collaborators. Once published, the North American Avian Interaction Database will be made open access.

## Workflow

The workflow for this repository follows the guidelines set out by the [Environmental Data Initiative (EDI)]((https://edirepository.org/)). Briefly, this involves aligning with FAIR data practices, and employing a workflow that uses different levels for harmonization and derived data products. The overall workflow aligns with this EDI diagram: 

<img src="https://edirepository.org/static/images/thematic-standardization-workflow.png" class="inline"/>

Order & description of scripts:

### For BBS analysis in North America: 
/R/bbs_specieslist_L0.R = reads in current BBS species list and adds any species that was split
/R/bbs_specieslist_L1.R = cleans the BBS species list, e.g., combining subspecies into species 

### For entire database: 
1. /R/AvianInteractionData_L0_stitch.R = stitches together all individual csvs in /L0/species and optionally /L0/species_in_review
2. /R/AvianInteractionData_L1.R = fixes species names, interaction codes, checks species name discrepencies based on current and past BOW names.  

## Description of subdirectories 

- **L0**: L0 (raw) data files = CSV files containing entries in the database from 2012-present; data entry procedure follows [/L0/AvianInteractionData_ENTRY_INSTRUCTIONS.md](L0/AvianInteractionData_ENTRY_INSTRUCTIONS.md)
- **L1**: L1 data; cleaned & edited L0 data. 
- **R**: Code to create L0 and L1 data.

### docs
- documents supporting the data and analysis.

## Funding 
Funding is provided by Michigan State University (to P.L. Zarnetske), and by a MSU Ecology Evolution, and Behavior SEED Grant (to P.L. Zarnetske). Original work on a subset of species was funded by the Yale Climate and Energy Institute (to P.L. Zarnetske), Erasmus Mundus Fellowship (to S. Zonneveld). 

## Authors of this repository

* Phoebe L. Zarnetske, PI, [Spatial and Community Ecology Lab (SpaCE Lab)](https://www.communityecologylab.com)

## Collaborators
* Emily Parker
* Pat Bills
* Sara Zonneveld

## Student Research Assistants
* 2025-
  - Caroline Roche
  - Liz Bauer
  - Vivian Smith
  - Olive Graves
  - Sarah Pecis
  - Addison Hoddinott
  - Elliot Palmer
  - Jamie Soehl
* 2024
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
