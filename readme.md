# Avian Interaction Database 

This repository contains code and workflows for the The Avian Interaction Database project, initially focusing on North America. Starting in 2019, a new protocol was established and the previous records were updated and further bird-bird interactions were added in by Zarnetske and MSU SpaCE Lab undergraduates (2019-present). 

Currently the main repository containing data workflows is https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database


The database is in preparation for publication as an open access data paper. From 2012-2014, Zarnetske and Zonneveld compiled bird-bird interactions among cavity nesting birds and their interacting bird species, based on species accounts in the Birds of North America (now Birds of the World). This led to an analysis of the influence of biotic interactions on North American cavity nesting bird species distributions ([Belmaker and Zarnetske et al. 2015 GEB](https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12311)). 

MSU Undergraduates led the presentation of a research poster at the 2024 [MSU Undergraduate University Research and Arts Forum](https://urca.msu.edu/uuraf):

DePasquale, G., I. Hirschowitz, C. Roche, E.G. Parker, P. Bills, P.L. Zarnetske. April 2024. The North American Avian Interaction Database. Michigan State University Undergraduate Research and Arts Forum (UURAF). East Lansing, MI. Poster Presentation.

## Documentation

see [R/readme.md](R/readme.md) for details about the R scripts that support this workflow.

## Location of data

These data are stored in  https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database and code 
in this repository read that in.  TBD in the future to combine these into one repository.

per the documentation above, create the file `R/filepaths.R` to point relevant data folders. 

## About this Data & Code Workflow

The workflow for this repository follows the guidelines set out by the [Environmental Data Initiative (EDI)]((https://edirepository.org/)). Briefly, this involves aligning with FAIR data practices, and employing a workflow that uses different levels for harmonization and derived data products. The overall workflow aligns with this EDI diagram: 

<img src="https://edirepository.org/static/images/thematic-standardization-workflow.png" class="inline"/>


### Description of subdirectories 

- data: not used in this repository
- R: code used by all layers
   - [readme.me](R/readme.md): main instructions for using Code/Workflows
   - filepaths_example.R: example file to create your own filepaths.R to point to data folders
   - R/L0: code to examine and prepare L0 data
   - R/L1: code and notebooks to build L1 files and prepare final database
   - R/obsolete-archive: code from previous versions saved for reference

- reports:  notebooks in the R folder that are 'rendered' to HTML are saved here 
- _quarto.yml: configuration file for rendering notebooks


## Funding 
Funding is provided by Michigan State University (to P.L. Zarnetske), and by a MSU Ecology Evolution, and Behavior SEED Grant (to P.L. Zarnetske). Original work on a subset of species was funded by the Yale Climate and Energy Institute (to P.L. Zarnetske), Erasmus Mundus Fellowship (to S. Zonneveld). 

## Authors of this repository

* Phoebe L. Zarnetske, PI, [Spatial and Community Ecology Lab (SpaCE Lab)](https://www.communityecologylab.com)

## Collaborators
* Emily Parker
* Patrick S. Bills, MSU Institute for Cyber-Enabled Research

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

