# Avian Interaction Database 

This repository contains code and workflows for the The Avian Interaction Database project, initially focusing on North America. Starting in 2019, a new protocol was established and the previous records were updated and further bird-bird interactions were added in by Zarnetske and MSU SpaCE Lab undergraduates (2019-present). The database is in preparation for publication as an open access data paper. From 2012-2014, Zarnetske and Zonneveld compiled bird-bird interactions among cavity nesting birds and their interacting bird species, based on species accounts in the Birds of North America (now Birds of the World). This led to an analysis of the influence of biotic interactions on North American cavity nesting bird species distributions ([Belmaker and Zarnetske et al. 2015 GEB](https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12311)). 

MSU Undergraduates led the presentation of a research poster at the 2024 [MSU Undergraduate University Research and Arts Forum](https://urca.msu.edu/uuraf):

DePasquale, G., I. Hirschowitz, C. Roche, E.G. Parker, P. Bills, P.L. Zarnetske. April 2024. The North American Avian Interaction Database. Michigan State University Undergraduate Research and Arts Forum (UURAF). East Lansing, MI. Poster Presentation.

## Location of data

These data are stored in this repository, accessible by collaborators (see below). 

Once published, the North American Avian Interaction Database will be made open access.

## Workflow

The workflow for this repository follows the guidelines set out by the [Environmental Data Initiative (EDI)]((https://edirepository.org/)). Briefly, this involves aligning with FAIR data practices, and employing a workflow that uses different levels for harmonization and derived data products. The overall workflow aligns with this EDI diagram: 

<img src="https://edirepository.org/static/images/thematic-standardization-workflow.png" class="inline"/>


### Description of subdirectories 

- **L0**: L0 (raw) data files = CSV files containing entries in the database from 2012-present; data entry procedure follows [/L0/AvianInteractionData_ENTRY_INSTRUCTIONS.md](L0/AvianInteractionData_ENTRY_INSTRUCTIONS.md)
- **L1**: L1 data; cleaned & edited L0 data. 
- **L2**: Derived data from L1 data (e.g., pulling in & checking data from other sources including: Hurlbert Bird Diet Database for North America: https://www.nature.com/articles/s41597-021-01049-9 & https://github.com/hurlbertlab/dietdatabase)).
- **R**: Code to create L0 and L1 data.

Our data and R folders have subfolder corresponding to the framework, with 
data from L0 -> L1 -> L2 being processed by scripts in R/L0, R/L1, RL2

For details about the scripts that support this workflow, see the `readme.md` file
in the R directory [R/readme.md](R/readme.md)

### docs

- documents supporting the data and analysis.

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

