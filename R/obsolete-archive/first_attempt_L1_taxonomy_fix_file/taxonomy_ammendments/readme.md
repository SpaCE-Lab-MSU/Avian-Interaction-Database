# Taxonomy Ammendments Data 
## Avian Interaction Network

- AUTHORS:        Phoebe Zarnetske, Pat Bills
- PROJECT:        Avian Interaction Database
- DATE:           1 JULY 2025 (last run on this date)
- NOTES:          Documentation of data files

The amendment tables folder contains look-up tables for various elements of raw data from the database to be updated systematically.  These tables document where updates were made and why.  


**taxonomy_ammendments.csv**

Look up table of scientific name changes to be applied to any cell of the database. 
This is an edge list of a graph with directionality from old to new
which may include 
a split where an originating node (previously accepted species scientific name) has multiple updates (newly accepted species).  In this case the update can't be made without 


- scientific_name : the scientific name to look-up
- updated_scientific_name : an alternate name (new or previous)
- update_type : what kind of update: replacement; split; lump
- reason : description of the change and the source
- who : who added this row (or from which process)
- date : date it was added (approximate)


**taxonomy_ammendments_by_cell.csv**

 - interaction_id : database record ID (row ID)
 - column : which column: sp1 or sp2
 - previous_scientific_name : what was the cell, or which needs to be updated
 - current_scientific_name : what the cell should have
 - reason : description of the change and the source
 - who : who added this row (or from which process)
 - date : date it was added (approximate)
