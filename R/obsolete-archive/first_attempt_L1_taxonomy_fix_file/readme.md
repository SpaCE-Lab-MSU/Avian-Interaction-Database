# Taxonomy Adjustments Data File
## Avian Interaction Network

- AUTHORS:        Phoebe Zarnetske, Pat Bills
- PROJECT:        Avian Interaction Database
- DATE:           31 August 2025
- NOTES:          Documentation of data file:taxonomy_adjustments.csv

**taxonomy_adjustments.csv**

This CSV file is a table of changes to scientific name changes 
This is an edge list of a graph with directionality from old to new
which may include 
a split where an originating node (previously accepted species scientific name) has multiple updates (newly accepted species).  In this case the update can't be made without 

Columns: 

- entered_scientific_name : the scientific name as entered
- common_name : (optional) common name as entered
- updated_scientific_name : the name to be stored in database
- update_type : what kind of update
- reason : description of the change and the source
- who : who added this change
- date : date it was added (approximate).  The original table was created based
  notes from PLZ from November 2024 to May 2025
- comments: additional comments on data processing, not part of the 'reason' for
  the change

Types of changes


Some changes are typo changes not caught until comparing with the checklist or
with taxadb

Some are species updates discovered when close matchers were made, but didn't 
use the match - used what was in checklist (based on common name)


