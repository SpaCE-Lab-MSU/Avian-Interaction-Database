# Taxonomy Tracing

finding the currently accepted scientific name of bird species when compiling historical data

## Goals

given either a species scientific name or common name, learn the history of it's naming: current, previous and maybe future names

make clements_updates db: lists of all years of clements data for looking up species in the end, should be a flat file with any and all species listed that ever were with current status (no longer used, in use ) last year found previous name -\> links back to species names that it was before (and ids) future names -\> links to all splits and future renames (and ids ) common name (plural?)

given a common name: list of all scientific names that it could refer to and year that appeared

Show what happen when looking up species (sciname, common name) from Clements in other databases using either TaxaDB or Taxize. show results of CURL from Avibase

## other sources of data

Avibase can compare any two lists:

Clements compare 2024 -\> 2023 https://avibase.bsc-eoc.org/compare.jsp?source1=clements&version1=CLEMENTS2024&source2=clements&version2=CLEMENTS2023&continent=NAM&region=NA1&reg_type=3

Clements compare 2023-\> 2024 https://avibase.bsc-eoc.org/compare.jsp?source1=clements&version1=CLEMENTS2023&source2=clements&version2=CLEMENTS2024&continent=NAM&region=ABA&reg_type=9

Clements compare 2022-\> 2023 https://avibase.bsc-eoc.org/compare.jsp?source1=clements&version1=CLEMENTS2022&source2=clements&version2=CLEMENTS2023&continent=NAM&region=ABA&reg_type=9

note when there is split, this tool maybe can't tell if they are connect.\
It says that the entry for 2022 Diomedea exulans Wandering Albatross id=wanalb sort row id = 6780

was removed in 2023, but in fact the common name was changed to 'Snowy Alabtross' same species name, same species_code ( wanalb1 ) , 2023 sort row 6816 but links back to 6781

2023 entry Diomedea antipodensis which points back to 2022 row id 6783, which is the Diomedea exulans

but new entry with category "slash" meaning either or subspecies and it does not have links back to previous record (no previous row id is given for slash record)

https://avibase.bsc-eoc.org/compare.jsp?source1=aou&version1=AOU_7_64&source2=aou&version2=AOU_7_65&continent=®\_type=3

given a scientific OR common name

## Structure

for each year, columns seem to have different names, so need harmonize structure across years

build both ways and put in same table

create new ClementsIDs which is `[year]_[row order]` (row is sometimes called taxon_order)

for each year create row looking back from existing data

-   clements_id same stuff and clements_id_prev

Tables to build

goal : given any sciname (OR common name) what is current state?

outcomes:

```         
1. sciname is current sciname, and there were no splits ever
2. sciname is a current one, BUT there was a split at some point so this may be the other species
3. sciname is not current - there is a new one but there was no split so just use the new one
4. sciname is not current and there was a split, so need to choose one of the new ones
```

possible structure:

sciname current? y/n\
has changed ever? y/n current_sciname(s) current_commonname other commonname(s) years is appears in Clements (this is mostly diagnostic)

```         
was split at some point? (is there a way we can see if a sciname ever had a change)
new split off species(es)
```

common name focused woudl be the same but index on common name

tables we can build from existing data

1.  backward looking across years, all backwards links to previous IDs, across years show if there is different name in history
    a.  take from just one file `sciname common year id   "previous year ID"`
2.  future looking

looking at two years' tables, for example 2023 and 2022

start with 2023 get the info, find row_idf for 2022, then build from 2022

`2022 sciname common  2023_sciname 2023_common  common_future_change: y/n sciname_future_change: y/n 2023_change_text`

2022 sciname could be repeated for a split. then a decision needs to be made!

3.  comprehensive table

combine all versions of future looking table group by sciname collect all future names and consolidate

9.  all clements row ids for all years can be creatd (but not that useful)

```         
species           2006 2007 2008 2009 etc
Diomedea exulans  id   id   id
```

how does this help? it doesn't find any changes

need a lookup, given a name, what is it now? what could it be in the past?

Diomedea exulans

species common name year1 rowid_this_year next year rowid_next_year

the "Clements" list - goes back to 2008 but not before - does not always have species code - has the sort_order = unique row number for that year - has last years sort_order

the "ebird-Clements" - does not go far back -

changes

no change

```         
sort_now -> sort_old
species_id = species_id
```

completely new: no sort_old sort_new

rename: easy trace sort_now -\> sort_old

split: split A: new names sp1, sp2 -\> spOrig split B: one same, one new spOrig could be sp1 spOrig -\> spOrig

```         
sort_now -> sort_old
```