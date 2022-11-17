# Instructions for Data Entry in Avian Interaction Database in L0 subdirectory

- NOTE: Refer to the main README.md file in the parent directory of this repository for information about the Workflow and subdirectory naming conventions.

## OVERVIEW: Data entry & checking on bird-bird interactions occurs within this L0 portion of the repository. The repository L0 folder contains the following files:

### **AvianInteractionData.csv** 
= Entry of all records. Each record in this csv file is a unique interaction between 2 species. Note that 2 species can interact in more than one way (equating to multiple rows), and there may be duplicate entries of interactions, each from different species accounts.

### **AvianInteractionData_metadata.csv** 
= Metadata for columns in **AvianInteractionData.csv**.

### **AvianInteractionData_BBS_Species.csv** 
= Species look-up table and assignments for data entry.

### **AvianInteractionData_metadata_interactiondefinitions.csv**
= Metadata for interaction types.


## Step 1: Get started in GitHub
The first time you use GitHub, do the following: 

- Sign into GitHub and navigate to: https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database. 

- Install GitHub Desktop (or if you use github command line, skip this step).

- Clone this entire repository by clicking on the green "Code" button and selecting "Open with GitHub Desktop". Save the location of the repository on your computer (not inside a Google Drive or Dropbox or One Drive folder).

## Step 2: Pull the most recent version of the Database
In GitHub Desktop, select Avian-Interaction-Database. Click "Fetch origin" to pull the most recent version of the database, which will save it to your GitHub location on your computer (established in Step 1). 

## Step 3: Select a Species to Work on
Navigate to the GitHub Avian-Interaction-Database repository folder on your computer. Open the **AvianInteraction_BBS_Species.csv** file in a spreadsheet program of your choice (you may have to import it; be sure to define it as comma separated values). Acceptable programs include Google Sheets or Excel. It is sorted by phylogeny - as you would find the list of birds flipping through a bird guide. Select a species with a "cavitynest_er_interactor" designation that has not been entered yet, fill in your name or initials with that entry in the "recorder" column. In GitHub Desktop, COMMIT & PUSH this change by:

- writing a note to describe the action as: "adding initials to <insert species name>." clicking "Commit to main", then syncing the changes with the version online by clicking "Push Origin". *EACH* time you select a new species, be sure to follow this step thoroughly so that others will see that you have selected the species (and won't also select it).

## Step 4: Data entry in the Database
a) Open the **AvianInteractionData.csv** file in a spreadsheet program of your choice (you may have to import it; be sure to define it as comma separated values). Acceptable programs include Google Sheets or Excel. 

- If you use Google Sheets and the view does not freeze the top row, select the top row with the headers, go to “View”, “Freeze”, “Freeze 1 Row”. If your Google Sheet view does not freeze the first 6 columns, select the top row with the headers, go to “View”, “Freeze”, “2 Columns”. Then you can drag the Freeze Column line to cover the next few columns so that the first 6 columns are frozen.

- If you use Excel and the view does not freeze the top row, go to "View" and select "Freeze top row". To freeze the first 6 columns, go to "View" and select "Split" then drag the split line to the end of the first 6 columns.

b) Go to Birds of the World Online (BOW, via the MSU Library electronic resource portal if you’re off campus). This is the source of natural history information for each bird species entered into the database. 

- In general, the best approach to entering the species' data is to open the BOW species account and just skim through it page by page (section by section, in order) for capitalized species names and italicized species names; this should catch nearly all the entries and will add new ones we missed before. 

c) Enter interactions for the species you selected by designating the selected species in "species1_scientific"" in **AvianInteractionData.csv**, and the species it interacts with in "species2_scientific", based on evidence from Birds of the World Online. Refer to **AvianInteractionData_metadata.csv** for rules about how to enter each column, and the information below:

- *Important:* you need to copy-paste the full URL for each URL page that has the source(s) of the interaction. Separate URLs are entered in “sourceAupdatedURL”, “sourceBupdatedURL” etc. if there are multiple BOW pages with information on the same interaction (e.g., “behavior” and “Introduction” pages for example). Then each text pertaining to the interaction is copied into the corresponding notes column: notesA for sourceAupdatedURL, etc.

- A full URL looks like this so that “behavior” is visible: https://birdsoftheworld-org.proxy2.cl.msu.edu/bow/species/gofwoo/cur/behavior
Refer to the species1 account on Birds of the World Online. Note that the website contains a table of contents with different sections (Introduction, Systematics, Appearance, etc.). Most of the information relevant to species interactions will occur in a few of these (namely, Behavior, Breeding, Habitat). However, please skim through ALL SECTIONS to be sure we catch all interactions with other birds.

- On each section, skim for Italicized words - these are usually Genus Species names. Occasionally species will be listed in just Capital Letters (e.g., Yellow Warbler). Determine if the description means that species1 is interacting (or has an inferred interaction) with species2. 

- If 2 species interact in multiple ways, fill in a separate row for each interaction.

- For “weak” or “strong” BNA_evidence, this can be less clear. Here are some guidelines:
Enter “weak” if the description lacks a source, mentions that it is a “possible” interaction, rare interaction, or inferred interaction.
Enter “strong” if the interaction was observed, was strongly state, and/or has a source. 

- If a specific subspecies is mentioned in reference, then include in species name (Genus species subspecies). If not, use Genus species.

- If you have questions on the interaction itself, enter “YES” into the “uncertain_interaction” column which will flag it for follow-up.

- Note that for species1, its interaction with species2 will become part of species2’s pairwise interactions. NOTE: if a duplicate entry occurs, it’s ok (we will edit in R). It takes too much time to avoid duplicates by searching for the species2 entries that already exist.

- As long as there is a genus & species for a Species1 and Species2, don’t worry about filling in the empty columns (we can fill them in with R). Don’t worry about AOU number being blank for some species (also fillable by R). 

d) When you have finished adding all new entries in **AvianInteractionData.csv** for a species1 and its interactions with species2:

- Update "BBS_InteractionPairs_entry" column in **AvianInteractionData_BBS_Species.csv** with a new date (add date to an existing date: e.g., 3/3/2014-11/23/2021, or just add the date if there is no date there already). Paste in the citation for the BOW_citation in **AvianInteractionData_BBS_Species.csv**. This can be found at the bottom of each webpage of the species’ BOW record.

## Step 5: Commit & Push the changes you made at least once/day (for days you entered data)

At least once a day, push the changes to **AvianInteractionData.csv**. In GitHub Desktop, select the Avian-Interaction-Database repository. Click "Fetch origin" to pull the most recent version of the database, which will save it to your GitHub location on your computer (established in Step 1). 

In GitHub Desktop, COMMIT & PUSH your changes by:

- writing a note to describe the action as: "adding entries for <insert species name>." clicking "Commit to main", then syncing the changes with the version online by clicking "Push Origin". 









