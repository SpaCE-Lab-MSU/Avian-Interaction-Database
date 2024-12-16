## additional code for AvianInterationData_L1
## for cleaning uncertain interactions
## add after line 555

#create second dataframe for checking
intxns12u <- intxns12

## removing interactions with certain keywords
intxns12u <- intxns12u[!grepl("alleged",intxns12u$uncertain_interaction,ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("anecdotal",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("artificial",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("assumed",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("captive",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("captivity",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("circumstantial",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("conjectured",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("disputed",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("does not",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("dubious",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("erroneous",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("human",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("hypothesized",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("little evidence",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("may",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("maybe",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("might",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("mounted",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("none",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("not",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("no ",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("perhaps",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("playback",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("possible",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("possibly",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("potential",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("presumed",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("purported",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("name changes",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("name_changes",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("name", intxns12u$uncertain_interaction,  ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("speculative",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("suggested",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("suggests",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("suspected",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("unclear",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("unknown",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("questionable",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("thought to be",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("uncertain",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("unconfirmed",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("unfounded",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("unlikely",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("unspecified",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("unsure",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("different",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("inferred",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("referring",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("experiment",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("specified",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("unidentified",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("recently split", intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("previously",intxns12u$uncertain_interaction, ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("?",intxns12u$uncertain_interaction, fixed = TRUE),]
intxns12u <- intxns12u[!grepl("yes",intxns12u$uncertain_interaction,ignore.case = TRUE),]
intxns12u <- intxns12u[!grepl("unsubstantiated",intxns12u$uncertain_interaction,ignore.case = TRUE),]

#check list for additional intxns to remove
sort(unique(intxns12u$uncertain_interaction))

# remove blanks
intxns12u <- intxns12u[!(intxns12u$uncertain_interaction=="" | is.na(intxns12u$uncertain_interaction)), ]

#Emily - for checking remaining uncertain intxns
write.csv(intxns12u,file.path(dir,"uncertain_cleaned.csv"),row.names = F)
