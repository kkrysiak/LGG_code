##########################################################################################################
################################# Summarize custom logbook for LGG #######################################
######################### Identify cases still needed based on 2019 requirements #########################
##########################################################################################################
library(openxlsx)
library(Amelia)

setwd('~/Box Sync/LGG/Training/')

##########################################################################################################
#### Read in files
##########################################################################################################
## Logbook
cases <- read.xlsx("Logbook.xlsx",colNames=T,detectDates=T)
lists <- read.xlsx("Logbook.xlsx",colNames=T,sheet="Dropdown inputs")

##########################################################################################################
#### Tidy logbook to relevant information
##########################################################################################################
## Remove unused rows (Drop if no CoPath identifier)
cases <- cases[!is.na(cases$`CoPath.#`),]

## Remove notes and extra columns that may have extraneous notes (Notes and any columns beginning in X)
cases <- cases[,colnames(cases)[!grepl("^X|Notes", colnames(cases))]]

## Add new column of results that simplifies anything abnormal
cases$Results_summary <- NA
cases[grepl("normal", cases$Results, ignore.case=T), "Results_summary"] <- "Normal"
cases[grepl("unknown", cases$Results, ignore.case=T), "Results_summary"] <- "Unknown"
cases[!grepl("normal|unknown", cases$Results, ignore.case=T) & !is.na(cases$Results), "Results_summary"] <- "Abnormal"

##########################################################################################################
#### Evaluate logbook overall
##########################################################################################################
## Visualize missing data from the logbook
print(missmap(cases, y.labels = cases$`CoPath.#`))
## Output visualization
pdf(file = "missing_logbook_values.pdf", height=nrow(cases)/4, width=10)
  print(missmap(cases, y.labels = cases$`CoPath.#`))
dev.off()


##########################################################################################################
#### Create tables of logbook requirements
##########################################################################################################
## Total
req <- matrix(nrow=1, ncol=6)
colnames(req) <- c("section", "main_section", "subsection", "min_req", "max_req", "current")
req[1,] <- c("Total", "Total", "-", "200", "0", length(unique(cases[which(cases$Results_summary %in% c("Normal","Abnormal")),"CoPath.#"])))

## Results
sect <- "Results"
main <- c("Normal","Abnormal")
req <- rbind(req,c(sect, main[1], "-", 0, 100, length(unique(cases[which(cases$Results_summary == "Normal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], "-", 100, 0, length(unique(cases[which(cases$Results_summary == "Abnormal"),"CoPath.#"]))))

## Testing category
sect <- "Category"
main <- lists$Testing.Category[!is.na(lists$Testing.Category)]
sub <- c("NIPS","NIPS with confirmation","NIPS with patient results")
req <- rbind(req,c(sect, main[1], "-", 30, 50, length(unique(cases[which(cases$Category == main[1]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[1], sub[1], 3, 10, length(unique(cases[which(cases$Category == main[1] & cases$Sample.Type == "NIPT"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[1], sub[2], 1, 10, length(unique(cases[which(cases$Category == main[1] & cases$Testing.Subtype == "concomitant cytogenetics or molecular method"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[1], sub[3], 1, 10, length(unique(cases[which(cases$Category == main[1] & cases$Roles == "10. Oral communication - patients"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], "-", 40, 50, length(unique(cases[which(cases$Category == main[2]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[3], "-", 10, 50, length(unique(cases[which(cases$Category == main[3]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[4], "-", 10, 50, length(unique(cases[which(cases$Category == main[4]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[5], "-", 0, 20, length(unique(cases[which(cases$Category == main[5]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[6], "-", 0, 10, length(unique(cases[which(cases$Category == main[6]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[7], "-", 60, 0, length(unique(cases[which(cases$Category == main[7]),"CoPath.#"]))))
sub <- c("Cytogenetic methods","Molecular methods")
req <- rbind(req,c(sect, main[7], sub[1], 30, 50, length(unique(cases[which(cases$Category == main[7] & cases$Lab.Testing.Methods %in% c("1. G-banding","2. FISH","3. CMA")),"CoPath.#"]))))
req <- rbind(req,c(sect, main[7], sub[2], 30, 50, length(unique(cases[which(cases$Category == main[7] & cases$Lab.Testing.Methods %in% c("4. Mutation Analysis","5. Sequence Analysis")),"CoPath.#"]))))

## Methodology
sect <- "Methodology"
main <- lists$Testing.Methodology[!is.na(lists$Testing.Methodology)]
sub <- lists$Testing.Method.Subcategory[!is.na(lists$Testing.Method.Subcategory)]
req <- rbind(req,c(sect, main[1], "-", 50, 0, length(unique(cases[which(cases$Category == main[1]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[1], sub[1], 0, 20, length(unique(cases[which(cases$Category == main[1] & cases$Testing.Subtype == "alone"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], "-", 30, 0, length(unique(cases[which(cases$Category == main[2]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], sub[1], 0, 10, length(unique(cases[which(cases$Category == main[2] & cases$Testing.Subtype == "alone"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[3], "-", 30, 0, length(unique(cases[which(cases$Category == main[3]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[3], sub[1], 0, 10, length(unique(cases[which(cases$Category == main[3] & cases$Testing.Subtype == "alone"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[4], "-", 20, 0, length(unique(cases[which(cases$Category == main[4]),"CoPath.#"]))))
mut <- sub[grepl(4, sub)]
for(i in 1:length(mut)){
  req <- rbind(req,c(sect, main[4], mut[i], 5, 5, length(unique(cases[which(cases$Category == main[4] & cases$Testing.Subtype == mut[i]),"CoPath.#"]))))
}
req <- rbind(req,c(sect, main[5], "-", 40, 0, length(unique(cases[which(cases$Category == main[5]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[5], "Sanger", 50, 0, length(unique(cases[which(cases$Category == main[5] & cases$Testing.Subtype == "Sequencing: Sanger [5.a]"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[5], "NGS", 0, 20, length(unique(cases[which(cases$Category == main[5] & grepl("NGS", cases$Testing.Subtype)),"CoPath.#"]))))

## Role 
sect <- "Role"
main <- lists$Roles[!is.na(lists$Roles)]
sub <- c("Cases with 3 roles","Abnormal results")
req <- rbind(req,c(sect, "Roles 1-7", "-", 100, 0, length(unique(cases[which(cases$Category %in% main[1:7]),"CoPath.#"]))))
req <- rbind(req,c(sect, "Roles 1-7", sub[1], 180, 0, "TBD")) ############################### revisit
req <- rbind(req,c(sect, main[1], "-", 0, 0, length(unique(cases[which(cases$Category == main[1]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], "-", 0, 0, length(unique(cases[which(cases$Category == main[2]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[3], "-", 0, 0, length(unique(cases[which(cases$Category == main[3]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[4], "-", 0, 0, length(unique(cases[which(cases$Category == main[4]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[5], "-", 0, 0, length(unique(cases[which(cases$Category == main[5]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[6], "-", 0, 0, length(unique(cases[which(cases$Category == main[6]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[7], "-", 0, 0, length(unique(cases[which(cases$Category == main[7]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[8], "-", 100, 0, length(unique(cases[which(cases$Category == main[8]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[9], "-", 10, 0, length(unique(cases[which(cases$Category == main[9]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[9], sub[2], 5, 0, length(unique(cases[which(cases$Category == main[9] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[10], "-", 20, 0, length(unique(cases[which(cases$Category == main[10]),"CoPath.#"]))))
req <- rbind(req,c(sect, main[10], sub[2], 10, 0, length(unique(cases[which(cases$Category == main[10] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))


summary(cases)
