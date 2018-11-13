##########################################################################################################
################################# Summarize custom logbook for LGG #######################################
######################### Identify cases still needed based on 2019 requirements #########################
##########################################################################################################
library(openxlsx)
library(Amelia)
library(ggplot2)
library(gridExtra)
library(stringr)
library(plyr)

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

## Remove PHI / unnecessary columns
drop <- c("Entry","Date.collected","Date.Accessioned","Patient.name","DOB","MRN.#","Nomenclature")
cases <- cases[,!(colnames(cases) %in% drop)]

##########################################################################################################
#### Get cases with 3 roles
##########################################################################################################
## Get a list of 7 required roles 
seven <- lists[!is.na(lists$Roles),"Roles"]
seven <- seven[!grepl("Roles|Written|Oral", seven)]

## get a subset of cases with at least 3 roles
three_roles <- cases[which(cases$Roles %in% seven),]
three_roles_count <- count(three_roles$`CoPath.#`)
three_roles_count <- three_roles_count[which(three_roles_count$freq >= 3),]
three_roles_abnormal <- count(three_roles[three_roles$Results_summary == "Abnormal","CoPath.#"])
three_roles_abnormal <- three_roles_abnormal[which(three_roles_abnormal$freq >= 3),]

##########################################################################################################
#### Create tables of logbook requirements
##########################################################################################################
## Total
req <- matrix(nrow=1, ncol=7)
colnames(req) <- c("section", "main_section", "subsection", "min_req", "max_req", "current", "current_abnormal")
req[1,] <- c("Total", "Total", "-", "200", "0", length(unique(cases[which(cases$Results_summary %in% c("Normal","Abnormal")),"CoPath.#"])), length(unique(cases[which(cases$Results_summary %in% c("Abnormal")),"CoPath.#"])))

## Results
sect <- "Total"
main <- c("Normal","Abnormal")
req <- rbind(req,c(sect, main[1], "All", 0, 100, length(unique(cases[which(cases$Results_summary == "Normal"),"CoPath.#"])), NA))
req <- rbind(req,c(sect, main[2], "All", 100, 0, length(unique(cases[which(cases$Results_summary == "Abnormal"),"CoPath.#"])), length(unique(cases[which(cases$Results_summary == "Abnormal"),"CoPath.#"]))))

## Testing category
sect <- "Category"
main <- lists$Testing.Category[!is.na(lists$Testing.Category)]
sub <- c("NIPS","NIPS with confirmation","NIPS with patient results")
req <- rbind(req,c(sect, main[1], "All", 30, 50, length(unique(cases[which(cases$Category == main[1]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[1] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[1], sub[1], 3, 10, length(unique(cases[which(cases$Category == main[1] & cases$Sample.Type == "NIPT"),"CoPath.#"])), length(unique(cases[which(cases$Category == main[1] & cases$Sample.Type == "NIPT" & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[1], sub[2], 1, 10, length(unique(cases[which(cases$Category == main[1] & cases$Testing.Subtype == "concomitant cytogenetics or molecular method"),"CoPath.#"])), length(unique(cases[which(cases$Category == main[1] & cases$Testing.Subtype == "concomitant cytogenetics or molecular method" & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[1], sub[3], 1, 10, length(unique(cases[which(cases$Category == main[1] & cases$Roles == "10. Oral communication - patients"),"CoPath.#"])), length(unique(cases[which(cases$Category == main[1] & cases$Roles == "10. Oral communication - patients" & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], "All", 40, 50, length(unique(cases[which(cases$Category == main[2]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[2] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[3], "All", 10, 50, length(unique(cases[which(cases$Category == main[3]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[3] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[4], "All", 10, 50, length(unique(cases[which(cases$Category == main[4]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[4] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[5], "All", 0, 20, length(unique(cases[which(cases$Category == main[5]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[5] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[6], "All", 0, 10, length(unique(cases[which(cases$Category == main[6]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[6] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[7], "All", 60, 0, length(unique(cases[which(cases$Category == main[7]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[7] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
sub <- c("Cytogenetic methods","Molecular methods")
req <- rbind(req,c(sect, main[7], sub[1], 30, 50, length(unique(cases[which(cases$Category == main[7] & cases$Lab.Testing.Methods %in% c("1. G-banding","2. FISH","3. CMA")),"CoPath.#"])), length(unique(cases[which(cases$Category == main[7] & cases$Lab.Testing.Methods %in% c("1. G-banding","2. FISH","3. CMA") & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[7], sub[2], 30, 50, length(unique(cases[which(cases$Category == main[7] & cases$Lab.Testing.Methods %in% c("4. Mutation Analysis","5. Sequence Analysis")),"CoPath.#"])), length(unique(cases[which(cases$Category == main[7] & cases$Lab.Testing.Methods %in% c("4. Mutation Analysis","5. Sequence Analysis") & cases$Results_summary == "Abnormal"),"CoPath.#"]))))

## Methodology
sect <- "Methodology"
main <- lists$Testing.Methodology[!is.na(lists$Testing.Methodology)]
sub <- lists$Testing.Method.Subcategory[!is.na(lists$Testing.Method.Subcategory)]
req <- rbind(req,c(sect, main[1], "All", 50, 0, length(unique(cases[which(cases$Category == main[1]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[1] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[1], sub[1], 0, 20, length(unique(cases[which(cases$Category == main[1] & cases$Testing.Subtype == "alone"),"CoPath.#"])), length(unique(cases[which(cases$Category == main[1] & cases$Testing.Subtype == "alone" & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], "All", 30, 0, length(unique(cases[which(cases$Category == main[2]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[2] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], sub[1], 0, 10, length(unique(cases[which(cases$Category == main[2] & cases$Testing.Subtype == "alone"),"CoPath.#"])), length(unique(cases[which(cases$Category == main[2] & cases$Testing.Subtype == "alone" & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[3], "All", 30, 0, length(unique(cases[which(cases$Category == main[3]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[3] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[3], sub[1], 0, 10, length(unique(cases[which(cases$Category == main[3] & cases$Testing.Subtype == "alone"),"CoPath.#"])), length(unique(cases[which(cases$Category == main[3] & cases$Testing.Subtype == "alone" & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[4], "All", 20, 0, length(unique(cases[which(cases$Category == main[4]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[4] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
mut <- sub[grepl(4, sub)]
for(i in 1:length(mut)){
  req <- rbind(req,c(sect, main[4], mut[i], 5, 5, length(unique(cases[which(cases$Category == main[4] & cases$Testing.Subtype == mut[i]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[4] & cases$Testing.Subtype == mut[i] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
}
req <- rbind(req,c(sect, main[5], "All", 40, 0, length(unique(cases[which(cases$Category == main[5]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[5] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[5], "Sanger", 50, 0, length(unique(cases[which(cases$Category == main[5] & cases$Testing.Subtype == "Sequencing: Sanger [5.a]"),"CoPath.#"])), length(unique(cases[which(cases$Category == main[5] & cases$Testing.Subtype == "Sequencing: Sanger [5.a]" & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[5], "NGS", 0, 20, length(unique(cases[which(cases$Category == main[5] & grepl("NGS", cases$Testing.Subtype)),"CoPath.#"])), length(unique(cases[which(cases$Category == main[5] & grepl("NGS", cases$Testing.Subtype) & cases$Results_summary == "Abnormal"),"CoPath.#"]))))

## Role 
sect <- "Role"
main <- lists$Roles[!is.na(lists$Roles)]
sub <- c("Cases with 3 roles","Abnormal results")
req <- rbind(req,c(sect, "Roles 1-7", "All", 100, 0, length(unique(cases[which(cases$Category %in% main[1:7]),"CoPath.#"])), length(unique(cases[which(cases$Category %in% main[1:7] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, "Roles 1-7", sub[1], 180, 0, nrow(three_roles_count), nrow(three_roles_abnormal)))
req <- rbind(req,c(sect, main[1], "All", 0, 0, length(unique(cases[which(cases$Category == main[1]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[1] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], "All", 0, 0, length(unique(cases[which(cases$Category == main[2]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[2] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[3], "All", 0, 0, length(unique(cases[which(cases$Category == main[3]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[3] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[4], "All", 0, 0, length(unique(cases[which(cases$Category == main[4]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[4] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[5], "All", 0, 0, length(unique(cases[which(cases$Category == main[5]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[5] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[6], "All", 0, 0, length(unique(cases[which(cases$Category == main[6]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[6] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[7], "All", 0, 0, length(unique(cases[which(cases$Category == main[7]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[7] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[8], "All", 100, 0, length(unique(cases[which(cases$Category == main[8]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[8] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[9], "All", 10, 0, length(unique(cases[which(cases$Category == main[9]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[9] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[9], sub[2], 5, 0, length(unique(cases[which(cases$Category == main[9] & cases$Results_summary == "Abnormal"),"CoPath.#"])), length(unique(cases[which(cases$Category == main[9] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[10], "All", 20, 0, length(unique(cases[which(cases$Category == main[10]),"CoPath.#"])), length(unique(cases[which(cases$Category == main[10] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[10], sub[2], 10, 0, length(unique(cases[which(cases$Category == main[10] & cases$Results_summary == "Abnormal"),"CoPath.#"])), length(unique(cases[which(cases$Category == main[10] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))

req <- as.data.frame(req, stringsAsFactors=F)
req$to_do <- as.numeric(req$min_req) - as.numeric(req$current)
req[which(req$to_do < 0),"to_do"] <- 0
req$section_label <- paste(req$main_section, "(", req$subsection,")")
req$min_req <- as.numeric(req$min_req)
req$max_req <- as.numeric(req$max_req)
req$current <- as.numeric(req$current)
req$current_abnormal <- as.numeric(req$current_abnormal)
req$requirement <- "Not met"
req[which(req$current > req$min_req),"requirement"] <- "Met"
req$requirement_abnormal <- "Not met"
req[which(req$current_abnormal > req$min_req),"requirement_abnormal"] <- "Met"
req$shape <- 21

## Define color palette
color_palette=c("Not met"="red","Met"="green","Abnormal" = "white")

tot <- ggplot(req[which(req$section == "Total"),], aes(section_label, max_req)) + geom_crossbar(aes(ymin = min_req, ymax = max_req), width = 0.2, fill="grey", linetype="blank") + geom_point(aes(section_label, current, color=requirement), size = 4) + geom_point(aes(section_label, current_abnormal), shape = 1, size = 4) + labs(x="", y="# Required") + scale_color_manual(values=color_palette) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) + theme(legend.position="none") + ggtitle("Total")

cat <- ggplot(req[which(req$section == "Category"),], aes(section_label, max_req)) + geom_crossbar(aes(ymin = min_req, ymax = max_req), width = 0.2, fill="grey", linetype="blank") + geom_point(aes(section_label, current, color=requirement), size = 4) + labs(x="", y="# Required") + geom_point(aes(section_label, current_abnormal), shape = 1, size = 4) + scale_color_manual("Requirement", values=color_palette) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) + ggtitle("Category")

meth <- ggplot(req[which(req$section == "Methodology"),], aes(section_label, max_req)) + geom_crossbar(aes(ymin = min_req, ymax = max_req), width = 0.2, fill="grey", linetype="blank") + geom_point(aes(section_label, current, color=requirement), size = 4) + geom_point(aes(section_label, current_abnormal), shape = 1, size = 4) + labs(x="", y="# Required") + scale_color_manual(values=color_palette) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) + theme(legend.position="none") + ggtitle("Methodology")

role <- ggplot(req[which(req$section == "Role"),], aes(section_label, max_req)) + geom_crossbar(aes(ymin = min_req, ymax = max_req), width = 0.2, fill="grey", linetype="blank") + geom_point(aes(section_label, current, color=requirement), size = 4) + geom_point(aes(section_label, current_abnormal), shape = 1, size = 4) + labs(x="", y="# Required") + scale_color_manual(values=color_palette) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 33)) + theme(legend.position="none") + ggtitle("Role")

png(file = "logbook_status.png", height=1700, width=2700, res=150)
  grid.arrange(tot,meth,role,cat, ncol=2, layout_matrix = rbind(c(1,2,2,2,2,2),c(3,3,3,4,4,4)))
dev.off()

## Export the table
write.table(req, file="logbook_summary.tsv", quote=F, row.names=F, col.names=T, sep="\t")


# ## Tidy naming
# 
# req$main <- gsub("\\S+\\. ","",req$main_section, perl=T)
# req$main <- gsub(" \\(\\N+\\)","",req$main, perl=T)
# req$main <- gsub(" - including digital image capture","",req$main, perl=T)
# req$main <- gsub(" ","_",req$main, perl=T)
# req$main <- gsub("-_","",req$main, perl=T)
# req$main <- gsub("&_","",req$main, perl=T)
# req$main <- gsub("G-","G_",req$main, perl=T)
# req$main <- gsub("1-7","1to7",req$main, perl=T)
# req$main <- gsub("1-7","1to7",req$main, perl=T)
# req$sub <- gsub(" \\[\\S+\\.\\S+\\]","",req$subsection, perl=T)
# 
# ## Plot using facets - not currently functional
# ggplot(req[,c("sub","max_req","min_req","current","requirement","main")], aes(sub, max_req)) + geom_crossbar(aes(ymin = min_req, ymax = max_req), width = 0.2, fill="grey", linetype="blank") + geom_point(aes(sub, current, color=requirement), size = 4) + labs(x="Section", y="Required") + scale_color_manual(values=color_palette) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(req$main)
# 
# ggplot(req, aes(sub, max_req)) + geom_crossbar(aes(ymin = min_req, ymax = max_req), width = 0.2, fill="grey", linetype="blank") + geom_point(aes(sub, current, color=requirement), size = 4) + labs(x="Section", y="Required") + scale_color_manual(values=color_palette) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(req$main)
# 
# 
# summary(cases)
# summary(req)