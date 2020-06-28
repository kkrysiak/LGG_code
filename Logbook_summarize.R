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
library(reshape2)

setwd('~/Box Sync/LGG/Training/Logbook/')  ## MGI macbook
#setwd('~/Box/LGG/Training/Logbook/') ## Pathology macbook

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

## Add new column of results that simplifies anything abnormal
cases$Results_summary <- NA
cases[grepl("normal", cases$Results, ignore.case=T), "Results_summary"] <- "Normal"
cases[grepl("unknown", cases$Results, ignore.case=T), "Results_summary"] <- "Unknown"
cases[!grepl("normal|unknown", cases$Results, ignore.case=T) & !is.na(cases$Results), "Results_summary"] <- "Abnormal"

## Output file in simple text format in case excel ever breaks
write.table(cases, file="logbook.tsv", quote=F, row.names=F, col.names=T, sep="\t")

## Remove notes and extra columns that may have extraneous notes (Notes and any columns beginning in X)
cases <- cases[,colnames(cases)[!grepl("^X|Notes", colnames(cases))]]

##########################################################################################################
#### Rework the logbook into ACMG versioning
##########################################################################################################
## Reduce to necessary columns
keep <- c("Entry","Date.collected","CoPath.#","Case.occurrence.tally","Category","Testing.Subtype","Lab.Testing.Methods","Results","Roles","Supervisor","Results_summary")
acmg <- cases[,(colnames(cases) %in% keep)]

## Remove problematic name
colnames(acmg)[which(colnames(acmg) == "CoPath.#")] <- "CoPath"

## Strip problematic characters
strip <- function(x){
  x <- gsub("\\s\\[|\\S*\\]|\\s\\(\\N*\\)", "", x, perl=T)
  x <- gsub("\\N+\\.\\s|:", "", x, perl=T)
  x <- gsub("\\s", "_", x, perl=T)
  x <- gsub("\\:|_-|_&", "", x, perl=T)
  x <- gsub("-", "_", x, perl=T)
}

acmg$Testing.Subtype <- strip(acmg$Testing.Subtype)
acmg$Lab.Testing.Methods <- strip(acmg$Lab.Testing.Methods)
acmg$Roles <- strip(acmg$Roles)
acmg$Category <- gsub("2. Diagnostic testing (postnatal, non-oncology)", "2. Diagnostic testing", acmg$Category)

## Replace testing subtype with NA unless NGS or mutation subtype
acmg1 <- acmg
acmg1$Testing.Subtype <- gsub("alone|concomitant_cytogenetics_or_molecular_method", NA, acmg1$Testing.Subtype)

## Alter columns to support ACMG excel style
acmg1 <- dcast(acmg1, Entry + Date.collected + CoPath + Category + Results  + Supervisor + Results_summary ~ Lab.Testing.Methods + Testing.Subtype, fun.aggregate = length)
colnames(acmg1) <- gsub("^NA","Missing.method", colnames(acmg1))
acmg2 <- dcast(acmg, Entry + Date.collected + CoPath + Category + Testing.Subtype + Results  + Supervisor + Results_summary ~ Roles, fun.aggregate = length)
colnames(acmg2) <- gsub("NA","Missing.role", colnames(acmg2))
acmg2$role.count <- rowSums(acmg2[colnames(acmg2) %in% unique(acmg$Roles)])
acmg_style <- merge(acmg1, acmg2, all=T)

# ## Change names to make export easier
# acmg_style$Category <- gsub("|\\(|\\s*\\)","",acmg_style$Category,perl=T)
# colnames(acmg_style) <- colnames(acmg_style)[!(colnames(acmg_style)) %in% c("NA","Roles")]
# colnames(acmg_style) <- gsub("\\s\\[|\\S*\\]|\\(|\\s*\\)","",colnames(acmg_style),perl=T)
# colnames(acmg_style) <- gsub("\\N+\\.\\s|:","",colnames(acmg_style),perl=T)
# colnames(acmg_style) <- gsub("\\s_-","_",colnames(acmg_style),perl=T)

## Output ACMG-style file
write.table(acmg_style, file=paste0("logbook_acmgstyle_",Sys.Date(),".tsv"), quote=F, row.names=F, col.names=T, sep="\t")

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
# ## Get a list of 7 required roles 
# seven <- lists[!is.na(lists$Roles),"Roles"]
# seven <- seven[!grepl("Roles|Written|Oral", seven)]
######### Roles 1-7 must be involved but 3 roles can come from roles 1-10

## get a subset of cases with at least 3 roles
three_roles <- unique(cases[,c("CoPath.#","Roles","Results_summary")])
three_roles_count <- ddply(three_roles, .(`CoPath.#`), nrow)
three_roles_count <- three_roles_count[which(three_roles_count$V1 >= 3),]
three_roles_noNA <- three_roles[!is.na(three_roles$Results_summary),]
three_roles_abnormal <- ddply(three_roles_noNA, .(three_roles_noNA[three_roles_noNA$Results_summary == "Abnormal","CoPath.#"]), nrow)
three_roles_abnormal <- three_roles_abnormal[which(three_roles_abnormal$V1 >= 3),]

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
req <- rbind(req,c(sect, main[2], "All", 100, 0, NA, length(unique(cases[which(cases$Results_summary == "Abnormal"),"CoPath.#"]))))

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
req <- rbind(req,c(sect, main[1], "All", 50, 0, length(unique(cases[which(cases$Lab.Testing.Methods == main[1]),"CoPath.#"])), length(unique(cases[which(cases$Lab.Testing.Methods == main[1] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[1], sub[1], 0, 20, length(unique(cases[which(cases$Lab.Testing.Methods == main[1] & cases$Testing.Subtype == "alone"),"CoPath.#"])), length(unique(cases[which(cases$Lab.Testing.Methods == main[1] & cases$Testing.Subtype == "alone" & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], "All", 30, 0, length(unique(cases[which(cases$Lab.Testing.Methods == main[2]),"CoPath.#"])), length(unique(cases[which(cases$Lab.Testing.Methods == main[2] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], sub[1], 0, 10, length(unique(cases[which(cases$Lab.Testing.Methods == main[2] & cases$Testing.Subtype == "alone"),"CoPath.#"])), length(unique(cases[which(cases$Lab.Testing.Methods == main[2] & cases$Testing.Subtype == "alone" & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[3], "All", 30, 0, length(unique(cases[which(cases$Lab.Testing.Methods == main[3]),"CoPath.#"])), length(unique(cases[which(cases$Lab.Testing.Methods == main[3] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[3], sub[1], 0, 10, length(unique(cases[which(cases$Lab.Testing.Methods == main[3] & cases$Testing.Subtype == "alone"),"CoPath.#"])), length(unique(cases[which(cases$Lab.Testing.Methods == main[3] & cases$Testing.Subtype == "alone" & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[4], "All", 20, 0, length(unique(cases[which(cases$Lab.Testing.Methods == main[4]),"CoPath.#"])), length(unique(cases[which(cases$Lab.Testing.Methods == main[4] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
mut <- sub[grepl(4, sub)]
for(i in 1:length(mut)){
  req <- rbind(req,c(sect, main[4], mut[i], 5, 5, length(unique(cases[which(cases$Lab.Testing.Methods == main[4] & cases$Testing.Subtype == mut[i]),"CoPath.#"])), length(unique(cases[which(cases$Lab.Testing.Methods == main[4] & cases$Testing.Subtype == mut[i] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
}
req <- rbind(req,c(sect, main[5], "All", 40, 0, length(unique(cases[which(cases$Lab.Testing.Methods == main[5]),"CoPath.#"])), length(unique(cases[which(cases$Lab.Testing.Methods == main[5] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[5], "Sanger", 10, 40, length(unique(cases[which(cases$Lab.Testing.Methods == main[5] & cases$Testing.Subtype == "Sequencing: Sanger [5.a]"),"CoPath.#"])), length(unique(cases[which(cases$Lab.Testing.Methods == main[5] & cases$Testing.Subtype == "Sequencing: Sanger [5.a]" & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[5], "NGS", 10, 40, length(unique(cases[which(cases$Lab.Testing.Methods == main[5] & grepl("NGS", cases$Testing.Subtype)),"CoPath.#"])), length(unique(cases[which(cases$Lab.Testing.Methods == main[5] & grepl("NGS", cases$Testing.Subtype) & cases$Results_summary == "Abnormal"),"CoPath.#"]))))

## Role 
sect <- "Role"
main <- lists$Roles[!is.na(lists$Roles)]
sub <- c("Cases with 3 roles","Abnormal results")
req <- rbind(req,c(sect, "Roles 1-7", "All", 100, 0, length(unique(cases[which(cases$Roles %in% main[1:7]),"CoPath.#"])), length(unique(cases[which(cases$Roles %in% main[1:7] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, "Roles 1-7", sub[1], 180, 0, nrow(three_roles_count), nrow(three_roles_abnormal)))
req <- rbind(req,c(sect, main[1], "All", 0, 0, length(unique(cases[which(cases$Roles == main[1]),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[1] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[2], "All", 0, 0, length(unique(cases[which(cases$Roles == main[2]),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[2] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[3], "All", 0, 0, length(unique(cases[which(cases$Roles == main[3]),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[3] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[4], "All", 0, 0, length(unique(cases[which(cases$Roles == main[4]),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[4] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[5], "All", 0, 0, length(unique(cases[which(cases$Roles == main[5]),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[5] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[6], "All", 0, 0, length(unique(cases[which(cases$Roles == main[6]),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[6] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[7], "All", 0, 0, length(unique(cases[which(cases$Roles == main[7]),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[7] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[8], "All", 100, 0, length(unique(cases[which(cases$Roles == main[8]),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[8] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[9], "All", 10, 0, length(unique(cases[which(cases$Roles == main[9]),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[9] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[9], sub[2], 5, 0, length(unique(cases[which(cases$Roles == main[9] & cases$Results_summary == "Abnormal"),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[9] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[10], "All", 20, 0, length(unique(cases[which(cases$Roles == main[10]),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[10] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))
req <- rbind(req,c(sect, main[10], sub[2], 10, 0, length(unique(cases[which(cases$Roles == main[10] & cases$Results_summary == "Abnormal"),"CoPath.#"])), length(unique(cases[which(cases$Roles == main[10] & cases$Results_summary == "Abnormal"),"CoPath.#"]))))

req <- as.data.frame(req, stringsAsFactors=F)
req$to_do <- as.numeric(req$min_req) - as.numeric(req$current)
req[which(req$to_do < 0),"to_do"] <- 0
req$section_label <- paste(req$main_section, "(", req$subsection,")")
req$min_req <- as.numeric(req$min_req)
req$max_req <- as.numeric(req$max_req)
req$current <- as.numeric(req$current)
req$current_abnormal <- as.numeric(req$current_abnormal)
req$requirement <- "Not met"
req[which(req$current >= req$min_req),"requirement"] <- "Met"
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

##########################################################################################################
#### Extra visualizations
##########################################################################################################
library(viridis)

## Restrict to a subset of columns and restrict to 1 line per case (cases duplicated by multiple roles)
case_bars <- unique(cases[,c("CoPath.#","Diagnosis/Indication","Category","Lab.Testing.Methods","NGS.panel","Sample.Type","Results_summary")])

## Tidy up column names for plotting
colnames(case_bars) <- gsub("Diagnosis/Indication", "Indication", colnames(case_bars))

## Create a function that will upper case the first letter (fix up Indication)
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

case_bars$Indication <- firstup(case_bars$Indication)

## Tidy up result names for plotting
case_bars$Results_summary <- gsub("Normal", "No VUS or pathogenic / actionable variant", case_bars$Results_summary)
case_bars$Results_summary <- gsub("Abnormal", "VUS or pathogenic / actionable variant", case_bars$Results_summary)
## Refactor order for plotting
case_bars$Results_summary <- factor(case_bars$Results_summary, levels=c("VUS or pathogenic / actionable variant","No VUS or pathogenic / actionable variant","Unknown"))

## Create a column to categorize myeloseq vs other NGS testing easily
case_bars$NGS_test <- "Not Myeloseq"
case_bars[which(case_bars$NGS.panel == "Myeloseq"),"NGS_test"] <- "Myeloseq"

## Bin NGS testing types
case_bars$NGS_category <- "NA"
case_bars[which(case_bars$NGS.panel == "Myeloseq"),"NGS_category"] <- "Myeloseq"
case_bars[grepl("SOMA",case_bars$NGS.panel),"NGS_category"] <- "SOMA"
case_bars[grepl("Renal",case_bars$NGS.panel),"NGS_category"] <- "Renal"
case_bars[grepl("Cancer|TP53",case_bars$NGS.panel),"NGS_category"] <- "Cancer"
case_bars[grepl("Send Out",case_bars$NGS.panel),"NGS_category"] <- "Send Out"
case_bars[grepl("Q/C|Neutropenia|Cardiac",case_bars$NGS.panel),"NGS_category"] <- "Other"

## Plot case results by Testing method
ggplot(case_bars, aes(x=Lab.Testing.Methods, fill=Results_summary)) + geom_bar(position = 'stack') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 22))  + scale_fill_manual(values=c("No VUS or pathogenic / actionable variant"="green3","VUS or pathogenic / actionable variant"="red2","Unknown"="grey"), na.value="grey50")

## Plot case results by NGS panel
ggplot(case_bars[case_bars$Lab.Testing.Methods == "5. Sequence Analysis",], aes(x=NGS.panel, fill=Results_summary)) + geom_bar(position = 'stack', stat="count") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 22)) + scale_fill_manual(values=c("No VUS or pathogenic / actionable variant"="green3","VUS or pathogenic / actionable variant"="red2","Unknown"="grey"), na.value="grey50")

## Plot case results by NGS testing bin
ggplot(case_bars[case_bars$Lab.Testing.Methods == "5. Sequence Analysis",], aes(x=NGS_category, fill=Results_summary)) + geom_bar(position = 'stack', stat="count") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 22)) + scale_fill_manual(values=c("No VUS or pathogenic / actionable variant"="green3","VUS or pathogenic / actionable variant"="red2","Unknown"="grey"), na.value="grey50")

## Plot number of cases in myeloseq vs not myeloseq by NGS panel ordered
ggplot(case_bars[which(case_bars$Lab.Testing.Methods == "5. Sequence Analysis"),], aes(x=NGS_test, fill=NGS.panel)) + geom_bar(position = 'stack') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + scale_fill_viridis(discrete = T, direction = -1, na.value="grey50") 

## Plot number of cases in myeloseq vs not myeloseq by indication
ggplot(case_bars[which(case_bars$Lab.Testing.Methods == "5. Sequence Analysis"),], aes(x=NGS_test, fill=Indication)) + geom_bar(position = 'stack') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + scale_fill_viridis(discrete = T, direction = -1, na.value="grey50") 

######## Create MyeloSeq-only subset
case_bars_myeloseq <- case_bars[which(case_bars$NGS.panel == "Myeloseq"),]

## create an indication category
case_bars_myeloseq$Indication_category <- NA
case_bars_myeloseq$Indication_category <- case_bars_myeloseq$Indication
## remove extra words
case_bars_myeloseq$Indication_category <- gsub("New ", "", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub("Relapsed ", "", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub(" relapse", "", case_bars_myeloseq$Indication_category)
## Merge groups
case_bars_myeloseq$Indication_category <- gsub("Aplastic Anemia", "Aplastic anemia", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub(" \\(PNH clone\\)", "", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub(", t-MDS", "", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub("Pancytopenia|Eosinophilia|Myeloma vs MDS vs CHIP/MGUS|Anemia, neutropenia, thrombocytopenia|Lymphopenia|Leukopenia, c/f MDS|Thrombocytopenia|Neutropenia r/o MDS|Pancytopenia|R/o MDS|Cytopenia, MDS, AML|Neutropenia|Cytopenia, MDS, AML|Cytopenia and myelofibrosis|Anemia|Cytopenia, MDS, AML|Cytopenia and myelofibrosis|ICUS", "Cytopenia", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub("Cytopenia, neutropenia and thrombocytopenia|Cytopenia, MDS, AML", "Cytopenia", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub("PMF|Myelofibrosis|Essential thrombocythemia|CMML", "MPN", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub("MDS, MPN", "MDS/MPN", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub("DLBCL post CAR-T, c/f tMDS", "R/o MDS", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub("Hereditary spherocytosis, leukocytosis|Thrombocytosis|Cytopenia, acute leukemia", "Other", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub("Cytopenia and myelofibrosis|MDS vs ITP|ET vs AML|MDS or AML|AML vs histiocytosis|Myeloid sarcoma|R/o MDS|Possible AML", "Uncertain myeloid disorder", case_bars_myeloseq$Indication_category)
case_bars_myeloseq$Indication_category <- gsub("B-ALL|ALL|MGUS|Hairy cell leukemia", "Lymphoid disorder", case_bars_myeloseq$Indication_category)

unique(case_bars_myeloseq$Indication_category)

######## Export plots
## Plot case results by Testing method
png(file = "Overall_case_distribution.png", height=1700, width=1400, res=150)
  ggplot(case_bars, aes(x=Lab.Testing.Methods, fill=Results_summary)) + geom_bar(position = 'stack') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 22))  + scale_fill_manual(values=c("No VUS or pathogenic / actionable variant"="green3","VUS or pathogenic / actionable variant"="red2","Unknown"="grey"), na.value="grey50")
dev.off()

## Plot case results by category
png(file = "Overall_case_category_distribution.png", height=1700, width=1400, res=150)
ggplot(case_bars, aes(x=Category, fill=Results_summary)) + geom_bar(position = 'stack') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 27))  + scale_fill_manual(values=c("No VUS or pathogenic / actionable variant"="green3","VUS or pathogenic / actionable variant"="red2","Unknown"="grey"), na.value="grey50")
dev.off()

## Plot number of cases in each NGS order bin by indication
png(file = "NGS_case_distribution.png", height=2700, width=1450, res=150)
  ggplot(case_bars[which(case_bars$Lab.Testing.Methods == "5. Sequence Analysis"),], aes(x=NGS_category, fill=Indication)) + geom_bar(position = 'stack') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + scale_fill_viridis(discrete = T, direction = -1, na.value="grey50") + theme(legend.position = "bottom", legend.key.size = unit(6, "points")) + guides(fill=guide_legend(nrow=100, byrow=TRUE))
dev.off()

## Plot number of cases in each NGS order bin by indication
png(file = "NGS_panel_distribution.png", height=1700, width=1400, res=150)
  ggplot(case_bars[which(case_bars$Lab.Testing.Methods == "5. Sequence Analysis"),], aes(x=NGS_category, fill=NGS.panel)) + geom_bar(position = 'stack') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + scale_fill_viridis(discrete = T, direction = -1, na.value="grey50") 
dev.off()

png(file = "NGS_result_distribution.png", height=1700, width=1400, res=150)
  ggplot(case_bars[which(case_bars$Lab.Testing.Methods == "5. Sequence Analysis"),], aes(x=NGS_category, fill=Results_summary)) + geom_bar(position = 'stack') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + scale_fill_manual(values=c("No VUS or pathogenic / actionable variant"="green3","VUS or pathogenic / actionable variant"="red2","Unknown"="grey"), na.value="grey50")
dev.off()
  
## Plot number of cases in myeloseq by indication
png(file = "MyeloSeq_indication.png", height=2000, width=1200, res=150)
  ggplot(case_bars[which(case_bars$NGS.panel == "Myeloseq"),], aes(x=NGS_test, fill=Indication)) + geom_bar(position = 'stack') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + scale_fill_viridis(discrete = T, direction = -1, na.value="grey50") + theme(legend.position = "right", legend.key.size = unit(6, "points")) + guides(fill=guide_legend(nrow=100, byrow=TRUE))
dev.off()

png(file = "MyeloSeq_indication_grouped.png", height=1700, width=1400, res=150)
  ggplot(case_bars_myeloseq, aes(x=Sample.Type, fill=Indication_category)) + geom_bar(position = 'stack') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + scale_fill_viridis(discrete = T, direction = -1, na.value="grey50") 
dev.off()

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