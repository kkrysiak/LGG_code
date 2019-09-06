##########################################################################################################
################## Identify genes recurrently mutation in FL on GPS panels ###############################
##########################################################################################################
library(openxlsx)
library(ggplot2)

setwd('~/Box/LGG/Projects/Lymphoma/')


##########################################################################################################
#### Read in files
##########################################################################################################
## GPS panels
gps_panels <- read.xlsx("CCGSv3 2 panels.xlsx",colNames=T,detectDates=T,sheet = "V3.2",startRow = 3)

## Lymphoma studies
lym_studies <- read.table(file="~/Box/LymphomaProject/v2_Panel_Design/Sequencing_study_mutation_distribution.tsv",header = T,sep = "\t")
lym_panel <- read.table(file="~/Box/LymphomaProject/v2_Panel_Design/Final_list_of_genes_lymphoma_panels_v1andv2_droppedfailed.tsv",header = T,sep = "\t")

##########################################################################################################
#### Clean up files
##########################################################################################################
###### Clean up GPS panel lists
## Drop 2 genes we don't cover
gps_panels <- gps_panels[!(gps_panels$`Gene/ROI` %in% c("CRLF2","IKZF1")),]

## Remove ? from Breast columns
gps_panels$Breast <- gsub("\\?",0,gps_panels$Breast)
## Convert column to numeric now that it only contains integers
gps_panels$Breast <- as.numeric(gps_panels$Breast)

## Replace NAs with 0
gps_panels[is.na(gps_panels)] <- 0

## Rename gene column
colnames(gps_panels) <- gsub("Gene/ROI","gene",colnames(gps_panels))

###### Clean up FL lists
## Get a list of which lymphoma research panel these are on
lym_studies <- merge(lym_studies,lym_panel,all.x=T)
colnames(lym_studies) <- gsub("panel","FL_research_panel",colnames(lym_studies))

## Get a list of column names to drop
drop <- c(colnames(lym_studies)[grepl("pastore",colnames(lym_studies))],"ATLL_candidate_genes","Fehniger","Lit")

## Drop unwanted columns
lym_studies <- lym_studies[,!(colnames(lym_studies) %in% drop)]

##########################################################################################################
#### Merge information and export
##########################################################################################################
all <- merge(gps_panels, lym_studies, by.x= ,all=T)

not_zero <- all[which(all$FL.rFL != 0 | all$FL.tFL != 0 | all$Total > 0),]

## Export the table
write.table(all, file="GPS_vs_FLrecurrence.tsv", quote=F, row.names=F, col.names=T, sep="\t")
write.table(not_zero, file="GPS_vs_FLrecurrence_notFLzero.tsv", quote=F, row.names=F, col.names=T, sep="\t")
