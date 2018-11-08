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

##########################################################################################################
#### Tidy logbook to relevant information
##########################################################################################################
## Remove unused rows (Drop if no CoPath identifier)
cases <- cases[!is.na(cases$`CoPath.#`),]

## Remove notes and extra columns that may have extraneous notes (Notes and any columns beginning in X)
cases <- cases[,colnames(cases)[!grepl("^X|Notes", colnames(cases))]]



summary(cases)
