##########################################################################################################
################################## SEER statistics and other plots #######################################
##########################################################################################################
library(openxlsx)
library(ggplot2)

setwd('~/Box Sync/LGG/Cases/GrandRounds_FLT3/')

##########################################################################################################
#### Read in files
##########################################################################################################
## AML incidence. SEER 21 2012-2016, All Races, Both Sexes. https://seer.cancer.gov/statfacts/html/amyl.html
incidence <- read.xlsx("AML_statistics.xlsx",colNames=T,detectDates=T)

incidence$Category <- factor(incidence$Category,levels = c("New Cases","Deaths"))

######## Export plots
## Plot incidence of new cases and deaths
png(file = "Incidence_newcases_death.png", height=3000, width=7000, res=150)
  ggplot(incidence, aes(x=Age, y=Percent, fill=Category)) + geom_col(position = 'dodge') + scale_fill_manual(values=c("New Cases"="grey50","Deaths"="red2")) + ylim(0,30) + theme(axis.text=element_text(size=80),axis.title=element_text(size=80),legend.text=element_text(size=80))
dev.off()

## Plot incidence of new cases
png(file = "Incidence_newcases.png", height=3000, width=7000, res=150)
  ggplot(incidence[which(incidence$Category == "New Cases"),], aes(x=Age, y=Percent, fill=Category)) + geom_col(position = 'dodge') + scale_fill_manual(values=c("New Cases"="grey50","Deaths"="red2")) + ylim(0,30) + theme(axis.text=element_text(size=80),axis.title=element_text(size=80),legend.text=element_text(size=80))
dev.off()