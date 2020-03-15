##########################################################################################################
######################## Identify longest transcript for  GPS panels #####################################
##########################################################################################################
library(openxlsx)
library(biomaRt)

## Set working directory
setwd('~/Box Sync/LGG/Projects/CGW_filters/')

##########################################################################################################
#### Read in files
##########################################################################################################
## GPS CCGSv3 panels
gps_panels <- read.xlsx("~/Box Sync/LGG/Projects/Lymphoma/CCGSv3 2 panels.xlsx",colNames=T,detectDates=T,sheet = "V3.2",startRow = 3)

###### Clean up GPS panel lists
## Drop 2 genes we don't cover
gps_panels <- gps_panels[!(gps_panels$`Gene/ROI` %in% c("CRLF2","IKZF1")),]
## Rename gene column
colnames(gps_panels) <- gsub("Gene/ROI","gene",colnames(gps_panels))

########## Get more information about the genes with respect to ensembl biotype and gene status to help with filtering
## Query mart for Ensembl data
mart <- useMart(biomart="ENSEMBL_MART_ENSEMBL", host="grch37.ensembl.org", path="/biomart/martservice", dataset="hsapiens_gene_ensembl")

## List all query attribute options
View(listAttributes(mart))

#Filter on gene symbol and return transcript information
biomart_subset <- getBM(values=gps_panels$gene, filters="hgnc_symbol", attributes=c("hgnc_symbol","ensembl_transcript_id_version","transcript_length","transcript_count","refseq_mrna","refseq_peptide"), mart = mart)

#Export this table to prevent having to do this arduous process again
write.table(biomart_subset, file="biomart_transcript_info.tsv",quote=F,sep="\t",row.names=F,col.names=T)