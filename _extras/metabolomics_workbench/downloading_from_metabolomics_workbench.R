# Packages ----------------------------------------------------------------
library(dplyr)
library(mwtabR)     # devtools::install_github("stanstrup/mwtabR")
library(listviewer) # devtools::install_github("timelyportfolio/listviewer")

setwd("_extras/metabolomics_workbench")





# Download list of all studies -------------------------------
data_table <- ME_get_studies_list()


# Filter some studies irrelevant right now --------------------------------
#data_table %>% select(accession,url,title,meta.analysis,meta.platform) %>% View
#data_table_filtered <- data_table %>% filter(!grepl("NMR",meta.analysis)) %>% filter(!grepl("GC",meta.platform)) %>% filter(grepl("ST",accession))
data_table_filtered <- data_table %>% filter(grepl("ST",accession))


# Download MW data --------------------------------------------------------
MW_mwtab_parsed <- MW_get_studies_data(data_table_filtered[,"accession"])
#saveRDS(MW_mwtab_parsed,"MW_mwtab_parsed.rds")



# View data ---------------------------------------------------------------
#jsonedit(MW_mwtab_parsed[1:2])


#View(MW_mwtab_parsed$ST000231$AN000345$METABOLITES)
