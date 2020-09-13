#parse american gut data to tibble

library(biomformat)
library(tidyverse)

data = read_biom('ag_10k_fecal.biom')
foobar = as(biom_data(data),'matrix')
baz = as.data.frame(t(foobar))
baz = baz %>% rownames_to_column('sampleID')
samples = baz %>% select(sampleID)
baz=baz %>% select(-sampleID)
toremove = which(colSums(baz==0)/nrow(baz)>=.9)
baz=baz %>% select(-all_of(toremove))
baz = bind_cols(samples,baz)
saveRDS(baz,'ag_fecal_tibble_data_10k.rds')

metadata=read.csv('ag_10k_fecal.txt',sep='\t') %>% select(-SURVEY_ID)

metadata[metadata=='Unknown']=NA
metadata[metadata=='no_data']=NA
metadata[metadata=='unspecified']=NA
metadata[metadata=='Unspecified']=NA
metadata[metadata=='Missing: Not provided']=NA
metadata[metadata=='Missing: Not provided']=NA
metadata[metadata=='Not sure']=NA
metadata[metadata=='not sure']=NA
metadata[metadata=='not Sure']=NA
metadata[metadata=='Not Sure']=NA 

colnames(metadata)[1]='sampleID'

metadata = metadata[!is.na(metadata$BMI_CORRECTED),]
metadata = metadata[!is.na(metadata$EXERCISE_FREQUENCY),]

toremove = which(colSums(is.na(metadata %>% dplyr::select(-sampleID)))/nrow(metadata)>=.5)
metadata= metadata %>% select(-c(names(toremove)))
metadata = metadata %>% select(-BMI)

metadata = metadata[, sapply(metadata, function(col) length(unique(col,na.rm=TRUE))) > 1]

metadata[metadata=='No']=FALSE
metadata[metadata=='no']=FALSE
metadata[metadata=='Yes']=TRUE
metadata[metadata=='yes']=TRUE

metadata = metadata %>% filter(!is.na(ENV_MATERIAL)) 
metadata_sub = type.convert(metadata %>% select(-sampleID))
metadata=bind_cols(metadata %>% select(sampleID),metadata_sub)

metadata = metadata %>% select(-c(LinkerPrimerSequence,shannon_1k,observed_otus_1k,chao1_1k,PD_whole_tree_1k,LATITUDE,TAXON_ID,BIRTH_YEAR,LONGITUDE,SAMPLE_PLATE,CENTER_NAME,PLATFORM,EXPERIMENT_CENTER,TARGET_SUBFRAGMENT,PUBLIC,QIITA_STUDY_ID,CENTER_PROJECT_NAME,PLATING,DEPTH,PRIMER_PLATE,TM50_8_TOOL,RUN_DATE,SAMP_SIZE,LIVINGWITH,EXTRACTIONKIT_LOT,TM1000_8_TOOL,EXPERIMENT_DESIGN_DESCRIPTION,RUN_CENTER,PCR_PRIMERS,INSTRUMENT_MODEL,TARGET_GENE,HOST_SUBJECT_ID,WELL_DESCRIPTION,AGE_YEARS,LINKER,ORIG_NAME,MASTERMIX_LOT,ENV_MATERIAL,ANONYMIZED_NAME,ENV_FEATURE,RUN_PREFIX,TM300_8_TOOL,QIITA_PREP_ID,EXTRACTION_ROBOT,PROJECT_NAME,COLLECTION_TIMESTAMP,PROCESSING_ROBOT,COLLECTION_DATE,LIBRARY_CONSTRUCTION_PROTOCOL,BarcodeSequence,PRIMER_DATE,ENV_PACKAGE,WELL_ID,WATER_LOT,ENV_BIOME,SCIENTIFIC_NAME,SUBSET_ANTIBIOTIC_HISTORY,SEQUENCING_METH,DNA_EXTRACTED,PHYSICAL_SPECIMEN_REMAINING,ASSIGNED_FROM_GEO,SUBSET_AGE,SUBSET_BMI,EXPERIMENT_TITLE,FLU_VACCINE_DATE))

metadata = metadata[c(colnames(metadata)[metadata %>% select_if(is.factor) %>% summarise_all(funs(n_distinct(.))) %>% unlist %>% unname <= 50])]

metadata = metadata %>% select(-c(colnames(metadata)[unlist(unname(metadata %>% select_if(is.factor) %>% summarise_all(funs(n_distinct(.))) == 1))]))

metadata$EXERCISE_FREQUENCY = unclass(metadata$EXERCISE_FREQUENCY)

merged = 

saveRDS(metadata,'metadata_ag_fecal.rds')





















