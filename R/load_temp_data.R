#example files for loading

library(tidyverse)

independent_variables = readRDS('~/GitHub/voe/data/final_metadata.rds')[[2]][[1]] %>% type.convert() %>% dplyr::select(-runID,-subjectID)

dependent_variables = readRDS('~/GitHub/voe/data/abundance_data.rds') %>% dplyr::select(-dataset_name)
dependent_variables = dependent_variables[,1:200]
primary_variable = 'BMI'
fdr_method='BY'
fdr_cutoff=.99

dependent_variables=list('dataset1'=dependent_variables,'dataset2'=dependent_variables)
independent_variables=list('dataset1'=independent_variables,'dataset2'=independent_variables)

#voe::full_voe_pipeline(dependent_variables,independent_variables,primary_variable,model_type='gaussian',fdr_method='BY',fdr_cutoff=0.99,max_vibration_num=10,meta_analysis=TRUE)


metadata = readRDS('~/Github/voe/data/final_metadata.rds')[[2]][[1]] %>% type.convert() %>% dplyr::select(-runID,-subjectID)
abundance = readRDS('~/Github/voe/data/abundance_data.rds') %>% dplyr::select(-dataset_name)
abundance = abundance[,1:10]
bound_data = dplyr::tibble(dependent_variables=list(abundance),independent_variables=list(metadata),dsid=1)
