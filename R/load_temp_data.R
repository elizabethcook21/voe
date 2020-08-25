#example files for loading

library(tidyverse)

independent_variables = readRDS('data/final_metadata.rds')[[2]][[1]] %>% type.convert() %>% dplyr::select(-runID,-subjectID)

dependent_variables = readRDS('data/abundance_data.rds') %>% dplyr::select(-dataset_name)
dependent_variables = dependent_variables[,1:200]
primary_variable = 'BMI'
fdr_method='BY'
fdr_cutoff=.99

dependent_variables=list('dataset1'=dependent_variables,'dataset2'=dependent_variables)
independent_variables=list('dataset1'=independent_variables,'dataset2'=independent_variables)