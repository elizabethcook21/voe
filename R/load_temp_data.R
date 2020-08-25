#example files for loading

independent_variables = readRDS('data/final_metadata.rds')[[2]][[1]] %>% type.convert() %>% select(-runID,-subjectID)

dependent_variables = readRDS('data/abundance_data.rds') %>% select(-dataset_name)
dependent_variables = dependent_variables[,1:200]
primary_variable = 'BMI'
fdr_method='BY'
fdr_cutoff=.99
