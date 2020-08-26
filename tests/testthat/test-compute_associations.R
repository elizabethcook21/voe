test_that("linear regression works", {
	metadata = readRDS('../../data/final_metadata.rds')[[2]][[1]] %>% type.convert() %>% dplyr::select(-runID,-subjectID)
	abundance = readRDS('../../data/abundance_data.rds') %>% dplyr::select(-dataset_name)
	abundance = abundance[,1:10]
	bound_data = dplyr::tibble(dependent_variables=list(abundance),independent_variables=list(metadata),dsid=1)
  	expect_error(compute_initial_associations(bound_data,primary_variable='BMI',model_type = 'gaussian',proportion_cutoff = 0.9), regexp = NA)
})

test_that("logistic regression works", {
	metadata = readRDS('../../data/final_metadata.rds')[[2]][[1]] %>% type.convert() %>% dplyr::select(-runID,-subjectID)
	abundance = readRDS('../../data/abundance_data.rds') %>% dplyr::select(-dataset_name)
	abundance = abundance[,1:10]
	bound_data = dplyr::tibble(dependent_variables=list(abundance),independent_variables=list(metadata),dsid=1)
  	expect_error(compute_initial_associations(bound_data,primary_variable='study_condition',model_type = 'binomial',proportion_cutoff = 0.9), regexp = NA)
})
