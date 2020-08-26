test_that("vibrations function appropriately", {
	metadata = readRDS('../../data/final_metadata.rds')[[2]][[1]] %>% type.convert() %>% dplyr::select(-runID,-subjectID)
	abundance = readRDS('../../data/abundance_data.rds') %>% dplyr::select(-dataset_name)
	abundance = abundance[,1:10]
	bound_data = dplyr::tibble(dependent_variables=list(abundance),independent_variables=list(metadata),dsid=1)
  	expect_error(compute_vibrations(bound_data=bound_data,primary_variable='BMI',model_type='gaussian',features_of_interest=c("2","3","4","5"),max_vibration_num=10,proportion_cutoff=.9), regexp = NA)
})