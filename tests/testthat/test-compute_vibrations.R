test_that("vibrations function appropriately", {
	metadata = readRDS('../../data/metadata_for_test.rds')
	abundance = readRDS('../../data/abundance_data_for_test.rds') 
	abundance = abundance[,1:10]
	bound_data = dplyr::tibble(dependent_variables=list(abundance),independent_variables=list(metadata),dsid=1)
  	expect_error(compute_vibrations(bound_data=bound_data,primary_variable='BMI',model_type='gaussian',features_of_interest=c("2","3","4","5"),max_vibration_num=10,proportion_cutoff=.9), regexp = NA)
})