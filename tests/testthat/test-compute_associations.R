test_that("linear regression works", {
	metadata = readRDS('metadata_for_test.rds')
	abundance = readRDS('abundance_data_for_test.rds') 
	bound_data = dplyr::tibble(dependent_variables=list(abundance),independent_variables=list(metadata),dsid=1)
  	expect_error(compute_initial_associations(bound_data,primary_variable='BMI',model_type = 'gaussian',regression_weights=NULL,proportion_cutoff = 0.9,vibrate=TRUE), regexp = NA)
})

test_that("negative binomial regression works", {
	metadata = readRDS('metadata_ag_fecal_for_test.rds')
	abundance = readRDS('ag_data_for_nb_test.rds') 
	bound_data = dplyr::tibble(dependent_variables=list(abundance),independent_variables=list(metadata),dsid=1)
  	expect_error(compute_initial_associations(bound_data,vibrate=TRUE,primary_variable='BMI_CORRECTED',model_type = 'negative_binomial',regression_weights=NULL,proportion_cutoff = 0.9), regexp = NA)
})
