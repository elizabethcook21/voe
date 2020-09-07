test_that("vibrations function appropriately", {
	metadata = readRDS('metadata_for_test.rds')
	abundance = readRDS('abundance_data_for_test.rds') 
	abundance = abundance[,1:100]
    logger <- initialize_logger(paste0('voe_pipeline_',format(Sys.time(), "%d-%b-%Y_%H.%M")), FALSE, NULL)
	bound_data = dplyr::tibble(dependent_variables=list(abundance),independent_variables=list(metadata),dsid=1)
  	expect_error(compute_vibrations(bound_data=bound_data,primary_variable='BMI',model_type='gaussian',regression_weights=NULL,logger=logger,features_of_interest=c("2","3","4","5"),max_vibration_num=10,proportion_cutoff=.9,cores=1,max_vars_in_model=100), regexp = NA)
})

test_that("multi-threading works", {
	metadata = readRDS('metadata_for_test.rds')
	abundance = readRDS('abundance_data_for_test.rds') 
	abundance = abundance[,1:100]
    logger <- initialize_logger(paste0('voe_pipeline_',format(Sys.time(), "%d-%b-%Y_%H.%M")), FALSE, NULL)
	bound_data = dplyr::tibble(dependent_variables=list(abundance),independent_variables=list(metadata),dsid=1)
  	expect_error(compute_vibrations(bound_data=bound_data,primary_variable='BMI',model_type='gaussian',regression_weights=NULL,logger=logger,features_of_interest=c("2","3","4","5"),max_vibration_num=10,proportion_cutoff=.9,cores=2,max_vars_in_model=100), regexp = NA)
})