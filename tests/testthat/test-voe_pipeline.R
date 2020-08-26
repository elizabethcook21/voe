test_that("non-meta-analytic pipeline works", {
	metadata = readRDS('../../data/final_metadata.rds')[[2]][[1]] %>% type.convert() %>% dplyr::select(-runID,-subjectID)
	abundance = readRDS('../../data/abundance_data.rds') %>% dplyr::select(-dataset_name)
	abundance = abundance[,1:100]
  	expect_error(full_voe_pipeline(abundance,metadata,primary_variable='BMI',fdr_method='BY',model_type='gaussian',fdr_cutoff=0.99,max_vibration_num=10,proportion_cutoff = 0.9, meta_analysis=FALSE), regexp = NA)
})

test_that("meta-analytic pipeline works", {
	metadata = readRDS('../../data/final_metadata.rds')[[2]][[1]] %>% type.convert() %>% dplyr::select(-runID,-subjectID)
	abundance = readRDS('../../data/abundance_data.rds') %>% dplyr::select(-dataset_name)
	abundance = abundance[,1:100]
  	expect_error(full_voe_pipeline(list('dataset1'=abundance,'dataset2'=abundance),list('dataset1'=metadata,'dataset2'=metadata),model_type='gaussian',primary_variable='BMI',fdr_method='BY',fdr_cutoff=0.99,max_vibration_num=10,proportion_cutoff = 0.9, meta_analysis=TRUE), regexp = NA)
})