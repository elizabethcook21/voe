test_that("non-meta-analytic pipeline works", {
	metadata = readRDS('../../data/metadata_for_test.rds')
	abundance = readRDS('../../data/abundance_data_for_test.rds')
	abundance = abundance[,1:100]
  	expect_error(full_voe_pipeline(abundance,metadata,primary_variable='BMI',fdr_method='BY',model_type='gaussian',fdr_cutoff=0.99,max_vibration_num=10,proportion_cutoff = 0.9, meta_analysis=FALSE), regexp = NA)
})

test_that("meta-analytic pipeline works", {
	metadata = readRDS('../../data/metadata_for_test.rds')
	abundance = readRDS('../../data/abundance_data_for_test.rds')
	abundance = abundance[,1:100]
  	expect_error(full_voe_pipeline(list('dataset1'=abundance,'dataset2'=abundance),list('dataset1'=metadata,'dataset2'=metadata),model_type='gaussian',primary_variable='BMI',fdr_method='BY',fdr_cutoff=0.99,max_vibration_num=10,proportion_cutoff = 0.9, meta_analysis=TRUE), regexp = NA)
})
