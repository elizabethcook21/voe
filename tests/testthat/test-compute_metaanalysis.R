test_that("meta-analysis works", {
	association_output = readRDS('sample_association_output.rds')
	association_output2 = association_output
	association_output2$dsid=2
    logger <- initialize_logger(paste0('voe_pipeline_',format(Sys.time(), "%d-%b-%Y_%H.%M")), FALSE, NULL)
	association_output_doubled = dplyr::bind_rows(association_output,association_output2)
	metaanalysis = compute_metaanalysis(association_output_doubled,logger=logger)
	metaanalysis_cleaned <- clean_metaanalysis(metaanalysis,dataset_num=2,logger=logger)
  	expect_equal(nrow(metaanalysis_cleaned),79, regexp = NA)
})
