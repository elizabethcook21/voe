test_that("metadata analysis works", {
  metadata <- readRDS('~/OneDrive/Harvard SIBMI/voe/data/metadata_for_test.rds')
  metadata <- metadata[2:length(metadata)]
  expect_error(ind_var_analysis(metadata), regexp = NA)
  ind_var_analysis(metadata)
})
