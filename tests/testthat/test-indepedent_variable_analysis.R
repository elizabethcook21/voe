test_that("metadata analysis works", {
  metadata <- readRDS('~/OneDrive/Harvard SIBMI/voe/data/final_metadata.rds')
  metadata <- unnest(metadata)
  metadata <- metadata[2:length(metadata)]
  metadata$LDL <- as.numeric(metadata$LDL)
  metadata$TGL <- as.numeric(metadata$TGL)
  metadata$HDL <- as.numeric(metadata$HDL)
  metadata$BMI <- as.numeric(metadata$BMI)
  metadata$age <- as.numeric(metadata$age)
  expect_error(ind_var_analysis(metadata), regexp = NA)
})
