# This is a modified test for a small abundance file
test_that("small sample voe pipeline works", {
  metadata <- readRDS('~/OneDrive/Harvard SIBMI/voe/data/final_metadata.rds')
  abundance <- readRDS('~/OneDrive/Harvard SIBMI/voe/data/abundance_data.rds')
  abundance <- abundance[1:100]
  mapping <- readRDS('~/OneDrive/Harvard SIBMI/voe/data/Karlsson_Mapping.rds')
  mapping <- mapping %>% head(100)
  expect_error(voepipeline(metadata, abundance, mapping), regexp = NA)
})
