
#' The VOE Pipeline
#'
#' This function will run the full pipeline
#' @param metadata See project descriptsions for more details. Needs to have two columns, dataset name, and then a tibble with all the data
#' @param abundance_data This is the abundance data you need. See project descriptions for more details
#' @param mapping This maps the column names in abundance data to their actual gene names
#' @keywords pipeine
#' @export
#' @examples
#' viepipeline(metadata, abundance_data, mapping)
voepipeline <- function(metadata, abundance_data, mapping){
  #Each line of code is a different step
  logger <- initialize_logger()
  log4r::info(logger, "Computing the Associations")
  data_assocations <- compute_assocations(metadata, abundance_data, 'CRC', logger)
  log4r::info(logger, "Computing the Metanalysis")
  metaanalysis <- compute_metaanalysis(data_assocations)
  log4r::info(logger, "Cleaning the Metanalysis")
  cleanmetaanalysis <- clean_metaanalysis(metaanalysis, mapping)
  log4r::info(logger, "Plotting the volcano plots")
  plot_volcano_and_find_vibrations(cleanmetaanalysis)
}



