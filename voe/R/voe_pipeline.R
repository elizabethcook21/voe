
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
  data_assocations <- compute_assocations(metadata, abundance_data, 'CRC')
  metaanalysis <- compute_metaanalysis(data_assocations)
  cleanmetaanalysis <- clean_metaanalysis(metaanalysis, mapping)
  plot_volcano_and_find_vibrations(cleanmetaanalysis)
}



