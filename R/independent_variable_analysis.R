#' Analysis of Indepdent Variables
#'
#' This function will run the full pipeline
#' #' @param independent_variables A tibble containing the information for your independent variables (e.g. age, sex). Each column should correspond to a different variable (e.g. age), with the first column containing the sample names matching those in the column anmes of the dependent_variables tibble.
#' @keywords independent variable
#' @export
#' @examples
#' ind_var_analysis(metadata)
ind_var_analysis <- function(independent_variables){
  my_logger <- initialize_logger(paste0('ind_var_analysis_',format(Sys.time(), "%d-%b-%Y_%H.%M")))
  ind_var <- dplyr::as_tibble(independent_variables)
  columnSummaries <- summary(ind_var)
  # This should probably be changed in the future to a more human readable file type like txt, html, pdf, or excel
  saveRDS(columnSummaries, 'metadata_information.rds')
  log4r::info(my_logger, 'You should have gotten a summary of all the columns in your table, for example their means and number of variables.')
  # Get the different types of columns by using summarise_all, then convert to a named list
  browser()

  colTypes <- ind_var %>%
    dplyr::summarise_all(class)
  colTypes <- as.list(colTypes)
  for(i in 1:length(colTypes)){
    #columnOfData <- ind_var[names(colTypes[i])
    if(colTypes[i] == "factor"){
      try(
        makeBarGraph(ind_var, names(colTypes[i]))
      )
    } else {
      try(
        makeHistogram(ind_var[names(colTypes[i])], names(colTypes[i]))
      )
    }
  }

}


makeBarGraph <- function(table, columnName){
  ggplot2::ggplot(table, ggplot2::aes_string(x=columnName, fill = columnName)) +
    ggplot2::geom_bar() +
    ggplot2::theme_light()
  try(ggplot2::ggsave(paste0(columnName,"_bargraph.png")))
}

makeHistogram <- function(table, columnName){
  ggplot2::ggplot(table, ggplot2::aes(x=columnName)) +
    ggplot2::geom_histogram() +
    ggplot2::theme_light()
  try(ggplot2::ggsave(paste0(columnName, "_histogram.png")))
}
