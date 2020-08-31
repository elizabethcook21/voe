#' Analysis of Indepdent Variables
#'
#' This function will run the full pipeline
#' #' @param independent_variables A tibble containing the information for your independent variables (e.g. age, sex). Each column should correspond to a different variable (e.g. age), with the first column containing the sample names matching those in the column anmes of the dependent_variables tibble.
#' #' @param output_location The path where you want to save all the ggplots and metadata
#' @keywords independent variable
#' @export
#' @examples
#' ind_var_analysis(metadata)
ind_var_analysis <- function(independent_variables, output_location = getwd(),log=TRUE,log_file_path=NULL){
  my_logger <- initialize_logger(paste0('ind_var_analysis_',format(Sys.time(), "%d-%b-%Y_%H.%M")),log,log_file_path)

  # Create a folder to store all the outputs
  pathToNewFolder <- paste0(output_location, '/Metadata_Files')
  dir.create(pathToNewFolder)

  ind_var <- dplyr::as_tibble(independent_variables)
  columnSummaries <- summary(ind_var)
  # This should probably be changed in the future to a more human readable file type like txt, html, pdf, or excel
  saveRDS(columnSummaries, paste0(pathToNewFolder, '/metadata_information.rds'))
  log4r::info(my_logger, 'You should have gotten a summary of all the columns in your table, for example their means and number of variables.')
  # Get the different types of columns by using summarise_all, then convert to a named list
  colTypes <- ind_var %>%
    dplyr::summarise_all(class)
  colTypes <- as.list(colTypes)
  for(i in 1:length(colTypes)){
    if (nrow(unique(ind_var[i])) > 10){
      histogram_tibble <- dplyr::as_tibble(ind_var[names(colTypes[i])])
      try(
        makeHistogram(histogram_tibble, names(colTypes[i]),pathToNewFolder)
      )
    }else{
      try(
        makeBarGraph(ind_var, names(colTypes[i]),pathToNewFolder)
      )
    }
  }

}


makeBarGraph <- function(table, columnName,pathToNewFolder){
  ggplot2::ggplot(table, ggplot2::aes_string(x=columnName, fill = columnName)) +
    ggplot2::geom_bar() +
    ggplot2::theme_light() +
    ggplot2::ggtitle(paste0(columnName, ' Comparison')) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  ggplot2::ggsave(filename = file.path(pathToNewFolder,paste0(columnName, "_bargraph.png")))

}

makeHistogram <- function(table, columnName,pathToNewFolder){
  ggplot2::ggplot(table, ggplot2::aes_string(x=columnName)) +
    ggplot2::geom_histogram() +
    ggplot2::theme_light() +
    ggplot2::ggtitle(paste0(columnName, ' Distribution')) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  ggplot2::ggsave(filename = file.path(pathToNewFolder,paste0(columnName, "_histogram.png")))
}
