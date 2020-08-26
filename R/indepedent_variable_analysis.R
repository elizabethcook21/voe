#' Analysis of Indepdent Variables
#'
#' This function will run the full pipeline
#' #' @param independent_variables A tibble containing the information for your independent variables (e.g. age, sex). Each column should correspond to a different variable (e.g. age), with the first column containing the sample names matching those in the column anmes of the dependent_variables tibble.
#' @keywords independent variable
#' @export
#' @examples
#' ind_var_analysis(metadata)
ind_var_analysis <- function(independent_variables){
  my_logger <- initialize_logger()
  ind_var <- as_tibble(independent_variables)
  columnSummaries <- summary(ind_var)
  # This should probably be changed in the future to a more human readable file type like txt, html, pdf, or excel
  saveRDS(columnSummaries, 'metadata_information.rds')
  log4r::info(my_logger, 'You should have gotten a summary of all the columns in your table, for example their means and number of variables.')
  for(i in colnames(ind_var[2:length(ind_var)])){
    # if(typeof(ind_var[[i]] == "character")){
    #   print("make a bar graph")
    # ggplot(tibbleData, aes(x=dataset, fill = Gender)) +
    #   geom_bar(position=position_dodge()) +
    #   theme_light()
    # } else {
    #   print("make a histogram")
    # ggplot(tibbleData, aes(x=BMI, fill=dataset, color=dataset)) +
    #   geom_histogram() +
    #   theme_light() +
    #   xlab('BMI (kg/m2)')
    # }
  }

  #if(colnames(ind_var)[1] != 'sampleID' || colnames(ind_var)[1] != 'SampleID'){
  #  return("Error. The first column is not named SampleID")
  #}

}
