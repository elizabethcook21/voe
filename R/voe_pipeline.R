
#' The VOE Pipeline
#'
#' This function will run the full pipeline
#' @param dependent_variables A tibble containing the information for your dependent variables (e.g. bacteria relative abundance, age). The first column should be the rownames (e.g. gene1, gene2, gene3), and the columns should correspond to different samples (e.g. individual1, individual2, etc).
#' @param independent_variables A tibble containing the independent variables you will want to vibrate over. Each column should correspond to a different variable (e.g. age), with the first column containing the sample names matching those in the column anmes of the dependent_variables tibble.
#' @param primary_variable The column name from the independent_variables tibble containing the key variable you want to associate with disease in your first round of modeling (prior to vibration). For example, if you are interested fundamentally identifying how well age can predict height, you would make this value a string referring to whatever column in said dataframe refers to "age."
#' @keywords pipeline
#' @export
#' @examples
#' viepipeline(metadata, abundance_data, mapping)
full_pipeline <- function(dependent_variables,independent_variables,primary_variable){
  #Each line of code is a different step
  ####pre-run check
  association_output <- compute_initial_associations(independent_variables, dependent_variables, primary_variable)
  ####save association output?
  #metaanalysis <- compute_metaanalysis(data_assocations)
  #cleanmetaanalysis <- clean_metaanalysis(metaanalysis, mapping)
  features_of_interest = plot_volcano_and_find_vibrations(association_output,FALSE)
  compute_vibrations(features_of_interest)
}

