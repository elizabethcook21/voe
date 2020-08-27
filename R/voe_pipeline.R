
#' The VOE Pipeline
#'
#' This function will run the full pipeline
#' @param dependent_variables A tibble containing the information for your dependent variables (e.g. bacteria relative abundance, age). The first column should be the rownames (e.g. gene1, gene2, gene3), and the columns should correspond to different samples (e.g. individual1, individual2, etc).
#' @param independent_variables A tibble containing the independent variables you will want to vibrate over. Each column should correspond to a different variable (e.g. age), with the first column containing the sample names matching those in the column names of the dependent_variables tibble.
#' @param primary_variable The column name from the independent_variables tibble containing the key variable you want to associate with disease in your first round of modeling (prior to vibration). For example, if you are interested fundamentally identifying how well age can predict height, you would make this value a string referring to whatever column in said dataframe refers to "age."
#' @param fdr_method Your choice of method for adjusting p-values. Options are BY (default), BH, or bonferroni.
#' @param fdr_cutoff Cutoff for an FDR significant association (default = 0.05).
#' @param max_vibration_num Maximum number of vibrations (default=50000).
#' @param proportion_cutoff Float between 0 and 1. Filter out dependent features that are this proportion of zeros or more (default = 1, so no filtering done.)
#' @param meta_analysis TRUE/FALSE -- indicates if computing meta-analysis across multiple datasets.
#' @param model_type Model family (e.g. gaussian, binomial, etc). Will determine if you are doing classification or regression. See GLM families for more information. (default="gaussian")
#' @keywords pipeline
#' @export
#' @examples
#' voepipeline(metadata, abundance_data, mapping)
full_voe_pipeline <- function(dependent_variables,independent_variables,primary_variable,fdr_method='BY',fdr_cutoff=0.05,max_vibration_num=50000,proportion_cutoff=.99,meta_analysis=FALSE, model_type='gaussian'){#rf_vibration_criteria = 'oob_r2', rf_vibration_criteria_cutoff = 0.2, mtry = 1, num.trees = 500 , importance ='none', min.node.size = 5, splitrule = 'variance'){
  logger <- initialize_logger()

  output_to_return = list()
  if(inherits(dependent_variables, "list")==TRUE){
    message('Identified multiple input datasets, preparing to run meta-analysis.')
    bound_data = dplyr::tibble(dependent_variables=dependent_variables,independent_variables=independent_variables,dsid = seq_along(independent_variables))
    if(meta_analysis==FALSE){
      return(message('The meta_analysis variable is set to FALSE, but you appear to have passed multiple datasets. Please switch it to TRUE, and/or adjust other parameters as needed, and try again. For more information, please see the documentation.'))
    }
  }
  else{
    bound_data = dplyr::tibble(dependent_variables=list(dependent_variables),independent_variables=list(independent_variables),dsid=1)
  }
  output_to_return[['original_data']] = bound_data
  passed = pre_pipeline_data_check(dependent_variables,independent_variables,primary_variable,fdr_method,fdr_cutoff,max_vibration_num,proportion_cutoff,meta_analysis,model_type)#, mtry, num.trees, importance,min.node.size,splitrule)
  if(passed==TRUE){
    Sys.sleep(2)
    message('Deploying initial associations...')
    association_output <- compute_initial_associations(bound_data, primary_variable,model_type,proportion_cutoff)#,mtry,num.trees,importance,min.node.size,splitrule)
    output_to_return[['initial_association_output']] = association_output
    if(meta_analysis == TRUE){
      metaanalysis <- compute_metaanalysis(association_output)
      metaanalysis_cleaned <- clean_metaanalysis(metaanalysis)
      output_to_return[['meta_analyis_output']] = metaanalysis_cleaned
      features_of_interest = metaanalysis_cleaned %>% dplyr::filter(!!rlang::sym(fdr_method)<=as.numeric(fdr_cutoff)) %>% dplyr::select(feature)
    }
    else{
      features_of_interest = association_output %>% dplyr::filter(!!rlang::sym(fdr_method)<=as.numeric(fdr_cutoff)) %>% dplyr::select(feature)
   }
   print(features_of_interest)
    if(length(features_of_interest)==0){
      message('No significant features found, consider adjusting parameters or data and trying again.')
      return(output_to_return)
    }
    output_to_return[['features_to_vibrate_over']] = features_of_interest
    vibration_output = compute_vibrations(bound_data,primary_variable,model_type,unname(unlist(features_of_interest)),max_vibration_num, proportion_cutoff)#, mtry, num.trees, importance, min.node.size, splitrule)
    output_to_return[['vibration_variables']] = vibration_output[[2]]
    analyzed_voe_data = analyze_voe_data(vibration_output)
    output_to_return[['analyzed_voe_data']] = analyzed_voe_data
    message('Done!')
    return(output_to_return)
  }
}
