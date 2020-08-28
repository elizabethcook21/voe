# For a single feature and single modeldf outputted by prune_metadata.R, computes the proportion of non-zero values per cohort.

regression <- function(j,independent_variables,dependent_variables,primary_variable,model_type,proportion_cutoff){
  feature_name = colnames(dependent_variables)[j+1]
  regression_df=dplyr::left_join(independent_variables %>% dplyr::mutate_if(is.factor, as.character), dependent_variables %>% dplyr::select(c(1),feature_name),by = c("sampleID")) %>% dplyr::mutate_if(is.character, as.factor) %>% tidyr::drop_na()
  regression_df = regression_df %>% dplyr::select(-sampleID)
  #run regression
    return(tryCatch(broom::tidy(stats::glm(formula=as.formula(stringr::str_c("I(`", feature_name,"`) ~ ",primary_variable)),family=model_type,data = regression_df)) %>% dplyr::mutate(feature=feature_name),
             # NOTE: tidy() ends up removing and singularity fits (e.g. NA fits from a feature that, for the dataset's samples all have 0 relative abundance) #####NEED TO LOG THIS SOMEHOW
             warning = function(w) w, # if warning or error, just return them instead of output
             error = function(e) e
    ))
}

run_associations <- function(x,primary_variable,model_type,proportion_cutoff,vibrate){
  dependent_variables <- dplyr::as_tibble(x[[1]])
  colnames(dependent_variables)[[1]]='sampleID'
  toremove = which(colSums(dependent_variables %>% dplyr::select(-sampleID) == 0,na.rm=TRUE)/nrow(dependent_variables)>proportion_cutoff)
  message(paste("Removing",length(toremove),"features that are at least",proportion_cutoff*100,"percent zero values."))
  dependent_variables=dependent_variables %>% dplyr::select(-(toremove+1))
  independent_variables <- dplyr::as_tibble(x[[2]])
  colnames(independent_variables)[[1]]='sampleID'
  message(paste('Computing',as.character(ncol(dependent_variables)-1),'associations for dataset',as.character(unname(unlist(x[[3]])))))
  colnames(dependent_variables)[1]='sampleID'
  colnames(independent_variables)[1]='sampleID'
  tokeep = independent_variables %>% tidyr::drop_na() %>% dplyr::select_if(~ length(unique(.)) > 1) %>% colnames
  todrop = setdiff(colnames(independent_variables),tokeep)
  if(length(todrop)>1){
    message('Dropping the following variables due to either lacking multiple levels or containing NaN values:')
    print(todrop)
  }
  independent_variables=independent_variables %>% dplyr::select(-all_of(todrop))
  if(ncol(independent_variables==2)){
    print(independent_variables)
    vibrate=FALSE
    message('We dropped all the variables that you could possible vibrate over due to NAs or lacking multiple levels. Vibrate parameter being set to FALSE.')
  }
  out = purrr::map(seq_along(dependent_variables %>% dplyr::select(-sampleID)), function(j) regression(j,independent_variables,dependent_variables,primary_variable,model_type,proportion_cutoff)) %>% dplyr::bind_rows() %>% dplyr::filter(term!='(Intercept)') %>% dplyr::mutate( bonferroni = p.adjust(p.value, method = "bonferroni"), BH = p.adjust(p.value, method = "BH"), BY = p.adjust(p.value, method = "BY"))
  out = out %>% dplyr::mutate(dataset_id=x[[3]])
  return(list('output' = out,'vibrate' = vibrate))
}

compute_initial_associations <- function(bound_data,primary_variable, model_type, proportion_cutoff,vibrate){
    output = apply(bound_data, 1, function(x) run_associations(x,primary_variable,model_type,proportion_cutoff))
    output[['output']] = dplyr::bind_rows(output[['output']])
  return(output)
}
