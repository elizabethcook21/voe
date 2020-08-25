# For a single feature and single modeldf outputted by prune_metadata.R, computes the proportion of non-zero values per cohort.

regression <- function(j,independent_variables,dependent_variables,primary_variable,proportion_cutoff){
  feature_name = colnames(dependent_variables)[j+1]
  regression_df=dplyr::left_join(independent_variables %>% dplyr::mutate_if(is.factor, as.character), dependent_variables %>% dplyr::select(c(1, (j + 1))),by = c("sampleID")) %>% dplyr::mutate_if(is.character, as.factor) %>% drop_na() ####NEED 
    regression_df = regression_df %>% dplyr::select(-sampleID)
    #run regression
    tryCatch(broom::tidy(stats::lm(as.formula(str_c("I(`", feature_name,"`) ~ ",primary_variable)),data = regression_df))%>% mutate(feature=feature_name),
             # NOTE: tidy() ends up removing and singularity fits (e.g. NA fits from a feature that, for the dataset's samples all have 0 relative abundance) #####NEED TO LOG THIS SOMEHOW
             warning = function(w) w, # if warning or error, just return them instead of output
             error = function(e) e
    )
}

run_associations <- function(x,primary_variable,proportion_cutoff){
  dependent_variables <- dplyr::as_tibble(x[[1]])
  toremove = which(colSums(dependent_variables %>% dplyr::select(-sampleID) == 0,na.rm=TRUE)/nrow(dependent_variables)>proportion_cutoff)
  message(paste("Removing",length(toremove),"features that are at least",proportion_cutoff*100,"percent zero values."))
  message(names(toremove))
  dependent_variables=dependent_variables %>% select(-(toremove+1))
  independent_variables <- dplyr::as_tibble(x[[2]])
  message(paste('Computing',as.character(ncol(dependent_variables)-1),'associations for dataset',as.character(unname(unlist(x[[3]])))))
  colnames(dependent_variables)[1]='sampleID'
  colnames(independent_variables)[1]='sampleID'
  tokeep = independent_variables %>% drop_na %>% dplyr::select_if(~ length(unique(.)) > 1) %>% colnames
  todrop = setdiff(colnames(independent_variables),tokeep)
  if(length(todrop)>1){
    message('Dropping the following variables due to either lacking multiple levels or NaN values:')
    print(todrop)
  }
  independent_variables=independent_variables %>% dplyr::select(-all_of(todrop))
  out = purrr::map(seq_along(dependent_variables %>% dplyr::select(-sampleID)), function(j) regression(j,independent_variables,dependent_variables,primary_variable,proportion_cutoff)) %>% bind_rows %>% filter(term!='(Intercept)') %>% mutate( bonferroni = p.adjust(p.value, method = "bonferroni"), BH = p.adjust(p.value, method = "BH"), BY = p.adjust(p.value, method = "BY")
  )
  out = out %>% dplyr::mutate(dataset_id=x[[3]])
  return(out)
}

compute_initial_associations <- function(bound_data,primary_variable, proportion_cutoff) {
    output = dplyr::bind_rows(apply(bound_data, 1, function(x) run_associations(x,primary_variable,proportion_cutoff)))
  return(output)
}
