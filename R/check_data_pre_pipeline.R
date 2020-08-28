#pre-flight checks

pre_pipeline_data_check <- function(dependent_variables,independent_variables,primary_variable,fdr_method,fdr_cutoff,max_vibration_num,proportion_cutoff,meta_analysis,model_type){
  message('Checking input data...')
  if(meta_analysis==TRUE){
#    if(model_type=='rf'){
#      message('You seem to be attempting to meta-analyze random forest output. This is not feasible with our current pipeline. Please change the meta_analysis parameter to FALSE and try again.')
#    }
    message(paste('Primary variable of interest: ',primary_variable,sep=''))
    message(paste('FDR method: ',fdr_method,sep=''))
    message(paste('FDR cutoff: ',as.character(fdr_cutoff),sep=''))
    message(paste('Max number of vibrations (if vibrate=TRUE): ',as.character(max_vibration_num),sep=''))
    message(paste('Only keeping features that are at least',proportion_cutoff*100,'percent nonzero.'))
    num_features = purrr::map(dependent_variables, function(x) ncol(x)-1)
    num_samples = purrr::map(dependent_variables, function(x) nrow(x)-1)
    num_ind = purrr::map(independent_variables, function(x) ncol(x)-1)
    data_summary = dplyr::bind_cols(list('Number of features' = unlist(unname(num_features)),'Number of samples' = unlist(unname(num_samples)),'Number of adjusters' = unlist(unname(num_ind)))) %>% dplyr::mutate(dataset_number=seq_along(num_features))    %>% dplyr::mutate(max_models_per_feature = `Number of adjusters`*max_vibration_num)
    message('Preparing to run pipeline with the following parameters:')
    print(data_summary)
    Sys.sleep(2)
    max_models = sum(data_summary$max_models_per_feature*data_summary$`Number of features`)
    message(paste('This works out to a max of',as.character(max_models),'models across all features and, assuming 0.1% of all features being significant,',as.character(.001*max_models),'vibrations.'))
    if(max_models>10000000){
      message('Warning: a run at this scale (over 10 million models fit) may take a long time.')
      Sys.sleep(2)
    }
    message('Checking for illegal variable names...')
    illegal_names = c('dependent_variables','independent_variables','feature','max_vibration_num','fdr_method','fdr_cutoff','primary_variable','independent_feature')
    allnames=unique(unname(unlist(c(purrr::map(dependent_variables, function(x) colnames(x)), purrr::map(independent_variables, function(x) colnames(x))))))
    to_change = intersect(illegal_names,allnames)
  }
  else{
    num_features = ncol(dependent_variables) - 1
    num_samples = nrow(dependent_variables) - 1
    num_ind = ncol(independent_variables) - 1
    data_summary = dplyr::bind_cols(list('feature_num' = unlist(unname(num_features)),'sample_num' = unlist(unname(num_samples)),'adjuster_num' = unlist(unname(num_ind)))) %>% dplyr::mutate(dataset=seq_along(num_features))
    message(paste('Preparing to run VoE pipeline for',as.character(num_features),'features,',as.character(num_samples),'samples, and',as.character(num_ind),'adjusters.'))
    message(paste('Model type: ',model_type,sep=''))
    message(paste('Primary variable of interest: ',primary_variable,sep=''))
    message(paste('FDR method: ',fdr_method,sep=''))
    message(paste('FDR cutoff: ',as.character(fdr_cutoff),sep=''))
    message(paste('Only keeping features that are at least',proportion_cutoff*100,'percent nonzero.'))
    message(paste('Max number of vibrations (if vibrate=TRUE): ',as.character(max_vibration_num),sep=''))
    max_models_per_feature = num_ind*max_vibration_num
    max_models = num_features*max_models_per_feature
    message(paste('This works out to a max of',as.character(max_models),'models across all features, with',max_models_per_feature,'per feature. Assuming 0.1% of all features being significant,',as.character(.001*max_models),'vibrations.'))
    if(max_models>10000000){
      message('Warning: a run at this scale (over 10 million models fit) may take a long time. If you\'re running this interactively, we recommend splitting your input features into batches or using our command line tool.')
      Sys.sleep(2)
    }
  message('Checking for illegal variable names...')
  illegal_names = c('dependent_variables','independent_variables','feature','max_vibration_num','fdr_method','fdr_cutoff','primary_variable','independent_feature')
  allnames=c(colnames(dependent_variables),colnames(independent_variables))
  to_change = intersect(illegal_names,allnames)
  }
  if(length(to_change)>0){
    message('Illegal variable names that may disrupt pipeline have been identified. Please adjust the following column names in your input data.')
    print(to_change)
    return(FALSE)
  }
  else{
    message('Pre-flight checks complete, you\'re ready to go.')
    return(TRUE)
  }
}
