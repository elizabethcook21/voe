# Vibrates over a single maximal model for a single feature
vibrate <- function(independent_variables, feature, dependent_variables,primary_variable,model_type,max_vibration_num,dataset_id,proportion_cutoff){#,mtry,num.trees,importance,min.node.size,splitrule) {
  colnames(dependent_variables)[[1]]='sampleID'
  colnames(independent_variables)[[1]]='sampleID'
  tokeep = independent_variables %>% tidyr::drop_na() %>% dplyr::select_if(~ length(unique(.)) > 1) %>% colnames
  todrop = setdiff(colnames(independent_variables),tokeep)
  if(length(todrop)>1){
    independent_variables=independent_variables %>% dplyr::select(-all_of(todrop))
  }
  regression_df=dplyr::left_join(independent_variables %>% dplyr::mutate_if(is.factor, as.character), dependent_variables %>% dplyr::select(sampleID,feature),by = c("sampleID")) %>% dplyr::mutate_if(is.character, as.factor) %>% tidyr::drop_na() ####NEED TO LOG HOW MANY ROWS DROPPED, SIZE OF DF, ETC
  variables_to_vibrate=colnames(regression_df %>% dplyr::select(-sampleID,-primary_variable,-c(as.character(feature))))
    varset=rje::powerSet(variables_to_vibrate)
      if(length(varset)>as.numeric(max_vibration_num)){
        varset=sample(varset,as.numeric(max_vibration_num))
    }
      return(tibble::tibble(
      independent_feature = feature,
      dataset_id = dataset_id,
      vars = varset,
      full_fits = purrr::map(vars, function(y) tryCatch(broom::tidy(stats::glm(formula=as.formula(paste("I(`",feature,"`) ~ ",primary_variable,'+',paste(y,collapse='+',sep='+'),sep='',collapse='')),family=model_type,data = regression_df)),warning = function(w) w, error = function(e) e)),
      feature_fit = purrr::map(full_fits, function(x) tryCatch(dplyr::filter(x, grepl(primary_variable,term)),warning = function(w) w,error = function(e) e)
      )
    ))
}

dataset_vibration <-function(subframe,primary_variable,model_type,features_of_interest,max_vibration_num, proportion_cutoff){#,mtry,num.trees,importance,min.node.size,splitrule){
  message(paste('Computing vibrations for',length(features_of_interest),'features in dataset number',subframe[[3]]))
  dep_sub = subframe[[1]]
  in_sub = subframe[[2]]
  purrr::reduce( # map over all feature's want to vibrate for
    purrr::map(features_of_interest, function(x) vibrate(in_sub, x, dep_sub,primary_variable,model_type,max_vibration_num, subframe[[3]], proportion_cutoff)),#,mtry,num.trees,importance,min.node.size,splitrule)),
    rbind, #combine all features
    .init = NA_real_ # .init is supplied as the first value to start the accumulation in reduce, as o.w. reduce with throw error for empty starting value
  )
}

compute_vibrations <- function(bound_data,primary_variable,model_type,features_of_interest,max_vibration_num,proportion_cutoff){#,mtry,num.trees,importance,min.node.size,splitrule){
  output = dplyr::bind_rows(apply(bound_data, 1, function(subframe) dataset_vibration(subframe, primary_variable,model_type ,features_of_interest,max_vibration_num, proportion_cutoff))) 
  output = output %>% dplyr::filter(!is.na(independent_feature))
  vibration_variables = unique(unlist(unname(apply(bound_data, 1, function(subframe) subframe[[2]] %>% dplyr::select(-sampleID,-primary_variable) %>% colnames))))
  return(list('vibration_output'=output,'vibration_variables'=vibration_variables))
}

