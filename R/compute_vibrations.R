# Vibrates over a single maximal model for a single feature
vibrate <- function(merged_data,variables_to_vibrate,max_vars_in_model,feature,primary_variable,model_type,max_vibration_num,dataset_id,proportion_cutoff,logger){#,mtry,num.trees,importance,min.node.size,splitrule) {
    if(is.null(max_vars_in_model)==FALSE & max_vars_in_model < length(variables_to_vibrate)){
      random_list = sample.int(max_vars_in_model,max_vibration_num,replace=TRUE)
      varset = purrr::map(random_list, function(x) sample(variables_to_vibrate,x))
    }
    else{
      varset=rje::powerSet(variables_to_vibrate)
    }
    if(length(varset)>as.numeric(max_vibration_num)){
      varset=sample(varset,as.numeric(max_vibration_num))
    }
    regression_df = merged_data %>% dplyr::select(c(dplyr::all_of(feature),dplyr::all_of(primary_variable),dplyr::all_of(variables_to_vibrate)))
      return(tibble::tibble(
      independent_feature = feature,
      dataset_id = dataset_id,
      vars = varset,
      full_fits = purrr::map(vars, function(y) tryCatch(broom::tidy(stats::glm(formula=as.formula(paste("I(`",feature,"`) ~ ",primary_variable,'+',paste(y,collapse='+',sep='+'),sep='',collapse='')),family=model_type,data = regression_df)),warning = function(w) w, error = function(e) e)),
      feature_fit = purrr::map(full_fits, function(x) tryCatch(dplyr::filter(x, grepl(primary_variable,term)),warning = function(w) w,error = function(e) e)
      )
    ))
}

dataset_vibration <-function(subframe,primary_variable,model_type,features_of_interest,max_vibration_num, proportion_cutoff,cores,logger,max_vars_in_model){#,mtry,num.trees,importance,min.node.size,splitrule){
  log4r::info(logger,paste('Computing vibrations for',length(features_of_interest),'features in dataset number',subframe[[3]]))
  dep_sub = subframe[[1]]
  in_sub = subframe[[2]]
  colnames(dep_sub)[[1]]='sampleID'
  colnames(in_sub)[[1]]='sampleID'
  tokeep = in_sub %>% dplyr::select_if(~ length(unique(.)) > 1) %>% colnames
  todrop = setdiff(colnames(in_sub),tokeep)
  if(length(todrop)>1){
    in_sub=in_sub %>% dplyr::select(-all_of(todrop))
  }
  dep_sub = dep_sub %>% select(sampleID,c(features_of_interest))
  variables_to_vibrate=colnames(in_sub %>% dplyr::select(-c(sampleID,all_of(primary_variable))))
  merged_data=dplyr::left_join(in_sub %>% dplyr::mutate_if(is.factor, as.character), dep_sub %>% dplyr::mutate_if(is.factor, as.character),by = c("sampleID")) %>% dplyr::mutate_if(is.character, as.factor) ####NEED TO LOG HOW MANY ROWS DROPPED, SIZE OF DF, ETC
  if(as.integer(cores)>1){
    library(future)
    options(future.globals.maxSize = +Inf)
    future::plan(multisession, workers = as.integer(cores))#,
    output = furrr::future_map(features_of_interest, function(x) vibrate(merged_data, variables_to_vibrate, max_vars_in_model, x, primary_variable,model_type,max_vibration_num, subframe[[3]], proportion_cutoff,logger))#,
    dplyr::bind_rows(output)
  }
  else{
    output = purrr::map(features_of_interest, function(x) vibrate(merged_data, variables_to_vibrate, max_vars_in_model, x, primary_variable,model_type,max_vibration_num, subframe[[3]], proportion_cutoff,logger))#,
    dplyr::bind_rows(output)
  }
}

compute_vibrations <- function(bound_data,primary_variable,model_type,features_of_interest,max_vibration_num,proportion_cutoff,cores,logger,max_vars_in_model){#,mtry,num.trees,importance,min.node.size,splitrule){
  output = dplyr::bind_rows(apply(bound_data, 1, function(subframe) dataset_vibration(subframe, primary_variable,model_type ,features_of_interest,max_vibration_num, proportion_cutoff,cores,logger,max_vars_in_model)))
  output = output %>% dplyr::filter(!is.na(independent_feature))
  vibration_variables = unique(unlist(unname(apply(bound_data, 1, function(subframe) subframe[[2]] %>% dplyr::select(-sampleID,-primary_variable) %>% colnames))))
  return(list('vibration_output'=output,'vibration_variables'=vibration_variables))
}

