
###mixed effects model to look at confounder analysis

filter_unnest_feature_vib <- function(vib_df,logger) {
  return(vib_df %>% dplyr::slice(which(purrr::map_lgl(vib_df$feature_fit, ~class(.)[[1]] == "tbl_df"))) %>% tidyr::unnest(feature_fit))
}

get_adjuster_expanded_vibrations <- function(voe_df, adjusters,logger) {
  copy_voe_df <- rlang::duplicate(voe_df, shallow = FALSE)
  adjusters= unique(unlist(unname(map(adjusters, function(x) unlist(x)))))
  for (variable in adjusters) {
    copy_voe_df  = copy_voe_df %>% dplyr::mutate(newcol = purrr::map_int(copy_voe_df$vars, ~(variable %in% .)))
    colnames(copy_voe_df)[length(colnames(copy_voe_df))] <- variable
  }
  return(copy_voe_df)
}

find_confounders_linear <- function(voe_list_for_reg,logger){
  ptype=unique(voe_list_for_reg$term)
  voe_adjust_for_reg_ptype <- voe_list_for_reg %>% dplyr::select_if(~ length(unique(.)) > 1) %>% dplyr::select(-c(full_fits,std.error,statistic))
  voe_adjust_for_reg_ptype$estimate=abs(voe_adjust_for_reg_ptype$estimate)
  if('independent_feature' %in% colnames(voe_adjust_for_reg_ptype) & !(1 %in% unique(unlist(unname(table(voe_adjust_for_reg_ptype$independent_feature)))))){
    fit_estimate=lme4::lmer(data=voe_adjust_for_reg_ptype,as.formula(estimate ~ . +(1|independent_feature) -independent_feature - estimate - p.value),control = lme4::lmerControl(optimizer = "bobyqa"))
    fit_estimate_forplot=broom.mixed::tidy(fit_estimate) %>% dplyr::mutate(sdmin=(estimate-std.error),sdmax=(estimate+std.error))
  }
  else{
    log4r::info(logger,'Note: Some features only had 1 vibration associated with them, likely due to a model failure or a paucity of vibration features. This means your confounder analysis will be done will a regular linear model, instead of a mixed effect one. See the documentation for more details.')
    fit_estimate=stats::lm(data=voe_adjust_for_reg_ptype,as.formula(estimate ~ . - estimate - p.value))
    fit_estimate_forplot=broom::tidy(fit_estimate) %>% dplyr::mutate(sdmin=(estimate-std.error),sdmax=(estimate+std.error))
  }
  return(fit_estimate_forplot)
}

summarize_vibration_data_by_feature <- function(df,logger){
  p <- c(0.01,.5,.99)
  p_names <- purrr::map_chr(p, ~paste0('estimate_quantile_',.x*100, "%"))
  p_funs <- purrr::map(p, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% purrr::set_names(nm = p_names)
  model_counts = df %>% dplyr::count(independent_feature) %>% dplyr::rename(number_of_models=n)
  df_estimates = suppressMessages(df %>% dplyr::group_by(independent_feature) %>% dplyr::summarize_at(dplyr::vars(estimate), tibble::lst(!!!p_funs)) %>% dplyr::mutate(estimate_diff_99_1 = `estimate_quantile_99%`-`estimate_quantile_1%`,janus_effect=df %>% dplyr::group_by(independent_feature) %>% dplyr::summarise(janus_effect = sum(estimate > 0, na.rm = TRUE)/sum(is.finite(estimate), na.rm = TRUE)) %>% dplyr::ungroup() %>% dplyr::select(janus_effect) %>% unname %>% unlist))
  p_names <- purrr::map_chr(p, ~paste0('pval_quantile_',.x*100, "%"))
  p_funs <- purrr::map(p, ~purrr::partial(quantile, probs = .x, na.rm = TRUE)) %>% purrr::set_names(nm = p_names)
  df_pval = df %>% dplyr::group_by(independent_feature) %>% dplyr::summarize_at(dplyr::vars(p.value), tibble::lst(!!!p_funs)) %>% dplyr::mutate(pvalue_diff_99_1 = `pval_quantile_99%`-`pval_quantile_1%`)
  summarized_voe_data=dplyr::bind_cols(model_counts, df_estimates %>% dplyr::select(-independent_feature),df_pval %>% dplyr::select(-independent_feature))
  return(summarized_voe_data)
}

analyze_voe_data <- function(vibration_output,logger){
  voe_annotated =get_adjuster_expanded_vibrations(vibration_output[[1]], vibration_output[[2]],logger)
  voe_unnested_annotated = filter_unnest_feature_vib(voe_annotated,logger) %>% dplyr::select(-vars)
  summarized = summarize_vibration_data_by_feature(voe_unnested_annotated,logger)
  confounder_analysis = find_confounders_linear(voe_unnested_annotated,logger)
  return(list('summarized_vibration_output'= summarized,'confounder_analysis'=confounder_analysis,'data'=voe_unnested_annotated))
}

