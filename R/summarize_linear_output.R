#summarize vibration of effects output
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(broom)
library(stringr) # regex pattern matching
library(rlang) # rlang::duplicate()
library(broom.mixed)
library(rje)
library(lmerTest)
###plot volcano colored by janus effect


###extract out vibration data


###mixed effects model to look at confounder analysis

filter_unnest_feature_vib <- function(vib_df) {
  return(vib_df %>% slice(which(map_lgl(vib_df$feature_fit, ~class(.)[[1]] == "tbl_df"))) %>% unnest(feature_fit))
}

get_adjuster_expanded_vibrations <- function(voe_df, adjusters) {
  copy_voe_df <- duplicate(voe_df, shallow = FALSE)
  for (variable in adjusters) {
    copy_voe_df %<>% mutate(newcol = map_int(copy_voe_df$vars, ~(variable %in% .)))
    colnames(copy_voe_df)[length(colnames(copy_voe_df))] <- variable
  }
  return(copy_voe_df)
}

find_confounders_linear <- function(voe_list_for_reg){
  ptype=unique(voe_list_for_reg$term)
  voe_adjust_for_reg_ptype <- voe_list_for_reg %>% select_if(~ length(unique(.)) > 1) %>% select(-c(full_fits,std.error,statistic))
  voe_adjust_for_reg_ptype$estimate=abs(voe_adjust_for_reg_ptype$estimate)
  fit_estimate=lmer(data=voe_adjust_for_reg_ptype,as.formula(estimate ~ . +(1|independent_feature) -independent_feature - estimate - p.value),control = lmerControl(optimizer = "bobyqa"))
  fit_estimate_forplot=tidy(fit_estimate) %>% mutate(sdmin=(estimate-std.error),sdmax=(estimate+std.error))
  #saveRDS(fit_estimate_forplot,paste('litvib_only_disease_specific_confounders_estimate_',ptype,'.rds',sep=''))
  return(fit_estimate_forplot)
}

summarize_vibration_data_by_feature <- function(df){
  p <- c(0.01,.5,.99)
  p_names <- map_chr(p, ~paste0('estimate_quantile_',.x*100, "%"))
  p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% set_names(nm = p_names)
  model_counts = df %>% count(independent_feature) %>% rename(number_of_models=n)
  df_estimates = suppressMessages(df %>% group_by(independent_feature) %>% summarize_at(vars(estimate), funs(!!!p_funs)) %>% mutate(estimate_diff_99_1 = `estimate_quantile_99%`-`estimate_quantile_1%`,janus_effect=df %>% group_by(independent_feature) %>% summarise(janus_effect = sum(estimate > 0, na.rm = TRUE)/sum(is.finite(estimate), na.rm = TRUE)) %>% ungroup() %>% select(janus_effect) %>% unname %>% unlist))
  p_names <- map_chr(p, ~paste0('pval_quantile_',.x*100, "%"))
  p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% set_names(nm = p_names)
  df_pval = df %>% group_by(independent_feature) %>% summarize_at(vars(p.value), funs(!!!p_funs)) %>% mutate(pvalue_diff_99_1 = `pval_quantile_99%`-`pval_quantile_1%`)
  summarized_voe_data=bind_cols(model_counts, df_estimates %>% select(-independent_feature),df_pval %>% select(-independent_feature))
  return(summarized_voe_data)
}


analyze_voe_data <- function(vibration_output){
  voe_annotated =get_adjuster_expanded_vibrations(vibration_output[[1]], vibration_output[[2]])
  voe_unnested_annotated = filter_unnest_feature_vib(voe_annotated) %>% select(-vars)
  summarized = summarize_vibration_data_by_feature(voe_unnested_annotated)
  confounder_analysis = find_confounders_linear(voe_unnested_annotated)
  return(list('summarized_vibration_output'= summarized,'confounder_analysis'=confounder_analysis,'data'=voe_unnested_annotated))
}

