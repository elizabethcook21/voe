
#' Unnest vibration data
#'
#' Unnest regression output for vibrations.
#' @param vib_df VoE dataframe with columns for each adjuster (output of get_adjuster_expanded_vibrations).
#' @param logger Logger object (default = NULL).
#' @keywords voe analysis
#' @examples
#' filter_unnest_feature_vib(vib_df,logger)
filter_unnest_feature_vib <- function(vib_df,logger) {
  return(vib_df %>% dplyr::slice(which(purrr::map_lgl(vib_df$feature_fit, ~class(.)[[1]] == "tbl_df"))) %>% tidyr::unnest(feature_fit))
}

#' Expand vibration data
#'
#' Add column to vibration output for each adjuster (indicating presence or absence in a model).
#' @param voe_df Raw vibration output, the first entry in the output of compute_vibrations.
#' @param adjusters A dataframe, each column corresponding to the adjusters used in each dataset for vibrations. This is the second entry in the compute_vibrations output. 
#' @param logger Logger object (default = NULL).
#' @keywords voe analysis
#' @examples
#' get_adjuster_expanded_vibrations(voe_df, adjusters,logger)
get_adjuster_expanded_vibrations <- function(voe_df, adjusters,logger) {
  copy_voe_df <- rlang::duplicate(voe_df, shallow = FALSE)
  adjusters= unique(unlist(unname(purrr::map(adjusters, function(x) unlist(x)))))
  for (variable in adjusters) {
    copy_voe_df  = copy_voe_df %>% dplyr::mutate(newcol = purrr::map_int(copy_voe_df$vars, ~(variable %in% .)))
    colnames(copy_voe_df)[length(colnames(copy_voe_df))] <- variable
  }
  return(copy_voe_df)
}

#' Find confounders
#'
#' Model confounding from vibration analysis.
#' @param voe_list_for_reg A dataframe of expanded VoE output (output of filter_unnest_feature_vib)
#' @param logger Logger object (default = NULL).
#' @keywords voe analysis
#' @examples
#' find_confounders_linear(voe_list_for_reg, logger)
find_confounders_linear <- function(voe_list_for_reg,logger){
  ptype=unique(voe_list_for_reg$term)
  voe_adjust_for_reg_ptype <- voe_list_for_reg %>% dplyr::select_if(~ length(unique(.)) > 1) %>% dplyr::select(-c(full_fits,std.error,statistic))
  #voe_adjust_for_reg_ptype$estimate=abs(voe_adjust_for_reg_ptype$estimate)
  if('independent_feature' %in% colnames(voe_adjust_for_reg_ptype) & !(1 %in% unique(unlist(unname(table(voe_adjust_for_reg_ptype$independent_feature)))))){
    tryCatch({
      fit_estimate=lme4::lmer(data=voe_adjust_for_reg_ptype,as.formula(estimate ~ . +(1|independent_feature) -independent_feature - estimate - p.value),control = lme4::lmerControl(optimizer = "bobyqa"))
      fit_estimate_forplot=broom.mixed::tidy(fit_estimate) %>% dplyr::mutate(sdmin=(estimate-std.error),sdmax=(estimate+std.error))
      },
    error = function(e){
      fit_estimate_forplot = 'Confounder analysis failed.'
      print(fit_estimate_forplot)
      log4r::info(logger,'Note: Mixed effect modeling to identify sources of confounding failed. Running a simple linear model instead. If you want to try this analysis yourself, you can access the raw data for this yourself in the output and follow the methodological layout in the docs.')
    })
  }
  else{
    tryCatch({
      log4r::info(logger,'Note: Some features only had 1 vibration associated with them, likely due to a model failure or a paucity of vibration features. This means your confounder analysis will be done will a regular linear model, instead of a mixed effect one. See the documentation for more details.')
      fit_estimate=stats::lm(data=voe_adjust_for_reg_ptype,as.formula(estimate ~ . - estimate - p.value))
      fit_estimate_forplot=broom::tidy(fit_estimate) %>% dplyr::mutate(sdmin=(estimate-std.error),sdmax=(estimate+std.error))
    },
    error = function(e){
      fit_estimate_forplot = 'Confounder analysis failed.'
      log4r::info(logger,'Confounder analysis failed despite multiple attempts. We recommend looking at the raw vibration output to see what the issue may be.')
    })
  }
  return(fit_estimate_forplot)
}

#' summarize_vibration_data_by_feature
#'
#' Summarize output of vibrations for each dependent feature of interest.
#' @param df A dataframe of expanded VoE output (output of filter_unnest_feature_vib)
#' @param logger Logger object (default = NULL).
#' @keywords voe analysis
#' @examples
#' summarize_vibration_data_by_feature(df, logger)
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


#' Analyze VoE data
#'
#' Post-process vibration output.
#' @param vibration_output Output list from the compute vibrations function.
#' @param confounder_analysis TRUE/FALSE -- run confounder analysis (default = TRUE).
#' @param logger Logger object (default = NULL).
#' @keywords voe analysis
#' @export
#' @examples
#' analyze_voe_data(vibration_output,confounder_analysis,logger)
analyze_voe_data <- function(vibration_output,confounder_analysis,logger){
  voe_annotated =get_adjuster_expanded_vibrations(vibration_output[[1]], vibration_output[[2]],logger)
  voe_unnested_annotated = filter_unnest_feature_vib(voe_annotated,logger) %>% dplyr::select(-vars)
  summarized = summarize_vibration_data_by_feature(voe_unnested_annotated,logger)
  c_analysis='No confounder analysis completed.'
  if(confounder_analysis==TRUE){
    if(nrow(voe_unnested_annotated)>=10){
      c_analysis = find_confounders_linear(voe_unnested_annotated,logger)
    }
    else{
      log4r::info(logger,'Skipping confounder analysis, as not enough vibrations (under 10) completed to make it worthwhile.') 
    }
  } 
  return(list('summarized_vibration_output'= summarized,'confounder_analysis'=c_analysis,'data'=voe_unnested_annotated))
}

