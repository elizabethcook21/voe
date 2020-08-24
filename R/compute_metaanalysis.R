
# Workflow:
# For both single-cohort and multi-cohort study conditions,
# for each tibble of tidy fit outputs, filter out microbial feature stats and combine them across cohorts into 1 tibble
# For only multi-cohort study conditions, then map over all microbial features' tibbles and compute meta-analysis (metagen())

# takes in df outputted by compute_microbial_associations, currently of form where
# each row is grouped by dataset_name, and all data is nested, with microbial feature fits starting in the 4th col (first 3 are dataset_name, model_selected_df, and tidy_fits)
# goes through and filters each nested tibble of summary outputs for microbial features (since some fit with singularity and are removed)
# NOTE: features must currently start from col 4 as first 3 are ommitted
# NEW BEHAVIOR: error handling! now for any assocs that are not tibbles (e.g. error handled assocs passed on from comput_microbial_associations.R) will be labeled in this step as NA, and removed in get_combined_feature_fits()

compute_metaanalysis <- function(df) {
  new_df <- tibble(analysis = "meta-analysis") # create new tibble with placeholder column
  message('Computing meta-analysis')
  for (i in seq_along(features)) {
    new_colname = features[[i]]
    df_sub = df %>% filter(feature==features[[i]])
    number_datasets = nrow(df_sub)
    new_df %<>% mutate(new = list(tryCatch(metagen(estimate,
                                                   std.error,
                                                   data = df_sub,
                                                   studlab = dataset_name,
                                                   comb.fixed = FALSE,
                                                   comb.random = TRUE, # random effects model
                                                   method.tau = 'REML', # using REML estimator for tau heterogeneity parameter
                                                   hakn = FALSE, # not using that conserative estimator adjuster
                                                   prediction = TRUE,
                                                   sm = "SMD",
                                                   control=list(maxiter=1000)), # using 10x default max iteractions
                                           warning = function(w) w, # if warning return warning, don't want results at all to keep data and outputs as clean as possible (previously outputted list of 2, results and warnings)
                                           error = function(e) e))  # if results in error, return error
    )
    new_df = new_df %>% rename(!!features[[i]]:=new)
  }
  return(new_df %>% select(-analysis)) # remove placeholder column
}

compute_metaanalysis <- function(associations) {
  #args <- c("CRC_example_associations.rds", "CRC_example_meta_analysis.rds", "study_condition" )
  #args <- c("Karlsson_associations.rds","2020_T2D_Data/Karlsson_metaanlysis.rds", "disease1" )
  for(x in seq_along(associations)){
    associations[[x]]$dataset_name=x
  }
  association_outputs <- bind_rows(associations)
  output <- compute_metaanalysis(association_outputs)
  return(output)
}

get_good_metadfs <- function(meta_df) {
  toremove=list()
  count=0
  for(x in 1:length(meta_df)){
    if(class(meta_df[[x]][[1]])[[1]]!='metagen'){
      toremove[as.character(count)]=x
      count=count+1
    }
  }
  if(length(toremove)>0){
    meta_df=meta_df[-unname(unlist(toremove))]
  }
  return(meta_df)
}

get_summary_stats <- function(input_meta_df) {
  meta_df <- get_good_metadfs(input_meta_df)
  return(
    tibble(
      feature = colnames(meta_df),
      estimate = map_dbl(meta_df, ~.[[1]]$TE.random),
      p.val = map_dbl(meta_df, ~.[[1]]$pval.random),
      bonferroni.p.val = p.adjust(p.val, method = "bonferroni"),
      fdr.p.val = p.adjust(p.val, method = "fdr"),
      by.p.val = p.adjust(p.val, method = "BY"),
      CI_95_lower = map_dbl(meta_df, ~.[[1]]$lower.random),
      CI_95_upper = map_dbl(meta_df, ~.[[1]]$upper.random)
    )
  )
}

clean_metaanalysis <- function(metaanalysis) {
  meta_outputs <- as_tibble(metaanalysis)
  output <- get_summary_stats(meta_outputs)
  return(output)
}
