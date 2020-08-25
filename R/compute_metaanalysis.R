
compute_metaanalysis <- function(df) {
  new_df <- tibble(analysis = "meta-analysis") # create new tibble with placeholder column
  message('Computing meta-analysis')
  features=unique(df$feature)
  for (i in seq_along(features)) {
    new_colname = features[[i]]
    df_sub = df %>% filter(feature==features[[i]])
    number_datasets = nrow(df_sub)
    new_df =new_df %>% mutate(new = list(tryCatch(metagen(estimate,
                                                   std.error,
                                                   data = df_sub,
                                                   studlab = dataset_id,
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

get_summary_stats <- function(input_meta_df) {
  meta_df <- get_good_metadfs(input_meta_df)
  return(
    tibble(
      feature = colnames(meta_df),
      estimate = map_dbl(meta_df, ~.[[1]]$TE.random),
      p.val = map_dbl(meta_df, ~.[[1]]$pval.random),
      bonferroni = p.adjust(p.val, method = "bonferroni"),
      BH = p.adjust(p.val, method = "fdr"),
      BY = p.adjust(p.val, method = "BY"),
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
