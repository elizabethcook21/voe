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







