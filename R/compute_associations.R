suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(tidyr))
suppressMessages(library(magrittr))
suppressMessages(library(broom))
suppressMessages(library(stringr))

# For a single feature and single modeldf outputted by prune_metadata.R, computes the proportion of non-zero values per cohort.
compute_microbial_feature_proportions_helper <- function(model_ready_df, metaphlan_df, feature_num) {
  #browser()
  return(
    length(
      which((left_join(model_ready_df %>% mutate_if(is.factor, as.character),
                       metaphlan_df %>% mutate(dataset_name=str_replace(dataset_name, "gene_families_", "")) %>% select(c(1,2, (feature_num + 2))),
                       by = c("dataset_name", "sampleID")))[[length(model_ready_df) + 1]] > 0)
    ) / nrow(model_ready_df)
  )
}

regression <- function(j,independent_variables,dependent_variables,primary_variable){
  feature_name = colnames(dependent_variables)[j+1]
  print(str_c("feature no.: ", j)) # print feature no. currently working on
  regression_df=left_join(independent_variables %>% mutate_if(is.factor, as.character), dependent_variables %>% select(c(1, (j + 1))),by = c("sampleID")) %>% mutate_if(is.character, as.factor) %>% drop_na() ####NEED TO LOG HOW MANY ROWS DROPPED, SIZE OF DF, ETC
  regression_df %<>% select(-sampleID)
  #run regression
  tryCatch(tidy(stats::lm(as.formula(str_c("I(`", feature_name,"`) ~ ",primary_variable)),data = regression_df))%>% mutate(feature=feature_name),
           # NOTE: tidy() ends up removing and singularity fits (e.g. NA fits from a feature that, for the dataset's samples all have 0 relative abundance) #####NEED TO LOG THIS SOMEHOW
           warning = function(w) w, # if warning or error, just return them instead of output
           error = function(e) e
  )
}

run_associations <- function(independent_variables,dependent_variables,primary_variable){
  ######POTENTIAL FEATURE: ADD FUDGE FACTOR? OR PRESPECIFY?
  #fudge_factor <- min((metaphlan_df %>% select(-1, -2) %>% unlist())[which((metaphlan_df %>% select(-1, -2) %>% unlist()) > 0)])
  #iterate through each cohort for each feature
  tokeep = independent_variables %>% drop_na %>% select_if(~ length(unique(.)) > 1) %>% colnames
  ####LOG WHAT YOU'RE LOSING
  todrop = setdiff(colnames(independent_variables),tokeep)
  if(length(todrop)>1){
    message('Dropping the following variables due to either lacking multiple levels or NaN values:')
    print(todrop)
  }
  independent_variables=independent_variables %>% select(-all_of(todrop))
  out = map(seq_along(dependent_variables %>% select(-sampleID)), function(j) regression(j,independent_variables,dependent_variables,primary_variable))%>% bind_rows %>% filter(term!='(Intercept)') %>% mutate( bonferroni.p.val = p.adjust(p.value, method = "bonferroni"), bh.p.val = p.adjust(p.value, method = "BH"), by.p.val = p.adjust(p.value, method = "BY")
  )
  return(out)
}

compute_initial_associations <- function(dependent_variables,independent_variables,primary_variable) {
  dependent_variables <- as_tibble(dependent_variables)
  independent_variables <- as_tibble(independent_variables)
  colnames(independent_variables)[1]='sampleID'
  colnames(dependent_variables)[1]='sampleID'
  output <- run_associations(independent_variables,dependent_variables,primary_variable)
  return(output)
}
