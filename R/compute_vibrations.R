suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(tidyr))
suppressMessages(library(magrittr))
suppressMessages(library(rje))
suppressMessages(library(broom))
suppressMessages(library(stringr))

# Workflow:
# 1) get features want to vibrate over, stored as a vector in an RDS (from and lit review)
# 2) get maximal model dataset (modeldfs from prune_metadata.R)
# 3) for each desired feature and for each dataset/cohort, perform vibrations
# 4) Aggregate outputs & visualize (in analyze_vibrations.R)

# Finds smallest non-zero value in input df
get_fudge_factor <- function(metaphlan_df) {
  datatype_df <- metaphlan_df %>% select_if(is.numeric)
  return(min(map_dbl(map(datatype_df, ~.[. > 0]), ~min(.))))
}
# print(get_fudge_factor(t_metaphlan_df))


# Vibrates over a single maximal model for a single feature
vibrate <- function(independent_variables, feature, dependent_variables,primary_variable) {
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
  regression_df=left_join(independent_variables %>% mutate_if(is.factor, as.character), dependent_variables %>% select(sampleID,feature),by = c("sampleID")) %>% mutate_if(is.character, as.factor) %>% drop_na() ####NEED TO LOG HOW MANY ROWS DROPPED, SIZE OF DF, ETC
    varset=powerSet(colnames(regression_df %>% select(-sampleID,primary_variable,-c(as.character(feature)))))
      if(length(varset)>10){
        varset=sample(varset,10)
    }
    return(tibble(
      independent_feature = feature,
      vars = varset,
      full_fits = map(vars, function(y) tryCatch(tidy(stats::lm(formula=as.formula(paste("I(`",feature,"`) ~ ",primary_variable,'+',paste(y,collapse='+',sep='+'),sep='',collapse='')),data = regression_df)),warning = function(w) w, error = function(e) e)),
      feature_fit = map(full_fits, function(x) tryCatch(filter(x, term == primary_variable),warning = function(w) w,error = function(e) e)
      )
    )
    )
}

compute_vibrations <- function(dependent_variables, independent_variables, primary_variable, features_of_interest) {
  message(paste('Computing vibrations for',length(features_of_interest),'features'))
  output = reduce( # map over all feature's want to vibrate for
    map(features_of_interest, function(x) vibrate(independent_variables, x, dependent_variables,primary_variable)),
    rbind, #combine all features
    .init = NA_real_ # .init is supplied as the first value to start the accumulation in reduce, as o.w. reduce with throw error for empty starting value
  )
  return(output)
}

independent_variables=readRDS('data/final_metadata.rds')[[2]][[1]] %>% select(-subjectID,-runID) %>% type.convert
dependent_variables=readRDS('data/abundance_data.rds') %>% select(-dataset_name)
features_of_interest=colnames(dependent_variables)[2:3]
test=compute_vibrations(dependent_variables, independent_variables, primary_variable, features_of_interest)


