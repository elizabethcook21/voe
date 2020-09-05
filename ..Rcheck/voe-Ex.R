pkgname <- "voe"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('voe')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("analyze_voe_data")
### * analyze_voe_data

flush(stderr()); flush(stdout())

### Name: analyze_voe_data
### Title: Analyze VoE data
### Aliases: analyze_voe_data
### Keywords: analysis voe

### ** Examples

analyze_voe_data(vibration_output,confounder_analysis,logger)



cleanEx()
nameEx("clean_metaanalysis")
### * clean_metaanalysis

flush(stderr()); flush(stdout())

### Name: clean_metaanalysis
### Title: Clean meta-analysis output and get summary statistics.
### Aliases: clean_metaanalysis
### Keywords: meta-analysis

### ** Examples

clean_metaanalysis(metaanalysis,logger)



cleanEx()
nameEx("compute_initial_associations")
### * compute_initial_associations

flush(stderr()); flush(stdout())

### Name: compute_initial_associations
### Title: Deploy associations across datasets
### Aliases: compute_initial_associations
### Keywords: assocatiation initial regression,

### ** Examples

compute_initial_associations(bound_data,primary_variable, model_type, proportion_cutoff,vibrate, regression_weights,logger)



cleanEx()
nameEx("compute_metaanalysis")
### * compute_metaanalysis

flush(stderr()); flush(stdout())

### Name: compute_metaanalysis
### Title: Run meta-analysis
### Aliases: compute_metaanalysis
### Keywords: meta-analysis

### ** Examples

compute_metaanalysis(df,logger)



cleanEx()
nameEx("compute_vibrations")
### * compute_vibrations

flush(stderr()); flush(stdout())

### Name: compute_vibrations
### Title: Vibrations
### Aliases: compute_vibrations
### Keywords: assocatiation initial regression,

### ** Examples

compute_vibrations(bound_data,primary_variable,model_type,features_of_interest,max_vibration_num,proportion_cutoff,regression_weights,cores,logger,max_vars_in_model)



cleanEx()
nameEx("dataset_vibration")
### * dataset_vibration

flush(stderr()); flush(stdout())

### Name: dataset_vibration
### Title: Vibration for dataset
### Aliases: dataset_vibration
### Keywords: assocatiation initial regression,

### ** Examples

dataset_vibration(subframe,primary_variable,model_type,features_of_interest,max_vibration_num, proportion_cutoff,regression_weights,cores,logger,max_vars_in_model)



cleanEx()
nameEx("filter_unnest_feature_vib")
### * filter_unnest_feature_vib

flush(stderr()); flush(stdout())

### Name: filter_unnest_feature_vib
### Title: Unnest vibration data
### Aliases: filter_unnest_feature_vib
### Keywords: analysis voe

### ** Examples

filter_unnest_feature_vib(vib_df,logger)



cleanEx()
nameEx("find_confounders_linear")
### * find_confounders_linear

flush(stderr()); flush(stdout())

### Name: find_confounders_linear
### Title: Find confounders
### Aliases: find_confounders_linear
### Keywords: analysis voe

### ** Examples

find_confounders_linear(voe_list_for_reg, logger)



cleanEx()
nameEx("full_voe_pipeline")
### * full_voe_pipeline

flush(stderr()); flush(stdout())

### Name: full_voe_pipeline
### Title: Full VoE Pipeline
### Aliases: full_voe_pipeline
### Keywords: pipeline

### ** Examples

full_voe_pipeline(dependent_variables,independent_variables,primary_variable,vibrate=TRUE,fdr_method='BY',fdr_cutoff=0.05,max_vibration_num=50000,regression_weights=NULL, max_vars_in_model = NULL,proportion_cutoff=.95,meta_analysis=FALSE, model_type='gaussian', log=FALSE, cores = 1, confounder_analysis=TRUE,log_file_path=NULL)



cleanEx()
nameEx("get_adjuster_expanded_vibrations")
### * get_adjuster_expanded_vibrations

flush(stderr()); flush(stdout())

### Name: get_adjuster_expanded_vibrations
### Title: Expand vibration data
### Aliases: get_adjuster_expanded_vibrations
### Keywords: analysis voe

### ** Examples

get_adjuster_expanded_vibrations(voe_df, adjusters,logger)



cleanEx()
nameEx("get_converged_metadfs")
### * get_converged_metadfs

flush(stderr()); flush(stdout())

### Name: get_converged_metadfs
### Title: Filter-meta analysis
### Aliases: get_converged_metadfs
### Keywords: meta-analysis

### ** Examples

get_converged_metadfs(meta_df)



cleanEx()
nameEx("get_summary_stats")
### * get_summary_stats

flush(stderr()); flush(stdout())

### Name: get_summary_stats
### Title: Extract meta-analysis summary statistics
### Aliases: get_summary_stats
### Keywords: meta-analysis

### ** Examples

get_summary_stats(input_meta_df,logger)



cleanEx()
nameEx("ind_var_analysis")
### * ind_var_analysis

flush(stderr()); flush(stdout())

### Name: ind_var_analysis
### Title: Analysis of Independent Variables
### Aliases: ind_var_analysis
### Keywords: independent variable

### ** Examples

ind_var_analysis(metadata)



cleanEx()
nameEx("initialize_logger")
### * initialize_logger

flush(stderr()); flush(stdout())

### Name: initialize_logger
### Title: Logger
### Aliases: initialize_logger
### Keywords: logging

### ** Examples

ind_var_analysis(metadata)



cleanEx()
nameEx("makeBarGraph")
### * makeBarGraph

flush(stderr()); flush(stdout())

### Name: makeBarGraph
### Title: Making a Bar Graph
### Aliases: makeBarGraph
### Keywords: bar graph

### ** Examples

makeBarGraph(ind_var, names(colTypes[i]),pathToNewFolder))



cleanEx()
nameEx("makeHistogram")
### * makeHistogram

flush(stderr()); flush(stdout())

### Name: makeHistogram
### Title: Making a Histogram
### Aliases: makeHistogram
### Keywords: histogram

### ** Examples

makeHistogram(histogram_tibble, names(colTypes[i]),pathToNewFolder)



cleanEx()
nameEx("pre_pipeline_data_check")
### * pre_pipeline_data_check

flush(stderr()); flush(stdout())

### Name: pre_pipeline_data_check
### Title: Pre-flight checks
### Aliases: pre_pipeline_data_check
### Keywords: pipeline

### ** Examples

pre_pipeline_data_check(dependent_variables,independent_variables,primary_variable,fdr_method,fdr_cutoff,max_vibration_num,max_vars_in_model,proportion_cutoff,meta_analysis,model_type, logger)



cleanEx()
nameEx("regression")
### * regression

flush(stderr()); flush(stdout())

### Name: regression
### Title: Run initial association
### Aliases: regression
### Keywords: assocatiation initial regression,

### ** Examples

regression(j,independent_variables,dependent_variables,primary_variable,model_type,proportion_cutoff,regression_weights,logger)



cleanEx()
nameEx("run_associations")
### * run_associations

flush(stderr()); flush(stdout())

### Name: run_associations
### Title: Run all associations for a given dataset
### Aliases: run_associations
### Keywords: assocatiation initial regression,

### ** Examples

run_associations(x,primary_variable,model_type,proportion_cutoff,vibrate, regression_weights,logger)



cleanEx()
nameEx("summarize_vibration_data_by_feature")
### * summarize_vibration_data_by_feature

flush(stderr()); flush(stdout())

### Name: summarize_vibration_data_by_feature
### Title: summarize_vibration_data_by_feature
### Aliases: summarize_vibration_data_by_feature
### Keywords: analysis voe

### ** Examples

summarize_vibration_data_by_feature(df, logger)



cleanEx()
nameEx("vibrate")
### * vibrate

flush(stderr()); flush(stdout())

### Name: vibrate
### Title: Vibration for feature
### Aliases: vibrate
### Keywords: assocatiation initial regression,

### ** Examples

vibrate(merged_data,variables_to_vibrate,max_vars_in_model,feature,primary_variable,model_type,regression_weights,max_vibration_num,dataset_id,proportion_cutoff,logger)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
