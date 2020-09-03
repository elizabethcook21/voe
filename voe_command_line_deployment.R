#!/usr/bin/env Rscript

suppressMessages(library(getopt))

### arguments
spec = matrix(c(
  'dependent_variables', 'd', 2, "character","Path to dependent variable matrix, stored as .rds file. If running meta-analysis, provide comma separated list of paths without spaces (or commas) in either paths or filenames.",
  'independent_variables', 'i', 2, "character","Path to dependent variable matrix, stored as .rds file. If running meta-analysis, provide comma separated list of paths without spaces (or commas) in either paths or filenames.",
  'primary_variable', 'v', 2, "character","Primary independent variable of interest.",
  'vibrate', 'b', 1, "logical","TRUE/FALSE -- Run vibrations (default: TRUE)",
  'output_path', 'o', 2, "character","Output rds name",
  'fdr_method', 'm', 1, "character","Multiple hypothesis testing method (BY, BH, bonferroni, default = BY)",
  'fdr_cutoff', 'c', 1, "double","Float between 0.0 and 1.0, FDR-adjusted p-value threshold. (default = 0.05)",
  'max_vibration_num', 'n', 1, "integer","Integer, max number of vibrations per feature. (default = 50000)",
  'max_vars_in_model', 'r', 1, "integer","Integer, max number of independent variables in vibration. (default = NULL)",
  'confounder_analysis', 'y', 1, "logical","TRUE/FALSE -- mixed effects analysis to identify sources of confounding in dataset (default=TRUE).",
  'proportion_cutoff', 'g', 1, "double","Float between 0.0 and 1.0. (default = 0.9",
  'meta_analysis', 'a', 1, "logical","TRUE/FALSE (default: FALSE)",
  'cores', 't', 1, "integer","Number of cores to use (default = 1).",
  'model_type', 'f', 1, "character","GLM family. (default: gaussian)",
  'help'   , 'h', 0, "display command line options","logical"
), byrow=TRUE, ncol=5)
opt = getopt(spec)

# if help was asked for print parameters
if ( !is.null(opt$help) ) {
  cat(getopt(spec, usage=TRUE))
  q(status=1)
}
max_vibration_num = opt$max_vibration_num
### set defaults if necessary
if ( is.null(opt$fdr_method    ) ) { opt$fdr_method    = 'BY'     }
if ( is.null(opt$max_vars_in_model    ) ) { opt$max_vars_in_model    = NULL     }
if ( is.null(opt$confounder_analysis    ) ) { opt$confounder_analysis    = TRUE     }
if ( is.null(opt$cores    ) ) { opt$cores    = 1     }
if ( is.null(opt$vibrate    ) ) { opt$vibrate    = TRUE     }
if ( is.null(opt$fdr_cutoff      ) ) { opt$fdr_cutoff      = 0.05     }
if ( is.null(opt$max_vibration_num   ) ) { opt$max_vibration_num   = 50000    }
if ( is.null(opt$proportion_cutoff ) ) { opt$proportion_cutoff = 0.9 }
if ( is.null(opt$model_type ) ) { opt$model_type = 'gaussian' }
if ( is.null(opt$meta_analysis ) ) { opt$meta_analysis = FALSE }

# run pipeline
suppressMessages(library(devtools))
suppressMessages(library(tidyverse))
suppressMessages(library(voe))

message('Parsing input data...')
dependent_variable_locs = strsplit(as.character(opt$dependent_variables),',')
dependent_variables=list()

if(length(unlist(dependent_variable_locs))>1){
	for(i in unlist(dependent_variable_locs)){
    data = tibble::tibble(readRDS(trimws(i)))
    colnames(data)[1]='sampleID'
		dependent_variables[[i]] = data
  }
}

if(length(unlist(dependent_variable_locs))==1){
	dependent_variables=readRDS(dependent_variable_locs[[1]])
  colnames(dependent_variables)[1]='sampleID'
}

independent_variable_locs = strsplit(as.character(opt$independent_variables),',')
independent_variables=list()

if(length(unlist(independent_variable_locs))>1){
  for(i in unlist(independent_variable_locs)){
    data = tibble::tibble(readRDS(trimws(i)))
    colnames(data)[1]='sampleID'
    independent_variables[[i]] = data
  }
}

if(length(unlist(independent_variable_locs))==1){
  independent_variables=readRDS(independent_variable_locs[[1]])
  colnames(independent_variables)[1]='sampleID'
}

message('Data parsed and loaded, running pipeline.')

output = voe::full_voe_pipeline(dependent_variables=dependent_variables,independent_variables=independent_variables,primary_variable=opt$primary_variable,fdr_method=opt$fdr_method,confounder_analysis=opt$confounder_analysis,fdr_cutoff=opt$fdr_cutoff,max_vibration_num=opt$max_vibration_num,proportion_cutoff=opt$proportion_cutoff,meta_analysis=opt$meta_analysis,max_vars_in_model = opt$max_vars_in_model, model_type=opt$model_type,cores=opt$cores)

if(exists("output")==TRUE){
  message('Writing output to file...')
  saveRDS(output,opt$output_path)
}

# q(status=0)


