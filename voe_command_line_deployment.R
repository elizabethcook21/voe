#!/usr/bin/env Rscript

library(getopt)

### arguments
spec = matrix(c(
  'dependent_variables', 'd', 2, "character","Path to dependent variable matrix, stored as .rds file. If running meta-analysis, provide comma separated list of paths without spaces (or commas) in either paths or filenames.",
  'independent_variables', 'i', 2, "character","Path to dependent variable matrix, stored as .rds file. If running meta-analysis, provide comma separated list of paths without spaces (or commas) in either paths or filenames.",
  'primary_variable', 'v', 2, "character","Primary independent variable of interest.",
  'output_path', 'o', 2, "character","Output rds name",
  'fdr_method', 'm', 1, "character","Multiple hypothesis testing method (BY, BH, bonferroni, default = BY)",
  'fdr_cutoff', 'c', 1, "double","Float between 0.0 and 1.0, FDR-adjusted p-value threshold. (default = 0.05)",
  'max_vibration_num', 'n', 1, "integer","Integer, max number of vibrations per feature. (default = 50000)",
  'proportion_cutoff', 'p', 1, "double","Float between 0.0 and 1.0. (default = 0.9",
  'meta_analysis', 'a', 1, "logical","TRUE/FALSE (default: FALSE)",
  'model_type', 'f', 1, "character","GLM family. (default: gaussian)",
  'help'   , 'h', 0, "display command line options","logical"
), byrow=TRUE, ncol=5)
opt = getopt(spec)

# if help was asked for print parameters
if ( !is.null(opt$help) ) {
  cat(getopt(spec, usage=TRUE))
  q(status=1)
}

### set defaults if necessary
if ( is.null(opt$fdr_method    ) ) { opt$fdr_method    = 'BY'     }
if ( is.null(opt$fdr_cutoff      ) ) { opt$fdr_cutoff      = 0.05     }
if ( is.null(opt$max_vibration_num   ) ) { opt$max_vibration_num   = 50000    }
if ( is.null(opt$proportion_cutoff ) ) { opt$proportion_cutoff = 0.95 }
if ( is.null(opt$model_type ) ) { opt$model_type = 'gaussian' }
if ( is.null(opt$meta_analysis ) ) { opt$meta_analysis = FALSE }

# run pipeline
library(devtools)
library(tidyverse)
devtools::install_local(opt$path_to_repository, force=TRUE)
library(voe)
devtools::load_all()

message('Parsing input data...')
dependent_variable_locs = strsplit(opt$dependent_variables,',')
dependent_variables=list()

print(length(dependent_variable_locs[[1]]))
if(length(dependent_variable_locs[[1]])>1){
	for(i in seq_along(dependent_variable_locs[[1]]))
    print(trimws(dependent_variable_locs[[1]][[i]]))
    data = readRDS(trimws(dependent_variable_locs[[1]][[i]]))
    colnames(data)[1]='sampleID'
    print('here')
		dependent_variables[[i]] = data
}
if(length(dependent_variable_locs[[1]])==1){
	dependent_variables=readRDS(dependent_variable_locs[[1]])
  colnames(dependent_variables)[1]='sampleID'
}

independent_variable_locs = strsplit(opt$independent_variables,',')
independent_variables=list()
if(length(independent_variable_locs[[1]])>1){
	for(i in seq_along(independent_variable_locs[[1]]))
    print(trimws(independent_variable_locs[[1]][[i]]))
		data = tibble:as_tibble(readRDS(trimws(independent_variable_locs[[1]][[i]])))
    colnames(data)[1]='sampleID'
    independent_variables[[i]]=data 
}
if(length(independent_variable_locs[[1]])==1){
	independent_variables=readRDS(independent_variable_locs[[1]])
  colnames(independent_variables)[1]='sampleID'
}

message('Data parsed and loaded, running pipeline.')

output = voe::full_voe_pipeline(dependent_variables=dependent_variables,independent_variables=independent_variables,primary_variable=opt$primary_variable,fdr_method=opt$fdr_method,fdr_cutoff=opt$fdr_cutoff,max_vibration_num=opt$max_vibration_num,proportion_cutoff=opt$proportion_cutoff,meta_analysis=opt$meta_analysis, model_type=opt$model_type)

saveRDS(output,opt$output_path)

# q(status=0)


