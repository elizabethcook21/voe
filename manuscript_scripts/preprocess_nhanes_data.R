library(tidyverse)

load('../data/nhanes9904_VoE.Rdata')
data_filtered = mainTab %>% filter(!is.na(BMXBMI),!is.na(DR1TFIBE))

toremove = which(colSums(is.na(data_filtered))/nrow(data_filtered)>=.5)
data_filtered= data_filtered %>% select(-c(names(toremove),area,bmi))  %>% mutate(sampleID=seq(nrow(data_filtered)))

data_filtered  = data_filtered %>% type.convert()

tibble_dep = data_filtered %>% select(sampleID,DR1TFIBE) 
tibble_ind = data_filtered %>% select(-DR1TFIBE,-bDIETARY_FIBER_gm) %>% relocate(sampleID)

saveRDS(tibble_dep,'../data/nhanes_dep_data.rds')
saveRDS(tibble_ind,'../data/nhanes_ind_data.rds')
