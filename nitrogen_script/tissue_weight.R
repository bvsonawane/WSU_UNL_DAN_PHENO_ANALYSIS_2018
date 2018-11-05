rm(list=ls())
require(dplyr)
require(plyr)
require(tidyr)
require(gdata)
library(skimr)
require(ggplot2)
require(data.table)
##read licor files
data_path <- "rawdata/tissue_weight/dry_tissue_weight/" 
all_files = list.files(path= data_path)
##loop to read all licor files
dry_dat = data.frame()
for (z in 1:length(all_files)){
  a.temp.path <- paste(data_path, all_files[z],sep="")
  temp_file <- read.csv(a.temp.path, header = F)
  dry_dat <- rbind.fill(dry_dat, temp_file)
}
dry_data<- dry_dat[, -1] 
dat_col<- c("weighing_date", "time", "bar_id", "dry_weight_g", "unit", "scale_stability")
names(dry_data)<- dat_col
dry_data$bar_id<- as.character(dry_data$bar_id)
dry_data$tissue_id<- substr(dry_data$bar_id,start= nchar(dry_data$bar_id)-2,stop= nchar(dry_data$bar_id))
dry_data$tissue<- ifelse(dry_data$tissue_id== "LGT", "leaf",ifelse(dry_data$tissue_id== "SGT", "stem",
ifelse(dry_data$tissue_id== "RGT", "root", "problem")))
dry_data$pot_bar_id<- trim(substr(dry_data$bar_id,start=1,stop= nchar(dry_data$bar_id)-4))
dry_data$pot_bar_id1<- trim(substr(dry_data$bar_id,start=1,stop= nchar(dry_data$bar_id)-4))
final<- dry_data %>% separate(pot_bar_id1, c("na1", "na2", "genotype", "replicate", "harvest", "treatment"))
write.csv(final[, c("pot_bar_id", "genotype", "replicate", "harvest", "treatment", "tissue","dry_weight_g")],"rawdata/tissue_weight/dry_weight_data_unl_phen_N_2018.csv")
##write.csv(final,"rawdata/dry_weight_test.csv")

dry_data%>%separate(pot_bar_id, c("na1", "na2", "genotype", "replicate", "harvest", "treatment"))
ftable(xtabs( ~ genotype+replicate+treatment+harvest, final))

dry_final<- final[, c("pot_bar_id", "genotype", "replicate", "harvest", "treatment", "tissue","dry_weight_g")]

#################################################################
##Fresh weight data

fw_ls<- read.csv("rawdata/tissue_weight/fresh_tissue_weight/fw_leaf_stem_nitrogen.csv")
fw_r<- read.csv("rawdata/tissue_weight/fresh_tissue_weight/fw_roots_nitrogen.csv")

fw_dat<- join(fw_ls, fw_r, by= c("plant_id", "bar_id"))
##rearrange data, 
#NOTE: we are dealing with only for water content which is measured, so tissue biomass samples for RN is still need to account.
fw_dat<- melt(as.data.table(fw_dat), id= c("plant_id", "bar_id")) 
fw_data<- droplevels(subset(fw_dat, variable=="leaf_fw_g"|variable=="root"|variable=="stem_fw_g"))
## rename names 
fw_data$tissue<- as.factor(ifelse(fw_data$variable=="leaf_fw_g", "leaf", ifelse(fw_data$variable=="stem_fw_g", "stem",
ifelse(fw_data$variable=="root", "root","PROBLEM"))))
summary(fw_data$tissue)
fw_data$pot_bar_id<- fw_data$bar_id
fw_data$fresh_weight_g<- fw_data$value
final_fw_data<- fw_data
final_fw_data$variable<- NULL
final_fw_data$value<- NULL
final_fw_data$bar_id<- NULL
write.csv(final_fw_data, "rawdata/tissue_weight/fresh_weight_data_unl_phen_N_2018.csv")


combo<- join(final_fw_data,dry_final, by= c("pot_bar_id", "tissue"))

combo$tissue_water_g<- combo$fresh_weight_g-combo$dry_weight_g

final_combo<- combo[, c("plant_id", "pot_bar_id", "genotype", "treatment","harvest", "replicate","tissue",           
                    "fresh_weight_g","dry_weight_g","tissue_water_g")]
          
          
write.csv(final_combo,"rawdata/biomass_fresh_dry_nitrogen_UNL_2018.csv")

final_combo$tissue<- factor(final_combo$tissue, levels= c("leaf", "stem", "root"))
###
##General overwiev of data
p1<- ggplot(final_combo, aes(y=fresh_weight_g, x= harvest, colour= genotype))+
  geom_boxplot()+facet_grid(vars(tissue),vars(treatment))

p2<- ggplot(final_combo, aes(y=dry_weight_g, x= harvest, colour= genotype))+
  geom_boxplot()+facet_grid(vars(tissue),vars(treatment))

p3<- ggplot(final_combo, aes(y=tissue_water_g, x= harvest, colour= genotype))+
  geom_boxplot()+facet_grid(vars(tissue),vars(treatment))


ggsave( "figures/fresh_wt_nitrogen.png", p1, height = 5, width = 5)
ggsave( "figures/dry_wt_nitrogen.png", p2, height = 5, width = 5)
ggsave( "figures/water_wt_nitrogen.png", p3, height = 5, width = 5)
