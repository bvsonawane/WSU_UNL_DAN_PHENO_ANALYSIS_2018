rm(list=ls())
require(plyr)
require(ggplot2)
require(dplyr)
require(doBy)
require(tidyr)
require(scales)
source("functions/multiplot.R")
source("functions/ggplot_theme.R")
library(cowplot)


data1<- read.csv("rawdata/rd_data/rd_data1.csv")
data2<- read.csv("rawdata/rd_data/rd_data2.csv")
dat<- rbind.fill(data1,data2)


rd_data<- dat[, c("plant_id", "leaf_no", "genotype", "replicate", "Treatment", "licor", "date", "Photo")]
rd_data$Treatment<- factor(rd_data$Treatment, levels = c("WW","WS"))

rd_shape<- summaryBy(.~plant_id, rd_data, keep.names = T, na.rm= T)
## Need plant_id in empty spaces

data<- read.csv("rawdata/transmitance_data.csv")
data$Treatment<- factor(data$Treatment, levels = c("WW","WS"))

rd_id<- droplevels(rd_shape[, "plant_id"])
trans_id<-  droplevels(data[, "plant_id"])

t<- join(data, rd_shape)
miss_rd<- anti_join(data, rd_shape)

final<- join(data, rd_shape, by= "plant_id")
final$Rd<- final$Photo*-1

out_final<- final[, c("plant_id", "transmittance", "alpha_red", "alpha_blue","Rd" )]
write.csv(out_final,"rawdata/shaped_rd_transmitance.csv")

colnames(final)

rd_lab<- expression(paste("Leaf respiration (",mu,"mol ",m^-2, s^-1,")", sep= " " ))


plto<- ggplot(final,aes(x= genotype, y= Photo*-1,color= Treatment))+ 
  geom_boxplot()+ 
  facet_rep_grid(harvest~., repeat.tick.labels = F)+ ylab(rd_lab)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")

ggsave("figures/Leaf_respiration.tiff", plto,
       height= 6.5, width= 3)

