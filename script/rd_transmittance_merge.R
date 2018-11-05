#This script added absorptence estimated using inhouse absorptance model 

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

## Note that calculations made for leaf absorptance in data frame were done suing
#Licor's Technical Note #128

#αleaf (red) = 0.9407 - 0.8836*Transmitance 
#αleaf (blue) = 0.9522 - 0.2546*Transmitance
data<- read.csv("rawdata/transmitance_data.csv")
data$Treatment<- factor(data$Treatment, levels = c("WW","WS"))
trans_data<- droplevels(data[, c("plant_id","alpha_red", "alpha_blue", "transmittance")])

## pred_absroptance using ground truth data; from leaf_absorptance_groundtruth.R script model.
trans_data$pred_absroptance<- (-0.998)*trans_data$transmittance+0.934


ggplot(trans_data, aes(x=(pred_absroptance), fill= "red")) + geom_density()

data1<- read.csv("rawdata/rd_data/rd_data1.csv")
data2<- read.csv("rawdata/rd_data/rd_data2.csv")
rd_dat<- rbind.fill(data1,data2)
rd<- droplevels(rd_dat[, c("plant_id", "Photo")])
trans_rd<- summaryBy(.~plant_id, join(trans_data, rd))


floro_1<- read.csv("rawdata/fluro_data/fluro_data1.csv")
fl_1<- droplevels(floro_1[, c("plant_id", "Photo")])
fl1_flr_rd<- join(fl_1, trans_rd,  by= "plant_id", type= "inner")


floro_2<- read.csv("rawdata/fluro_data/fluro_data2.csv")
fl_2<- droplevels(floro_2[, c("plant_id", "Photo")])
fl2_flr_rd<- join(fl_2, trans_rd, type= "inner", by= "plant_id")


write.csv(fl1_flr_rd, "rawdata/fluro_data/flr1_flr_rd.csv")
write.csv(fl2_flr_rd, "rawdata/fluro_data/flr2_flr_rd.csv")
