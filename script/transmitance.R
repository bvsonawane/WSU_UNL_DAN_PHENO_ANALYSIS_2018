rm(list=ls())
require(plyr)
require(ggplot2)
require(dplyr)
require(doBy)
require(tidyr)
require(scales)
source("functions/multiplot.R")
source("functions/ggplot_theme.R")
library(lemon)
require(ggpubr)
library(cowplot)

## Note that calculations made for leaf absorptance in data frame were done suing
#Licor's Technical Note #128

#αleaf (red) = 0.9407 - 0.8836*Transmitance 
#αleaf (blue) = 0.9522 - 0.2546*Transmitance

data<- read.csv("rawdata/transmitance_data.csv")
data$Treatment<- factor(data$Treatment, levels = c("WW","WS"))

p1<- ggplot(data,aes(x= genotype, y= transmittance*100,color= Treatment))+ 
  geom_boxplot()+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+ ylab("Light transmittance (%)")+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())
  


p2<- ggplot(data,aes(x= genotype, y= alpha_red*100,color= Treatment))+ 
  geom_boxplot()+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+ ylab("Light absorbtance (Red light; %)")+xlab("Genotype")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
 theme(legend.position='none')+
  background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())
p3<-  ggplot(data,aes(x= genotype, y= alpha_blue*100,color= Treatment))+ 
  geom_boxplot()+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+ ylab("Light absorbtance (Blue light; %)")+xlab("")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position=c(-1,-0.067), legend.direction = "horizontal")+
  theme(legend.title=element_blank())+
  background_grid(major = "xy", minor = "none")
 



ggsave("figures/Leaf_transmittance.tiff",
       ggarrange(p1,p2,p3 ,  labels = c("A", "B", "C"),ncol = 3,
                 widths=c(0.8,0.8,0.9)),height= 6.5, width= 8)
