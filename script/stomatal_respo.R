rm(list=ls())
require(plyr)
require(ggplot2)
require(dplyr)
require(doBy)
require(tidyr)
require(scales)

file1<- read.csv("rawdata/stomata_response/stomata_respo_t3_file1.csv")
file2<- read.csv("rawdata/stomata_response/stomata_respo_t3_file2.csv")
data1<- rbind.fill(file1, file2)
data2<- data1[!is.na(data1$genotype), ]



add_ref<-function(x) 
{x$ref<- 1:nrow(x)
return(x)}
##Finction to normalise conductance
dat_ref<-ddply(data2, c("genotype", "rep", "treat", "licor"), .fun = add_ref)


## Normalise Conductance
cond_norm<-function(x) 
{ t<- x$Cond
tt<- as.data.frame(t)
x$cond_norm<- t/tt[1,]
return(x)
}

dat_cond<-ddply(dat_ref, c("genotype", "rep", "treat", "licor"), .fun = cond_norm)

dat_cond[[9]]$cond_norm

ggplot(dat_ref,aes(x= ref, y= Cond))+ geom_point(aes(color= genotype), size= 4, alpha=0.6)+
  facet_grid(~treat)


ggplot(dat_cond,aes(x= ref, y= cond_norm))+
 geom_point(aes(color= treat), size= 3, alpha=0.8)+facet_grid(~genotype)+
  ylab("Normalised stomatal conductance")+  xlab("Time (Min after darkeness)")

