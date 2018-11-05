rm(list=ls())
require(plyr)
require(ggplot2)
require(dplyr)
require(doBy)
require(tidyr)
#require(scales)
source("functions/multiplot.R")
source("functions/ggplot_theme.R")
library(cowplot)
library(lemon)
require(ggpubr)

se<- function(x, ...) {
  se <- sd(x, ...)/sqrt(sum(!is.na(x)))
  return(se)
}

data1<- read.csv("rawdata/fluro_data/fluro_data1.csv")
data2<- read.csv("rawdata/fluro_data/fluro_data2.csv")
fluro_data<- rbind.fill(data1, data2)
fluro_data$treat<- factor(fluro_data$treat, levels = c("WW","WS"))
fluro_data$harvest<-as.factor(ifelse(fluro_data$date== "6032018", "TO",ifelse(fluro_data$date== "6062018", "T1",
   ifelse(fluro_data$date== "6072018", "T1",ifelse(fluro_data$date== "6082018", "T2",
       ifelse(fluro_data$date== "6102018", "T3", "proble"))))))
fluro<- summaryBy(.~plant_id+genotype+treat+rep+date+harvest, fluro_data, keep.names = T, na.rm= T)
fluro$harvest<- factor(fluro$harvest,levels(fluro$harvest)[c(4,1:3)])
### Isotope data
iso<- read.csv("rawdata/isotope_dat.csv")
fluro_iso<- join(fluro, iso, by="plant_id", type="inner")
fluro_iso$D13_dm<- ((-8)- (fluro_iso$d13c))/(1+fluro_iso$d13c/1000)
## Double check this one ALL
plant_ud<- fluro_iso[, c("plant_id", "raw_plant_id","harvest")]

t3<- subset(plant_ud, plant_ud$harvest=="T3")

cond_lab<- expression(paste("Stomatal conductance, ", italic(g)[s],"(","mol ",H[2], "O ",m^-2, s^-1,")", sep= " " ))
photo_lab<- expression(paste("Net C", O[2], " assimilation, ",italic(A),"(", mu,"mol ",m^-2, s^-1,")", sep= " " ))
te_lab<- expression(paste("Transpiration efficiency, ", TE[i]," (", mu,"mol ",CO[2], mol^-1, H[2],"O",")", sep= " " ))
trans_lab<- expression(paste("Leaf traspiration, ", italic(E)," (","mmol ", H[2], "O ",m^-2, s^-1,")", sep= " " ))



photo<- ggplot(fluro,aes(x= genotype, y= Photo,color= treat))+ 
geom_boxplot()+ ylab(photo_lab)+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+xlab(" ")+
scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())

cond<- ggplot(fluro,aes(x= genotype, y= Cond,color= treat))+ 
  geom_boxplot()+  ylab(cond_lab)+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())

te<- ggplot(fluro,aes(x= genotype, y= Photo/Cond,color= treat))+ 
  geom_boxplot()+ ylab(te_lab)+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())

trans<- ggplot(fluro,aes(x= genotype, y= Trmmol,color= treat))+ 
  geom_boxplot()+ ylab(trans_lab)+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")


ggarrange(photo, cond, te, trans ,  labels = c("A", "B", "C", "D"),ncol = 4,
          widths=c(0.8,0.8,0.8,0.9))


ggsave("figures/gase_exchange.tiff", 
       ggarrange(photo, cond, te, trans ,  labels = c("A", "B", "C", "D"),ncol = 4,
                 widths=c(0.8,0.8,0.8,0.9)),height= 6, width= 10)




pCO<- ggplot(fluro,aes(x= genotype, y= PhiCO2,color= treat))+ 
  geom_boxplot()+ ylab(expression(paste("Phi ", CO[2], sep= "")))+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())



pps<- ggplot(fluro,aes(x= genotype, y= PhiPS2,color= treat))+ 
  geom_boxplot()+ ylab(expression("Phi PSII"))+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())



etr<- ggplot(fluro,aes(x= genotype, y= ETR,color= treat))+ 
  geom_boxplot()+ ylab(expression("ETR"))+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())

qp<- ggplot(fluro,aes(x= genotype, y= qP,color= treat))+ 
  geom_boxplot()+ ylab(expression("qN"))+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())

nP<- ggplot(fluro,aes(x= genotype, y= qN,color= treat))+ 
  geom_boxplot()+ ylab(expression("NPQ"))+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")

ggsave("figures/fluorometry_gase_exchange.tiff", 
       ggarrange(pCO, pps, etr ,qp, nP, labels = c("A", "B", "C", "D", "E"),ncol = 5,
                 widths=c(0.8,0.8,0.8,0.8,0.9)),height= 6, width= 13)





ggplot(fluro,aes(x= genotype, y= Fv./Fm.,color= treat))+ 
  geom_boxplot()+ ylab(expression("F'/Fm'"))+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")




ggplot(fluro,aes(y= Photo, x= Ci.Ca,color= treat))+ geom_point(aes(shape= genotype), size= 3)



rel<- ggplot(fluro,aes(y= Photo, x= Cond, fill= treat))+ 
  geom_point(aes(shape= genotype),size= 4,pch= 21, color= "black")+
  ylab(photo_lab)+xlab(cond_lab)+ 
  theme(legend.position = c(0.8, 0.2), legend.title = element_blank())+
  scale_fill_manual('Treatment', values = alpha(c('WW' = 'blue', 'WS' = 'red'), 0.7))

ggsave("figures/Photo-cond.tiff",rel, height= 4.5, width= 4.5, dpi= 300)



rel2<-ggplot(fluro,aes(y= Photo, x= Ci.Ca))+
  geom_point(size= 4, aes(shape= genotype, fill= treat))+
  scale_shape_manual(values = c(21,22,24))+
  ylab(photo_lab)+xlab(expression(C[i]/C[a]))+ 
  theme(legend.position = c(0.15, 0.3), legend.title = element_blank())+
  scale_fill_manual('Treatment', values = alpha(c('WW' = 'blue', 'WS' = 'red'), 0.7))+
  facet_rep_grid(~harvest)+
  guides(fill = guide_legend(override.aes=list(shape=21)))+
  background_grid(major = "xy", minor = "none")


ggsave("figures/Photo-_Ci.Ca.tiff",rel2, height= 3.5, width= 9, dpi= 300)


## Isotope analysis

iso_ci<- ggplot(fluro_iso,aes(x= Ci.Ca, y= d13c,color= genotype, shape= treat))+ 
  geom_point(size= 4)+ ylim(-12, -14.5)+xlim(0,0.5)+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+guides(fill=guide_legend(nrow = 2))+
  theme(legend.direction ="vertical",legend.position = "top")
ggsave("figures/isotope_gase_exchange.tiff", 
       ggarrange(iso_ci,widths=c(0.8,0.8,0.8,0.8,0.9)),height= 8, width= 3.5)


phi_co2<- ggplot(fluro_iso,aes(y= d13c, x= PhiCO2))+
  geom_point(aes(color= genotype, shape= harvest),size= 4)+
  ylim(-12, -14.5)+theme(legend.direction ="vertical",legend.position="top")+
  geom_smooth(method = "lm", color= "black")

etr<- ggplot(fluro_iso,aes(y= d13c, x= ETR,color= genotype, shape= harvest))+
  geom_point(size= 4)+ ylim(-12, -14.5)+theme(legend.position="none")

ggsave("figures/phoco2_gase_exchange.tiff",plot_grid( phi_co2, etr,nrow = 2, 
 rel_heights = c(0.6,0.4),labels = c("A", "B")),height= 10, width= 5)



summarySE
require(doBy)

cond_lab<- expression(paste("Stomatal conductance, ", italic(g)[s],"\(","mol ",H[2], "O ",m^-2, s^-1,")", sep= " " ))
photo_lab<- expression(paste("Net C", O[2], " assimilation, ",italic(A),"(", mu,"mol ",m^-2, s^-1,")", sep= " " ))
te_lab<- expression(paste("Transpiration efficiency, ", TE[i]," (", mu,"mol ",CO[2], mol^-1, H[2],"O",")", sep= " " ))
trans_lab<- expression(paste("Leaf traspiration, ", italic(E)," (","mmol ", H[2], "O ",m^-2, s^-1,")", sep= " " ))


###################################################################

nitro<- ggplot(photo_dat,aes(x=harvest, y= n_gperm2.mean,shape= genotype,color= treat))+ 
  geom_point(size= 5)+geom_errorbar(aes(ymin=n_gperm2.mean-n_gperm2.se, ymax=n_gperm2.mean+n_gperm2.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop( Leaf~Nitrogen,N[area]~(g~m^-2))))+xlab("Harvest time")+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  theme(plot.margin=unit(c(-1,0,0,0.081), "cm"))



dev.off()

ggplot(fluro_iso,aes(x= cent_n, y= d13c,color= treat))+ 
  geom_point()


ggplot(fluro_iso,aes(x= Ci.Ca, y= d13c,color= treat))+ 
  geom_point()+ ylab("d13C")+xlab("Ci/Ca")+
  facet_rep_grid(harvest~., repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())


ggplot(fluro_iso,aes(x= Ci.Ca, y= d13c,color= treat))+ 
  geom_point(size= 4)+ ylim(-12, -14.5)+xlim(0,0.5)


ggplot(fluro_iso,aes(x= Ci.Ca, y= d13c,color= genotype, shape= treat))+ 
  geom_point(size= 4)+ ylim(-12, -14.5)+xlim(0,0.5)+
  facet_rep_grid(harvest~., repeat.tick.labels = F)

ggplot(fluro_iso,aes(x= Ci.Ca/Photo, y= d13c,color= genotype, shape= treat))+ 
  geom_point(size= 4)+ ylim(-12, -14.5)+
  facet_rep_grid(harvest~., repeat.tick.labels = F)

ggplot(fluro_iso,aes(x= Ci.Ca/Cond, y= d13c,color= genotype, shape= treat))+ 
  geom_point(size= 4)+ ylim(-12, -14.5)+
  facet_rep_grid(harvest~., repeat.tick.labels = F)

ggplot(fluro_iso,aes(x= cent_n, y= d13c,color= genotype))+
geom_point(size= 4)+ ylim(-12, -14.5)

ggplot(fluro_iso,aes(x= Photo, y= d13c,color= genotype))+
  geom_point(size= 4)+ ylim(-12, -14.5)

ggplot(fluro_iso,aes(x= cent_n, y= Photo,color= genotype, shape= harvest))+
  geom_point(size= 4)

ggplot(fluro_iso,aes(x= cent_n, y=Photo,color= genotype, shape =treat))+ 
  geom_point(size= 4)+ ylab("d13C")+xlab("leaf N (%)")+ ylim(-12, -14.5)

ggplot(fluro_iso,aes(x= Cond, y= Photo,color= genotype, shape= harvest))+
  geom_point(size= 4)

ggplot(fluro_iso,aes(y= d13c, x= PhiCO2,color= genotype, shape= harvest))+
  geom_point(size= 4)+ ylim(-12, -14.5)

ggplot(fluro_iso,aes(y= d13c, x= ETR,color= genotype, shape= harvest))+
  geom_point(size= 4)+ ylim(-12, -14.5)
mod<- lm(PhiCO2~ d13c, fluro_iso)


summary(mod)
require(visreg)

visreg(mod)
require(smatr)


sm<- sma(PhiCO2~ d13c*treat, fluro_iso)
plot(sm)

sg<- sma(PhiCO2~ d13c*genotype, fluro_iso)
plot(sg)

sh<- sma(PhiCO2~ d13c*harvest, fluro_iso)
plot(sh)
