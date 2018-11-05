rm(list=ls())
source("script/updates_arrangement.R")
dev.off()
photo_dat<- summaryBy(SLA_m2per_g+Photo+Cond+te+n_gperm2+c_gperm2+cbyn+
                        LeafAbs+VpdL+Rd+PhiPS2+PhiCO2+ETR+qP+Fv..Fm.+d13c+Ci.Ca~genotype+treat+harvest, data= fluro_iso,
                      FUN = c(mean, se), na.rm=T)
pd <- position_dodge(0.1)

a1<- ggplot(photo_dat,aes(x=harvest, y= SLA_m2per_g.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(0,0.06))+
  geom_point(size= 5)+  geom_errorbar(aes(ymin=SLA_m2per_g.mean-SLA_m2per_g.se, ymax=SLA_m2per_g.mean+SLA_m2per_g.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop( Specific~leaf~area,SLA~(m^2~g^-1))))+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='top')+ background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(0,0,0,0), "cm"), axis.text.x=element_blank())+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))

a2<- ggplot(photo_dat,aes(x=harvest, y= n_gperm2.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(0,1.8))+
  geom_point(size= 5)+  geom_errorbar(aes(ymin=n_gperm2.mean-n_gperm2.se, ymax=n_gperm2.mean+n_gperm2.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop( Leaf~nitrogen,N[area]~(g~m^-2))))+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlab(" ")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='top')+ background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(-0.7,0,0,0.25), "cm"), axis.text.x=element_blank())+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))

a3<- ggplot(photo_dat,aes(x=harvest, y= c_gperm2.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(0,17))+
  geom_point(size= 5)+  geom_errorbar(aes(ymin=c_gperm2.mean-c_gperm2.se, ymax=c_gperm2.mean+c_gperm2.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop( Leaf~carbon,C[area]~(g~m^-2))))+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='top')+ background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(-0.6,0,0,0.36), "cm"), axis.text.x=element_blank())+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))


a4<- ggplot(photo_dat,aes(x=harvest, y= cbyn.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(0,19))+
  geom_point(size= 5)+geom_errorbar(aes(ymin=cbyn.mean-cbyn.se, ymax=cbyn.mean+cbyn.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop('Leaf C/N')))+xlab("Harvest time")+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  theme(plot.margin=unit(c(-0.6,0,0,0.52), "cm"))+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")),
        axis.text.x = element_text(margin = unit(c(t = 0.5, r = 0, b = 0, l = 0), "cm")))

ggsave("figures/Leaf_chemistry.tiff",height= 10, width= 8,
       grid_arrange_shared_legend(a1,a2,a3,a4,heights = c(1,0.8,0.8,1),
                                  ncol = 1, nrow=4, position = "top"))


ggplot(fluro_iso,aes(x=n_gperm2, y= Photo,shape= harvest,color= treat))+
  scale_y_continuous(expand = c(0, 0), limits =c(0,65))+geom_point(size= 5)+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(-0.6,0,0,0.52), "cm"))+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")),
        axis.text.x = element_text(margin = unit(c(t = 0.5, r = 0, b = 0, l = 0), "cm")))



ggplot(photo_dat,aes(x=harvest, y= LeafAbs.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(0.87,0.94))+
  geom_point(size= 5)+  geom_errorbar(aes(ymin=LeafAbs.mean-LeafAbs.se, ymax=LeafAbs.mean+LeafAbs.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop( Light~absorptance~('%'))))+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='top')+ background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(0,0,0,0.19), "cm"), axis.text.x=element_blank())+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))

