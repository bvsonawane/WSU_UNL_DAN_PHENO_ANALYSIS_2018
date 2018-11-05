rm(list=ls())
source("script/updates_arrangement.R")
dev.off()
photo_dat<- summaryBy(ps2byco2+Photo+Cond+te+n_gperm2+LeafAbs+VpdL+Rd+PhiPS2+PhiCO2+ETR+qP+Fv..Fm.+d13c+Ci.Ca~genotype+treat+harvest, data= fluro_iso,
                      FUN = c(mean, se), na.rm=T)
pd <- position_dodge(0.1) # move them .05 to the left and right

p1<- ggplot(photo_dat,aes(x=harvest, y= PhiPS2.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(0,0.45))+
  geom_point(size= 5)+  geom_errorbar(aes(ymin=PhiPS2.mean-PhiPS2.se, ymax=PhiPS2.mean+PhiPS2.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop( Phi~PSII)))+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='top')+ background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(0,0,0,0.19), "cm"), axis.text.x=element_blank())+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))



p2<- ggplot(photo_dat,aes(x=harvest, y= PhiCO2.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(0,0.045))+
  geom_point(size= 5)+  geom_errorbar(aes(ymin=PhiCO2.mean-PhiCO2.se, ymax=PhiCO2.mean+PhiCO2.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop( PhiCO[2],(mu~mol~CO[2]/mu~mol~quanta))))+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  theme(plot.margin=unit(c(-0.55,0,0,0.06), "cm"),axis.text.x=element_blank())+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))

p3<- ggplot(photo_dat,aes(x=harvest, y= ETR.mean,shape= genotype,color= treat))+ylim(0,320)+
  geom_point(size= 5)+geom_errorbar(aes(ymin=ETR.mean-ETR.se, ymax=ETR.mean+ETR.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop(ETR)))+xlab("")+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  theme(plot.margin=unit(c(-0.55,0,0,0.45), "cm"), axis.text.x=element_blank())+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))

 p4<- ggplot(photo_dat,aes(x=harvest, y= Fv..Fm..mean,shape= genotype,color= treat))+ylim(0,0.6)+
  geom_point(size= 5)+geom_errorbar(aes(ymin=Fv..Fm..mean-Fv..Fm..se, ymax=Fv..Fm..mean+Fv..Fm..se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop("Fv'/Fm'")))+xlab("")+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  theme(plot.margin=unit(c(-0.55,0,0,0.45), "cm"), axis.text.x=element_blank())+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))


p5<- ggplot(photo_dat,aes(x=harvest, y= qP.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(0,0.8))+
  geom_point(size= 5)+geom_errorbar(aes(ymin=qP.mean-qP.se, ymax=qP.mean+qP.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop(qP)))+xlab("Harvest time")+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  theme(plot.margin=unit(c(-0.55,0,0,0.45), "cm"))+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")),
        axis.text.x = element_text(margin = unit(c(t = 0.5, r = 0, b = 0, l = 0), "cm")))

dev.off()
ggsave("figures/florometry_arranges.tiff",height= 10, width= 8,
       grid_arrange_shared_legend(p1,p2,p3,p4,p5,heights = c(1,0.8,0.8,0.8,1), 
                                  ncol = 1, nrow=5, position = "top"))


d13c<- ggplot(photo_dat,aes(x=harvest, y= d13c.mean,shape= genotype,color= treat))+
 ylim(-12,-14.5)+
  geom_point(size= 5)+geom_errorbar(aes(ymin=d13c.mean-d13c.se, ymax=d13c.mean+d13c.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop(delta^13~C~("‰"))))+xlab("Harvest time")+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(-0.55,0,0,0.45), "cm"))+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")),
        axis.text.x = element_text(margin = unit(c(t = 0.5, r = 0, b = 0, l = 0), "cm")))

ggsave("figures/isotope_arranges.tiff",height= 3, width= 6,
      d13c)




 phips2byco2<- ggplot(photo_dat,aes(x=harvest, y= ps2byco2.mean,shape= genotype,color= treat))+
  ylim(0,14)+
  geom_point(size= 5)+geom_errorbar(aes(ymin=ps2byco2.mean-ps2byco2.se, ymax=ps2byco2.mean+ps2byco2.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop(Phi~"PSII/"~Phi~CO[2])))+xlab("Harvest time")+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(-0.55,0,0,0.45), "cm"))+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")),
        axis.text.x = element_text(margin = unit(c(t = 0.5, r = 0, b = 0, l = 0), "cm")))

ggsave("figures/phips2byco2.tiff",height= 3, width= 6,
       phips2byco2)


rt<- ggplot(photo_dat,aes(x=Ci.Ca.mean, y= d13c.mean,shape= harvest,color= treat))+geom_point(size=4)+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlim(0,0.31)+ylim(-12,-14.5)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(-0.55,0,0,0.45), "cm"))+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")),
        axis.text.x = element_text(margin = unit(c(t = 0.5, r = 0, b = 0, l = 0), "cm")))

ggsave("figures/cica_isotope_arranges.tiff",height= 3, width= 9,
       rt)

pi<- ggplot(photo_dat,aes(x=PhiCO2.mean, y= PhiPS2.mean,shape= harvest,color= treat))+geom_point(size=4)+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlim(0,0.037)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(-0.55,0,0,0.45), "cm"))+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")),
        axis.text.x = element_text(margin = unit(c(t = 0.5, r = 0, b = 0, l = 0), "cm")))

ggsave("figures/phi_isotope_arranges.tiff",height= 3, width= 9,
       pi)



ggplot(fluro_iso,aes(y=PhiCO2, x= PhiPS2))+geom_point(aes(colour= treat, shape= harvest),size=4)+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+stat_smooth(method = "lm")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))


ggplot(fluro_iso,aes(y=d13c, x= Ci.Ca))+geom_point(aes(colour= treat, shape= harvest),size=4)+ylim(-12, -14.5)+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)

ggplot(photo_dat,aes(x=harvest, y= LeafAbs.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(0.85,0.93))+
  geom_point(size= 5)+  geom_errorbar(aes(ymin=LeafAbs.mean-LeafAbs.se, ymax=LeafAbs.mean+LeafAbs.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop( Leaf~light~absorptance)))+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='top')+ background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(0,0,0,0.19), "cm"))+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))



