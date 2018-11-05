rm(list=ls())
source("script/updates_arrangement.R")
photo_dat<- summaryBy(Photo+Cond+te+n_gperm2+VpdL+Rd+pnue~genotype+treat+harvest, data= fluro_iso,
                      FUN = c(mean, se), na.rm= T)
pd <- position_dodge(0.1) # move them .05 to the left and right

p1<-  ggplot(photo_dat,aes(x=harvest, y= Photo.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(0,60))+
  geom_point(size= 5)+  geom_errorbar(aes(ymin=Photo.mean-Photo.se, ymax=Photo.mean+Photo.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop( Net~CO[2]~assimilation,italic(A)[net]~(mu*mol~m^-2~s^-1))))+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='top')+ background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(0,0,0,0.19), "cm"), axis.text.x=element_blank())+
 theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
          axis.ticks.length=unit(-0.15, "cm"), 
          axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))



p2<-ggplot(photo_dat,aes(x=harvest, y= Cond.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(0,0.35))+
  geom_point(size= 5)+  geom_errorbar(aes(ymin=Cond.mean-Cond.se, ymax=Cond.mean+Cond.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop("Stomatal conductance",italic(g)[s]~(mol~H[2]*O~m^-2~s^-1))))+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  theme(plot.margin=unit(c(-0.55,0,0,0.06), "cm"),axis.text.x=element_blank())+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
          axis.ticks.length=unit(-0.15, "cm"), 
          axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))

p3<-ggplot(photo_dat,aes(x=harvest, y= VpdL.mean,shape= genotype,color= treat))+ylim(0,3.4)+
  geom_point(size= 5)+geom_errorbar(aes(ymin=VpdL.mean-VpdL.se, ymax=VpdL.mean+VpdL.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop(VPD[LEAF],(kPa))))+xlab("")+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  theme(plot.margin=unit(c(-0.55,0,0,0.45), "cm"), axis.text.x=element_blank())+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
          axis.ticks.length=unit(-0.15, "cm"), 
          axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))

p4<-ggplot(photo_dat,aes(x=harvest, y= te.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0), limits =c(160, 205))+
  geom_point(size= 5)+geom_errorbar(aes(ymin=te.mean-te.se, ymax=te.mean+te.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop(TE[i],(mu*mol~CO[2]/mol~H[2]*O))))+xlab("")+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  theme(plot.margin=unit(c(-0.55,0,0,-0.02), "cm"), axis.text.x=element_blank())+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
          axis.ticks.length=unit(-0.15, "cm"), 
          axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))

rd<- ggplot(photo_dat,aes(x=harvest, y= Rd.mean,shape= genotype,color= treat))+
  scale_y_continuous(expand = c(0, 0), limits =c(0,4))+
  geom_point(size= 5)+geom_errorbar(aes(ymin=Rd.mean-Rd.se, ymax=Rd.mean+Rd.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop( Dark~respiration,italic(R)[d]~(mu*mol~m^-2~s^-1))))+xlab("Harvest time")+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='none')+ background_grid(major = "xy", minor = "none")+
  theme(strip.background = element_blank(), strip.text = element_blank())+
  theme(plot.margin=unit(c(-0.55,0,0,0.45), "cm"))+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")),
        axis.text.x = element_text(margin = unit(c(t = 0.5, r = 0, b = 0, l = 0), "cm")))


ggsave("figures/gase_exchange_arranged.tiff",height= 12, width= 8,
       grid_arrange_shared_legend(p1,p2,p3,p4,rd,heights = c(1,0.8,0.8,0.8,1), ncol = 1, nrow=5, position = "top"))

E18_ww_t3<- droplevels(subset(fluro_data, treat=="WW"& genotype=="E18"&harvest=="T3"))

E18_ws_t3<- droplevels(subset(fluro_data, treat=="WS"& genotype=="E18"&harvest=="T3"))

# Lincoln Sunrise on June 8 was about 6 AM, 
#our measurments span for E18-WW-T3, 8 AM, 10 AM and 12 noon.


ggplot(photo_dat,aes(x=harvest, y= pnue.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0))+
  geom_point(size= 5)+  geom_errorbar(aes(ymin=pnue.mean-pnue.se, ymax=pnue.mean+pnue.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop( Photosynthetic~NUE,(mu*mol~CO[2]~g^-1~N~s^-1))))+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='top')+ background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(0,0,0,0.19), "cm"))+
  theme(panel.spacing.x=unit(-0.2, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))


mer1<- lm(Photo~ genotype*harvest*treat, data= fluro_iso)



anova(mer1)

visreg(mer1)
library(phia)
testInteractions(mer1)
