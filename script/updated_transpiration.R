#Run updates_arrangement.R and then transpiration.R

require(doBy)

fluro_iso$plant_id
str(test)
meta_data<- droplevels(fluro_iso[, c("plant_id", "harvest", "genotype", "treat", "rep")])
meta_data$numb<- 33
mean_t<- summaryBy(numb~ genotype+harvest+treat+rep, data= meta_data, keep.names =T, na.rm= T)

daily_trans<- droplevels(subset(test, interval=="Daily"))
daily_trans$plant_id<- daily_trans$Snapshot.ID.Tag
join_test<- join( meta_data,daily_trans,by= "plant_id")


## generate test data
sep_l<- dlply(join_test, "plant_id")
te14<- droplevels(sep_l$"529-324-E14-3-T3-WW")
te14$harvest
sum(te14$Transpiration)

## take sum of the transpired water for each plant ID,
mean_E<- summaryBy(Transpiration~ plant_id, FUN=sum, data= join_test, keep.names =T)
mean_E<- join(mean_E, meta_data)
## make sure sum is perfect
dat_all<- join( fluro_iso,mean_E, by= "plant_id")
mean_E[mean_E$plant_id=="529-324-E14-3-T3-WW",]



## plot by each harvest and treatment to confirm obervaations.
plot_daily<- join_test[,c("plant_id","Transpiration","genotype","harvest","treat", "Date")]
str(plot_daily)
g1to<- subset(plot_daily,genotype=="G1" & harvest=="TO")

filter(plot_daily,genotype=="G1" & harvest=="TO")

#generate plot
ggplot(plot_daily[plot_daily$genotype=="G1"& plot_daily$harvest=="TO", ],
       aes(y=Transpiration, x= Date))+geom_point()+ facet_grid(~plant_id)

ggplot(plot_daily[plot_daily$genotype=="E14"& plot_daily$harvest=="TO", ],
       aes(y=Transpiration, x= Date))+geom_point()+ facet_grid(~plant_id)

ggplot(plot_daily[plot_daily$genotype=="E18"& plot_daily$harvest=="TO", ],
       aes(y=Transpiration, x= Date))+geom_point()+ facet_grid(~plant_id)


ggplot(plot_daily[plot_daily$genotype=="G1"& plot_daily$harvest=="T1", ],
       aes(y=Transpiration, x= Date, color= treat))+geom_point()+ facet_grid(plant_id~treat)

ggplot(plot_daily[plot_daily$genotype=="E14"& plot_daily$harvest=="T1", ],
       aes(y=Transpiration, x= Date, color= treat))+geom_point()+ facet_grid(plant_id~treat)

ggplot(plot_daily[plot_daily$genotype=="E18"& plot_daily$harvest=="T1", ],
       aes(y=Transpiration, x= Date, color= treat))+geom_point()+ facet_grid(plant_id~treat)


ggplot(plot_daily[plot_daily$genotype=="G1"& plot_daily$harvest=="T2", ],
       aes(y=Transpiration, x= Date, color= treat))+geom_point()+ facet_grid(plant_id~treat)

ggplot(plot_daily[plot_daily$genotype=="E14"& plot_daily$harvest=="T2", ],
       aes(y=Transpiration, x= Date, color= treat))+geom_point()+ facet_grid(plant_id~treat)

ggplot(plot_daily[plot_daily$genotype=="E18"& plot_daily$harvest=="T2", ],
       aes(y=Transpiration, x= Date, color= treat))+geom_point()+ facet_grid(plant_id~treat)

ggplot(plot_daily[plot_daily$genotype=="G1"& plot_daily$harvest=="T3", ],
       aes(y=Transpiration, x= Date, color= treat))+geom_point()+ facet_grid(plant_id~treat)

ggplot(plot_daily[plot_daily$genotype=="E14"& plot_daily$harvest=="T3", ],
       aes(y=Transpiration, x= Date, color= treat))+geom_point()+ facet_grid(plant_id~treat)

ggplot(plot_daily[plot_daily$genotype=="E18"& plot_daily$harvest=="T3", ],
       aes(y=Transpiration, x= Date, color= treat))+geom_point()+ facet_grid(plant_id~treat)

##looks like we dont have transpiration number for all plants for all the days!!

# we have all pots in TO starting from day May 26,
#while in T1, T2 and T3 some pots start from May 26 and some from June1.
# Considering drought treatment started on June 4th,
# I am remoing observation before June 1 for T1,T2 and T3 and 
#going to add cumulatative transpired water from T0 for each genotype 


To<- plot_daily[plot_daily$harvest=="TO", ]
To_sum<- summaryBy(Transpiration~plant_id+genotype+harvest+treat+rep, data= To,
                   FUN=sum, keep.names = T, na.rm= T)
## get mean values so that we can add them on
to_mean<- summaryBy(Transpiration~genotype+harvest+treat,data= To_sum,
                    FUN=c(mean, se), na.rm= T)

to_mean$E.mean<- to_mean$Transpiration.mean
to_mean$E.se<- to_mean$Transpiration.se
to_mean$before_june_E<- to_mean$Transpiration.mean
## other than To
To_than<- plot_daily[!plot_daily$harvest=="TO", ]


# remove data points before June 1
To_than_june<- To_than[!To_than$Date< "2018-06-01", ]
To_than_june_sum<- summaryBy(Transpiration~plant_id+genotype+harvest+treat+rep, 
          data= To_than_june, FUN=sum, keep.names= T, na.rm= T)
To_than_june_mean<- summaryBy(Transpiration~genotype+harvest+treat, 
                              data= To_than_june_sum,FUN=c(mean, se), na.rm= T)

#add column in To_than_june_mean data frame with mean of sum E before JUne 1
To_than_june_mean$before_june_E<-as.numeric( ifelse(To_than_june_mean$genotype=="E14",
                                                    to_mean[to_mean$genotype=="E14", ]$Transpiration.mean,
                                                    ifelse(To_than_june_mean$genotype=="E18",
                                                           to_mean[to_mean$genotype=="E18", ]$Transpiration.mean,
                                                           ifelse(To_than_june_mean$genotype=="G1",
                                                                  to_mean[to_mean$genotype=="G1", ]$Transpiration.mean, "PRB"))))


To_than_june_mean$E.mean<- To_than_june_mean$Transpiration.mean+To_than_june_mean$before_june_E
To_than_june_mean$E.se<- To_than_june_mean$Transpiration.se

E_meanse<- rbind(to_mean[, c("genotype", "harvest", "treat", "E.mean", "E.se")],
                 To_than_june_mean[, c("genotype", "harvest", "treat", "E.mean", "E.se")])



## Final Mean and error data
E.meanse<- na.omit(E_meanse)
fluro_iso$AbyE<- fluro_iso$Photo/ fluro_iso$Trans
ge_meanse<- summaryBy(AbyE+d13c+D13_dm+Trans+Ci.Ca+Photo+Cond+te+cbyn+cent_n+cent_c~genotype+harvest+treat,
                      data=fluro_iso, FUN= c(mean,se), na.rm= T)

meanse<- join(E.meanse, ge_meanse)


fluro_iso$c

ggplot(meanse, aes(x= E.mean, y=D13_dm.mean))+
  geom_point(aes(colour=treat,shape=harvest))+
  facet_grid(~genotype)+geom_smooth(method="lm", level=0.95)


PCH <- function(df){ifelse(df$treat=="WW",  21, ifelse(df$treat=="WS", 24, 2))}
COL<- function(df){ifelse(df$harvest=="TO", "white", 
                          ifelse(df$harvest=="T1","grey90",
                                 ifelse(df$harvest=="T2", "grey40","grey5")))}

tiff(filename = "figures/Delta_E.tiff",
     width = 4, height = 4.5, units = "in",
     compression = c("none"),bg = "white", res = 800)

par(omi= c(1,1,0.8,0.1), tcl= 0.2, xpd=F,xaxs = "i", yaxs = "i", mar= c(0,0,0,0))

plot(D13_dm.mean~E.mean, data= meanse, pch=PCH(meanse),
     bg= alpha(COL(meanse), 0.8),xaxt= "n",yaxt= "n",xlim= c(0, 3200), ylim= c(2,4.5),
     ylab="", xlab= "", cex= 1.5)

with( meanse, arrows(E.mean, D13_dm.mean + D13_dm.se, E.mean, 
                     D13_dm.mean - D13_dm.se,code=3,angle=90,len=0.03))

with( meanse, arrows(E.mean+E.se, D13_dm.mean, E.mean-E.se, 
                     D13_dm.mean,code=3,angle=90,len=0.03))


axis(2, las=2)
axis(3, labels = F)
axis(4, labels = F)
axis(1, las=2)



mod_e14<- lm(D13_dm.mean~E.mean, droplevels(meanse[meanse$genotype=="E14",]))
summary(mod_e14)
#abline(mod_e14)
mod_e18<- lm(D13_dm.mean~E.mean, droplevels(meanse[meanse$genotype=="E18",]))
summary(mod_e18)
#abline(mod_e18, lty= 2)

mod_g1<- lm(D13_dm.mean~E.mean, droplevels(meanse[meanse$genotype=="G1",]))
summary(mod_g1)
#abline(mod_g1, lty= 2)

legend(x= 500, y= 5.2,title="Harvest", 
       legend= c("TO", "T1", "T2", "T3"), pch=c(22),
       pt.bg= c("white", "grey90","grey40","grey5"),
       horiz=T,
       xpd= NA, box.lwd= -1, pt.cex=1.8)

legend(x= 100, y= 3, legend= c("WW", "WS"),
       pch=c(21, 24),pt.bg= c("white"), horiz=F,
       xpd= NA, box.lwd= -1, pt.cex=1.5)

mtext(expression(paste(Delta[" Leaf DM"]," (", "‰",")", sep= "" )), side= 2, line=3, cex=1.2)
mtext(expression(paste("Plant transpiration, ", italic(E)[plant]," (", "g ", H[2], "O ",plant^-1,")", sep= "" )), 
      side= 1, line=4)

dev.off()


tiff(filename = "figures/Delta_Eleaf.tiff",
     width = 4, height = 4.5, units = "in",
     compression = c("none"),bg = "white", res = 800)

par(omi= c(1,1,0.8,0.1), tcl= 0.2, xpd=F,xaxs = "i", yaxs = "i", mar= c(0,0,0,0))

plot(D13_dm.mean~Trans.mean, data= meanse, pch=PCH(meanse),
     bg= alpha(COL(meanse), 0.8),xaxt= "n",yaxt= "n",xlim= c(0,0.009), ylim= c(2,4.5),
     ylab="", xlab= "", cex= 1.5)

with( meanse, arrows(Trans.mean, D13_dm.mean + D13_dm.se, Trans.mean, 
                     D13_dm.mean - D13_dm.se,code=3,angle=90,len=0.03))

with( meanse, arrows(Trans.mean+Trans.se, D13_dm.mean, Trans.mean-Trans.se, 
                     D13_dm.mean,code=3,angle=90,len=0.03))


axis(2, las=2)
axis(3, labels = F)
axis(4, labels = F)
axis(1, las=2)



mod_e14<- lm(D13_dm.mean~Trans.mean, droplevels(meanse[meanse$genotype=="E14",]))
summary(mod_e14)
#abline(mod_e14)
mod_e18<- lm(D13_dm.mean~Trans.mean, droplevels(meanse[meanse$genotype=="E18",]))
summary(mod_e18)
#abline(mod_e18, lty= 2)

mod_g1<- lm(D13_dm.mean~Trans.mean, droplevels(meanse[meanse$genotype=="G1",]))
summary(mod_g1)
#abline(mod_g1, lty= 2)

legend(x= 0.001, y= 5.2,title="Harvest", 
       legend= c("TO", "T1", "T2", "T3"), pch=c(22),
       pt.bg= c("white", "grey90","grey40","grey5"),
       horiz=T,
       xpd= NA, box.lwd= -1, pt.cex=1.8)

legend(x= 0.0005, y= 3, legend= c("WW", "WS"),
       pch=c(21, 24),pt.bg= c("white"), horiz=F,
       xpd= NA, box.lwd= -1, pt.cex=1.5)

mtext(expression(paste(Delta[" Leaf DM"]," (", "‰",")", sep= "" )), side= 2, line=3, cex=1.2)
mtext(expression(paste("Leaf transpiration, ", italic(E)[leaf]," (", "mol ", H[2], "O ",m^-2,s^-1,")", sep= "" )), 
      side= 1, line=4)
dev.off()




pd <- position_dodge(0.1)

E_plot<- ggplot(meanse,aes(x=harvest, y= E.mean,shape= genotype,color= treat))+scale_y_continuous(expand = c(0, 0))+
  geom_point(size= 5, alpha=0.7)+  geom_errorbar(aes(ymin=E.mean-E.se, ymax=E.mean+E.se), colour="black", width=.1, position=pd)+
  labs(y =bquote(atop(Plant~transpiration,italic(E)~(g~H[2]*O~plant^-1))))+
  facet_rep_grid(.~genotype, repeat.tick.labels = F)+xlab(" ")+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  theme(legend.position='top')+ background_grid(major = "xy", minor = "none")+
  theme(plot.margin=unit(c(0,0,0,0.19), "cm"))+
  theme(panel.spacing.x=unit(0, "lines"),panel.spacing.y=unit(1, "lines"),
        axis.ticks.length=unit(-0.15, "cm"), 
        axis.text.y = element_text(margin = unit(c(t = 0, r = 0.5, b = 0, l = 0), "cm")))

ggsave("figures/Transpiration_timepoint.tiff", E_plot,height= 4, width= 8)

mer1<- lm(Photo~ genotype*harvest*treat, data= fluro_iso)




ggplot(meanse, aes(x= cbyn.mean, y=D13_dm.mean))+
  geom_point(aes(colour=treat,shape=harvest),  size= 2)+
  facet_grid(~genotype)+geom_smooth(method="lm", level=0.95)

ggplot(meanse, aes(x= cent_n.mean, y=D13_dm.mean))+
  geom_point(aes(colour=treat,shape=harvest),  size= 2)+
  facet_grid(~genotype)+geom_smooth(method="lm", level=0.95)

ggplot(meanse, aes(x= cent_c.mean, y=D13_dm.mean))+
  geom_point(aes(colour=treat,shape=harvest),  size= 2)+
  facet_grid(~genotype)+geom_smooth(method="lm", level=0.95)


ggplot(meanse, aes(x= Cond.mean, y=D13_dm.mean))+
  geom_point(aes(colour=treat,shape=harvest),  size= 2)+
  facet_grid(~genotype)+geom_smooth(method="lm", level=0.95)

ggplot(meanse, aes(x= Ci.Ca.mean, y=D13_dm.mean))+
  geom_point(aes(colour=treat,shape=harvest),  size= 2)+
  facet_grid(~genotype)+geom_smooth(method="lm", level=0.95)

ggplot(meanse, aes(x= te.mean, y=D13_dm.mean))+
  geom_point(aes(colour=treat,shape=harvest),  size= 2)+
  facet_grid(~genotype)+geom_smooth(method="lm", level=0.95)

ggplot(meanse, aes(x= AbyE.mean, y=D13_dm.mean))+
  geom_point(aes(colour=treat,shape=harvest),  size= 2)+
  facet_grid(~genotype)+geom_smooth(method="lm", level=0.95)
