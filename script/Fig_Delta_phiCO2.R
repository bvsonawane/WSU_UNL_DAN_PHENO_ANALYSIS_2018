rm(list=ls())

source("script/updates_arrangement.R")
photo_dat<- summaryBy(Ci.Ca+D13_dm+phi_rat~genotype+treat+harvest, data= fluro_iso,
                      FUN = c(mean, se))

##1.2 dry_delta ~ Ci.Ca
a= 4.4
s=1.8
b3=29
b4= -5.7
phi=0.20
ci_o<- 0
ci_lim<- 0.94
delta_0_20<- a+(b4+phi*(b3-s)-a)*ci_o
delta_lim_20<- a+(b4+phi*(b3-s)-a)*ci_lim
delta_0_40<- a+(b4+0.4*(b3-s)-a)*ci_o
delta_lim_40<- a+(b4+0.4*(b3-s)-a)*ci_lim

(0-a)/ (b4+phi*(b3-s)-a)
## for dry delta model line
#ddelta_0_20<- a+(b4+phi*(b3-s)-a)*ci_o
#ddelta_lim_20<- a+(b4+phi*(b3-s)-a)*ci_lim
#ddelta_0_40<- a+(b4+0.4*(b3-s)-a)*ci_o
#ddelta_lim_40<- a+(b4+0.4*(b3-s)-a)*ci_lim
#ddelta_0_60<- a+(b4+0.6*(b3-s)-a)*ci_o
#ddelta_lim_60<- a+(b4+0.6*(b3-s)-a)*ci_lim


COL <- function(df){ifelse(df$treat=="WW",  "white", ifelse(df$treat=="WS", "grey5","red1"))}
  
PCH<- function(df){ifelse(df$genotype=="E14", 21, ifelse(df$genotype=="E18", 24,ifelse(df$genotype=="G1", 22,11)))}

dev.off()
tiff(file = "figures/delta_phiPS2.tiff", units="in",  
    width=3.5, height=6.5, res=300 )
par(omi= c(0,0.1,0.4,0.15), tcl= 0.2, xpd=F,xaxs = "i", yaxs = "i", mar= c(3.5,3.5,0,0))
par(mfrow=c(2,1))

plot(D13_dm ~ Ci.Ca, fluro_iso,ylim= c(0,6), xlim= c(0,1), pch=PCH(fluro_iso),bg= alpha(COL(fluro_iso), 0.8),
     yaxt= "n",ylab="", xlab= "", cex= 1.2)
axis(2, las=2)
axis(3, labels = F)
axis(4, labels = F)
segments(ci_o,delta_0_20, x1 =ci_lim, y1 = delta_lim_20,
         col = par("fg"), lty = 1, lwd = 1.5)
segments(ci_o,delta_0_40, x1 =ci_lim, y1 = delta_lim_40,
         col = par("fg"), lty = 5, lwd = 1.5)
mtext(at= 0.25,"ɸ = 0.20", side= 1, line=-5, cex= 0.9)
mtext(at= 0.25,"ɸ = 0.40", side= 1, line=-10.5, cex=0.9)
mtext(expression(paste(Delta^13,C[leaf]," (","assuming ", delta^13,C[atm],"=-10‰",")", sep= "" )), side= 2, line= 2)
mtext(expression(paste(italic(C)[i],"/",italic(C)[a],sep= "" )), side= 1, line= 2.5)

mtext("A", at=5.6,side= 2, line= -1, las=2)
legend( 0.01,7.2,legend= c("E14","E18", "G1"),pch= c(21,24,22), horiz= T, 
        box.col= "white",border= "black", pt.cex=1.5,text.font=1, pt.lwd=1, box.lwd=-1, xpd= NA)

legend( 0.01,2,pt.bg = c("white", "grey5"),legend= c("WW","WS"),pch= c(21), horiz= F, 
        box.col= "white",border= "black", pt.cex=1.5,text.font=1, pt.lwd=1, box.lwd=-1, xpd= NA)


plot(PhiPS2~PhiCO2 , fluro_iso,xlim= c(0,0.04), ylim= c(0,0.45), pch=PCH(fluro_iso),bg= alpha(COL(fluro_iso), 0.8),
     yaxt= "n",ylab="", xlab= "", cex=1.2)
axis(2, las=2)
axis(3, labels = F)
axis(4, labels = F)
mtext(expression(paste(Phi, CO[2], sep= "" )), side= 1, line= 2.5)
mtext(expression(paste(Phi,"PSII" ,sep= "" )), side= 2, line= 3)

e14_mod<- lm( PhiPS2~PhiCO2 , data= subset(fluro_iso, genotype=="E14"))
abline(e14_mod, lty= 1)
e18_mod<- lm(PhiPS2~PhiCO2, data= subset(fluro_iso, genotype=="E18"))
abline(e18_mod, lty= 2)
g1_mod<- lm(PhiPS2~PhiCO2, data= subset(fluro_iso, genotype=="G1"))
abline(g1_mod, lty= 3)
legend(x=0.005, y= 0.4, legend = c("E14", "E18", "G1"), lty= c(1,2,3), xpd= NA, box.lwd=-2, cex= 0.8)

mtext("B", at=0.42,side= 2, line= -1, las=2)

dev.off()



fluro_iso$phi_rat<- fluro_iso$PhiCO2/fluro_iso$PhiPS2

tiff(file = "figures/delta_phiPS2_phiCO2.tiff", units="in",  
     width=4, height=4, res=300 )
par(omi= c(0,0.1,0.4,0.1), tcl= 0.2, xpd=F,xaxs = "i", yaxs = "i", mar= c(3.5,3.5,0,0))
par(mfrow=c(1,1))

plot(D13_dm ~ ps2byco2, fluro_iso,ylim= c(0,6), xlim= c(0,15), pch=PCH(fluro_iso),bg= alpha(COL(fluro_iso), 0.8),
     yaxt= "n",ylab="", xlab= "", cex= 1.2)
axis(2, las=2)
axis(3, labels = F)
axis(4, labels = F)
mtext(expression(paste(Delta^13,C[leaf]," (","assuming ", delta^13,C[atm],"=-10‰",")", sep= "" )), side= 2, line= 2)
mtext(expression(paste(Phi,"PSII/", Phi,CO[2],sep= "" )), side= 1, line= 2.5)

legend( 4,6.9,legend= c("E14","E18", "G1"),pch= c(21,24,22), horiz= T, 
        box.col= "white",border= "black", pt.cex=1.5,text.font=1, pt.lwd=1, box.lwd=-1, xpd= NA)

legend( 2,2,pt.bg = c("white", "grey5"),legend= c("WW","WS"),pch= c(21), horiz= F, 
        box.col= "white",border= "black", pt.cex=1.5,text.font=1, pt.lwd=1, box.lwd=-1, xpd= NA)

dev.off()


COL <- function(df){ifelse(df$treat=="WW",  "blue", ifelse(df$treat=="WS", "red1","black"))}


dev.off()

PCH<- function(df){ifelse(df$harvest=="T0", 21, 
                          ifelse(df$harvest=="T1", 22,ifelse(df$harvest=="T2", 24,
                                                             ifelse(df$harvest=="T3", 25,11))))}


tiff(file = "figures/delta_cic_timepoint.tiff", units="in",  
     width=7, height=3, res=300 )
par(omi= c(0,0.7,0.5,0.1), tcl= 0.2, xpd=F,xaxs = "i", yaxs = "i", mar= c(3.5,0,0,0))
par(mfrow=c(1,3), cex.axis=1.3)

plot(D13_dm ~ Ci.Ca, data=fluro_iso[fluro_iso$genotype== "E14",],ylim= c(0,6), xlim= c(0,0.9), pch=PCH(fluro_iso),bg= alpha(COL(fluro_iso), 0.8),
     yaxt= "n",ylab="", xlab= "", cex= 1.5)
title("E14", line = -1, font=1)
axis(2, las=2, cex.axis=1.5)
axis(3, labels = F)
axis(4, labels = F)
segments(ci_o,delta_0_20, x1 =ci_lim, y1 = delta_lim_20,
         col = par("fg"), lty = 1, lwd = 1.5)
segments(ci_o,delta_0_40, x1 =ci_lim, y1 = delta_lim_40,
         col = par("fg"), lty = 5, lwd = 1.5)
mtext(at= 0.6,"ɸ = 0.20", side= 1, line=-5, cex= 0.9)
mtext(at= 0.6,"ɸ = 0.40", side= 1, line=-13.5, cex=0.9)
mtext(expression(paste(Delta^13,C[leaf]," (","assuming ", delta^13,C[atm],"=-10‰",")", sep= "" )), side= 2, line= 3.5)
#mtext(expression(paste(C[i],"/",C[a],sep= "" )), side= 1, line= 2.5)
mtext("A", at=5.6,side= 2, line= -2, las=2)


plot(D13_dm ~ Ci.Ca, data=fluro_iso[fluro_iso$genotype== "E18",],ylim= c(0,6), xlim= c(0,0.9), pch=PCH(fluro_iso),bg= alpha(COL(fluro_iso), 0.8),
     yaxt= "n",ylab="", xlab= "", cex= 1.5)
title("E18", line = -1)
axis(2, las=2, labels = F)
axis(3, labels = F)
axis(4, labels = F)
segments(ci_o,delta_0_20, x1 =ci_lim, y1 = delta_lim_20,
         col = par("fg"), lty = 1, lwd = 1.5)
segments(ci_o,delta_0_40, x1 =ci_lim, y1 = delta_lim_40,
         col = par("fg"), lty = 5, lwd = 1.5)
mtext(at= 0.6,"ɸ = 0.20", side= 1, line=-5, cex= 0.9)
mtext(at= 0.6,"ɸ = 0.40", side= 1, line=-13.5, cex=0.9)
#mtext(expression(paste(Delta^13,C[leaf]," (","assuming ", delta^13,C[atm],"=-10‰",")", sep= "" )), side= 2, line= 2)
mtext(expression(paste(C[i],"/",C[a],sep= "" )), side= 1, line= 2.5)

mtext("B", at=5.6,side= 2, line= -2, las=2)
legend( -0.2,7.2,legend= c("T0","T1", "T2", "T3"),pch= c(21,22,24,25), horiz= T, 
        box.col= "white",border= "black", pt.cex=2.5,
        cex= 1.5,text.font=1, pt.lwd=1, box.lwd=-1, xpd= NA)

legend( 0.9,7.2,pt.bg = c("blue", "red"),legend= c("WW","WS"),pch= c(21),
        box.col= "white",border= "black", pt.cex=2.5,cex= 1.5,text.font=1,
        pt.lwd=1, box.lwd=-1, xpd= NA, horiz= T)


plot(D13_dm ~ Ci.Ca, data=fluro_iso[fluro_iso$genotype== "G1",],ylim= c(0,6), xlim= c(0,0.9), pch=PCH(fluro_iso),bg= alpha(COL(fluro_iso), 0.8),
     yaxt= "n",ylab="", xlab= "", cex= 1.5)
title("G1", line = -1)
axis(2, las=2, labels = F)
axis(3, labels = F)
axis(4, labels = F)
segments(ci_o,delta_0_20, x1 =ci_lim, y1 = delta_lim_20,
         col = par("fg"), lty = 1, lwd = 1.5)
segments(ci_o,delta_0_40, x1 =ci_lim, y1 = delta_lim_40,
         col = par("fg"), lty = 5, lwd = 1.5)
mtext(at= 0.6,"ɸ = 0.20", side= 1, line=-5, cex= 0.9)
mtext(at= 0.6,"ɸ = 0.40", side= 1, line=-13.5, cex=0.9)
#mtext(expression(paste(Delta^13,C[leaf]," (","assuming ", delta^13,C[atm],"=-10‰",")", sep= "" )), side= 2, line= 2)
#mtext(expression(paste(C[i],"/",C[a],sep= "" )), side= 1, line= 2.5)
mtext("C", at=5.6,side= 2, line= -2, las=2)

dev.off()


meanse_dat<- summaryBy(Photo+Cond+te+n_gperm2+VpdL+Rd+pnue+D13_dm+d13c+
       Ci.Ca+PhiCO2+PhiPS2~genotype+treat+harvest, data= fluro_iso,
                      FUN = c(mean, se), na.rm= T)

write.csv(meanse_dat, "water_output/mean_se_UNL_pheno_2018_water.csv")
