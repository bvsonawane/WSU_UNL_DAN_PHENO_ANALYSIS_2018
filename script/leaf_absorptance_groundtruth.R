## This script plots the groundtrhuth data for measred
#reflectance, transmittance and absorptance values on E14 under drought.
#This measurments were done on using an integrated light sphere at WSU.
require(plyr)
require(ggplot2)
require(dplyr)
require(doBy)
require(tidyr)
require(smatr)
dat<- read.csv("rawdata/absoptance_sorghum_groundthrough.csv")
COL <- function(df){ifelse(df$treat=="WW",  "white", ifelse(df$treat=="WS", "grey15","black"))}
PCH<- function(df){ifelse(df$harvest=="h1", 21, ifelse(df$harvest=="h2", 24,ifelse(df$harvest=="h3", 22,11)))}

dev.off()
tiff(file = "figures/absorptance_ground_truth.tiff", units="in",  
     width=4, height=6.5, res=300 )
par(omi= c(1,1,0.53,0.1), tcl= 0.2, xpd=F,xaxs = "i", yaxs = "i", mar= c(0,0,0,0))
par(mfrow=c(2,1))

plot(refl ~ trans, data= dat, pch=PCH(dat),bg= alpha(COL(dat), 0.8),ylim= c(0,0.1),xlim= c(0,0.05),
     yaxt= "n",ylab="", xlab= "",xaxt= "n", cex= 1.2)

m1<- lm(refl~trans, dat)
#abline(m1)
summary(m1)
mtext(expression(paste("Leaf reflectance", sep= "" )), side= 2, line=3.5)
axis(2, las=2)
axis(3, labels = F)
axis(4, labels = F)
axis(1, labels = F)
#add legends
legend(x= 0.012, y= 0.1245,title="Harvest", legend= c("H1", "H2", "H3"), pch=c(21,24,22), horiz=T,
       xpd= NA, box.lwd= -1, pt.cex=1.5)

legend(x= 0.005, y= 0.044,title="Treatment", legend= c("WW", "WS"),
       pch=c(21),pt.bg= c("white", "grey15"), horiz=F,
       xpd= NA, box.lwd= -1, pt.cex=1.5)

mtext(at= 0.005,expression(paste("A", sep= "" )), side= 3, line=-1.5)

## Plot 2
plot(absorb ~ trans, data= dat, pch=PCH(dat),bg= alpha(COL(dat), 0.8),ylim= c(0.86,0.95),xlim= c(0,0.05),
     yaxt= "n",ylab="", xlab= "",xaxt= "n", cex= 1.2)
m2<- lm(absorb~trans, dat)
abline(m2)
##add confidence interval
xval <- seq(0, 0.05, by=0.05)
conf_interval <- predict(m2, newdata=data.frame(trans=xval), interval="confidence",
                         level = 0.95)
#lines(xval, conf_interval[,2], col="blue", lty=2)
#lines(xval, conf_interval[,3], col="blue", lty=2)
## decided to not ad CI
summary(m2)
axis(1, las=2)
axis(2, las=2)
axis(3, labels = F)
axis(4, labels = F)
mtext(expression(paste("y = -0.998x + 0.934", sep= "" )), side= 3, line= -2)
mtext(expression(paste("Leaf transmittance", sep= "" )), side= 1, line=3.5)
mtext(expression(paste("Leaf absorptance", sep= "" )), side= 2, line=3.5)

mtext(expression(paste(italic(r)^2, "= 0.12",", ",italic(P),"< 0.05", sep= "" )), side= 3, line=-3)

mtext(at= 0.005,expression(paste("B", sep= "" )), side= 3, line=-1.5)
dev.off()



