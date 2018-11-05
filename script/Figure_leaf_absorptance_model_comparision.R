
source("script/updates_arrangement.R")

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
tiff(file = "figures/Leaf_absorptance_model_comparison.tiff", units="in",  
     width=7, height=6.5, res=300 )
par(omi= c(1,0.8,0.54,0.1), tcl= 0.2, xpd=F,xaxs = "i", yaxs = "i", mar= c(0,0,0,3))
par(mfrow=c(2,2))

plot(refl ~ trans, data= dat, pch=PCH(dat),bg= alpha(COL(dat), 0.8),ylim= c(0,0.1),xlim= c(0,0.05),
     yaxt= "n",ylab="", xlab= "",xaxt= "n", cex= 1.5)

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
       xpd= NA, box.lwd= -1, pt.cex=1.8)

legend(x= 0.005, y= 0.044,title="Treatment", legend= c("WW", "WS"),
       pch=c(21),pt.bg= c("white", "grey15"), horiz=F,
       xpd= NA, box.lwd= -1, pt.cex=1.5)

mtext(at= 0.005,expression(paste("A", sep= "" )), side= 3, line=-1.5)



par(mar= c(0,3.5,0,0))
plot(LeafAbs ~lic_abs, data= abs_plot, pch=21,bg= alpha("black", 0.5),
     ylim= c(0.86,0.93),xlim= c(0.86,0.93),
     yaxt= "n",ylab="", xlab= "",xaxt= "n", cex= 1.5)
mod<- lm(LeafAbs ~ lic_abs, abs_plot)
newx <- seq(min(abs_plot$lic_abs,na.rm=T),max(abs_plot$lic_abs, na.rm= T),by = 0.0005)
conf_interval <- predict(mod, newdata=data.frame(lic_abs=newx), interval="confidence",
                         level = 0.95)
#matlines(newx, conf_interval[,2:3], col = "black", lty=2)
mod_sum<- summary(mod)
abline(mod, lty= 2)
## add equation
rp <-  vector('expression',2)   # vector to create list of expression val 2 mean 2 val can be added , here i added onle one
rp[1]<- substitute(expression(paste(italic(y) == SLOPE *italic(x), INTER , sep =" ")),
                   list(SLOPE = format(mod_sum$coefficients[2,1],dig=3),
                        INTER= format(mod_sum$coefficients[1,1], dig= 3)))[2]
legend(x= 0.86, y= 0.91, legend = rp, bty = 'n', cex= 1, ncol=2)
# add 1:1 line
abline(a=0,b=1)
axis(1, labels = T)
axis(2, labels = T, las=2)
axis(3, labels = F)
axis(4, labels = F)
## axis labels
mtext(expression(paste("Predicted leaf absorptance")), 
      side= 2, line= 4.5)
mtext(expression(paste("using Sorghum model")), 
      side= 2, line= 3)
mtext(expression(paste("Predicted leaf absorptance")), 
      side= 1, line= 3)
mtext(expression(paste("using Licor's model")), 
      side= 1, line= 4.5)

mtext(at= 0.865,expression(paste("C", sep= "" )), side= 3, line=-1.5)



## Plot 2
par(mar= c(0,0,0,3))
plot(absorb ~ trans, data= dat, pch=PCH(dat),bg= alpha(COL(dat), 0.8),ylim= c(0.86,0.95),xlim= c(0,0.05),
     yaxt= "n",ylab="", xlab= "",xaxt= "n", cex= 1.5)
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
mtext(expression(paste(italic(y), "= -0.998", italic(x),"+0.934", sep= "" )), side= 3, line= -2)
mtext(expression(paste("Leaf transmittance", sep= "" )), side= 1, line=3.5)
mtext(expression(paste("Leaf absorptance", sep= "" )), side= 2, line=3.5)

mtext(expression(paste(italic(r)^2, "= 0.12",", ",italic(P),"< 0.05", sep= "" )), side= 3, line=-3)


mtext(at= 0.005,expression(paste("B", sep= "" )), side= 3, line=-1.5)
dev.off()
