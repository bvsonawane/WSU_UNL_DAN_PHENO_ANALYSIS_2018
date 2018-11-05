
source("functions/barplot_function.R")

require(scales)

nitro<- read.csv("rawdata/biomass_fresh_dry_nitrogen_UNL_2018.csv")


grmeans <- with(subset(nitro[nitro$treatment=="FN", ]), tapply(dry_weight_g, list(harvest, genotype),mean))
X <-barplot(grmeans,space= c(0.15,1), beside=TRUE, las=2, ylim=c(0,60))

x_cod<- c(X[,1], X[,2], X[,3])
har_lab<- c("H0","H1", "H2", "H3","H0","H1", "H2", "H3","H0","H1", "H2", "H3")


X <-apply(barplot(grmeans,space= c(0.15,1), beside=TRUE, las=2, ylim=c(0,60)), 2, mean)

###
##barplot
tiff(file = "nitrogen_figure/dry_mass.tiff", units="in",  
     width=9, height=6, res=300 )
par(omi= c(0,0.5,0.5,0.6), tcl= 0.2, xpd=F,xaxs = "i", yaxs = "i", mar= c(0,0,0,0))

par(mfrow= c(1,2))
### Row 2 Dry weight

## Full N
bar(dry_weight_g, c(harvest, genotype), dataframe= subset(nitro[nitro$treatment=="FN", ], tissue== "leaf"), space= c(0.15,1),
    ylab= "",xlab= "",half.errbar=F, box= F, whisker=0.008,legend=F, las=1, mar= c(4,2,0,0), yaxt= "n",xaxt= "n", col= "white", ylim= c(0,8.5))
axis(1, at= c(-5:38),lwd.ticks=0, labels = F)
axis(2,line=0.4, labels = T, las= 2)
text(x= x_cod,y= -0.3,labels = har_lab, xpd= NA)
text(x= X,y= -1,labels =levels(subset(nitro[nitro$treatment=="FN", ])$genotype), xpd= NA)

legend(x= 11, y= 9.5, legend = c("Leaves"), pch = 22, pt.cex=1.5, horiz= T,
      pt.bg= c("white"), box.lwd= -1, xpd= NA)
legend(x= 15, y= 9.5, legend = c( "Stem"), horiz= T,
       density=c(30),angle=45, box.lwd= -1, xpd= NA)
legend(x= 19, y= 9.5, legend = c("Roots"), pch = 22, pt.cex=1.5, horiz= T,
       pt.bg= c(alpha("grey80", 0.7)), box.lwd= -1, xpd= NA)

bar(dry_weight_g, c(harvest, genotype), dataframe= subset(nitro[nitro$treatment=="FN", ], tissue== "stem"), space= c(0.15,1),
    ylab= "",xlab= "", xaxt= "n",half.errbar=F, box= F, whisker=0.008,legend=F, las=1, mar= c(4,5,0,0), yaxt= "n", 
    add= T, col= alpha("grey5", 0.7), density=c(30))
bar(dry_weight_g, c(harvest, genotype), dataframe= subset(nitro[nitro$treatment=="FN", ], tissue== "root"), space= c(0.15,1),
    ylab= "",xlab= "",xaxt= "n",half.errbar=F, box= F, whisker=0.008,legend=F, las=1, mar= c(4,5,0,0), yaxt= "n", 
    add= T,col= alpha("grey70", 0.7))


mtext(expression(paste("Dry biomass (g)")), side= 2, line= 6)
mtext(expression(paste("A")), at= 2, side= 3, line= -2)
mtext(expression(paste("Full N")),  side= 3, line= -1)

##LuwN
bar(dry_weight_g, c(harvest, genotype), dataframe= subset(nitro[nitro$treatment=="LN", ], tissue== "leaf"), space= c(0.15,1),
    ylab= "",xlab= "", xaxt= "n",half.errbar=F, box= F, whisker=0.008,legend=F, las=1, mar= c(4,2,0,0), xaxt="n",yaxt= "n", col= "white", ylim= c(0,8.5))
text(x= x_cod,y= -0.3,labels = har_lab, xpd= NA)
text(x= X,y= -1,labels =levels(subset(nitro[nitro$treatment=="FN", ])$genotype), xpd= NA)


bar(dry_weight_g, c(harvest, genotype), dataframe= subset(nitro[nitro$treatment=="LN", ], tissue== "stem"), space= c(0.15,1),
    ylab= "", xlab= "",xaxt= "n",half.errbar=F, box= F, whisker=0.008,legend=F, las=1, mar= c(4,5,0,0), yaxt= "n", 
    add= T, col= alpha("grey5", 0.7), density=c(30))
bar(dry_weight_g, c(harvest, genotype), dataframe= subset(nitro[nitro$treatment=="LN", ], tissue== "root"), space= c(0.15,1),
    ylab= "",xlab= "",xaxt= "n",half.errbar=F, box= F, whisker=0.008,legend=F, las=1, mar= c(4,5,0,0), yaxt= "n", 
    add= T,col= alpha("grey70", 0.7))
axis(1, at= c(-5:38),lwd.ticks=0, labels = F)
axis(2,line=4, labels = F, las= 2)
axis(4,line=0.4, labels = T, las= 2)
abline(h= 0, xpd= F)
mtext(expression(paste("B")), at= 2, side= 3, line= -2)
mtext(expression(paste("Low N")),  side= 3, line= -1)
dev.off()

