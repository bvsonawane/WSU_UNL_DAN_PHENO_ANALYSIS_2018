## This script is making final version of dataset using updated absorptance model.(manulaly updated in excel and then CSV file)
## Removing drought stress GE measuremtns on day first for T1 harvest,
#because plants on day 1 for T1 harvest were not at targeted stress values

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
require(lme4)
require(visreg)
require(phia)


#STANDARTD ERROR FUNCTION
se<- function(x, ...) {
  se <- sd(x, ...)/sqrt(sum(!is.na(x)))
  return(se)
}

## Read data files 
data1<- read.csv("rawdata/fluro_data/fluro_data1_updated_trans.csv")
data2<- read.csv("rawdata/fluro_data/fluro_data2_updated_trans.csv")
fluro_data<- rbind.fill(data1, data2)
## Order treatment in  WW and WS sequence
fluro_data$treat<- factor(fluro_data$treat, levels = c("WW","WS"))

## Add harvest time based on measurment date.
fluro_data$harvest<-as.factor(ifelse(fluro_data$date== "6032018", "TO",
  ifelse(fluro_data$date== "6062018", "T1",
  ifelse(fluro_data$date== "6072018", "T1",ifelse(fluro_data$date== "6082018", "T2",
  ifelse(fluro_data$date== "6102018", "T3", "problem"))))))

## We know that, in T1 harvest,we dont want measurments conducted for WS 
#on day "6062018". Therefore, remove data for WS on 6062018 and dont consider for
#future analyses.

test<- droplevels(subset(fluro_data, fluro_data$date=="6062018" & fluro_data$treat=="WS"))
## looks like there are 10 observations needed to be removed.
#original fluro_data has 94 observations now they should be 84
fluro_data<- droplevels(subset(fluro_data, !(fluro_data$date=="6062018" & fluro_data$treat=="WS")))
#they are 84!

##we missed some abosrbent measurments, 
#substitute them by using mean value for respective treatment group.

#_____________________________________________________________________________________
# Find missing absorbent numbers in data $pred_absroptance.mean
sum(is.na(fluro_data$pred_absroptance.mean)) ## three observation missing
missing<- which(is.na(fluro_data$pred_absroptance.mean))## rownumbers for those observation
fluro_data[missing,] ## becuase we updated mannualy missing values in future are not showing up
#Following plant id need to be updated 
#T1 WWS
#"529-463-E14-4-T1-WW" 
#"529-465-E18-8-T1-WW"
#T3 WW
#"529-318-E18-6T3-WW"   
#Find mean values to be inserted
abs_mean<- summaryBy(pred_absroptance.mean~genotype+treat+harvest, fluro_data, keep.names = T, na.rm= T)
#T1 WWS
#"529-463-E14-4-T1-WW" == 0.9013121
#"529-465-E18-8-T1-WW" ==0.9025604
#T3 WW
#"529-318-E18-6T3-WW" == 0.8854193

## I prefere to update this number namully in crude row data (excel files) 
#to avoid any computational errors : Updated mannualy
#________________________________________________________________________________________

fluro<- summaryBy(.~plant_id+genotype+treat+rep+date+harvest, fluro_data, keep.names = T, na.rm= T)
fluro$harvest<- factor(fluro$harvest,levels(fluro$harvest)[c(4,1:3)])
### Isotope data
iso<- read.csv("rawdata/isotope_dat.csv")
fluro_iso<- join(fluro, iso, by="plant_id")

fluro_iso$D13_dm<- ((-10)- (fluro_iso$d13c))/(1+fluro_iso$d13c/1000)
fluro_iso$te<- fluro_iso$Photo/fluro_iso$Cond
fluro_iso$pnue<- fluro_iso$Photo/fluro_iso$n_gperm2
fluro_iso$phi_rat<- fluro_iso$PhiCO2/fluro_iso$PhiPS2
fluro_iso$ps2byco2<- fluro_iso$PhiPS2/fluro_iso$PhiCO2



## which ratio we exactly looking for BAlA??
boxplot(fluro_iso$PhiPS2/fluro_iso$PhiCO2)
boxplot(fluro_iso$PhiPS2)
boxplot(fluro_iso$PhiCO2)
summary(fluro_data$Rd)

## Demonstrate difference in absorptence number predicted from Licor's model and in-house model.
#read data files where licor model was used to predict blue and red ansrptence and then calculated total absroptence.
data_L1<- read.csv("rawdata/fluro_data/fluro_data1.csv")
data_L2<- read.csv("rawdata/fluro_data/fluro_data2.csv")
fluro_data_L<- rbind.fill(data_L1, data_L2)
##rename absroptance number
fluro_data_L$lic_abs<- fluro_data_L$LeafAbs
#subset important data
fluro_data_L<- fluro_data_L[, c("plant_id","lic_abs" )]
## updated dataframe
fluro_data_updated<- fluro_data[, c("plant_id","LeafAbs" )]
abs_plot<- join(fluro_data_updated,fluro_data_L, by= "plant_id")
plot(LeafAbs ~ lic_abs, abs_plot)
mod<- lm(LeafAbs ~ lic_abs, abs_plot)
summary(mod)
require(visreg)
visreg(mod, xlab= "", ylab= expression(paste("Leaf absorptence predi")))

## Plot
dev.off()
tiff(file = "figures/absorptance_comparison_with_licor.tiff", units="in",  
     width=5, height=5, res=300 )
par(omi= c(1,1,0.1,0.15), tcl= 0.2, xpd=F,xaxs = "i", yaxs = "i", mar= c(0,0,0,0))
plot(LeafAbs ~lic_abs, data= abs_plot, pch=21,bg= alpha("black", 0.5),
     ylim= c(0.86,0.93),xlim= c(0.86,0.93),
     yaxt= "n",ylab="", xlab= "",xaxt= "n")

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
legend("top", legend = rp, bty = 'n', cex= 1, ncol=2)
# add 1:1 line
abline(a=0,b=1)
axis(1, labels = T)
axis(2, labels = T, las=2)
axis(3, labels = F)
axis(4, labels = F)
## axis labels
mtext(expression(paste("Predicted leaf absorptance using Sorghum model")), 
                 side= 2, line= 3)
mtext(expression(paste("Predicted leaf absorptance using Licor's model")), 
      side= 1, line= 3)

dev.off()


