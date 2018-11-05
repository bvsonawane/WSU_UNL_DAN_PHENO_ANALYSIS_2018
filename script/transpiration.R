rm(list=ls())
require(plyr)
require(ggplot2)
require(dplyr)
require(doBy)
require(tidyr)
require(scales)
#install.packages("lemon")
library(lemon)
source("functions/multiplot.R")
#source("functions/ggplot_theme.R")
library(cowplot)
require(lubridate)
require(date)
data<- read.csv("rawdata/drought_watering_data.csv")
date<- as.character(data$date)
time<- as.character(data$time)
data$datetime<- paste(date, time)

##As per communication some pots were removed  on June 9th howevere water was added
## remove those data points
data<-data[ !data$Weight.Before < 2300,] ## there were 7 pot with error
data<- subset(data, !Snapshot.ID.Tag =="0-WW")## wiered pot

#data$date_time<- strptime(data$datetime,  "%m/%d/%Y  %H:%M:%S")
#data$Date<- as.Date(data$date_time)


##Function to calculate day and night time transpiration
#This function will apply on each plant is separately
cal_E<- function(plant){
#samp<- plant[, c("Snapshot.ID.Tag", "datetime", "Water.Amount","Weight.After",
 #         "Weight.Before")]
#Collect useful varables
samp= plant
samp$date_time<- strptime(samp$datetime,  "%m/%d/%Y  %H:%M:%S")# datetime object
samp$date_time <- as.POSIXct(samp$date_time, tz = Sys.timezone()) 
samp$delta<- samp$Weight.After-samp$Weight.Before#to test water apply and delta
#Split data for weight after water
dat_after<- within(samp, rm("Weight.Before"))
dat_after_min<- dat_after[-nrow(dat_after), ]# last row can be removed
#Split data for weight before water
dat_before<- within(samp, rm("Weight.After"))
dat_before$before_datetime<- dat_before$date_time
dat_before_min<- dat_before[-1,]# # first row can be removed
dat_before_min<- dat_before_min[, c("Weight.Before","before_datetime" )]
sub_dat<- cbind(dat_after_min, dat_before_min) # join dataframe
sub_dat$raw_trans<- sub_dat$Weight.After- sub_dat$Weight.Before
sub_dat$interval<- as.factor(as.character(ifelse(hour(sub_dat$before_datetime)< 10, "Night", "Day")))
return(sub_dat)
}


## Find empty pots and account evapotrasnpiration diurnaly
empty_pots<- droplevels(data[grep("Empty", data$Snapshot.ID.Tag), ])
## calculate transpiration for individual empty pots
empty_trans <- ddply(empty_pots, "Snapshot.ID.Tag", .fun= cal_E , .inform = T)
#add date column
empty_trans$Date<- as.Date(empty_trans$date_time)

ggplot( empty_trans, aes(x= date_time, y= raw_trans, color= treat))+geom_point()+ylim(0,250)+
  facet_wrap(~Snapshot.ID.Tag, ncol= 3)

mean_empty_trans<- summaryBy(.~interval+Date+treat, empty_trans, keep.names = T, na.rm=T)

empty_p1<-ggplot(empty_trans, aes(x= date_time, y= raw_trans))+
  geom_point(size=2,aes(color= treat))+facet_grid(~interval)+
  ylab(expression(paste("Evapotranspiration ( mL  ", pot^-1,")", sep= "")))+
  xlab(" ")+theme(legend.title = element_blank(), legend.position = c(0.7, 0.8))
empty_p1

empty_p1<-ggplot( mean_empty_trans, aes(x= Date, y= raw_trans))+
geom_point(size=2,aes(color= treat))+facet_grid(~interval)+
ylab(expression(paste("Mean evapotranspiration ( mL  ", pot^-1,")", sep= "")))+
  xlab("Time (days)")+theme(legend.position ="none")
empty_p1
## Plot for empty pot evapotranspiration
ggsave("figures/evapotranspiration_empty_pot.tiff", multiplot(empty_p1,empty_p2),
       height = 7, width = 5)

## Note that till June 2 all measurments are for daily E and considered in night
#therefore there are no data points in the day transpiration.

summary(empty_pots$Snapshot.ID.Tag)
summary(levels(empty_pots$Snapshot.ID.Tag))## there are fourteen empty pots
## calculate transpiration for all data points.
trans_all <- ddply(data, "Snapshot.ID.Tag", .fun= cal_E , .inform = T)
trans_all$Date<- as.Date(trans_all$date_time)


##remove all the empty pots from dataframe
trans_plant<- droplevels(trans_all[!grepl("Empty", trans_all$Snapshot.ID.Tag), ])

## add real harvest time point, this is important because our harvest
#time from barcode is different than that of real one
plant_To<- droplevels(subset(trans_plant, Snapshot.ID.Tag =="529-542-G1-5-T0-WW"))
plant_T1<- droplevels(subset(trans_plant, Snapshot.ID.Tag =="529-359-E18-10-T2-WS"))
plant_T3<- droplevels(subset(trans_plant, Snapshot.ID.Tag =="529-312-G1-9-T3-WS"))

### Function to calculate correct trasnpiration by sunstracting empty pot evapotranspiration
corr_trans<- function(x){
  mean_empty_trans$trans_empty<- mean_empty_trans$raw_trans
  ww_mean_empty_trans<- droplevels(mean_empty_trans[mean_empty_trans$treat=="WW", ])
  ws_mean_empty_trans<- droplevels(mean_empty_trans[mean_empty_trans$treat=="WS", ])
  ww_x<- droplevels(x[x$treat=="WW", ])
  ws_x<- droplevels(x[x$treat=="WS", ])
  ww_dat<- join(ww_x, ww_mean_empty_trans[, c("interval", "Date", "trans_empty", "treat")], type= "inner", by= c("interval", "Date", "treat")) 
  ws_dat<- join(ws_x, ws_mean_empty_trans[, c("interval", "Date", "trans_empty", "treat")], type= "inner", by= c("interval", "Date", "treat")) 
  dat<- rbind(ww_dat, ws_dat)
  dat$Transpiration<- dat$raw_trans-dat$trans_empty
  return(dat)
}

##test on WW ans WS plant
plant_To<- droplevels(subset(trans_plant, Snapshot.ID.Tag =="529-352-G1-7-T3-WW"))
plant_ws<- droplevels(subset(trans_plant, Snapshot.ID.Tag =="529-351-E14-12-T3-WS"))
# great working


#Run script on all data
final_trans<- corr_trans(trans_plant)


ggplot( final_trans, aes(x= date_time, y= Transpiration, color= treat))+geom_point()+ylim(0,500)+
  facet_grid(interval~genotype)+geom_smooth(span = 0.8)
## in above plot looks like,
#1. First day measurment doesnt make sense.
#2. Night time measurments start on June 2
#3. before June 2, transpiration in totdal day transpiration.

## Now add label with "daily", 'day", "night", transpiration


final_trans[final_trans$Snapshot.ID.Tag =="529-324-E14-3-T3-WW",]

# Collect data with diurnal measurments
Date1<-as.Date("2018-05-25")  
Date2<-as.Date("2018-06-02")
daily<-droplevels(final_trans[(final_trans$Date < Date2 & final_trans$Date> Date1), ])
daily$interval<- "Daily"

diurnal<-droplevels(final_trans[!(final_trans$Date < Date1|final_trans$Date < Date2), ])

diurnal[diurnal$Date== as.Date("2018-06-09"),][, c("Transpiration", "treat")]

## Function to calculate daily trasnpiration form day and night measurments
merge_daily<- function(x){
plant_d<- droplevels(x[x$interval== "Day",])
plant_d$corr_date<- plant_d$Date
plant_n<- droplevels(x[x$interval== "Night",])
plant_d$Transpiration_day<- plant_d$Transpiration
plant_n$corr_date<- plant_n$Date-1
plant_n$Transpiration_night<- plant_n$Transpiration

daily_dat<- join(plant_d, plant_n[, c("corr_date", "Snapshot.ID.Tag" ,"Transpiration_night")],
              type= "inner", by= c("Snapshot.ID.Tag", "corr_date")) 
daily_dat$Transpiration<- daily_dat$Transpiration_day + daily_dat$Transpiration_night
daily_dat$interval<-"Daily"
return(daily_dat)}

daily_diurnal<- merge_daily(diurnal)
test<- rbind.fill(daily, daily_diurnal, diurnal)
## need to diurnal in plot of plotting purpose

test$interval<- as.factor(test$interval)
#test$TRANS<- ifelse(test$Transpiration>1000, NA, test$Transpiration)

summary(test$interval)

## 

plot_data<-  ggplot( test, aes(x= Date, y= Transpiration, color= treat))+geom_point()+
  facet_rep_grid(interval~genotype, scales = "free",repeat.tick.labels = F)+
  stat_smooth(span = 0.8, level= 0.95)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab(expression(paste("Plant transpiration (g)", sep= "")))+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  scale_x_date(date_breaks = "2 day", date_labels =  "%d %b")+xlab("")+
  background_grid(major = "xy", minor = "none")+
  theme(legend.title=element_blank())

plot_sexy<-ggplot( test, aes(x= Date, y= Transpiration, color= treat))+
  facet_rep_grid(interval~genotype, scales = "free",repeat.tick.labels = F)+
  stat_smooth(span = 0.8, level= 0.95)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab(expression(paste("Plant transpiration (g)", sep= "")))+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  scale_x_date(date_breaks = "2 day", date_labels =  "%d %b")+xlab("")+
  background_grid(major = "xy", minor = "none")+
  theme(legend.title=element_blank())

ggsave( "figures/trasnpiration_data.tiff", plot_data, height= 6, width= 10)

ggsave( "figures/trasnpiration.tiff", plot_sexy, height= 6, width= 10)

#################################################################################
#IMPORTANT DESCISION

data$date_time<- strptime(data$datetime,  "%m/%d/%Y  %H:%M:%S")# datetime object
data$date_time <- as.POSIXct(data$date_time, tz = Sys.timezone())
data$hour<- hour(data$date_time)
t_all <- dlply(data, "Snapshot.ID.Tag")

## what is wrong with weighing
t1<- t_all$"529-312-G1-9-T3-WS"
#looks like there is only one measurment on June ( and its around mid-day)
# This will add more night time transpiration for June 8 and reduce day time for June 10.
#it thi problem with all pots?

june9<- subset(data, date=="6/9/2018" )
june9$interval<- as.factor(as.character(ifelse(june9$hour< 10, "Night", "Day")))
hour(data$date_time)
str(data)
sub<- droplevels(subset(test, date_time < "2018-06-07 04:01:19" ))
ggplot( sub, aes(x= Date, y= Transpiration, color= treat))+
  facet_rep_grid(interval~genotype, scales = "free",repeat.tick.labels = F)+geom_point()
ggplot( sub, aes(x= Date, y= Transpiration, color= treat))+
  facet_rep_grid(interval~genotype, scales = "free",repeat.tick.labels = F)+
  stat_smooth(span = 0.8, level= 0.95)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab(expression(paste("Plant transpiration (g)", sep= "")))+
  scale_colour_manual('Treatment', values = c('WW' = 'blue', 'WS' = 'red'))+
  scale_x_date(date_breaks = "2 day", date_labels =  "%d %b")+xlab("")+
  background_grid(major = "xy", minor = "none")+
  theme(legend.title=element_blank())


ggplot( test, aes(x= date, y= Transpiration, color= treat))+
  facet_rep_grid(interval~genotype, scales = "free",repeat.tick.labels = F)+
  geom_boxplot(span = 0.8, level= 0.95)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Overall, it can be concluded that nigh time transpiration is not seems to be very IMPORTAN TRAIN other than summarising
## Yes there was treatment effect but kind of same intensity with day time.
## we may collesct this data for for fwes day to draw conclusion,
#how much night time transpiration invovle in daytime.

#Becasue of problem on June 8, we can show correct numbers for daily tramspiration on
#June June 8 and June 9!!
# so decided to calculate total transpired water by individual plant.
dev.off()
