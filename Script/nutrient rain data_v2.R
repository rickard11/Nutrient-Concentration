library(readxl)
library(ggplot2)
library(dplyr)
library(zoo)
# upload historic rain data for santa barbara
rain<-read_excel("Data/Rain/200dailys.xls",skip=9)
colnames(rain)[4]<-"month"
colnames(rain)[6]<-"Daily_Rain"
rain$Date<-as.Date(paste0(rain$year,rain$month,rain$day),format="%Y%m%d")
rain_wtryr<-aggregate(Daily_Rain~`water year`,rain,FUN=sum)
rain<-rain[,c(6,8)]

#Looking at rainfall totals- very different, so going off County webpage instead
rain_wtryr<-rain_wtryr[rain_wtryr$`water year`>2002,]
rain_wtryr$`water year`<-format(rain_wtryr$`water year`,format="%Y")
rain_wtryr$rainfall <- ifelse(rain_wtryr$Daily_Rain > 18.39,"Wet","dry")
ggplot(rain_wtryr)+geom_col(aes(x=`water year`,y=Daily_Rain,fill=rainfall))+theme_bw()


#rainfall dataset only includes days with rain. Adding in 0 values
ts <- seq.POSIXt(as.POSIXct("1951-10-25",'%m/%d/%y'), as.POSIXct("2023-10-01 ",'%m/%d/%y'), by="day")
ts <- seq.POSIXt(as.POSIXlt("1951-10-25"), as.POSIXlt("2023-10-01"), by="day")
ts <- format.POSIXct(ts,'%Y-%m-%d')
df <- data.frame(Date=ts)
df$Date<-as.Date(df$Date,format="%Y-%m-%d")
rainfull <- full_join(df,rain)
rainfull$Daily_Rain[is.na(rainfull$Daily_Rain)] <- 0

#This dataset only accounts for the rain for the previous 24 hours starting at 8am.
#We are now using the rolling 2 day average, but we want it to be the day and the day after.
rainfull<-data.frame(rainfull, nextdayrain=dplyr::lead(rainfull,1))
rainfull$twodayrain<-rainfull$Daily_Rain+rainfull$nextdayrain.Daily_Rain

#Upload and format old nutrient data from 2004-2011
old_Dev<-read.csv("Data/Nutrients/sbclter_stream_chemistry_allyears_non_registered_stations_20190628.csv")
old_Dev<-old_Dev[grepl("^DV", old_Dev$site_code), ]
old_Dev$Date<-as.Date(old_Dev$timestamp_local,format="%Y-%m-%dT%H:%M:%S")
old_Dev_m<-old_Dev[,c(3:5,13)]
colnames(old_Dev_m)<-c("Ammonia.um","Nitrite.Nitrate.um","Phosphate.um","Date")
old_Dev_m$Environment<-"Golf Course" #naming environment for later merging

#Upload and format new nutrient data from 2017-2022
new_Dev<-read.csv("Data/nutrients/Nutrients_2018-2022.csv",strip.white = TRUE)
new_Dev$Sample.Date<-as.Date(new_Dev$Sample.Date,format="%m/%d/%Y")
new_Dev<-new_Dev[new_Dev$Site=="Venoco"|new_Dev$Site=="Venoco Bridge",]
new_Dev<-new_Dev[,c(2,4:6)]
colnames(new_Dev)[1]<-"Date"
new_Dev$Environment<-"Wetland" #naming environment for later merging

#Merge old and new data together
Dev_all<-rbind(new_Dev,old_Dev_m)
Dev_all_rain<-merge(Dev_all,rainfull,by="Date",all.x=TRUE)
Dev_all_rain<-Dev_all_rain[Dev_all_rain$Phosphate.um>=0&Dev_all_rain$Nitrite.Nitrate.um>=0&Dev_all_rain$Ammonia.um>=0,]

#Plot data
ggplot(Dev_all_rain)+geom_point(aes(x=twodayrain,y=Ammonia.um, color=Environment))+theme_bw()
ggplot(Dev_all_rain)+geom_boxplot(aes(x=Environment,y=Nitrite.Nitrate.um))+theme_bw()
ggplot(Dev_all_rain)+geom_point(aes(x=twodayrain,y=Nitrite.Nitrate.um, color=Environment))+theme_bw()
ggplot(Dev_all_rain)+geom_boxplot(aes(x=Environment,y=Ammonia.um))+theme_bw()
ggplot(Dev_all_rain)+geom_point(aes(x=Daily_Rain,y=Phosphate.um, color=Environment))+theme_bw()
ggplot(Dev_all_rain)+geom_boxplot(aes(x=Environment,y=Phosphate.um))+theme_bw()
summary<- Dev_all_rain %>% 
  group_by(Year) %>% 
  summarize(
    avg_P = mean(Phosphate.um),
    med_P = median(Phosphate.um),
    max_P = max(Phosphate.um),
    avg_N = mean(Nitrite.Nitrate.um),
    med_N = median(Nitrite.Nitrate.um),
    max_N = max(Nitrite.Nitrate.um),
    avg_A = mean(Ammonia.um),
    med_A = median(Ammonia.um),
    max_A = max(Ammonia.um)
  )
rain_wtryr
#possibly will want to get daily averages.
Dev_all_rain_Daily<-aggregate(cbind(Ammonia.um,Phosphate.um,Nitrite.Nitrate.um)~Date+Daily_Rain+Environment+twodayrain,Dev_all_rain,FUN=mean)

#Plot data- Ammonia
ggplot(Dev_all_rain_Daily)+geom_point(aes(x=Daily_Rain,y=Ammonia.um, color=Environment))+
  theme_bw()

#Plot data- Nitrate
ggplot(Dev_all_rain_Daily)+geom_point(aes(x=Daily_Rain,y=Nitrite.Nitrate.um, color=Environment))+
  theme_bw()

ggplot(Dev_all_rain_Daily)+geom_point(aes(x=Daily_Rain,y=Phosphate.um, color=Environment))+
  theme_bw()+ylim(0,100)


# There are a lot of high nutrient concentrations for the golf course, but also a lot more samples taken now try to seperate by month
Dev_all_rain$month<-format(Dev_all_rain$Date,format="%b")
Dev_all_rain$Year<-format(Dev_all_rain$Date,format="%Y")

ggplot(Dev_all_rain)+geom_boxplot(aes(x=month,y=Phosphate.um, color=Environment))
ggplot(Dev_all_rain)+geom_boxplot(aes(x=month,y=Nitrite.Nitrate.um, color=Environment))
ggplot(Dev_all_rain)+geom_boxplot(aes(x=month,y=Ammonia.um, color=Environment))+ylim(0,100)

## What does old devereux data look like?
old_Dev<-old_Dev[old_Dev$po4_uM>=0&old_Dev$no3_uM>=0&old_Dev$nh4_uM>=0,]
old_Dev$Year<-format(old_Dev$Date,format="%Y")

ggplot(old_Dev)+geom_boxplot(aes(x=site_code,y=po4_uM))
ggplot(old_Dev)+geom_boxplot(aes(x=site_code,y=nh4_uM))
ggplot(old_Dev)+geom_boxplot(aes(x=site_code,y=no3_uM))

ggplot(old_Dev)+geom_boxplot(aes(x=Year,y=po4_uM))
ggplot(old_Dev)+geom_boxplot(aes(x=Year,y=nh4_uM))
ggplot(old_Dev)+geom_boxplot(aes(x=Year,y=no3_uM))

ggplot(Dev_all_rain)+geom_boxplot(aes(x=Year,y=Phosphate.um))
ggplot(Dev_all_rain)+geom_boxplot(aes(x=Year,y=Nitrite.Nitrate.um))
ggplot(Dev_all_rain)+geom_boxplot(aes(x=Year,y=Ammonia.um))




# compare water years (dry) to water years (wet)



