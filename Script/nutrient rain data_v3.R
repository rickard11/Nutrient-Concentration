library(readxl)
library(ggplot2)
library(dplyr)
library(zoo)
install.packages("readxl")
library(readxl)
library(plotly)
# upload historic rain data for santa barbara
rain<-read_excel("Data/Rain/200dailys.xls",skip=9)
NOAA_Rain<-read.csv("Data/Rain/NOAA_2008-2011_2017-2022.csv")
NOAA_Rain<-NOAA_Rain[NOAA_Rain$P_DAILY_CALC>-1,]
NOAA_Rain$LST_DATE<-format(NOAA_Rain$LST_DATE,format="%Y%m%d")
NOAA_Rain$LST_DATE<-as.Date(NOAA_Rain$LST_DATE,format="%Y%m%d")
unique(NOAA_Rain$LST_DATE)
NOAA_Rain<-NOAA_Rain[,c(3,11)]
colnames()
plot(NOAA_Rain$P_DAILY_CALC~NOAA_Rain$LST_DATE)
colnames(rain)[4]<-"month"
colnames(rain)[6]<-"Daily_Rain"
rain$Date<-as.Date(paste0(rain$year,"/",rain$month,"/",rain$day),format="%Y/%m/%d")
rain<-rain[!is.na(rain$Date),]
rain$wtr_yr <- getYearQuarter(rain$Date, firstMonth=10)
rain<-rain[rain$`water year`>2002,]
rain<-as.data.frame(rain)
rain_wtryr<-aggregate(Daily_Rain~wtr_yr,rain,FUN=sum)

#Looking at rainfall totals- very different, so going off County webpage instead
rain_wtryr$rainfall <- ifelse(rain_wtryr$Daily_Rain > 18.39,"Wet","dry")
ggplot(rain_wtryr)+geom_col(aes(x=wtr_yr,y=Daily_Rain,fill=rainfall))+theme_bw()

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
colnames(new_Dev)[2]<-"Date"
new_Dev$Environment<-"Wetland" #naming environment for later merging
NCOS_All<-new_Dev
new_Dev<-new_Dev[new_Dev$Site=="Venoco"|new_Dev$Site=="Venoco Bridge",]
new_Dev<-new_Dev[,c(2,4:6)]
#Merge old and new data together
Dev_all<-rbind(new_Dev,old_Dev_m)
Dev_all_rain<-merge(Dev_all,rainfull,by="Date",all.x=TRUE)
Dev_all_rain<-Dev_all_rain[Dev_all_rain$Phosphate.um>=0&Dev_all_rain$Nitrite.Nitrate.um>=0&Dev_all_rain$Ammonia.um>=0,]

#Plot data
ggplot(Dev_all_rain)+geom_point(aes(x=twodayrain,y=Ammonia.um, color=Environment))+theme_bw()
ggplot(Dev_all_rain)+geom_boxplot(aes(x=Environment,y=Nitrite.Nitrate.um))+theme_bw()
ggplot(Dev_all_rain)+geom_point(aes(x=twodayrain,y=Nitrite.Nitrate.um, color=Environment))+theme_bw()
ggplot(Dev_all_rain)+geom_boxplot(aes(x=Environment,y=Ammonia.um))+theme_bw()
ggplot(Dev_all_rain)+geom_boxplot(aes(x=Environment,y=Ammonia.um))+theme_bw()+ylim(0,50)
ggplot(Dev_all_rain)+geom_point(aes(x=Daily_Rain,y=Phosphate.um, color=Environment))+theme_bw()
ggplot(Dev_all_rain)+geom_boxplot(aes(x=Environment,y=Phosphate.um))+theme_bw()
Dev_all_rain
#Summarize data and combine with rain years
Dev_all_rain$wtr_yr <- getYearQuarter(Dev_all_rain$Date, firstMonth=10)
Dev_all_rain<-Dev_all_rain[Dev_all_rain$wtr_yr!="WY03",]
summary<- Dev_all_rain %>% 
  group_by(wtr_yr) %>% 
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
waterstats<-merge(summary,rain_wtryr,by="wtr_yr")
plot(summary$max_A~summary$wtr_yr)
plot(summary$max_N~summary$wtr_yr)
plot(summary$max_P~summary$wtr_yr)
Dev_all_rain_mg<-Dev_all_rain
Dev_all_rain_mg$P_mg<-(Dev_all_rain_mg$Phosphate.um*30.97)/1000
Dev_all_rain_mg$N_mg<-(Dev_all_rain_mg$Nitrite.Nitrate.um*14.0067)/1000
Dev_all_rain_mg$A_mg<-(Dev_all_rain_mg$Ammonia.um*14.0067)/1000

summary2<- Dev_all_rain_mg %>% 
  group_by(Environment) %>% 
  summarize(
    avg_P = mean(P_mg),
    med_P = median(P_mg),
    max_P = max(P_mg),
    avg_N = mean(N_mg),
    med_N = median(N_mg),
    max_N = max(N_mg),
    avg_A = mean(A_mg),
    med_A = median(A_mg),
    max_A = max(A_mg)
  )
summary2<-as.data.frame(summary2)
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


Dev_all$month<-format(Dev_all$Date,format="%m")
Dev_all<-Dev_all[Dev_all$month!="06" &Dev_all$month!="07"&Dev_all$month!="08"&Dev_all$Phosphate.um>0&Dev_all$Nitrite.Nitrate.um>0
                 &Dev_all$Ammonia.um>0&Dev_all$month!="04" &Dev_all$month!="05",]
Dev_all


ggplot(Dev_all)+geom_boxplot(aes(x=month,y=Phosphate.um, color=Environment))
ggplot(Dev_all)+geom_boxplot(aes(x=month,y=Nitrite.Nitrate.um, color=Environment))
ggplot(Dev_all)+geom_boxplot(aes(x=month,y=Ammonia.um, color=Environment))#+ylim(0,100)
ggplot(Dev_all)+geom_boxplot(aes(x=month,y=Ammonia.um, color=Environment))+ylim(0,100)
str(Dev_all)
colnames(Dev_all)
Dev_all$wtr_yr <- getYearQuarter(Dev_all$Date, firstMonth=10)
Dev_long<-Dev_all %>%
  pivot_longer(cols = Phosphate.um:Ammonia.um, names_to = "Nutrient", values_to = "mg_L")
Dev_long<-Dev_long[Dev_long$Year!=2003&Dev_long$Year!=2012&Dev_long$Year!=2011&Dev_long$Year!=2010&Dev_long$Year!=2009&Dev_long$Year!=2017,]


##Jitter plots-ylim removes values from analysis.
ggplot(Dev_long)+geom_boxplot(outlier.shape = NA,aes(x=Environment,y=mg_L))+
  geom_jitter(aes(x=Environment,y=mg_L,color=Year))+facet_wrap(vars(Nutrient))+theme_bw()
ggplot(Dev_long)+geom_boxplot(outlier.shape = NA,aes(x=Environment,y=mg_L))+
  geom_jitter(aes(x=Environment,y=mg_L,color=Year))+
  facet_wrap(vars(Nutrient))+theme_bw()+coord_cartesian(ylim = c(0, 500))




###z values
old_Dev<-old_Dev
old_Dev$z_nh4 <- scale(old_Dev$nh4_uM)
hist(old_Dev$z_nh4)
summary(old_Dev$z_nh4)
which(old_Dev$z_nh4 > 3.29)

lower_bound <- median(old_Dev$nh4) - 3 * mad(old_Dev$nh4, constant = 1)
lower_bound
upper_bound <- median(old_Dev$nh4) + 3 * mad(old_Dev$nh4, constant = 1)
upper_bound

outlier_ind <- which(old_Dev$nh4 > upper_bound)
outlier_ind


new_Dev$z_nh4 <- scale(new_Dev$Ammonia.um)
hist(new_Dev$z_nh4)
summary(new_Dev$z_nh4)
which(new_Dev$z_nh4 > 3.29)

lower_bound <- median(new_Dev$Ammonia.um) - 3 * mad(new_Dev$Ammonia.um, constant = 1)
lower_bound
upper_bound <- median(new_Dev$Ammonia.um) + 3 * mad(new_Dev$Ammonia.um, constant = 1)
upper_bound

outlier_ind <- which( new_Dev$Ammonia.um > upper_bound)
outlier_ind

str(new_Dev)
str(old_Dev)
Dev_all$count<-1
dev_sum<-aggregate(count~Environment,Dev_all,FUN=sum)
46/217
11/101

old_Dev
ggplot(old_Dev)+geom_point(aes(x=Date,y=nh4_uM))
ggplot(old_Dev)+geom_point(aes(x=Date,y=no3_uM))
ggplot(old_Dev)+geom_point(aes(x=Date,y=po4_uM))

ggplot(new_Dev)+geom_point(aes(x=Date,y=Ammonia.um))
ggplot(new_Dev)+geom_point(aes(x=Date,y=Nitrite.Nitrate.um))
ggplot(new_Dev)+geom_point(aes(x=Date,y=Phosphate.um))

Dev_all_rain
str(Dev_all_rain)
unique(Dev_all_rain$Daily_Rain)
ggplot(Dev_all_rain,aes(x=Date))+geom_point(aes(y=Ammonia.um))+geom_col(aes(y=Daily_Rain))
ggplot(Dev_all_rain)+geom_point(aes(x=Date,y=Nitrite.Nitrate.um))
ggplot(Dev_all_rain)+geom_point(aes(x=Date,y=Phosphate.um))


##########################################################################################
##########################################################################################
################NCOS site comparison######################################################
##########################################################################################
Dev
NCOS_All
#create date time and Year
NCOS_All$Datetime<-as.POSIXct(paste0(NCOS_All$Date," ",NCOS_All$Time),format="%Y-%m-%d %H:%M")
NCOS_All$wtr_yr<-getYearQuarter(NCOS_All$Date,firstMonth = 10)
#Create better location labels
NCOS_All<-NCOS_All[NCOS_All$Site!="Blank"&NCOS_All$Site!="COPR",]
NCOS_All<- within(NCOS_All, Site[Site == "Phelps Creek (Marymount)"] <- 'Phelps')
NCOS_All<- within(NCOS_All, Site[Site == "Whittier Stormdrain"] <- 'Whittier')
NCOS_All<- within(NCOS_All, Site[Site == "Venoco Bridge"] <- 'Venoco')
NCOS_All<- within(NCOS_All, Site[Site == "Devereux Creek (Ellwood)"] <- 'Devereux')

#plot by datetime
ggplot(NCOS_All)+geom_point(aes(x=Datetime,y=Phosphate.um,color=Site))#+facet_wrap(~wtr_yr)


#TRy plotly

fig <- plot_ly() %>% 
  add_lines(x = c("a","b","c"), y = c(1,3,2))%>% 
  layout(title="sample figure", xaxis = list(title = 'x'), yaxis = list(title = 'y'), plot_bgcolor = "#c7daec") 
fig

plot_ly(NCOS_All, x = ~Datetime, y = ~Phosphate.um,color = ~Site)

#Add rain to plotly graph
colnames(NOAA_Rain)[1]<-"Date"
NOAA_Rain_Wetland<-NOAA_Rain[NOAA_Rain$Date>="2017-09-12",]
NOAA_Rain_Wetland$Site<-"Venoco"
NCOS_All_Daily<-aggregate(cbind(Phosphate.um,Nitrite.Nitrate.um,Ammonia.um)~Date+Site+wtr_yr,NCOS_All,FUN=mean)


NCOS_plusRain<-merge(NCOS_All_Daily,NOAA_Rain_Wetland,by=cbind("Date","Site"),all.x=TRUE)
NCOS_plusRain$rain_cm<-NCOS_plusRain$P_DAILY_CALC*0.1


p <- plot_ly(NCOS_plusRain, x = ~Date)
p <- p %>% add_trace(y = ~Phosphate.um,type='scatter',mode = 'markers',color = ~Site)
p <- p %>% add_trace(y = ~rain_cm, name = 'Rain',type = 'bar',marker= list(color = 'rgb(158,202,225)',
                                                                           line = list(color = 'rgb(8,48,107)', width = 0.5)))
p



