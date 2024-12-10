## Isolating Phelps samples for TSS to determine kg/ha

TSS_data<-read.csv("data/2020_2021_TSS_Fulldataset.csv")

#Phelps watershed size= about 500 ha

#clean TSS data a bit
TSS_data<-TSS_data[!is.na(TSS_data$TSS..mg.L.),]
TSS_data$Datetime<-as.POSIXct(paste0(TSS_data$Date," ",TSS_data$Time),format="%Y-%m-%d %H:%M:%S")
TSS_data$Date<-as.POSIXct(TSS_data$Date,format="%Y-%m-%d")
str(TSS_data)
mean(TSS_data$TSS..mg.L.)
TSS_data<-TSS_data[!is.na(TSS_data$Date),]
#Get wateryear function
getYearQuarter <- function(x,
                           firstMonth=10,
                           fy.prefix='WY',
                           quarter.prefix='Q',
                           sep='-',
                           level.range=c(min(x), max(x)) ) {
  if(level.range[1] > min(x) | level.range[2] < max(x)) {
    warning(paste0('The range of x is greater than level.range. Values ',
                   'outside level.range will be returned as NA.'))
  }
  quarterString <- function(d) {
    year <- as.integer(format(d, format='%Y'))
    month <- as.integer(format(d, format='%m'))
    y <- ifelse(firstMonth > 1 & month >= firstMonth, year+1, year)
    q <- cut( (month - firstMonth) %% 12, breaks=c(-Inf,2,5,8,Inf),
              labels=paste0(quarter.prefix, 1:4))
    return(paste0(fy.prefix, substring(y,3,4)))
  }
  vals <- quarterString(x)
  levels <- unique(quarterString(seq(
    as.Date(format(level.range[1], '%Y-%m-01')),
    as.Date(format(level.range[2], '%Y-%m-28')), by='month')))
  return(factor(vals, levels=levels, ordered=TRUE))
} 
TSS_data$wtr_yr <- getYearQuarter(TSS_data$Date, firstMonth=10)


# separate locations into separate spreadsheets
unique(TSS_data$Location)
Phelps_TSS<-TSS_data[TSS_data$Location=="NCOS-P"|TSS_data$Location=="Phelps",]
Devereux_TSS<-TSS_data[TSS_data$Location=="NCOS-D"|TSS_data$Location=="Devereux",]
Whittier_TSS<-TSS_data[TSS_data$Location=="NCOS-W"|TSS_data$Location=="Whittier",]
Venoco_TSS<-TSS_data[TSS_data$Location=="NCOS-V"|TSS_data$Location=="Venoco",]


## Load depth data for each site to calculate volume weighted mean
Phelpsdepth<-read.csv("data/Phelps_PTdepth_FlowCFS.csv")
Devereuxdepth<-read.csv("data/DevereuxCreek_PTdata_2018-2022wy.csv")
Venocodepth<-read.csv("data/Venoco_PTdata_2018-2022wy.csv")


## Structure date times to be mergable and remove unneccessary columns
Phelpsdepth$Date<-as.Date(Phelpsdepth$Date,format="%Y-%m-%d")
Phelpsdepth$Datetime<-as.POSIXct(paste0(Phelpsdepth$Date," ",Phelpsdepth$Time),format="%Y-%m-%d %H:%M:%S")
Phelpsdepth<-Phelpsdepth[,c(2,5,6)]
Devereuxdepth$Date<-as.Date(Devereuxdepth$Date,format="%m/%d/%Y")
Devereuxdepth$Datetime<-as.POSIXct(paste0(Devereuxdepth$Date," ",Devereuxdepth$Time),format="%Y-%m-%d %H:%M:%S")
Devereuxdepth<-Devereuxdepth[,c(7,4)]
Venocodepth$Date<-as.Date(Venocodepth$Date,format="%m/%d/%Y")
Venocodepth$Datetime<-as.POSIXct(paste0(Venocodepth$Date," ",Venocodepth$Time),format="%Y-%m-%d %H:%M:%S")
Venocodepth<-Venocodepth[,c(8,4)]
#Clean phelps tss to 15 min intervals so it merges nicely with other data
library(lubridate)
Phelps_TSS$Datetime<-round_date(Phelps_TSS$Datetime, unit="15 mins")
Phelpsdepth$Datetime<-round_date(Phelpsdepth$Datetime, unit="15 mins")
## Merge TSS data with depth data and keep all TSS data (will involve some data cleaning first)
Phelpsall<-merge(Phelps_TSS,Phelpsdepth,by="Datetime",all.x=TRUE)
Venocoall<-merge(Venoco_TSS,Venocodepth,by="Datetime",all.x=TRUE)
Devereuxall<-merge(Devereux_TSS,Devereuxdepth,by="Datetime",all.x=TRUE)


##Calculate volume weighted average
# delete unneccessary columns
Phelpsall<-Phelpsall[c(1,4,8,11,12,13)]
Venocoall<-Venocoall[1:123,c(1,4,8,11,12)]
Devereuxall<-Devereuxall[1:64,c(1,4,8,11,12)]
#Get proportional depths
Phelpsall<-Phelpsall[!is.na(Phelpsall$ptdepth_ft),]
Devereuxall<-Devereuxall[!is.na(Devereuxall$LEVEL),]
Venocoall<-Venocoall[!is.na(Venocoall$LEVEL),]

#Phelps yearly weighted average of TSS
for(i in unique(Phelpsall$wtr_yr)){
  PA<-Phelpsall[Phelpsall$wtr_yr==i,]
  print(PA$wtr_yr)
  print(weighted.mean(PA$TSS..mg.L.,PA$discharge.cfs))
}


#Technically depth weighted mean... volume is not a linear relationship so that would change.
TSS.mg.L<-c(366.09,589.70,751.93)
year<-c(2020,2021,2022)
Phelps.VWA<-data.frame(TSS.mg.L,year)
Phelps.VWA$Site<-"Phelps"

#Devereux
for(i in unique(Devereuxall$wtr_yr)){
  DA<-Devereuxall[Devereuxall$wtr_yr==i,]
  print(DA$wtr_yr)
  print(weighted.mean(DA$TSS..mg.L.,DA$LEVEL))
}

TSS.mg.L<-c(131.54,303.81,223.60)
year<-c(2020,2021,2022)
Devereux.VWA<-data.frame(TSS.mg.L,year)
Devereux.VWA$Site<-"Devereux"

#Venoco
for(i in unique(Venocoall$wtr_yr)){
  VA<-Venocoall[Venocoall$wtr_yr==i,]
  print(VA$wtr_yr)
  print(weighted.mean(VA$TSS..mg.L.,VA$LEVEL))
}

TSS.mg.L<-c(3231.82,1011.47,1818.81)
year<-c(2020,2021,2022)
Venoco.VWA<-data.frame(TSS.mg.L,year)
Venoco.VWA$Site<-"Venoco"

##merge back together
NCOS_TSS_VWM_mg_L<-rbind(Phelps.VWA,Devereux.VWA,Venoco.VWA)
NCOS_TSS_VWM_mg_L

##Need to redo with volume for devereux and venoco
method<-c("volume","volume","volume","Level","Level","Level","Level","Level","Level")
NCOS_TSS_VWM_mg_L<-cbind(NCOS_TSS_VWM_mg_L,method)

Phelps_mean<-aggregate(TSS..mg.L.~wtr_yr,Phelpsall,FUN=mean)
Phelps_mean

