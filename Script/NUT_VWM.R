library(lubridate)
##read in nutrient data
nut_vwm<-read.csv("Data/nutrients/Nutrients_2018-2022.csv",strip.white = TRUE)

#Clean data and make date time readable
nut_vwm<-nut_vwm[,1:7]
nut_vwm$Sample.Date<-as.POSIXct(nut_vwm$Sample.Date,format="%m/%d/%Y")
nut_vwm$Datetime<-as.POSIXct(paste0(nut_vwm$Sample.Date," ",nut_vwm$Time),format="%Y-%m-%d %H:%M")


#Get wateryear function
getYearQuarter <- function(x,firstMonth=10,fy.prefix='WY',
   quarter.prefix='Q',sep='-',level.range=c(min(x), max(x)) ) {
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
  levels <- unique(quarterString(seq( as.Date(format(level.range[1], '%Y-%m-01')),
    as.Date(format(level.range[2], '%Y-%m-28')), by='month')))
  return(factor(vals, levels=levels, ordered=TRUE))
} 
nut_vwm$wtr_yr <- getYearQuarter(nut_vwm$Sample.Date, firstMonth=10)


# separate locations into separate spreadsheets
unique(nut_vwm$Site)
Phelps_nut<-nut_vwm[nut_vwm$Site=="Phelps"|nut_vwm$Site=="Phelps Creek (Marymount)",]
Devereux_nut<-nut_vwm[nut_vwm$Site=="Devereux Creek (Ellwood)"|nut_vwm$Site=="Devereux",]
Whittier_nut<-nut_vwm[nut_vwm$Site=="Whittier Stormdrain"|nut_vwm$Site=="Whittier",]
Venoco_nut<-nut_vwm[nut_vwm$Site=="Venoco Bridge"|nut_vwm$Site=="Venoco",]

#Clean nutrient data to 15 min intervals so it merges nicely with pt data
Phelps_nut$Datetime<-round_date(Phelps_nut$Datetime, unit="15 mins")
Devereux_nut$Datetime<-round_date(Devereux_nut$Datetime, unit="15 mins")
Whittier_nut$Datetime<-round_date(Whittier_nut$Datetime, unit="15 mins")
Venoco_nut$Datetime<-round_date(Venoco_nut$Datetime, unit="15 mins")

## Load depth data for each site to calculate volume weighted mean
Phelpsdepth<-read.csv("data/Phelps_PTdepth_FlowCFS.csv")
Devereuxdepth<-read.csv("data/DevereuxCreek_PTdata_2018-2022wy.csv")
Venocodepth<-read.csv("data/Venoco_PTdata_2018-2022wy.csv")

## Structure pt date times to be mergable with nutrients and remove unneccessary columns
Phelpsdepth$Date<-as.Date(Phelpsdepth$Date,format="%Y-%m-%d")
Phelpsdepth$Datetime<-as.POSIXct(paste0(Phelpsdepth$Date," ",Phelpsdepth$Time),format="%Y-%m-%d %H:%M:%S")
Phelpsdepth<-Phelpsdepth[,c(2,5,6)]
Devereuxdepth$Date<-as.Date(Devereuxdepth$Date,format="%m/%d/%Y")
Devereuxdepth$Datetime<-as.POSIXct(paste0(Devereuxdepth$Date," ",Devereuxdepth$Time),format="%Y-%m-%d %H:%M:%S")
Devereuxdepth<-Devereuxdepth[,c(7,4)]
Venocodepth$Date<-as.Date(Venocodepth$Date,format="%m/%d/%Y")
Venocodepth$Datetime<-as.POSIXct(paste0(Venocodepth$Date," ",Venocodepth$Time),format="%Y-%m-%d %H:%M:%S")
Venocodepth<-Venocodepth[,c(8,4)]

#Clean pt data to merge nicely with nutrient data and no data goes missing
Phelpsdepth$Datetime<-round_date(Phelpsdepth$Datetime, unit="15 mins")
Devereuxdepth$Datetime<-round_date(Devereuxdepth$Datetime, unit="15 mins")
Venocodepth$Datetime<-round_date(Venocodepth$Datetime, unit="15 mins")

## Merge TSS data with depth data and keep all TSS data (will involve some data cleaning first)
Phelpsall<-merge(Phelps_nut,Phelpsdepth,by="Datetime",all.x=TRUE)
Venocoall<-merge(Venoco_nut,Venocodepth,by="Datetime",all.x=TRUE)
Devereuxall<-merge(Devereux_nut,Devereuxdepth,by="Datetime",all.x=TRUE)

##Calculate volume weighted average

# First delete unnecessary columns
Phelpsall<-Phelpsall[c(1,5:11)]
Venocoall<-Venocoall[,c(1,5-10)]
Devereuxall<-Devereuxall[,c(1,5:10)]
#Get proportional depths
Phelpsall<-Phelpsall[!is.na(Phelpsall$ptdepth_ft),]
Devereuxall<-Devereuxall[!is.na(Devereuxall$LEVEL),]
Venocoall<-Venocoall[!is.na(Venocoall$LEVEL),]

#Phelps yearly weighted average of TSS
for(i in unique(Phelpsall$wtr_yr)){
  PA<-Phelpsall[Phelpsall$wtr_yr==i,]
  print(PA$wtr_yr)
  print(weighted.mean(PA$Phosphate.um,PA$discharge.cfs))
  print(weighted.mean(PA$Ammonia.um,PA$discharge.cfs))
  print(weighted.mean(PA$Nitrite.Nitrate.um,PA$discharge.cfs))
}

#putting the results into a dataframe
P_um<-c(18.926,6.938,13.119,11.602,14.65,19.1)
A_um<-c(1.044,0.879,2.494,0.444,9.01,40.2)
N_um<-c(115.731,52.301,132.785,50.005,122.5,299)
year<-c(2019,2020,2021,2022, 2018, 2017)
Phelps.VWA<-data.frame(P_um,A_um,N_um,year)
Phelps.VWA$Site<-"Phelps"


#Devereux
for(i in unique(Devereuxall$wtr_yr)){
  DA<-Devereuxall[Devereuxall$wtr_yr==i,]
  print(DA$wtr_yr)
  print(weighted.mean(DA$Phosphate.um,DA$LEVEL))
  print(weighted.mean(DA$Ammonia.um,DA$LEVEL))
  print(weighted.mean(DA$Nitrite.Nitrate.um,DA$LEVEL))
}

P_um<-c(10.15631,15.86781,18.49038,17.75)
A_um<-c(4.788671,7.733583,8.08494,25.895)
N_um<-c(50.85828,166.083,145.2934,113.55)
year<-c(2020,2021,2022,2018)
Devereux.VWA<-data.frame(P_um,A_um,N_um,year)
Devereux.VWA$Site<-"Devereux"

#Venoco
for(i in unique(Venocoall$wtr_yr)){
  VA<-Venocoall[Venocoall$wtr_yr==i,]
  print(VA$wtr_yr)
  print(weighted.mean(VA$Phosphate.um,VA$LEVEL))
  print(weighted.mean(VA$Ammonia.um,VA$LEVEL))
  print(weighted.mean(VA$Nitrite.Nitrate.um,VA$LEVEL))
}

P_um<-c(12.45304,9.955912,9.625064,8.998853,9.47,0.76)
A_um<-c(3.16293,21.40097,5.590495,12.20106,9.73,6.36)
N_um<-c(69.54218,9.140282,73.94291,54.17138,56.5,14.6)
year<-c(2019,2020,2021,2022,2018,2017)
Venoco.VWA<-data.frame(P_um,A_um,N_um,year)
Venoco.VWA$Site<-"Venoco"





















