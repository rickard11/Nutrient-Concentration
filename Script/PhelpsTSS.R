## Isolating Phelps samples for TSS to determine kg/ha

TSS_data<-read.csv("Data/2020_2021_TSS_Fulldataset.csv")

#Phelps watershed size= about 500 ha

#calculate mean of TSS
TSS_data<-TSS_data[!is.na(TSS_data$TSS..mg.L.),]
TSS_data$Datetime<-as.POSIXct(paste0(TSS_data$Date," ",TSS_data$Time),format="%Y-%m-%d %H:%M:%S")
str(TSS_data)
mean(TSS_data$TSS..mg.L.)

# separate locations into separate spreadsheets
unique(TSS_data$Location)
Phelps_TSS<-TSS_data[TSS_data$Location=="NCOS-P"|TSS_data$Location=="Phelps",]
Devereux_TSS<-TSS_data[TSS_data$Location=="NCOS-D"|TSS_data$Location=="Devereux",]
Whittier_TSS<-TSS_data[TSS_data$Location=="NCOS-W"|TSS_data$Location=="Whittier",]
Venoco_TSS<-TSS_data[TSS_data$Location=="NCOS-V"|TSS_data$Location=="Venoco",]


## Load depth data for each site to calculate volume weighted mean
Phelpsdepth<-read.csv("Data/PhelpsBridge_PTdata_2018-2022wy.csv")
Devereuxdepth<-read.csv("Data/DevereuxCreek_PTdata_2018-2022wy.csv")
Venocodepth<-read.csv("Data/Venoco_PTdata_2018-2022wy.csv")


## Structure date times to be mergable and remove unneccessary columns
Phelpsdepth$Date<-as.Date(Phelpsdepth$Date,format="%Y-%m-%d")
Phelpsdepth$Datetime<-as.POSIXct(paste0(Phelpsdepth$Date," ",Phelpsdepth$Time),format="%Y-%m-%d %H:%M:%S")
Phelpsdepth<-Phelpsdepth[,c(8,4)]
Devereuxdepth$Date<-as.Date(Devereuxdepth$Date,format="%m/%d/%Y")
Devereuxdepth$Datetime<-as.POSIXct(paste0(Devereuxdepth$Date," ",Devereuxdepth$Time),format="%Y-%m-%d %H:%M:%S")
Devereuxdepth<-Devereuxdepth[,c(7,4)]
Venocodepth$Date<-as.Date(Venocodepth$Date,format="%m/%d/%Y")
Venocodepth$Datetime<-as.POSIXct(paste0(Venocodepth$Date," ",Venocodepth$Time),format="%Y-%m-%d %H:%M:%S")
Venocodepth<-Venocodepth[,c(8,4)]


## Merge TSS data with depth data and keep all TSS data (will involve some data cleaning first)
Phelpsall<-merge(Phelps_TSS,Phelpsdepth,by="Datetime",all.x=TRUE)
Venocoall<-merge(Venoco_TSS,Venocodepth,by="Datetime",all.x=TRUE)
Devereuxall<-merge(Devereux_TSS,Devereuxdepth,by="Datetime",all.x=TRUE)


##Calculate volume weighted average
# delete unneccessary columns
Phelpsall<-Phelpsall[1:190,c(1,4,8,11)]
Venocoall<-Venocoall[1:123,c(1,4,8,11)]
Devereuxall<-Devereuxall[1:64,c(1,4,8,11)]
#Get proportional depths
Phelpsall<-Phelpsall[!is.na(Phelpsall$LEVEL),]
Devereuxall<-Devereuxall[!is.na(Devereuxall$LEVEL),]
Venocoall<-Venocoall[!is.na(Venocoall$LEVEL),]

Phelpsall$Year<-format(Phelpsall$Datetime,format="%Y")
Devereuxall$Year<-format(Devereuxall$Datetime,format="%Y")
Venocoall$Year<-format(Venocoall$Datetime,format="%Y")

#Phelps
for(i in unique(Phelpsall$Year)){
  PA<-Phelpsall[Phelpsall$Year==i,]
  print(weighted.mean(PA$TSS..mg.L.,PA$LEVEL))
}

TSS.mg.L<-c(496.112,463.8917,545.9612,1043.171)
year<-c(2019,2020,2021,2022)
Phelps.VWA<-data.frame(TSS.mg.L,year)
Phelps.VWA$Site<-"Phelps"

#Devereux
for(i in unique(Devereuxall$Year)){
  DA<-Devereuxall[Devereuxall$Year==i,]
  print(weighted.mean(DA$TSS..mg.L.,DA$LEVEL))
}

TSS.mg.L<-c(102.4426,182.1591,307.4044,118.2593)
year<-c(2019,2020,2021,2022)
Devereux.VWA<-data.frame(TSS.mg.L,year)
Devereux.VWA$Site<-"Devereux"

#Venoco
for(i in unique(Venocoall$Year)){
  VA<-Venocoall[Venocoall$Year==i,]
  print(weighted.mean(VA$TSS..mg.L.,VA$LEVEL))
}

TSS.mg.L<-c(15758.14,330.6706,1703.431,28.97308)
year<-c(2019,2020,2021,2022)
Venoco.VWA<-data.frame(TSS.mg.L,year)
Venoco.VWA$Site<-"Venoco"

##merge back together
NCOS_TSS_VWM_mg_L<-rbind(Phelps.VWA,Devereux.VWA,Venoco.VWA)
NCOS_TSS_VWM_mg_L

##Need to redo with water year
