old_Dev<-read.csv("Data/nutrients/Devereux_Nutrients_2004_2011.csv")
old_Dev$timestamp_local<-as.POSIXct(old_Dev$timestamp_local,format="%Y-%m-%dT%H:%M:%S")
old_Dev$Date<-format(old_Dev$timestamp_local,format="%Y-%m-%d")
old_Dev$Date<-as.POSIXct(old_Dev$Date,format="%Y-%m-%d")
old_Dev<-old_Dev[,3:6]
colnames(old_Dev)<-c("Ammonia.um","Nitrite.Nitrate.um","Phosphate.um","Date")


new_Dev<-read.csv("Data/nutrients/Nutrients_2018-2022.csv",strip.white = TRUE)
unique(new_Dev$Site)
new_Dev$Sample.Date<-as.POSIXct(new_Dev$Sample.Date,format="%m/%d/%Y")
NCOS<-new_Dev
str(NCOS)
NCOS$wtr_yr <- getYearQuarter(NCOS$Sample.Date, firstMonth=10)
new_Dev<-new_Dev[,c(2,4:6)]
colnames(new_Dev)[1]<-"Date"
new_Dev<-new_Dev[new_Dev$Site=="Venoco"|new_Dev$Site=="Venoco Bridge",]

all_Dev<-rbind(old_Dev,new_Dev)
all_Dev$wtr_yr <- getYearQuarter(all_Dev$Date, firstMonth=10)

ggplot(all_Dev,aes(x=wtr_yr,y=Ammonia.um))+geom_boxplot()+ylim(0,50)

ggplot(all_Dev,aes(x=wtr_yr,y=Nitrite.Nitrate.um))+geom_boxplot()+ylim(0,50)

ggplot(all_Dev,aes(x=wtr_yr,y=Phosphate.um))+geom_boxplot()+ylim(0,50)


p1 <- ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~treatment)



########### comparing Phelps and devereux to Venoco
NCOS
NCOS<- within(NCOS, Site[Site == "Phelps Creek (Marymount)"] <- 'Phelps')
NCOS<- within(NCOS, Site[Site == "Devereux Creek (Ellwood)"] <- 'Devereux')
NCOS<- within(NCOS, Site[Site == "Venoco Bridge"] <- 'Venoco')
NCOS<- within(NCOS, Site[Site == "Phelps Creek (Marymount)"] <- 'Phelps')
NCOS<- within(NCOS, Site[Site == "Whittier Stormdrain"] <- 'Whittier')

NCOS<-NCOS[NCOS$Site!="Blank"&NCOS$Site!="COPR",]



unique(NCOS$Site)
ggplot(NCOS,aes(x=Site,y=Phosphate.um))+geom_boxplot()+ylim(0,50)+facet_wrap(~wtr_yr)
ggplot(NCOS,aes(x=Site,y=Ammonia.um))+geom_boxplot()+ylim(0,50)+facet_wrap(~wtr_yr)
ggplot(NCOS,aes(x=Site,y=Nitrite.Nitrate.um))+geom_boxplot()+ylim(0,50)+facet_wrap(~wtr_yr)

####Now we need to differentiate storm nutrients and base nutrients

#Add NOAA rain data- this data will be exported asa single file that can be found in git
WY08 <- read.table("Data/Rain/txtfiles/2008.txt",header=FALSE,sep="")
WY09 <- read.delim("Data/Rain/txtfiles/2009.txt",header=FALSE,sep="")
WY10 <- read.delim("Data/Rain/txtfiles/2010.txt",header=FALSE,sep="")
WY11 <- read.delim("Data/Rain/txtfiles/2011.txt",header=FALSE,sep="")
WY17 <- read.delim("Data/Rain/txtfiles/2017.txt",header=FALSE,sep="")
WY18 <- read.delim("Data/Rain/txtfiles/2018.txt",header=FALSE,sep="")
WY19 <- read.delim("Data/Rain/txtfiles/2019.txt",header=FALSE,sep="")
WY20 <- read.delim("Data/Rain/txtfiles/2020.txt",header=FALSE,sep="")
WY21 <- read.delim("Data/Rain/txtfiles/2021.txt",header=FALSE,sep="")
WY22 <- read.delim("Data/Rain/txtfiles/2022.txt",header=FALSE,sep="")
NOAA<-rbind(WY08,WY09,WY10,WY11,WY17,WY18,WY19,WY20,WY21,WY22)

NOAA_headers<-c("WBANNO", "LST_DATE", "CRX_VN", "LONGITUDE", "LATITUDE", "T_DAILY_MAX",
                "T_DAILY_MIN", "T_DAILY_MEAN","T_DAILY_AVG", "P_DAILY_CALC",
                "SOLARAD_DAILY", "SUR_TEMP_DAILY_TYPE", "SUR_TEMP_DAILY_MAX",
                "SUR_TEMP_DAILY_MIN", "SUR_TEMP_DAILY_AVG", "RH_DAILY_MAX", "RH_DAILY_MIN",
                "RH_DAILY_AVG", "SOIL_MOISTURE_5_DAILY", "SOIL_MOISTURE_10_DAILY",
                "SOIL_MOISTURE_20_DAILY", "SOIL_MOISTURE_50_DAILY", "SOIL_MOISTURE_100_DAILY",
                "SOIL_TEMP_5_DAILY", "SOIL_TEMP_10_DAILY", "SOIL_TEMP_20_DAILY",
                "SOIL_TEMP_50_DAILY", "SOIL_TEMP_100_DAILY")
colnames(NOAA)<-NOAA_headers
write.csv(NOAA,"Data/Rain/NOAA_2008-2011_2017-2022.csv")

############ reading back in the exported file
NOAA<-read.csv("Data/Rain/NOAA_2008-2011_2017-2022.csv")



