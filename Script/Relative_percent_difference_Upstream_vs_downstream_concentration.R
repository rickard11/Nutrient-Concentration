#Upload and format new nutrient data from 2017-2022
new_Dev<-read.csv("Data/nutrients/Nutrients_2018-2022.csv",strip.white = TRUE)
new_Dev$Sample.Date<-as.Date(new_Dev$Sample.Date,format="%m/%d/%Y")
colnames(new_Dev)[2]<-"Date"
new_Dev$Environment<-"Wetland" #naming environment for later merging
NCOS_All<-new_Dev
#create date time and Year
NCOS_All$Datetime<-as.POSIXct(paste0(NCOS_All$Date," ",NCOS_All$Time),format="%Y-%m-%d %H:%M")
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
NCOS_All$wtr_yr<-getYearQuarter(NCOS_All$Date,firstMonth = 10)
#Create better location labels
NCOS_All<-NCOS_All[NCOS_All$Site!="Blank"&NCOS_All$Site!="COPR",]
NCOS_All<- within(NCOS_All, Site[Site == "Phelps Creek (Marymount)"] <- 'Phelps')
NCOS_All<- within(NCOS_All, Site[Site == "Whittier Stormdrain"] <- 'Whittier')
NCOS_All<- within(NCOS_All, Site[Site == "Venoco Bridge"] <- 'Venoco')
NCOS_All<- within(NCOS_All, Site[Site == "Devereux Creek (Ellwood)"] <- 'Devereux')

library(tidyr)

##Begin
unique(NCOS_All$Notes)

NCOS_All<-NCOS_All[NCOS_All$Notes!="cracked vial",]
#subdivide into each nutrient
NCOS_All_P<-NCOS_All[,c(1,4,13)]
NCOS_All_P<-na.omit(NCOS_All_P)
NCOS_All_N<-NCOS_All[,c(1,5,13)]
NCOS_All_N<-na.omit(NCOS_All_N)
NCOS_All_A<-NCOS_All[,c(1,6,13)]
NCOS_All_A<-na.omit(NCOS_All_A)


Phosphorus<- NCOS_All_P  %>%
  group_by(Datetime) %>%
  pivot_wider(names_from = Site, 
              values_from = Phosphate.um,
              values_fn = mean)

Phosphorus<-as.data.frame(Phosphorus)
#seperate out Devereux
dev<-Phosphorus[,c(1,5)]
dev<-na.omit(dev)
dev$Date<-as.Date(dev$Datetime)
dev_daily<-aggregate(Devereux~Date,dev,FUN=mean)
#remove devereux samples
Phosphorus<-Phosphorus[,1:4]
Phosphorus<-na.omit(Phosphorus)
Phosphorus$Date<-as.Date(Phosphorus$Datetime)
#Bring Phosphorus back in
Phosphorus_remerge<-merge(Phosphorus,dev,by="Date",all.x=TRUE)
Phosphorus_remerge<-Phosphorus_remerge[,c(1,3:5,7)]
Phosphorus_remerge<-na.omit(Phosphorus_remerge)
Phosphorus_remerge$Phelps_proportion<-Phosphorus_remerge$Phelps*0.51
Phosphorus_remerge$Whittier_proportion<-Phosphorus_remerge$Whittier*0.08
Phosphorus_remerge$Devereux_proportion<-Phosphorus_remerge$Devereux*0.42
Phosphorus_remerge$proportional_nutrients<-Phosphorus_remerge$Phelps_proportion+Phosphorus_remerge$Whittier_proportion+Phosphorus_remerge$Devereux_proportion
Phosphorus_remerge <- Phosphorus_remerge %>%
  mutate(
    rpd = (abs(proportional_nutrients-Venoco) / ((Venoco + proportional_nutrients) / 2)) * 100,
    direction = ifelse(proportional_nutrients>Venoco , "positive", "negative"),
    rpd = ifelse(direction == "negative", -rpd, rpd)
  ) %>%
  select(-direction)  # Optionally remove the direction column if not needed

mean(Phosphorus_remerge$rpd)
##37 percent removal of phosphorus


#nitrogen
Nitrogen<- NCOS_All_N  %>%
  group_by(Datetime) %>%
  pivot_wider(names_from = Site, 
              values_from = Nitrite.Nitrate.um,
              values_fn = mean)

Nitrogen<-as.data.frame(Nitrogen)
#seperate out Devereux
dev<-Nitrogen[,c(1,5)]
dev<-na.omit(dev)
dev$Date<-as.Date(dev$Datetime)
dev_daily<-aggregate(Devereux~Date,dev,FUN=mean)
#remove devereux samples
Nitrogen<-Nitrogen[,1:4]
Nitrogen<-na.omit(Nitrogen)
Nitrogen$Date<-as.Date(Nitrogen$Datetime)
#Bring Nitrogen back in
Nitrogen_remerge<-merge(Nitrogen,dev,by="Date",all.x=TRUE)
Nitrogen_remerge<-Nitrogen_remerge[,c(1,3:5,7)]
Nitrogen_remerge<-na.omit(Nitrogen_remerge)
Nitrogen_remerge$Phelps_proportion<-Nitrogen_remerge$Phelps*0.51
Nitrogen_remerge$Whittier_proportion<-Nitrogen_remerge$Whittier*0.08
Nitrogen_remerge$Devereux_proportion<-Nitrogen_remerge$Devereux*0.42
Nitrogen_remerge$proportional_nutrients<-Nitrogen_remerge$Phelps_proportion+Nitrogen_remerge$Whittier_proportion+Nitrogen_remerge$Devereux_proportion
Nitrogen_remerge<-Nitrogen_remerge[Nitrogen_remerge$Venoco>=0,]
Nitrogen_remerge <- Nitrogen_remerge %>%
  mutate(
    rpd = (abs(proportional_nutrients-Venoco) / ((Venoco + proportional_nutrients) / 2)) * 100,
    direction = ifelse(proportional_nutrients>Venoco , "positive", "negative"),
    rpd = ifelse(direction == "negative", -rpd, rpd)
  ) %>%
  select(-direction)  # Optionally remove the direction column if not needed

mean(Nitrogen_remerge$rpd)
##119 percent removal of nitrogen

#Ammonia
Ammonia<- NCOS_All_A  %>%
  group_by(Datetime) %>%
  pivot_wider(names_from = Site, 
              values_from = Ammonia.um,
              values_fn = mean)

Ammonia<-as.data.frame(Ammonia)
#seperate out Devereux
dev<-Ammonia[,c(1,5)]
dev<-na.omit(dev)
dev$Date<-as.Date(dev$Datetime)
dev_daily<-aggregate(Devereux~Date,dev,FUN=mean)
#remove devereux samples
Ammonia<-Ammonia[,1:4]
Ammonia<-na.omit(Ammonia)
Ammonia$Date<-as.Date(Ammonia$Datetime)
#Bring Ammonia back in
Ammonia_remerge<-merge(Ammonia,dev,by="Date",all.x=TRUE)
Ammonia_remerge<-Ammonia_remerge[,c(1,3:5,7)]
Ammonia_remerge<-na.omit(Ammonia_remerge)
Ammonia_remerge$Phelps_proportion<-Ammonia_remerge$Phelps*0.51
Ammonia_remerge$Whittier_proportion<-Ammonia_remerge$Whittier*0.08
Ammonia_remerge$Devereux_proportion<-Ammonia_remerge$Devereux*0.42
Ammonia_remerge$proportional_nutrients<-Ammonia_remerge$Phelps_proportion+Ammonia_remerge$Whittier_proportion+Ammonia_remerge$Devereux_proportion
Ammonia_remerge <- Ammonia_remerge %>%
  mutate(
    rpd = (abs(proportional_nutrients-Venoco) / ((Venoco + proportional_nutrients) / 2)) * 100,
    direction = ifelse(proportional_nutrients>Venoco , "positive", "negative"),
    rpd = ifelse(direction == "negative", -rpd, rpd)
  ) %>%
  select(-direction)  # Optionally remove the direction column if not needed

mean(Ammonia_remerge$rpd)
##-22 percent removal of Ammonia

# This is disproportionally as a result of really high Ammonia values from a shart time period 2020-03-11 through 03-16


