##########################################
##ADD in with our data
#Add rain to plotly graph
#pdaily calc is in mm's
NOAA_Rain<-read.csv("Data/Rain/NOAA_2008-2011_2017-2022.csv")
NOAA_Rain<-NOAA_Rain[NOAA_Rain$P_DAILY_CALC>0,]
NOAA_Rain$LST_DATE<-format(NOAA_Rain$LST_DATE,format="%Y%m%d")
NOAA_Rain$LST_DATE<-as.Date(NOAA_Rain$LST_DATE,format="%Y%m%d")
NOAA_Rain<-NOAA_Rain[,c(3,11)]
colnames(NOAA_Rain)[1]<-"Date"
colnames(NOAA_Rain)[2]<-"Rain_mm"
NOAA_Rain$Source<-"NOAA"


#County rainfall is in inches and days with no rain are omitted.
County_Rain<-read_excel("Data/Rain/440dailys.xls",skip=9)
County_Rain<-as.data.frame(County_Rain)
County_Rain$Date<-as.Date(paste0(County_Rain$year,"-",County_Rain$monthnum,"-",County_Rain$day),format="%Y-%m-%d")
County_Rain$Rain_mm<-County_Rain$dailyrain*25.4
County_Rain<-County_Rain[,8:9]
str(County_Rain)

##NOAA Rain starts on 2008-08-14, old dev starts 2003-05-17
County_Rain2<-County_Rain[County_Rain$Date<="2008-08-14" & County_Rain>= "2003-05-17",]
County_Rain2<-County_Rain2[,1751:1936]
County_Rain3 <- County_Rain2[complete.cases(County_Rain2), ]
County_Rain3$Source<-"County"

##Add number of samples to the summary table 
old_Dev<-read.csv("Data/Nutrients/sbclter_stream_chemistry_allyears_non_registered_stations_20190628.csv")
old_Dev<-old_Dev[grepl("^DV", old_Dev$site_code), ]
old_Dev$Date<-as.Date(old_Dev$timestamp_local,format="%Y-%m-%dT%H:%M:%S")
old_Dev$samples<-1
old_Dev_summary<-old_Dev[,13:14]
old_Dev_summary

#Upload and format new nutrient data from 2017-2022
new_Dev_data<-read.csv("Data/nutrients/Nutrients_2018-2022.csv",strip.white = TRUE)
new_Dev_data$Sample.Date<-as.Date(new_Dev_data$Sample.Date,format="%m/%d/%Y")
colnames(new_Dev_data)[2]<-"Date"
new_Dev_Venoco_data<-new_Dev[new_Dev_data$Site=="Venoco Bridge"|new_Dev_data$Site=="Venoco",]
new_Dev_Venoco_data$samples<-1
new_Dev_Venoco_summary<-new_Dev_Venoco_data[,c(2,12)]
new_Dev_Venoco_summary

Dev_samples_data<-rbind(new_Dev_Venoco_summary,old_Dev_summary)
Dev_samples<-aggregate(samples~Date,Dev_samples_data,FUN=sum)

#Merge county rain and NOAA rain to have a dataset that covers alll sasmple dates.
Sample_Rain<-merge(County_Rain3,NOAA_Rain,all.x = TRUE,all.y=TRUE)
Sample_Rain<-merge(Sample_Rain,Dev_samples,by="Date",all.x=TRUE,all.y=TRUE)


# Group by storm group to calculate total rainfall and find start/end dates
# Calculate the difference between consecutive dates
data <- Sample_Rain %>%
  mutate(date_diff = c(1, diff(Date)))

# Identify storm periods
data <- data %>%
  mutate(storm_group = cumsum(date_diff > 1))

#Replace NA with 0
data$Rain_mm[is.na(data$Rain_mm)] <- 0
data$samples[is.na(data$samples)] <- 0
dates_keep<-data[data$samples!=0 & data$Rain_mm!=0,]

# Group by storm group to calculate total rainfall and find start/end dates
storm_summary <- data %>%
  group_by(storm_group) %>%
  summarise(
    start_date = first(Date),
    end_date = last(Date),
    total_rainfall = sum(Rain_mm),
    sample_number = sum(samples)
  ) %>%
  ungroup() %>%
  select(-storm_group)

# Display the result
print(storm_summary)


storm_summary<-as.data.frame(storm_summary)
storm_summary$wtr_yr<-getYearQuarter(storm_summary$start_date,firstMonth = 10)

zero_sample<-storm_summary[storm_summary$sample_number==0,]
zero_rainfall<-storm_summary[storm_summary$total_rainfall==0,]
storm_sample<-storm_summary[storm_summary$sample_number!=0 & storm_summary$total_rainfall!=0,]
storm_sample_short<-aggregate(sample_number~wtr_yr,storm_sample,FUN=sum)

head(old_Dev)
head(new_Dev_Venoco_data)
old_Dev
old_Dev<-old_Dev[,c(3:5,13)]
colnames(old_Dev)<-c("Ammonia.um","Nitrite.Nitrate.um","Phosphate.um","Date")

new_Dev_Venoco_data<-new_Dev_Venoco_data[,c(2,4:6)]

Dev_rain<-merge(new_Dev_Venoco_data,old_Dev,all.x = TRUE,all.y = TRUE)
Dev_rain<-merge(Dev_rain,dates_keep,all.y = TRUE)
Dev_rain<-Dev_rain[,1:6]
Dev_rain$wtr_yr<-getYearQuarter(Dev_rain$Date,firstMonth = 10)
Dev_rain$samples<-1
Dev_rain_summary<-aggregate(samples~wtr_yr,Dev_rain,FUN=sum)




Dev_rain
############ Now display difference of calculated rain for county nad NOAA
County_Rain_overlap<-County_Rain[County_Rain>="2008-09-13" & County_Rain<="2022-12-31",]
County_Rain_overlap <- County_Rain_overlap[complete.cases(County_Rain_overlap), ]
colnames(County_Rain_overlap)[2]<-"Rain_mm_County"

NOAA_Rain_overlap<-NOAA_Rain
colnames(NOAA_Rain_overlap)[2]<-"Rain_mm_NOAA"

Rain_overlap<-merge(County_Rain_overlap,NOAA_Rain_overlap,all.x = TRUE,all.y=TRUE)
Rain_overlap


##Replace NA's with 0's 
Rain_overlap<-Rain_overlap[,1:3]
Rain_overlap[is.na(Rain_overlap)] <- 0

# calculate the percent difference of the 2 columns 
Rain_overlap$Co_No_Diff <- Rain_overlap$Rain_mm_County-Rain_overlap$Rain_mm_NOAA

mean(Rain_overlap$Co_No_Diff)
hist(Rain_overlap$Co_No_Diff,breaks = 100)
str(Rain_overlap)

## Results show that County rain is often less than NOAA rain



