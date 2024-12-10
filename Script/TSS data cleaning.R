#This is the first if the scripts that needs to be ran.
#First download needed csv's and libraries
library(readxl)
library(ggplot2)
TSS<-read_xlsx("data/TSS_allyears.xlsx",sheet=3)
TSS<-as.data.frame(TSS)
sample_locations<-read_xlsx("data/TSS_allyears.xlsx",sheet=4)
sample_locations<-sample_locations[!is.na(sample_locations$Location),] #remove NA's from dataframe

##Upload new file that has some 2020 data in it to reduce missing data sheet
locations2020<-read_xlsx("data/NCOS_Stormwater_SSC_Data_WY2020.xlsx")
colnames(locations2020)<-c("Sample Number","Location","Date","Time") #rename to mathc other files
locations2020<-locations2020[,1:4] #Select only necessary columns
locations2020$`Sample type`<-NA #Add Sample Type column to allow rbind
locations2020<-locations2020[!is.na(locations2020$Location),] #remove NA's from dataframe

##Load 2021 data
data2021<-read_xlsx("data/NCOS_Stormwater_Sample_Log_WY2021.xlsx")
data2021<-data2021[,c(1:3,5:6)] #select only necessary columns
colnames(data2021)<-c("Location","Sample type","Date","Time","Sample Number") #rename to match other files
data2021<-data2021[!is.na(data2021$Location),]#remove NA's from dataframe

#check data titles
allsamplelocations<-rbind(data2021,locations2020,sample_locations)
#change time structure
allsamplelocations$Time<-format(allsamplelocations$Time,format("%H:%M:%S"))
allsamplelocations$Date<-format(allsamplelocations$Date,format("%Y-%m-%d"))
#check all data
locations2020<-as.data.frame(locations2020)
sample_locations<-as.data.frame(sample_locations)
locations2020<-locations2020[order(locations2020$`Sample Number`),]
sample_locations<-sample_locations[order(sample_locations$`Sample Number`),]
allsamplelocations<-as.data.frame(allsamplelocations)
#remove negative values
TSS<-TSS[TSS$`TSS (mg/L)`>0 & TSS$`TSS (mg/L)`!="see above",]

#Check the notes and delete bad data
TSS<-TSS[TSS$`Keepfor analysis`=="Y",]

#Remove a and b so that titles match
TSS$`Sample Number`<-gsub("a","",as.character(TSS$`Sample Number`))
TSS$`Sample Number`<-gsub("b","",as.character(TSS$`Sample Number`))

#Merge with location and date
TSS<-TSS[order(TSS$`Sample Number`),]
allsamplelocations<-allsamplelocations[order(allsamplelocations$`Sample Number`),]
data<-merge(TSS,allsamplelocations,by="Sample Number",all.x=TRUE)

#change Location names to match
data$Location<-gsub("NCOS-P","Phelps",data$Location)
data$Location<-gsub("NCOS-V","Venoco",data$Location)
data$Location<-gsub("NCOS-D","Devereux",data$Location)
data$Location<-gsub("NCOS-W","Whittier",data$Location)
unique(data$Location)

#Remove base level observations because there would be no flow at base level. 
#I am removing dates with less than 0.1 inches of rain- assuming this would produce a negigable amout of flow
data<-data[data$Date!="2019-12-15"&data$Date!="2020-01-08"& data$Date!="2020-12-04"
           &data$Date!="2021-03-03"&data$Date!="2021-11-09"&data$Date!="2022-01-03"
           &data$Date!="2019-12-16",]
getwd()
#print final dataframe
write.csv(data,"C:/Users/rickard/Documents/test/Data/2020_2021_TSS_Fulldataset.csv")





