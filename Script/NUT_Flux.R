library(tidyr)
#merge with nutrient data
#use existing number to fill in gaps
#Phelpsstorms<-Phelpsstorms[,c(1,4:7)]
Phelps_nut<-Phelps_nut[,c(4:6,8)]
Phelpsflux<-merge(Phelps_nut,Phelpsstorms,by="Datetime",all.x=TRUE,all.y = TRUE)
#write.csv(Phelpsflux,"Data/phelpsflux_test.csv")
#There are a lot of NA's in phelps flux because pt is at 15 min variables and nutrient is sporatic. We will later interpolate missing nut data
Phelpsflux<-Phelpsflux[!is.na(Phelpsflux$Datetime),]
Phelpsflux$wtr_yr <- getYearQuarter(Phelpsflux$Datetime, firstMonth=10)#GetYear function is written in data cleaning script

#use existing number to fill in gaps
na_indices <- which(is.na(Phelpsflux$Phosphate.um))
# Interpolate NA values
unique(Phelpsflux$Phosphate.um)
Phelpsflux$Phosphate.um[na_indices] <- approx(seq_along(Phelpsflux$Phosphate.um)
                                            [!is.na(Phelpsflux$Phosphate.um)],Phelpsflux$Phosphate.um
                                            [!is.na(Phelpsflux$Phosphate.um)], xout = na_indices)$y

Phelpsflux$Nitrite.Nitrate.um[na_indices] <- approx(seq_along(Phelpsflux$Nitrite.Nitrate.um)
                                              [!is.na(Phelpsflux$Nitrite.Nitrate.um)],Phelpsflux$Nitrite.Nitrate.um
                                              [!is.na(Phelpsflux$Nitrite.Nitrate.um)], xout = na_indices)$y
Phelpsflux$Ammonia.um[na_indices] <- approx(seq_along(Phelpsflux$Ammonia.um)
                                                    [!is.na(Phelpsflux$Ammonia.um)],Phelpsflux$Ammonia.um
                                                    [!is.na(Phelpsflux$Ammonia.um)], xout = na_indices)$y

#I also need to seperate into years because the change from one water year to the next (and one storm to the next)
#is being interpolated based on the previous value which would not make sense.

#multiple by number of litres
Phelpsflux$umP_15m<-Phelpsflux$Phosphate.um*Phelpsflux$discharge_litre_15m
Phelpsflux$umN_15m<-Phelpsflux$Nitrite.Nitrate.um*Phelpsflux$discharge_litre_15m
Phelpsflux$umA_15m<-Phelpsflux$Ammonia.um*Phelpsflux$discharge_litre_15m

#aggregate the sum of um N, P and A per year
yrlyphelpsflux<-aggregate(cbind(umP_15m,umN_15m,umA_15m)~wtr_yr,Phelpsflux,FUN=sum)

#convert to kg
yrlyphelpsflux$MP_yr<-yrlyphelpsflux$umP_15m*0.000001
yrlyphelpsflux$MN_yr<-yrlyphelpsflux$umN_15m*0.000001
yrlyphelpsflux$MA_yr<-yrlyphelpsflux$umA_15m*0.000001


#divide by ha
yrlyphelpsflux$MP_yr_ha<-yrlyphelpsflux$MP_yr/450
yrlyphelpsflux$MN_yr_ha<-yrlyphelpsflux$MN_yr/450
yrlyphelpsflux$MA_yr_ha<-yrlyphelpsflux$MA_yr/450



#compare to Melacks findings 

#split into seperate water years
str(Phelpsdischarge)
Pdis2020<-Phelpsstorms[Phelpsstorms$Datetime>="2019-10-01 00:00:00"&
                         Phelpsstorms$Datetime<="2020-10-01 00:00:00",]
Pdis2020$discharge.cf15min<-Pdis2020$discharge.cfs*60*15
Pdis2020<-Pdis2020[!is.na(Pdis2020$discharge.cf15min),]


Pdis2021<-Phelpsstorms[Phelpsstorms$Datetime>="2020-10-01 00:00:00"&
                         Phelpsstorms$Datetime<="2021-10-01 00:00:00",]
Pdis2021$discharge.cf15min<-Pdis2021$discharge.cfs*60*15
Pdis2021<-Pdis2021[!is.na(Pdis2021$discharge.cf15min),]

Pdis2022<-Phelpsstorms[Phelpsstorms$Datetime>="2021-10-01 00:00:00"&
                         Phelpsstorms$Datetime<="2022-10-01 00:00:00",]
Pdis2022$discharge.cf15min<-Pdis2022$discharge.cfs*60*15
Pdis2022<-Pdis2022[!is.na(Pdis2022$discharge.cf15min),]




sum(Pdis2020$discharge.cf15min)
sum(Pdis2021$discharge.cf15min)
sum(Pdis2022$discharge.cf15min)


#Determine area of Phelps
## Phelps = ~450 ha
## Devereux = ~250 ha
## Venoco =~100 + Devereux + Phelps
#Multiple nutrients by estimate liters of water for given time period
#divide for number of Ha (size of Phelps watershed) final answer will be kg/ha/ water year
