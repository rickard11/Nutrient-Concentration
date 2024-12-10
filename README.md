# Devereux Slough Solinst Levelogger data 2018-
---

The devereux Slough is a temperary open closed estuary (TOCE) we use pressure transducers across the slough and associated wetlands and inlets to monitor water quantity. The Hydrology_equipment_data.csv lists the location of each pressure transducer.

## Description of the data and file structure

Each data set has a LEVEL column and a WSE_ft column. The LEVEL column is how much water is above the pressure transducer and does not translate to water depth as most pressure transducers are not at the bottom of the water body. LEVEL is produced by the solinst software and is representating values compensated for barometric pressure. The dataset is not, however corrected for device malfunctions. There are times that the device seem to malfunction based on our knowledge of the stream level and numbers are not cleaned for such malfunctions. Missing data are representative of times at which the sensors were sent out for maintenance. WSE_ft is the water surface elevation in feet. Using known pt depth and known local elevation we convert the Level into water surface elevation.

LEVEL is in feet, Temperature is in C and Conductivity is in uS/cm. 
All pressure transducers are Solinst brand Solinst LT record level and temperature and Solinst LTE record level, temperature and electroconductivity. Datasets with Conductivity are recorded with a Solinst LTE and datasets with no conductivity readings are recorded wtih Solinst LT.


## Sharing/Access information

Data was downloaded quarterly and combined thereafter. Expect yearly data updates near the begining of the year (jan/feb). Data is also available by emailing rickard@ucsb.edu.
