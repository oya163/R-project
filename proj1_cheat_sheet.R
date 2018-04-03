#---------Set working directory and load dataset
library(tidyverse)
setwd('DATA602/OpenBaltimore/')
data_311 <- read.csv('311_Customer_Service_Requests.csv')

# ------------Remove all rows containing Rat Rubout-------------

data_311 <- data_311[ grep("SW-Rat Rubout", data_311$SRType, invert = TRUE) , ]
data_311 <- data_311[ grep("SSW-Bulk-Scheduled", data_311$SRType, invert = TRUE) , ]
high_freq_srtype <- data.frame(data_311 %>% group_by(data_311$SRType) %>% tally())
high_freq_srtype_order <- high_freq_srtype[order(-high_freq_srtype$n),]
head(high_freq_srtype_order)

#------------------SORT---------------------
high_freq_srtype[order(-high_freq_srtype$n),]

#----------------check if any SRTYPE has more that agency---------
water_leak <- subset(data_311$Agency, data_311$SRType == "WW Water Leak")
water_leak <- subset(data_311$Agency, data_311$SRType %like% "WW Water Leak")
parking_complaints <- subset(data_311$Agency, data_311$SRType == "TRS-Parking Complaints")
summary(water_leak)

#----------------List based on Agency------------
dept_transport = subset(data_311$SRType, data_311$Agency %like% "Department of Transportation")
dept_transport %>% unique()
head(summary(dept_transport))

waste_water = subset(data_311$SRType, data_311$Agency %like% "Bureau of Water and Waste Water") 
waste_water %>% unique()

mayor_it = subset(data_311$SRType, data_311$Agency %like% "Mayors Office of Information Technology") 
mayor_it %>% unique()
summary(mayor_it)

southwest_data = subset(data_311$GeoLocation, data_311$Neighborhood == "SOUTHWEST")
glimpse(southwest_data)
head(southwest_data,n=1)
head_data <- head(southwest_data,1)

#-------set max print options
options(max.print=1000)
 
data_311$Agency %>% summary()
 
 
#---------List of agencies
agencies = unique(data_311$Agency)
agencies
 
#---------List of SRStatus
status = unique(data_311$SRStatus)
length(status)
 
#---------List of MethodReceived
methods = unique(data_311$MethodReceived)
methods

unique(data_311$Neighborhood)
head(data_311[ grep("MADISON", data_311$Neighborhood),])


#-----------------CENSUS--------------------#
data_census <- read.csv('BNIA_Census_2010.csv')
glimpse(data_census)
length(data_census$CSA2010)

data_cen_2015 <- read.csv('Census_Demographics__2015__data.csv')
glimpse(data_cen_2015)
length(data_cen_2015$OBJECTID)

#-----------------CENSUS--------------------#
zipcode = unique(data_311$ZipCode)
zipcode
























 
 