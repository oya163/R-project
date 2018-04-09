#############################
# Author - Oyesh Mann Singh
# Dataset - 311_Customer_Service_Request
# Link - https://data.baltimorecity.gov/City-Services/311-Customer-Service-Requests/9agw-sxsr
# Description - 
#   Performing Data wrangling for various SRTypes
#   Performing regression/classification/clustering analysis
#   
# Course - Data Analaysis and Machine Learning (DATA 602)
# Due Date - 2nd week of May, 2018
  

#---------Set working directory and load dataset--------------
library(tidyverse)
library(lubridate)
setwd('DATA602/OpenBaltimore/')
data_311 <- read.csv('311_Customer_Service_Requests.csv')

#---------View the headers---------
names(data_311)

#---------Remove unnecessary columns---------
new_data_311 <- data_311[c(3:15)]
data_311 <- new_data_311
glimpse(data_311)


# ------------Remove all rows containing Rat Rubout & Bulk-Scheduled-------------
data_311 <- data_311[ grep("SW-Rat Rubout", data_311$SRType, invert = TRUE) , ]
data_311 <- data_311[ grep("SSW-Bulk-Scheduled", data_311$SRType, invert = TRUE) , ]
data_311 <- data_311[ grep("SW-Bulk-Scheduled", data_311$SRType, invert = TRUE) , ]

high_freq_srtype <- data.frame(data_311 %>% group_by(data_311$SRType) %>% tally())
high_freq_srtype_order <- high_freq_srtype[order(-high_freq_srtype$n),]
head(high_freq_srtype_order,n = 10)

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


#----------------Data based on SRType SANITATION------------
# Take only HCD-Sanitation Property with status CLOSED
srtype_sanit <- filter(data_311, SRType == "HCD-Sanitation Property" & SRStatus == "CLOSED")
summary(srtype_sanit)

# Calculate time take
createdDate <- mdy_hms(srtype_sanit$CreatedDate, tz = "America/New_York")
statusDate <- mdy_hms(srtype_sanit$StatusDate, tz = "America/New_York")
TimeTaken = interval(createdDate, statusDate)/dminutes()    # interval in minutes

srtype_sanit$TimeTaken <- TimeTaken

srtype_sanit$createdDay = weekdays(createdDate)
srtype_sanit$StatusDay = weekdays(statusDate)

names(srtype_sanit)
summary(srtype_sanit) 
nrow(srtype_sanit)    # --150769

# data_311 %>% filter(SRType == "HCD-Sanitation Property") %>% group_by(Neighborhood) %>% summarise(Count = n())


#-------set max print options-----------
options(max.print=1000)
 
data_311$Agency %>% summary()
 
 
#---------List of agencies-----------
agencies = unique(data_311$Agency)
agencies
 
#---------List of SRStatus-----------
status = unique(data_311$SRStatus)
length(status)
 
#---------List of MethodReceived-----------
methods = unique(data_311$MethodReceived)
methods

unique(data_311$Neighborhood)
head(data_311[ grep("MADISON", data_311$Neighborhood),])


#-----------------CENSUS--------------------
data_census <- read.csv('BNIA_Census_2010.csv')
glimpse(data_census)
length(data_census$CSA2010)

data_cen_2015 <- read.csv('Census_Demographics__2015__data.csv')
glimpse(data_cen_2015)
length(data_cen_2015$OBJECTID)

#-----------------Zipcode--------------------
zipcode = unique(data_311$ZipCode)
zipcode

#-----------------SRType--------------------
srtype = unique(data_311$SRType)
length(srtype)
srtype

#-----------------Get time zone--------------------
grep("America",OlsonNames(),value=TRUE)

#-----------------Working with date and time--------------------
# lubridate package required
createdDate <- mdy_hms(srtype_sanit$CreatedDate, tz = "America/New_York")
statusDate <- mdy_hms(srtype_sanit$StatusDate, tz = "America/New_York")

# interval in minutes
time_period = interval(createdDate, statusDate)/dminutes()


#-----------------Checking SRType having NULL outcome--------------------
summary(srtype_sanit$Outcome)

onlyOutcome = subset(srtype_sanit, srtype_sanit$Outcome == "" )
head(onlyOutcome)

unique(srtype_sanit$Outcome)
summary(srtype_sanit$Outcome)

#---------Predict Outcome----------------
# What does the Outcome = "" signify? Missing value? or Service Request solved smoothly??
# Because we can predict the Outcome given SRType, Neighborhood, MethodReceived, DayOfWeek


#-----------------Things to do--------------------
# For a specific SRType, 
# perform TimeTaken ~ Neighborhood + ZipCode + MethodReceived + CreatedDay
# Create a table of top 10 service calls

# perform SRType ~ Neighborhood + DayOfWeek

#-----------------Predict time taken--------------------
# perform TimeTaken ~ Neighborhood + MethodReceived + CreatedDay
sanit_var <- c("TimeTaken","Neighborhood","MethodReceived","createdDay")
sanit_table <- srtype_sanit[sanit_var]
glimpse(sanit_table)

set.seed(1324)
TrainIndex = sample(1:nrow(sanit_table), round(0.75 * nrow(sanit_table)))
sanit_table_train = sanit_table[TrainIndex, ]
sanit_table_test = sanit_table[-TrainIndex, ]

# Things doing currently
TimeTakenModel <- glm(TimeTaken ~ factor(Neighborhood)
                      + factor(MethodReceived) 
                      + factor(createdDay), data=sanit_table_train)
summary(TimeTakenModel)

TimeTakenModel <- lm(TimeTaken ~ Neighborhood
                      + MethodReceived 
                      + createdDay, data=sanit_table_train)

summary(TimeTakenModel)

summary(sanit_table$createdDay)

typeof(sanit_table$TimeTaken)

TimeTakenModel.res = resid(TimeTakenModel)

# plot(sanit_table$Neighborhood, TimeTakenModel.res, ylab = "Residuals", xlab = "Neighborhood", main = "Residual Plot" )

ggplot(data = sanit_table, mapping = aes(x = TimeTaken)) +
  geom_histogram(aes(fill = createdDay), color = "white", bins = 15) +
  facet_wrap(~createdDay, nrow = 7) +
  labs(title = "Distribution of TimeTaken by createdDay", x = "TimeTaken", y = "Total time taken") +
  theme_light()

# Plot Number of service requested per day
ggplot(sanit_table, aes(createdDay)) + geom_bar()

# #----------TimeTaken Model by Neighbordhood---------
# ttModel_hood <- lm(TimeTaken ~ Neighborhood, data = sanit_table)
# summary(ttModel_hood)
# 
# #----------TimeTaken and Neighborhood only---------
# hood_tt_var <- c("TimeTaken","Neighborhood")
# hood_tt_data <- srtype_sanit[hood_tt_var]
# 
# hood_tt_avg <- hood_tt_data %>% group_by(Neighborhood) %>% summarise(AvgTimeTaken = mean(TimeTaken))
# 
# nrow(hood_tt_avg)
# 
# length(unique(sanit_table$Neighborhood))
# 
# glimpse(hood_tt_avg)
# 
# hood_tt_model <- glm(AvgTimeTaken ~ factor(Neighborhood), data = hood_tt_avg)
# summary(hood_tt_model)
# 
# ggplot(hood_tt_avg) + geom_bar()

