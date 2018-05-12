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
library(readxl)
setwd('../../DATA602/OpenBaltimore/')
data_311 <- read.csv('311_Customer_Service_Requests.csv')

#---------View the headers---------
names(data_311)

#---------Remove unnecessary columns---------
new_data_311 <- data_311[c(3:15)]
data_311 <- new_data_311
names(data_311)
glimpse(data_311)


# ------------Remove all rows containing Rat Rubout & Bulk-Scheduled-------------
data_311 <- data_311[ grep("SW-Rat Rubout", data_311$SRType, invert = TRUE) , ]
data_311 <- data_311[ grep("SSW-Bulk-Scheduled", data_311$SRType, invert = TRUE) , ]
data_311 <- data_311[ grep("SW-Bulk-Scheduled", data_311$SRType, invert = TRUE) , ]

high_freq_srtype <- data.frame(data_311 %>% group_by(data_311$SRType) %>% tally())
high_freq_srtype_order <- high_freq_srtype[order(-high_freq_srtype$n),]
head(high_freq_srtype_order,n = 20)

#----------------Check NULL values------------
is.na(data_311)
data_311[!complete.cases(data_311),]


#----------------Data based on SRType SANITATION------------
# Take only HCD-Sanitation Property with status CLOSED
srtype_pot <- filter(data_311, SRType == "TRM-Potholes" & SRStatus == "CLOSED")
summary(srtype_pot)

# Calculate time take
createdDate <- mdy_hms(srtype_pot$CreatedDate, tz = "America/New_York")
statusDate <- mdy_hms(srtype_pot$StatusDate, tz = "America/New_York")
TimeTaken = interval(createdDate, statusDate)/dhours()    # interval in hours
srtype_pot$TimeTaken <- TimeTaken

names(srtype_pot)
glimpse(srtype_pot)
nrow(srtype_pot)    # --26623

#-----------------START POTHOLES--------------------
pot_var <- c("TimeTaken","Neighborhood")
pot_table <- srtype_pot[pot_var]
names(pot_table)
nrow(pot_table)
glimpse(pot_table)


# Get the mean of time taken for each neighborhood
pot_avg <- aggregate(TimeTaken ~ Neighborhood, pot_table, mean)
print(pot_avg[2], row.names = FALSE)
pot_desc <- pot_avg[order(-pot_avg$TimeTaken),]

#-------MOST TIME TAKEN----------
pot_high_time <- head(pot_desc, n=20)
pot_high_time

ggplot(pot_high_time,aes(x=reorder(Neighborhood, -TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Highest time taking neighborhood for potholes') +
  scale_y_continuous(limits = c(0,200)) 

#-------LEAST TIME TAKEN----------
pot_asc <- pot_avg[order(pot_avg$TimeTaken),]
pot_low_time <- head(pot_asc, n=20)
pot_low_time

ggplot(pot_low_time,aes(x=reorder(Neighborhood, TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Lowest time taking neighborhood for potholes') +
  scale_y_continuous(limits = c(0,200)) 

#--------Mixture time taken-----------
pot_mix_lowest <- head(pot_desc, n=5)
pot_mix_highest <- head(pot_asc, n=5)

pot_mix <- rbind(pot_mix_lowest, pot_mix_highest)

ggplot(pot_mix,aes(x=reorder(Neighborhood, TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Highest/Lowest time taking neighborhood for potholes') +
  scale_y_continuous(limits = c(0,200)) 

#------Number of neighborhood----------
nrow(pot_asc)

#-----Display neighbordhood having highest/lowest frequency---------------
pot_freq <- aggregate(TimeTaken~Neighborhood, pot_table, FUN=length)
colnames(pot_freq)[2] <- "Count"
pot_sort <- pot_freq[order(-pot_freq$Count),]

pot_high_freq <- head(pot_sort,10)
pot_low_freq <- tail(pot_sort,10)

pot_mix_freq <- rbind(pot_low_freq, pot_high_freq)

ggplot(pot_mix_freq,aes(x=reorder(Neighborhood, Count), y=Count))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=Count), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Highest/Lowest Frequency neighborhood for potholes') +
  scale_y_continuous(limits = c(0,600))


#-----------After dataset creation------------
library(MASS)
pot_data <- read_excel('Population_By_Neighborhood.xlsx', sheet = 'pot_table')
names(pot_data)

pot_model_all <- lm(TimeTaken~Population + White + 
                        Blk_AfAm + Pop_dens + Housing + 
                        Occupied + Vacant , data = pot_data)
summary(pot_model_all)

pot_model_pop <- lm(TimeTaken~Population, data = pot_data)
summary(pot_model_pop)

pot_model_black <- lm(TimeTaken~Blk_AfAm, data = pot_data)
summary(pot_model_black)

pot_model_white <- lm(TimeTaken~White, data = pot_data)
summary(pot_model_white)
