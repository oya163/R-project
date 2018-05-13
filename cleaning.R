#############################
# Author - Oyesh Mann Singh
# Dataset - 311_Customer_Service_Request
# Link - https://data.baltimorecity.gov/City-Services/311-Customer-Service-Requests/9agw-sxsr
# Description - 
#   Performing Data wrangling for various SRTypes
#   Performing regression analysis for Cleaning SRType
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
data_311 <- data_311[c(3:15)]
names(data_311)
glimpse(data_311)


# ------------Remove all rows containing Rat Rubout & Bulk-Scheduled-------------
data_311 <- data_311[ grep("SW-Rat Rubout", data_311$SRType, invert = TRUE) , ]
data_311 <- data_311[ grep("SSW-Bulk-Scheduled", data_311$SRType, invert = TRUE) , ]
data_311 <- data_311[ grep("SW-Bulk-Scheduled", data_311$SRType, invert = TRUE) , ]

# Selecting high frequency table
high_freq_srtype <- data.frame(data_311 %>% group_by(data_311$SRType) %>% tally())
high_freq_srtype_order <- high_freq_srtype[order(-high_freq_srtype$n),]
srtype_sorted <- head(high_freq_srtype_order,n = 20)

# Renaming columns
colnames(srtype_sorted)[1] <- "SRType"
colnames(srtype_sorted)[2] <- "Count"
srtype_sorted

ggplot(srtype_sorted,aes(x=reorder(SRType, -Count), y=Count))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=Count), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Most requested SRTypes', x="SRType", y="Count") +
  scale_y_continuous(limits = c(0,156000)) 

#----------------Check NULL values------------
is.na(data_311)
data_311[!complete.cases(data_311),]


#----------------Data based on SRType SANITATION------------
# Take only HCD-Sanitation Property with status CLOSED
srtype_cleaning <- filter(data_311, SRType == "SW-Cleaning" & SRStatus == "CLOSED")
summary(srtype_cleaning)

#-----------Experimental---------
# names(srtype_cleaning)
# test_srtype <- subset(srtype_cleaning, select=c(3,7,8,9,10,11))
# test_srtype


# Calculate time take
createdDate <- mdy_hms(srtype_cleaning$CreatedDate, tz = "America/New_York")
statusDate <- mdy_hms(srtype_cleaning$StatusDate, tz = "America/New_York")
TimeTaken = interval(createdDate, statusDate)/dhours()    # interval in hours
srtype_cleaning$TimeTaken <- TimeTaken

names(srtype_cleaning)
glimpse(srtype_cleaning)
nrow(srtype_cleaning)    # --32575

#-----------------START STREET cleaningS--------------------
cleaning_var <- c("TimeTaken","Neighborhood")
cleaning_table <- srtype_cleaning[cleaning_var]
names(cleaning_table)
nrow(cleaning_table)
glimpse(cleaning_table)


# Get the mean of time taken for each neighborhood
cleaning_avg <- aggregate(TimeTaken ~ Neighborhood, cleaning_table, mean)

# Print neighborhood only
print(cleaning_avg[1], row.names = FALSE)

# Print time taken only
print(cleaning_avg[2], row.names = FALSE)

cleaning_desc <- cleaning_avg[order(-cleaning_avg$TimeTaken),]

#-------MOST TIME TAKEN----------
cleaning_high_time <- head(cleaning_desc, n=20)
cleaning_high_time

ggplot(cleaning_high_time,aes(x=reorder(Neighborhood, -TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Highest time taking neighborhoods for cleaningholes SRType', x="Neighborhoods") +
  scale_y_continuous(limits = c(0,450)) 

#-------LEAST TIME TAKEN----------
cleaning_asc <- cleaning_avg[order(cleaning_avg$TimeTaken),]
cleaning_low_time <- head(cleaning_asc, n=20)
cleaning_low_time

ggplot(cleaning_low_time,aes(x=reorder(Neighborhood, TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Lowest time taking neighborhood for cleaningholes', x="Neighborhoods") +
  scale_y_continuous(limits = c(0,450)) 

#--------Mixture time taken-----------
cleaning_mix_lowest <- head(cleaning_desc, n=5)
cleaning_mix_highest <- head(cleaning_asc, n=5)

cleaning_mix <- rbind(cleaning_mix_lowest, cleaning_mix_highest)

ggplot(cleaning_mix,aes(x=reorder(Neighborhood, TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Highest/Lowest time taking neighborhood for cleaningholes', x="Neighborhoods") +
  scale_y_continuous(limits = c(0,450)) 

#------Number of neighborhood----------
nrow(cleaning_asc)

#-----Display neighbordhood having highest/lowest frequency---------------
cleaning_freq <- aggregate(TimeTaken~Neighborhood, cleaning_table, FUN=length)
colnames(cleaning_freq)[2] <- "Count"
cleaning_sort <- cleaning_freq[order(-cleaning_freq$Count),]

cleaning_high_freq <- head(cleaning_sort,10)
cleaning_low_freq <- tail(cleaning_sort,10)

cleaning_mix_freq <- rbind(cleaning_low_freq, cleaning_high_freq)
cleaning_mix_freq

ggplot(cleaning_mix_freq,aes(x=reorder(Neighborhood, Count), y=Count))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=Count), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Highest/Lowest Frequency neighborhood for cleaningholes') +
  scale_y_continuous(limits = c(0,820))


#----------CRIME DATASET---------------------
crime_data <- read.csv('BPD_Part_1_Victim_Based_Crime_Data.csv')
names(crime_data)
crime_freq <- data.frame(crime_data %>% group_by(Neighborhood) %>% tally())
colnames(crime_freq)[1] <- "Neighborhood"
colnames(crime_freq)[2] <- "Count"

nrow(crime_freq)
head(crime_freq)

# Remove empty rows
crime_freq <- crime_freq[!apply(crime_freq[1] == "", 1, all),]
nrow(crime_freq)
head(crime_freq)

# Print neighborhood and crime_count separately
print(crime_freq[1], row.names = FALSE)
print(crime_freq[2], row.names = FALSE)

crime_freq


#-----------AFTER DATASET CREATION------------
library(MASS)
cleaning_data <- read_excel('Population_By_Neighborhood.xlsx', sheet = 'cleaning_table')
names(cleaning_data)
nrow(cleaning_data)

# Remove rows having population less than 100
cleaning_data <- cleaning_data[!(cleaning_data$Population <= 100), ]
nrow(cleaning_data)


#------------FINDING CORRELATION---------------
names(cleaning_data)
cor(cleaning_data[2:10])


#------------MODEL CREATION---------------

cleaning_model_all <- lm(TimeTaken~Population + White + 
                        Blk_AfAm + Pop_dens + Housing + 
                        Occupied + Vacant + Crime_count , data = cleaning_data)
summary(cleaning_model_all)

cleaning_model_crime<- lm(TimeTaken~Population + Crime + Blk_AfAm, data = cleaning_data)
summary(cleaning_model_crime)

cleaning_model_pop <- lm(TimeTaken~Population, data = cleaning_data)
summary(cleaning_model_pop)

cleaning_model_black <- lm(TimeTaken~Blk_AfAm, data = cleaning_data)
summary(cleaning_model_black)

cleaning_model_white <- lm(TimeTaken~White, data = cleaning_data)
summary(cleaning_model_white)

# Shuffle data
cleaning_dataset <- data.frame(cleaning_data[sample(1:nrow(cleaning_data)),])
glimpse(cleaning_dataset)

cleaning_dataset$normal_pop <- scale(cleaning_dataset[3], center = TRUE, scale = TRUE)
cleaning_dataset$normal_white <- scale(cleaning_dataset[4], center = TRUE, scale = TRUE)
cleaning_dataset$normal_black <- scale(cleaning_dataset[5], center = TRUE, scale = TRUE)
glimpse(cleaning_dataset)

ggplot(data = cleaning_dataset, mapping = aes(x = White, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken White", y = "TimeTaken", x = "White population") + 
  theme_cleaning()

ggplot(data = cleaning_dataset, mapping = aes(x = Blk_AfAm, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken Black", y = "TimeTaken", x = "Black population") + 
  theme_cleaning()

#-----------Cross validation-------------------
library(caret)
glimpse(cleaning_dataset)

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=5)

tt_cleaning_model <- train(TimeTaken~Population, data=cleaning_dataset, trControl=train_control, method="lm")
summary(tt_cleaning_model)

testPop <- data.frame(Population=cleaning_dataset[3])
predict(tt_cleaning_model, testPop)

tt_model_normal_blk <- train(TimeTaken~normal_black, data=cleaning_dataset, trControl=train_control, method="lm")
summary(tt_model_normal_blk)

tt_model_normal_wht <- train(TimeTaken~normal_white, data=cleaning_dataset, trControl=train_control, method="lm")
summary(tt_model_normal_wht)

tt_model <- train(TimeTaken~Population, data=sanit_data, trControl=train_control, method="lm")
summary(tt_model)


