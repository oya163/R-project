#############################
# Author - Oyesh Mann Singh
# Dataset - 311_Customer_Service_Request
# Link - https://data.baltimorecity.gov/City-Services/311-Customer-Service-Requests/9agw-sxsr
# Description - 
#   Performing Data wrangling for various SRTypes
#   Performing regression analysis
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
srtype_light <- filter(data_311, SRType == "BGE-StLight(s) Out" & SRStatus == "CLOSED")
summary(srtype_light)

# Calculate time take
createdDate <- mdy_hms(srtype_light$CreatedDate, tz = "America/New_York")
statusDate <- mdy_hms(srtype_light$StatusDate, tz = "America/New_York")
TimeTaken = interval(createdDate, statusDate)/dhours()    # interval in hours
srtype_light$TimeTaken <- TimeTaken

names(srtype_light)
glimpse(srtype_light)
nrow(srtype_light)    # --32575

#-----------------START STREET LIGHTS--------------------
light_var <- c("TimeTaken","Neighborhood")
light_table <- srtype_light[light_var]
names(light_table)
nrow(light_table)
glimpse(light_table)


# Get the mean of time taken for each neighborhood
light_avg <- aggregate(TimeTaken ~ Neighborhood, light_table, mean)

# Print neighborhood only
print(light_avg[1], row.names = FALSE)

# Print time taken only
print(light_avg[2], row.names = FALSE)

light_desc <- light_avg[order(-light_avg$TimeTaken),]

#-------MOST TIME TAKEN----------
light_high_time <- head(light_desc, n=20)
light_high_time

ggplot(light_high_time,aes(x=reorder(Neighborhood, -TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Highest time taking neighborhoods for lightholes SRType', x="Neighborhoods") +
  scale_y_continuous(limits = c(0,450)) 

#-------LEAST TIME TAKEN----------
light_asc <- light_avg[order(light_avg$TimeTaken),]
light_low_time <- head(light_asc, n=20)
light_low_time

ggplot(light_low_time,aes(x=reorder(Neighborhood, TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Lowest time taking neighborhood for lightholes', x="Neighborhoods") +
  scale_y_continuous(limits = c(0,450)) 

#--------Mixture time taken-----------
light_mix_lowest <- head(light_desc, n=5)
light_mix_highest <- head(light_asc, n=5)

light_mix <- rbind(light_mix_lowest, light_mix_highest)

ggplot(light_mix,aes(x=reorder(Neighborhood, TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Highest/Lowest time taking neighborhood for lightholes', x="Neighborhoods") +
  scale_y_continuous(limits = c(0,450)) 

#------Number of neighborhood----------
nrow(light_asc)

#-----Display neighbordhood having highest/lowest frequency---------------
light_freq <- aggregate(TimeTaken~Neighborhood, light_table, FUN=length)
colnames(light_freq)[2] <- "Count"
light_sort <- light_freq[order(-light_freq$Count),]

light_high_freq <- head(light_sort,10)
light_low_freq <- tail(light_sort,10)

light_mix_freq <- rbind(light_low_freq, light_high_freq)
light_mix_freq

ggplot(light_mix_freq,aes(x=reorder(Neighborhood, Count), y=Count))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=Count), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Highest/Lowest Frequency neighborhood for lightholes') +
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
light_data <- read_excel('Population_By_Neighborhood.xlsx', sheet = 'light_table')
names(light_data)
nrow(light_data)

# Remove rows having population less than 100
light_data <- light_data[!(light_data$Population <= 100), ]
nrow(light_data)

# Create average columns
light_data <- light_data %>% mutate(Avg_white=White/Population)
light_data <- light_data %>% mutate(Avg_black=Blk_AfAm/Population)
light_data <- light_data %>% mutate(Avg_crime=Crime/Population)
head(light_data)

#------------FINDING CORRELATION---------------
names(light_data)
cor(light_data[2:10])

# test_light_data <- subset(light_data, select = c(2,11,12,13))
test_light_data <- subset(light_data, select = c(2,4,5,10))
cor(test_light_data)


#------------MODEL CREATION---------------

light_model_all <- lm(TimeTaken~Population + White + 
                      Blk_AfAm + Pop_dens + Housing + 
                      Occupied + Vacant + Crime_count , data = light_data)
summary(light_model_all)

light_model_crime<- lm(TimeTaken~Population + Crime + Blk_AfAm, data = light_data)
summary(light_model_crime)

light_model_pop <- lm(TimeTaken~Population, data = light_data)
summary(light_model_pop)

light_model_black <- lm(TimeTaken~Blk_AfAm, data = light_data)
summary(light_model_black)

light_model_white <- lm(TimeTaken~White, data = light_data)
summary(light_model_white)

# Shuffle data
light_dataset <- data.frame(light_data[sample(1:nrow(light_data)),])
glimpse(light_dataset)

light_dataset$normal_pop <- scale(light_dataset[3], center = TRUE, scale = TRUE)
light_dataset$normal_white <- scale(light_dataset[4], center = TRUE, scale = TRUE)
light_dataset$normal_black <- scale(light_dataset[5], center = TRUE, scale = TRUE)
glimpse(light_dataset)

ggplot(data = light_dataset, mapping = aes(x = White, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken White", y = "TimeTaken", x = "White population") + 
  theme_light()

ggplot(data = light_dataset, mapping = aes(x = Blk_AfAm, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken Black", y = "TimeTaken", x = "Black population") + 
  theme_light()

#-----------Cross validation-------------------
library(caret)
glimpse(light_dataset)

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=5)

tt_light_model <- train(TimeTaken~Population, data=light_dataset, trControl=train_control, method="lm")
summary(tt_light_model)

testPop <- data.frame(Population=light_dataset[3])
predict(tt_light_model, testPop)

tt_model_normal_blk <- train(TimeTaken~normal_black, data=light_dataset, trControl=train_control, method="lm")
summary(tt_model_normal_blk)

tt_model_normal_wht <- train(TimeTaken~normal_white, data=light_dataset, trControl=train_control, method="lm")
summary(tt_model_normal_wht)

tt_model <- train(TimeTaken~Population, data=sanit_data, trControl=train_control, method="lm")
summary(tt_model)


