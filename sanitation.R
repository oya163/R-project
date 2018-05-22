# NOTE
# analysis for income, normalize by population
# Take only 5 neighborhood and check

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
setwd('C:/Users/uttam/Documents/DATA602/OpenBaltimore/')
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
# data_311[!complete.cases(data_311),]


#----------------Data based on SRType SANITATION------------
# Take only HCD-Sanitation Property with status CLOSED
srtype_sanit <- filter(data_311, SRType == "HCD-Sanitation Property" & SRStatus == "CLOSED")
summary(srtype_sanit)

# Calculate time take
createdDate <- mdy_hms(srtype_sanit$CreatedDate, tz = "America/New_York")
statusDate <- mdy_hms(srtype_sanit$StatusDate, tz = "America/New_York")
TimeTaken = interval(createdDate, statusDate)/dhours()    # interval in hours
srtype_sanit$TimeTaken <- TimeTaken

names(srtype_sanit)
glimpse(srtype_sanit)
nrow(srtype_sanit)    # --150769

# data_311 %>% filter(SRType == "HCD-Sanitation Property") %>% group_by(Neighborhood) %>% summarise(Count = n())

#-----------------START SANITATION--------------------
sanit_var <- c("TimeTaken","Neighborhood")
sanit_table <- srtype_sanit[sanit_var]
names(sanit_table)
nrow(sanit_table)
glimpse(sanit_table)


# Get the mean of time taken for each neighborhood
sanit_avg <- aggregate(TimeTaken ~ Neighborhood, sanit_table, mean)

# Print neighborhood only
print(sanit_avg[1], row.names = FALSE)

# Print time taken only
print(sanit_avg[2], row.names = FALSE)


#-------MOST TIME TAKEN----------
sanit_desc <- sanit_avg[order(-sanit_avg$TimeTaken),]
sanit_desc

sanit_high_time <- head(sanit_desc, n=20)
sanit_high_time

ggplot(sanit_high_time,aes(x=reorder(Neighborhood, -TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Highest time taking neighborhoods for sanit SRType', x="Neighborhoods") +
  scale_y_continuous(limits = c(0,400)) 

ggsave(filename = "../Project/graphs/highest_tt_sanit_srt.png", plot = last_plot(),
       width = 15, height = 7,
       units = "in", dpi = 300)

#-------LEAST TIME TAKEN----------
sanit_asc <- sanit_avg[order(sanit_avg$TimeTaken),]
sanit_low_time <- head(sanit_asc, n=20)
sanit_low_time

ggplot(sanit_low_time,aes(x=reorder(Neighborhood, TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Lowest time taking neighborhood for sanit', x="Neighborhoods") +
  scale_y_continuous(limits = c(0,400)) 

ggsave(filename = "../Project/graphs/lowest_tt_sanit_srt.png", plot = last_plot(),
       width = 15, height = 7,
       units = "in", dpi = 300)

#--------Mixture time taken-----------
sanit_mix_lowest <- head(sanit_desc, n=5)
sanit_mix_highest <- head(sanit_asc, n=5)

sanit_mix <- rbind(sanit_mix_lowest, sanit_mix_highest)

ggplot(sanit_mix,aes(x=reorder(Neighborhood, TimeTaken), y=TimeTaken))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=round(TimeTaken,2)), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Highest/Lowest time taking neighborhood for sanit', x="Neighborhoods") +
  scale_y_continuous(limits = c(0,400)) 

ggsave(filename = "../Project/graphs/mix_tt_sanit_srt.png", plot = last_plot(),
       width = 15, height = 7,
       units = "in", dpi = 300)

#------Number of neighborhood----------
nrow(sanit_asc)

#-----Display neighbordhood having highest/lowest frequency---------------
sanit_freq <- aggregate(TimeTaken~Neighborhood, sanit_table, FUN=length)
colnames(sanit_freq)[2] <- "Count"
sanit_sort <- sanit_freq[order(-sanit_freq$Count),]

sanit_high_freq <- head(sanit_sort,10)
sanit_low_freq <- tail(sanit_sort,10)

sanit_mix_freq <- rbind(sanit_low_freq, sanit_high_freq)
sanit_mix_freq

ggplot(sanit_mix_freq,aes(x=reorder(Neighborhood, Count), y=Count))+
  geom_bar(stat='identity', fill='orange', width = 0.5) + theme_bw() + 
  geom_text(aes(label=Count), colour="black", size=3, vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8)) +
  labs(title = 'Highest/Lowest Frequency neighborhood for sanit', x='Neighborhood') +
  scale_y_continuous(limits = c(0,4600))

ggsave(filename = "../Project/graphs/freq_tt_sanit_srt.png", plot = last_plot(),
       width = 15, height = 7,
       units = "in", dpi = 300)

#-----Display neighbordhood having highest frequency---------------
sanit_table %>% group_by(sanit_table$Neighborhood) %>% tally(sort = TRUE) 


# Add this to arrange in ascending order
# %>% arrange(desc(-n))

#------------------SANITATION MODEL CREATION-----------------------
sanitation <- read_excel('Population_By_Neighborhood.xlsx', sheet = 'sanitation')
names(sanitation)

glimpse(sanitation)

fill_up_sanit <- function(vec, temp_sanit){
  for (i in vec){
    temp_sanit$population[temp_sanit$Neighborhood == i] <- 
      sanitation$population[which(sanitation$neighborhood == i)]
    
    temp_sanit$crime[temp_sanit$Neighborhood == i] <- 
        sanitation$crime[which(sanitation$neighborhood == i)]
    
    temp_sanit$income[temp_sanit$Neighborhood == i] <- 
      sanitation$median_income[which(sanitation$neighborhood == i)]
    
    temp_sanit$White[temp_sanit$Neighborhood == i] <- 
      sanitation$White[which(sanitation$neighborhood == i)]
    
    temp_sanit$Blk_AfAm[temp_sanit$Neighborhood == i] <- 
      sanitation$Blk_AfAm[which(sanitation$neighborhood == i)]
  }
  return (temp_sanit)
}

sanit_table <- fill_up_sanit(neighbor, sanit_table)
glimpse(sanit_table)

crime_per_population <- sanit_table$crime/sanit_table$population
sanit_table$crimepop = crime_per_population

summary(crime_per_population)

nrow(sanit_table)
glimpse(sanit_table)
summary(sanit_table)

#------Randomly shuffle rows----------
sanit_table <- sanit_table[sample(1:nrow(sanit_table)),]
glimpse(sanit_table)

#------Linear Regression Model-------------
tt_model_all <- lm(TimeTaken ~ population + income + crime + White + Blk_AfAm, data = sanit_table)
summary(tt_model_all)

tt_model_non <- lm(TimeTaken ~ population + I(population^2) + income + I(income^2)+ crime, data = sanit_table)
summary(tt_model_non)

tt_model_crime_pop <- lm(TimeTaken ~ population + income + crime, data = sanit_table)
summary(tt_model_crime_pop)

tt_model_brook <- lm(TimeTaken ~ population + income + crime, data = sanit_table_brooklyn)
summary(tt_model_brook)

tt_model_pop <- lm(TimeTaken ~ population, data = sanit_table)
summary(tt_model_pop)

tt_model_inc <- lm(TimeTaken ~ income, data = sanit_table)
summary(tt_model_inc)

tt_model_crime <- lm(TimeTaken ~ crime, data = sanit_table)
summary(tt_model_crime)

tt_model_glm <- glm(TimeTaken ~ population + income + crime + Neighborhood, data = sanit_table)
summary(tt_model_glm)

#------Linear Regression Model Summary-------------
summary(tt_model_all)
coefficients(tt_model_all)
confint(tt_model_all, level=0.95)
modfit <- fitted(tt_model_all) # predicted values
modres <- residuals(tt_model_all)
anova(tt_model_all)

tt.mod <- data.frame(Fitted = modfit, Residuals = modres, Neighborhood=sanit_table$Neighborhood)
ggplot(tt.mod, aes(Fitted, Residuals, colour = Neighborhood)) + geom_point()


glimpse(sanit_table)

newdatacor = cor(sanit_table[1,3],sanit_table[1,3])
newdatacor

head(sanit_table[3])

sanit_table_small <- subset(sanit_table[1,3:5])
glimpse(sanit_table_small)

#------------Misc--------
tt_model_all.res = resid(tt_model_all)
plot(sanit_table$Neighborhood, tt_model_all.res, ylab = "Residuals", xlab = "Neighborhood", main = "Residual Plot" )

plot(sanit_table$Neighborhood)

only_neighbor = str(sanit_table$Neighborhood %>% unique())
droplevels(sanit_table$Neighborhood)


ggplot(data = sanit_sort_asc, mapping = aes(x = Neighborhood)) +
  geom_bar(aes(fill = TimeTaken), color = "white", bins = 15) +
  labs(title = "Distribution of TimeTaken by Neighborhood", x = "TimeTaken", y = "Total time taken") +
  theme_light()


#--------------VISUALIZATION--------------
ggplot(sanit_sort_desc,aes(y=TimeTaken, x=Neighborhood))+
  geom_bar(stat='identity', fill='blue') + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Sum of Top 10 high frequency SRTypes by Neighborhoods')


#----------------SANITATION NEIGHBORHOOD--------------
library(MASS)
sanit_table <- read_excel('Population_By_Neighborhood.xlsx', sheet = 'sanit_table')
names(sanit_table)

# Remove rows having population less than 100
sanit_table <- sanit_table[!(sanit_table$Population <= 100), ]
nrow(sanit_table)

# Create average columns
sanit_table <- sanit_table %>% mutate(Avg_white=White/Population)
sanit_table <- sanit_table %>% mutate(Avg_black=Blk_AfAm/Population)
sanit_table <- sanit_table %>% mutate(Avg_crime=Crime/Population)
head(sanit_table)

#------------FINDING CORRELATION---------------
names(sanit_table)
corMat <- cor(sanit_table[2:10])
corMat
remove(corMat)

test_sanit_data <- subset(light_data, select = c(2,11,12,13))
# test_sanit_data <- subset(sanit_table, select = c(2,4,5,10))
corMat <- cor(test_sanit_data)
corMat
highlyCorrelated <- findCorrelation(corMat, cutoff=0.50)
highlyCorrelated

#------------MODELLING SECTION-------------
sanit_model_all <- lm(TimeTaken~Population + White + 
                        Blk_AfAm + Pop_dens + Housing + 
                        Occupied + Vacant , data = sanit_table)
summary(sanit_model_all)


ggplot(data = sanit_table, mapping = aes(x = Population, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken", y = "TimeTaken", x = "Population") + 
  theme_light()

ggplot(data = sanit_table, mapping = aes(x = White, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken White", y = "TimeTaken", x = "Population") + 
  theme_light()

ggplot(data = sanit_table, mapping = aes(x = Blk_AfAm, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken Black_AfAm", y = "TimeTaken", x = "Population") + 
  theme_light()


sanit_pop_model <- lm(TimeTaken~Population, data=sanit_data)
summary(sanit_pop_model)
plot(sanit_pop_model)

#----------Black Model-----------------
sanit_black_model <- lm(TimeTaken~Avg_black, data=sanit_data)
summary(sanit_black_model)
plot(sanit_black_model)

#----------White Model-----------------
sanit_white_model <- lm(TimeTaken~Avg_white, data=sanit_data)
summary(sanit_white_model)
plot(sanit_white_model)

sanit_quadratic_model <- lm(TimeTaken~Blk_AfAm + I(Blk_AfAm^2), data=sanit_data)
summary(sanit_quadratic_model)

# Plot actual vs fitted values
ggplot(data = sanit_table, mapping = aes(x = Blk_AfAm, y = TimeTaken)) +
  geom_point(color = "#006EA1") + 
  geom_point(aes(x=Blk_AfAm, y=sanit_black_model$fitted.values), color="black" , shape=5) +
  geom_smooth(method = "gam", formula = y~poly(x, 1), se = FALSE, color = "orange") +
  geom_smooth(method = "gam", formula = y~poly(x, 2), se = FALSE, color = "red") +
  geom_smooth(method = "gam", formula = y~poly(x, 3), se = FALSE, color = "blue") +
  labs(title = "Population vs TimeTaken Black for sanitation SRType", y = "TimeTaken", 
       x = "Black Population") + 
  theme_light()

ggsave(filename = "../Project/graphs/sanit_black_lm_poly.png", plot = last_plot(),
       width = 15, height = 7,
       units = "in", dpi = 300)

ggplot(data = sanit_table, mapping = aes(x = White, y = TimeTaken)) + 
  geom_point(aes(x=White, y=sanit_white_model$fitted.values), color="black" , shape=5) +
  geom_point(color = "#006EA1") + 
  geom_smooth(method = "gam", formula = y~poly(x, 1), se = FALSE, color = "orange") +
  geom_smooth(method = "gam", formula = y~poly(x, 2), se = FALSE, color = "red") +
  geom_smooth(method = "gam", formula = y~poly(x, 3), se = FALSE, color = "blue") +
  labs(title = "Population vs TimeTaken White for sanitation SRType", y = "TimeTaken", 
       x = "White Population") + 
  theme_light()

ggsave(filename = "../Project/graphs/sanit_white_lm_poly.png", plot = last_plot(),
       width = 15, height = 7,
       units = "in", dpi = 300)


names(sanit_table)
# Correlation between variables
cor(sanit_table[2:10], sanit_table[2:10])

#------------FINDING CORRELATION---------------
names(light_data)
cor(light_data[2:10])


#-----------SVR for Blk_AfAm----------------------
names(sanit_data)
sanit_svr_data <- subset(sanit_data, select=c(5,2))
plot(sanit_svr_data, pch=16)

sanit_lm_model <- lm(TimeTaken ~Blk_Afm, sanit_svr_data)
abline(sanit_lm_model)

predictY <- predict(sanit_lm_model, sanit_svr_data)

points(sanit_svr_data$Blk_AfAm, predictY, col="blue", pch=4)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- sanit_lm_model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 50.80559
predictionRMSE

library(e1071)
sanit_svm_model <- svm(TimeTaken~Blk_AfAm, sanit_svr_data)
predictedY <- predict(sanit_svm_model, sanit_svr_data)
points(sanit_svr_data$Blk_AfAm, predictedY, col = "red", pch=4)

error <- sanit_svr_data$TimeTaken - predictedY
svrPredictionRMSE <- rmse(error)
svrPredictionRMSE

#-----------SVR for Population----------------------
names(sanit_data)
remove(sanit_svr_data)
sanit_svr_data <- subset(sanit_data, select=c(3,2))
plot(sanit_svr_data, pch=16)

sanit_lm_model <- lm(TimeTaken~Population, sanit_svr_data)
summary(sanit_lm_model)
abline(sanit_lm_model)

predictY <- predict(sanit_lm_model, sanit_svr_data)

points(sanit_svr_data$Population, predictY, col="blue", pch=4)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- sanit_lm_model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 50.80559
predictionRMSE

library(e1071)
sanit_svm_model <- svm(TimeTaken~Population, sanit_svr_data)
summary(sanit_svm_model)
predictedY <- predict(sanit_svm_model, sanit_svr_data)
points(sanit_svr_data$Population, predictedY, col = "red", pch=4)

error <- sanit_svr_data$TimeTaken - predictedY
svrPredictionRMSE <- rmse(error)
svrPredictionRMSE


#-----------Cross validation-------------------
library(caret)

# Shuffle data
sanit_data <- data.frame(sanit_table[sample(1:nrow(sanit_table)),])

glimpse(sanit_data)

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=5)

tt_sanit_model_blk <- train(TimeTaken~Avg_black, data=sanit_data, trControl=train_control, method="lm")
summary(tt_sanit_model_blk)

ggplot(data = sanit_table, mapping = aes(x = Avg_black, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + 
  geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken Black", y = "TimeTaken", x = "Population") + 
  theme_light()

tt_sanit_model_wht <- train(TimeTaken~Avg_white, data=sanit_data, trControl=train_control, method="lm")
summary(tt_sanit_model_wht)

ggplot(data = sanit_table, mapping = aes(x = Avg_white, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken Whote", y = "TimeTaken", x = "Population") + 
  theme_light()

names(sanit_data)

plot(sanit_data[c(4,2)])
x11()
plot(sanit_data[c(5,2)])

sanit_quadratic_model <- train(TimeTaken~Avg_black + I(Avg_black^2), data=sanit_data, trControl=train_control, method="lm")
summary(sanit_quadratic_model)



#-----------Support Vector Regression-------------------
library(e1071)

sanit_data <- sanit_data[c(2:8)]
head(sanit_data)

white_pop <- sanit_data[c(5,6)]
head(white_pop)

plot(white_pop)

plot(x = sanit_data$normal_black, y = sanit_data$normal_white)
plot(sanit_data[6:7])

summary(tt_model_normal_blk)

tt_svm = svm(TimeTaken~normal_pop + normal_white + normal_black, sanit_data)
summary(tt_svm)

tt_pop_svm = svm(TimeTaken~normal_white, sanit_data)
summary(tt_pop_svm)

predYsvm = predict(tt_pop_svm, sanit_data[6])

points(sanit_data$normal_white, predYsvm, col = "red", pch=16)
