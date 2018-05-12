#----------------Data based on SRType SANITATION------------
# Take only HCD-Sanitation Property with status CLOSED
srtype_sanit <- filter(data_311, SRType == "TRS-Parking Complaints" & SRStatus == "CLOSED")
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
sanit_table_final <- srtype_sanit[sanit_var]
names(sanit_table_final)
nrow(sanit_table_final)
glimpse(sanit_table_final)

sanit_table_final


# # Neighborhood of interest
neighbor <- c("FRANKFORD", "BELAIR-EDISON", "CANTON", "BROOKLYN",
              "SANDTOWN-WINCHESTER", "CHERRY HILL", "CHARLES VILLAGE",
              "GLEN", "CHESWOLDE", "COLDSTREAM HOMESTEAD MONTEBELLO", "HAMPDEN")

# neighbor <- c("FRANKFORD", "BELAIR-EDISON", "CANTON", "BROOKLYN",
#               "SANDTOWN-WINCHESTER")

sanit_table <- sanit_table_final[sanit_table_final$Neighborhood %in% neighbor,]
sanit_table %>% group_by(Neighborhood) %>% tally()

#-----------Convert neighborhood to factor and remove unused factor levels -----
sanit_table$Neighborhood <- factor(sanit_table$Neighborhood)
sanit_table$Neighborhood <- droplevels(sanit_table$Neighborhood)
levels(sanit_table$Neighborhood)

#-------Create Neighborhood a factor---------
# sanit_table$Neighborhood <- str(sanit_table$Neighborhood)

#-----CHECK NEIGHBOR of sanit_table_final------
# only_neighbor <- sanit_table_final$Neighborhood %>% unique()
# only_neighbor
# length(only_neighbor)

#-----CHECK NEIGHBOR of sanit_table------
only_neighbor <- sanit_table$Neighborhood %>% unique()
only_neighbor
length(only_neighbor)


# Get the mean of time taken for each neighborhood
sanit_avg <- aggregate(TimeTaken ~ Neighborhood, sanit_table_final, mean)
sanit_avg
print(sanit_avg[2], row.names = FALSE)
sanit_sort_desc <- sanit_avg[order(-sanit_avg$TimeTaken),]

#-------MOST TIME TAKEN
head(sanit_sort_desc, n=20)

#-------LEAST TIME TAKEN
sanit_sort_asc <- sanit_avg[order(sanit_avg$TimeTaken),]
head(sanit_sort_asc, n=20)

#------Number of neighborhood
nrow(sanit_sort_asc)

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


#----------------Extra stuff--------------
parking <- read_excel('Population_By_Neighborhood.xlsx', sheet = 'parking')
names(parking)
parking_model <- lm(TimeTaken ~ Population, data = parking)
summary(parking_model)

ggplot(data = parking, mapping = aes(x = Population, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken Parking SRType", y = "TimeTaken", x = "Population") + 
  theme_light()

ggplot(data = parking, mapping = aes(x = White, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken White For Parking SRType", y = "TimeTaken", x = "Population") + 
  theme_light()

ggplot(data = parking, mapping = aes(x = Blk_AfAm, y = TimeTaken)) + 
  geom_point(color = "#006EA1") + geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Population vs TimeTaken Black_AfAm For Parking SRType", y = "TimeTaken", x = "Population") + 
  theme_light()



cor(sanit_clean[2:5], sanit_clean[2:5])

#--------------VISUALIZATION--------------
ggplot(sanit_sort_desc,aes(y=TimeTaken, x=Neighborhood))+
  geom_bar(stat='identity', fill='blue') + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8)) +
  labs(title = 'Sum of Top 10 high frequency SRTypes by Neighborhoods')





