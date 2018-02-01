library(ggplot2)
library(dplyr)
library(randomForest)

trainingData <- read.csv('train.csv', stringsAsFactors = F)
testData <- read.csv('test.csv', stringsAsFactors = F)


#########   BIND FILES   #########
names(trainingData)[1] <- 'ID'
testData$ID <- as.character(testData$ID)

data <- bind_rows(trainingData, testData)


#########   GRAPHICS   #########
ggplot(trainingData, aes(AnimalType, fill = OutcomeType))+
  geom_bar(position = position_dodge())



#########   AGE    #########
numbersAge <- sapply(data$AgeuponOutcome, function(x) strsplit(x, split=" ")[[1]][1]) # Get number
timeAge <- sapply(data$AgeuponOutcome, function(x) strsplit(x, split=" ")[[1]][2]) # Day, month or year
timeAge <- gsub('s', '', timeAge) # delete plurals ('s')

data$AgeuponOutcome <- NULL
numbersAge <- as.numeric(numbersAge)
timeAge <- as.factor(timeAge)

toDays <- ifelse(timeAge == "day", 1, 
          ifelse(timeAge == 'week', 7,
          ifelse(timeAge == 'month', 30,
          ifelse(timeAge == 'year', 365, NA))))

data$Age <- numbersAge * toDays

data$Age[is.na(data$Age)] <- 0

# Remove data that is no longer used
remove(numbersAge, timeAge, toDays)
data$Time <- NULL
data$Unit <- NULL



#########   NAME    #########
data$Name <- ifelse(nchar(data$Name) == 0, 'Unknown', data$Name) # If NULL, then "Unknown"



#########   SEX + CASTRATED   #########
data$SexuponOutcome <- ifelse(nchar(data$SexuponOutcome) == 0, "Unknown", data$SexuponOutcome) # If NULL, then "Unknown"

data$Castrated <- ifelse(grepl("Neutered", data$SexuponOutcome), 1, # Neutered male
                          ifelse(grepl("Spayed", data$SexuponOutcome), 1, # Spayed female
                          ifelse(grepl("Intact", data$SexuponOutcome), 0, #Intact male/female
                          ifelse(grepl('Unknown', data$SexuponOutcome), 0, 0)))) # Unknown


# Clear string from Castrated data
data$SexuponOutcome <- ifelse(grepl("Male", data$SexuponOutcome), 'Male', # Male
                               ifelse(grepl('Female', data$SexuponOutcome), 'Female', 'Unknown')) # Female, else Unknown


#########   BREED    #########
data$isMix <- ifelse(grepl("Mix", data$Breed), 1,0)

data$Breed <- sapply(data$Breed, function(x) gsub(' Mix', '', strsplit(x, split="/")[[1]][1])) # Delete "mix" and "/" from string



#########   COLOR    #########
data$Color <- sapply(data$Color, function(x) strsplit(x, split = "/| ")[[1]][1])


#########   IRRELEVANT DELETED DATA    #########
data$DateTime <- NULL
data$OutcomeSubtype <- NULL




#########   PREPARE DATA FOR PREDICTION    #########
columnsNames <- names(data)
data[columnsNames] <- lapply(data[columnsNames], function(x) as.factor(x))
remove(columnsNames)



#########   PREDICTION    #########
trainingData <- data[1:26729, ]
testData  <- data[26730:nrow(data), ]

set.seed(107)
dataModel <- randomForest(OutcomeType ~ AnimalType+SexuponOutcome+Color+Age+Castrated+isMix, 
                       data = trainingData, 
                       ntree = 600, 
                       importance = TRUE)

prediction <- predict(dataModel, testData, type = 'prob')
solution <- data.frame('ID' = testData$ID, prediction)

remove(dataModel, prediction, data)



######## CREATE FILES ############
write.csv(trainingData, "train_tratado.csv", row.names = F)
write.csv(testData, "test_tratado.csv", row.names = F)
write.csv(solution, "prediccion.csv", row.names = F)
