#libraries to load
library(tidyverse)
library(lubridate)
library(eeptools)
library(reshape2)
library(plotly)
library(glmnet)

#read in data files
player_info <- readRDS('~/Desktop/CMSAC/CMU_project/player_information.RData')
View(player_info)
season_stat <- readRDS('~/Desktop/CMSAC/CMU_project/player_season_statistics.RData')
View(season_stat)
draft_data <- readRDS('~/Desktop/CMSAC/CMU_project/NHL_draft_data.RData')
m_cleaned <- readRDS('~/Desktop/CMSAC/CMU_project/hockey_cleaned_age.RData')

#preliminary plots
plot(m_cleaned$age,m_cleaned$PPG)
hist(m_cleaned$age)
hist(log(m_cleaned$PPG), breaks = 60)

########

##Creating the models

# There are two ways we can create subsets: by hand and through for loop
# Adding both bc it's easier to understand the code by hand, but obviously looping is faster!

##BY HAND

#creating the different subsets from 1998-1998 to 2018-2019
#subset1998 = pctfull%>% dplyr::filter(season == "19981999" | season == "19992000")

subset1998 = logppg2%>% dplyr::filter(season == "19981999" | season == "19992000")
subset2002 = logppg2%>% dplyr::filter(season == "20022003" | season == "20032004")

#creating the new data 
merge1 = merge(subset2002,subset2002,by = 'player_id') #change num of subset
merge2 = merge1 %>% dplyr:: select(player_id,player_name.x.x,league_id.x,league_id.y,season.x,season.y,zscore.x,zscore.y,season_type.x,season_type.y)
merge3 = subset(merge2,league_id.x !=league_id.y & season_type.x == "regular" & season_type.y == "regular")
merge4 <- subset(merge3, season.x <=season.y)
leaguenum <- length(unique(merge4$league_id.x)) #just checks # of leagues each given year

#Starting Data Matrix (help from Alden)
data_matrix <- matrix(0,nrow = nrow(merge4), ncol = length(unique(merge4$league_id.x)))
y <- numeric(nrow(merge4))
colnames(data_matrix) <- unique(merge4$league_id.x)
for(i in 1:nrow(data_matrix)) 
{
  # Build design matrix
  obs <- merge4[i,]
  data_matrix[i,obs$league_id.x] <- 1
  data_matrix[i,obs$league_id.y] <- -1
  
  # Build response
  if(obs$percrank.x > obs$percrank.y){
    y[i] <- 0
  } else  if(obs$percrank.x < obs$percrank.y){
    y[i] <- 1
  }
}

## strength coefficients for each league in the 2017-2018 and 2018-2019 seasons

# here you can run different models on the subsets. In this case, it's an OLS used to mimic a bradley terry model
glm_df <- data.frame(y = y, data_matrix)
bradley_terry_model <- glm(y ~ ., data = glm_df, family = 'binomial')
centered_coefficients07 <- bradley_terry_model$coefficients[-1] - mean(bradley_terry_model$coefficients[-1], na.rm=TRUE) #change name num of coeff

#We also later use actual bradley terry models
library(BradleyTerry2)
glm_df1 <- merge4 %>% dplyr::mutate(outcome = as.numeric(zscore.x < zscore.y),
                                    League1 = factor(league_id.x, levels = unique(league_id.x)),
                                    League2 = factor(league_id.y, levels =levels(League1)))
fit <- BTm(outcome, League1, League2, data = glm_df1)
centered_coefficients16 <- fit$coefficients[-1] - mean(fit$coefficients[-1], na.rm=TRUE) #change name num of coeff

## BY LOOP

# Here, we also split the data into a training and testing data for future evaluation 

set.seed(101) # Set Seed so that same sample can be reproduced in future also

# Now Selecting 70% of data as sample from total 'n' rows of the data
sample <- sample.int(n = nrow(logppg2), size = floor(.70*nrow(logppg2)), replace = F)
train <- logppg2[sample, ]
test  <- logppg2[-sample, ]

#for the train data
seasonvector = c("20072008","20082009","20092010","20102011","20112012","20122013","20132014", "20142015","20152016","20162017","20172018","20182019")
training1 <- matrix(NA,nrow = length(seasonvector)-1, ncol = length(unique(train$league_id)))
rownames(training1) = seasonvector[-length(seasonvector)]
colnames(training1)=unique(train$league_id)
for(i in 1:(length(seasonvector)-1)){
  early_year = seasonvector[i]
  later_year = seasonvector[i+1]
  trainsubset = train %>% dplyr::filter(season == early_year| season == later_year) #change
  merge1 = merge(trainsubset,trainsubset,by = 'player_id') #change num of subset
  merge2 = merge1 %>% dplyr:: select(player_id,player_name.x.x,league_id.x,league_id.y,season.x,season.y,zscore.x,zscore.y,season_type.x,season_type.y)
  merge3 = subset(merge2,league_id.x !=league_id.y & season_type.x == "regular" & season_type.y == "regular")
  merge4 <- subset(merge3, season.x == early_year) #change to early year
  leaguenum <- length(unique(merge4$league_id.x)) #just checks # of leagues each given year
  glm_df1 <- merge4 %>% dplyr::mutate(outcome = as.numeric(zscore.x < zscore.y),
                                      League1 = factor(league_id.x, levels = unique(league_id.x)),
                                      League2 = factor(league_id.y, levels =levels(League1)))
  fit <- BTm(outcome, League1, League2, data = glm_df1)
  train_centered_coefficients <- fit$coefficients[-1] - mean(fit$coefficients[-1], na.rm=TRUE) #change name num of coeff
  training1[i,which(paste0("..",unique(train$league_id)) %in% names(train_centered_coefficients))] =  train_centered_coefficients 
}


#for the test data
seasonvector = c("20072008","20082009","20092010","20102011", "20112012","20122013","20132014", "20142015" ,"20152016","20162017","20172018","20182019")
testing1 <- matrix(0,nrow = length(seasonvector)-1, ncol = length(unique(test$league_id)))
#rownames(data_btm) = seasonvector[-length(seasonvector)]
colnames(testing1)=unique(test$league_id)
for(i in 1:(length(seasonvector)-1)){
  early_year = seasonvector[i]
  later_year = seasonvector[i+1]
  testsubset = test %>% dplyr::filter(season == early_year | season == later_year)
  merge1 = merge(testsubset,testsubset,by = 'player_id')
  merge2 = merge1 %>% dplyr:: select(player_id,player_name.x.x,league_id.x,league_id.y,season.x,season.y,zscore.x,zscore.y,season_type.x,season_type.y)
  merge3 = subset(merge2,league_id.x !=league_id.y & season_type.x == "regular" & season_type.y == "regular")
  merge4 <- subset(merge3, season.x == early_year)
  y <- numeric(nrow(merge4))
  for(i in 1:nrow(merge4)) {
    # Build design matrix
    obs <- merge4[i,]
    testing1[i,obs$league_id.x] <- 1
    testing1[i,obs$league_id.y] <- -1
    y[i] <- -(obs$zscore.x - obs$zscore.y)
  }
  ## strength coefficients for each league in the 2017-2018 and 2018-2019 seasons
  glm_df <- data.frame(y = y, testing1)
  idk <- lm(y ~ ., data = glm_df)
  cc <- idk$coefficients[-1] - mean(idk$coefficients[-1], na.rm=TRUE) #change name num of coeff
  testing1[i,which(paste0("..",unique(test$league_id)) %in% names(cc))] =  cc 
}

# Evaluating the model 
model <- lm(y ~ ., data = glm_df)
# make predictions
x_test <- test[,1:4]
y_test <- test[,5]
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions$class, y_test)
