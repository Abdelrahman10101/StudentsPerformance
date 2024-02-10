#Step0--------------Adding Necessary Libraries-----
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("rpart")

library(dplyr)
library(ggplot2)
library(corrplot)
library(rpart)

#Step1--------------Getting Info about the Dataset-----

#Reading Data set
StudentsData <- read.csv("D:/year 3/Stat_proj/StudentsPerformance.csv")

#Getting started with the dataset
str(StudentsData)
summary(StudentsData)
head(StudentsData)

#Determining row number
nrow(StudentsData)
ncol(StudentsData)

#Step2-------------Data Cleaning------------

#Droping first column
StudentsData <- StudentsData[, -1]

#Formatting data
StudentsData$sex[StudentsData$sex == "Male"] <- "M"
StudentsData$sex[StudentsData$sex == "Female"] <- "F"

#Replacing every free space with NA
for (col in names(StudentsData)) {
  StudentsData[col][StudentsData[col] == ""] <- NA
}


#Determining Null Values
NullValues <- colSums(is.na(StudentsData))
NullValues

#Another method for determining null values
sum(is.na(StudentsData))

#Determining if the dataset has a duplicates or not
if(anyDuplicated(StudentsData)>0){
  df_nodupes <- StudentsData[!duplicated(StudentsData), ]
}else
  print("No Duplicates")

#Another method for determining duplicates
sum(duplicated(StudentsData))

#Data Encoding
StudentsData$romantic <- as.integer(StudentsData$romantic == "yes") #yes = 1 && no = 0 
StudentsData$internet <- as.integer(StudentsData$internet == "yes") #yes = 1 && no = 0 

StudentsData$sex <- as.integer(StudentsData$sex == "M")             #M = 1 && F = 0

StudentsData$Fjob <- factor(StudentsData$Fjob, levels = c("at_home", "health", "services", "teacher", "other"))
StudentsData$Fjob <- as.numeric(StudentsData$Fjob)     #      1         2           3         4           5     
StudentsData$Mjob <- factor(StudentsData$Mjob, levels = c("at_home", "health", "services", "teacher", "other"))
StudentsData$Mjob <- as.numeric(StudentsData$Mjob)
                                                        #       1     2
StudentsData$school <- factor(StudentsData$school, levels = c("GP", "MS"))
StudentsData$school <- as.numeric(StudentsData$school)


#Clearing all rows with missing values
df_with_dropNA <- na.omit(StudentsData)
colSums(is.na(df_with_dropNA))


#Calculating the mode of the internet col
internet_mode <- as.character(names(sort(table(StudentsData$internet), decreasing = TRUE)[1]))


#Replace Null Values with the mode
StudentsData$internet <- ifelse(is.na(StudentsData$internet), internet_mode, StudentsData$internet)
df_with_mode <- StudentsData
colSums(is.na(df_with_mode))

#Recheck on the dataframes
head(df_with_dropNA)
df_with_mode <- mutate_all(df_with_mode, as.numeric)
head(df_with_mode)

# Step3-------Data Visualization------
par(mfcol = c(1,1))
df_numeric <- as.data.frame(sapply(df_with_dropNA, function(x) if(is.factor(x)) as.numeric(x) else x))
df_numeric <- as.data.frame(sapply(df_numeric, as.numeric))
cor_matrix <- cor(df_numeric)
corrplot(cor_matrix, method = "color",main = "Correlation Plot of dropNA")

par(mfcol = c(1,1))
df_numeric <- as.data.frame(sapply(df_with_mode, function(x) if(is.factor(x)) as.numeric(x) else x))
df_numeric <- as.data.frame(sapply(df_numeric, as.numeric))
cor_matrix <- cor(df_numeric)
corrplot(cor_matrix, method = "color",main = "Correlation Plot of Mode")


#Plotting a scatter plot
par(mfcol = c(2,1))
for (i in 1:ncol(df_with_dropNA)) {
  
  if (class(df_with_dropNA[[i]]) != "factor"){
    plot(df_with_dropNA[[i]], main = "Scatter Plot for dropNA", ylab =  names(df_with_dropNA)[i])
    plot(df_with_mode[[i]], main = "Scatter Plot for Mode", ylab =  names(df_with_dropNA)[i])
  }
}

#Creating a simple box plot
par(mfcol = c(1,2))
for (i in 1:ncol(df_with_dropNA)) {

  if (class(df_with_dropNA[[i]]) != "factor"){
    boxplot(df_with_dropNA[i], main = "Box Plot of dropNA", ylab = names(df_with_dropNA)[i])
    boxplot(df_with_mode[i], main = "Box Plot of Mode", ylab = names(df_with_dropNA)[i])
  }
}

#Creating a Histogram
par(mfcol = c(2,1))
for (i in 1:ncol(df_with_dropNA)) {
  
  if (class(df_with_dropNA[[i]]) == "numeric"){
    hist(df_with_dropNA[[i]], main = "Histogram of dropNA",xlab = "Values",  ylab = names(df_with_dropNA)[i])
    hist(df_with_mode[[i]], main = "Histogram of Mode",xlab = "Values", ylab = names(df_with_dropNA)[i])
  }
}

# Step4--------------Outlier Detection and Removal using IQR------------

# Function to detect and remove outliers using IQR
remove_outliers_iqr <- function(data, column) {
  q <- quantile(data[[column]], c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  # Identify and remove outliers
  outliers <- data[[column]] < lower_bound | data[[column]] > upper_bound
  data[outliers, column] <- NA
  
  return(data)
}

# Apply IQR outlier detection and removal for numeric columns in df_with_dropNA
numeric_columns_dropNA <- sapply(df_with_dropNA, is.numeric)
df_outliers_removed_dropNA <- df_with_dropNA

for (column in names(df_outliers_removed_dropNA)[numeric_columns_dropNA]) {
  df_outliers_removed_dropNA <- remove_outliers_iqr(df_outliers_removed_dropNA, column)
}

# Apply IQR outlier detection and removal for numeric columns in df_with_mode
numeric_columns_mode <- sapply(df_with_mode, is.numeric)
df_outliers_removed_mode <- df_with_mode

for (column in names(df_outliers_removed_mode)[numeric_columns_mode]) {
  df_outliers_removed_mode <- remove_outliers_iqr(df_outliers_removed_mode, column)
}

# Recheck for missing values after outlier removal
colSums(is.na(df_outliers_removed_dropNA))
colSums(is.na(df_outliers_removed_mode))

# Data visualization after outlier removal for both datasets

# Box plots for both dataframes
par(mfcol = c(1,2))
for (i in 1:ncol(df_with_dropNA)) {
  if (class(df_with_dropNA[[i]]) != "factor"){
    boxplot(df_outliers_removed_dropNA[i], main = "Box Plot of outliers dropNA", ylab = names(df_with_dropNA)[i])
    boxplot(df_outliers_removed_mode[i], main = "Box Plot of outliers Mode", ylab = names(df_with_dropNA)[i])
  }
}

# Histogram plot for both dataframes
par(mfcol = c(2,1))
for (i in 1:ncol(df_with_dropNA)) {
  if (class(df_with_dropNA[[i]]) == "numeric"){
    hist(df_outliers_removed_dropNA[[i]], main = "Histogram of dropNA",xlab = "Values",  ylab = names(df_with_dropNA)[i])
    hist(df_outliers_removed_mode[[i]], main = "Histogram of Mode",xlab = "Values", ylab = names(df_with_dropNA)[i])
  }
}



#step5-------Feature Selection------------------

cor_matrix <- cor(df_with_mode)
#Correlation with G1, G2, and G3
cor_with_G1 <- abs(cor_matrix["G1", ])
cor_with_G2 <- abs(cor_matrix["G2", ])
cor_with_G3 <- abs(cor_matrix["G3", ])

# Sort attributes by absolute correlation values
top_features_G1 <- sort(cor_with_G1, decreasing = TRUE)
top_features_G2 <- sort(cor_with_G2, decreasing = TRUE)
top_features_G3 <- sort(cor_with_G3, decreasing = TRUE)

#Print top features for G1
print(top_features_G1)

#Print top features for G2
print(top_features_G2)

#Print top features for G3
print(top_features_G3)

set.seed(123)  # For reproducibility

#Create an index to split the data (80% train, 20% test)
train_index <- sample(seq_len(nrow(df_with_mode)), 0.8 * nrow(df_with_mode))

#Split the data
train_data <- df_with_mode[train_index, ]
test_data <- df_with_mode[-train_index, ]


#step6-------Linear Regression-------------------


min_mae_G1_LR<-Inf
min_mae_G2_LR<-Inf
min_mae_G3_LR<-Inf

index_mae_G1_LR<-0
index_mae_G2_LR<-0
index_mae_G3_LR<-0
for (i in 1:12) {
#Sort attributes by absolute correlation values without G1, G2, G3 
top_features_G1_without_G_LR <- names(sort(cor_with_G1[!names(cor_with_G1) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]
top_features_G2_without_G_LR <- names(sort(cor_with_G2[!names(cor_with_G2) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]
top_features_G3_without_G_LR <- names(sort(cor_with_G3[!names(cor_with_G3) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]

#----G1---
#Subset train and test data using only the selected features for G1
train_subset_G1_LR <- train_data[, c("G1", top_features_G1_without_G_LR)]
test_subset_G1_LR <- test_data[, c("G1", top_features_G1_without_G_LR)]

#Fit a linear regression model for G1
lm_model_G1 <- lm(G1 ~ ., data = train_subset_G1_LR)

#Predict on test data for G1
test_predictions_G1_LR <- predict(lm_model_G1, newdata = test_subset_G1_LR)

#Calculate Mean Absolute Error (MAE) for G1 
mae_G1_LR <- mean(abs(test_subset_G1_LR$G1 - test_predictions_G1_LR))

#To get min mae and it's index
if(mae_G1_LR <min_mae_G1_LR){
  min_mae_G1_LR<-mae_G1_LR
  index_mae_G1_LR<-i
}

#----G2---
#Subset train and test data using only the selected features for G2
train_subset_G2_LR <- train_data[, c("G2", top_features_G2_without_G_LR)]
test_subset_G2_LR <- test_data[, c("G2", top_features_G2_without_G_LR)]

#Fit a linear regression model for G2
lm_model_G2 <- lm(G2 ~ ., data = train_subset_G2_LR)

#Predict on test data for G2
test_predictions_G2_LR <- predict(lm_model_G2, newdata = test_subset_G2_LR)

#Calculate Mean Absolute Error (MAE) for G2 
mae_G2_LR <- mean(abs(test_subset_G2_LR$G2 - test_predictions_G2_LR))

#To get min mae and it's index
if(mae_G2_LR <min_mae_G2_LR){
  min_mae_G2_LR<-mae_G2_LR
  index_mae_G2_LR<-i
}

#----G3---
#Subset train and test data using only the selected features for G2
train_subset_G3_LR <- train_data[, c("G3", top_features_G3_without_G_LR)]
test_subset_G3_LR <- test_data[, c("G3", top_features_G3_without_G_LR)]

#Fit a linear regression model for G3
lm_model_G3 <- lm(G3 ~ ., data = train_subset_G3_LR)

#Predict on test data for G3
test_predictions_G3_LR <- predict(lm_model_G3, newdata = test_subset_G3_LR)

#Calculate Mean Absolute Error (MAE) for G3 
mae_G3_LR <- mean(abs(test_subset_G3_LR$G3 - test_predictions_G3_LR))

#To get min mae and it's index
if(mae_G3_LR <min_mae_G3_LR){
  min_mae_G3_LR<-mae_G3_LR
  index_mae_G3_LR<-i
}
}

cat(min_mae_G1_LR, " ", index_mae_G1_LR,"\n")
cat(min_mae_G2_LR, " ", index_mae_G3_LR,"\n")
cat(min_mae_G3_LR, " ", index_mae_G3_LR,"\n")


#step7-------Decision Tree---------------------------------


min_mae_G1_DT<-Inf
min_mae_G2_DT<-Inf
min_mae_G3_DT<-Inf

index_mae_G1_DT<-0
index_mae_G2_DT<-0
index_mae_G3_DT<-0
for (i in 1:12) {
#Sort attributes by absolute correlation values without G1, G2, G3 
top_features_G1_without_G_DT <- names(sort(cor_with_G1[!names(cor_with_G1) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]
top_features_G2_without_G_DT <- names(sort(cor_with_G2[!names(cor_with_G2) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]
top_features_G3_without_G_DT <- names(sort(cor_with_G3[!names(cor_with_G3) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]

#-----G1-----
#Subset train and test data using only the selected features for G1
train_subset_G1_DT <- train_data[, c("G1", top_features_G1_without_G_DT)]
test_subset_G1_DT <- test_data[, c("G1", top_features_G1_without_G_DT)]

#Fit a decision tree model for G1
tree_model_G1 <- rpart(G1 ~ ., data = train_subset_G1_DT, method = "anova")

#Predict on test data for G1
test_predictions_G1_DT <- predict(tree_model_G1, newdata = test_subset_G1_DT)

#Calculate Mean Absolute Error (MAE) for G1 
mae_G1_DT <- mean(abs(test_subset_G1_DT$G1 - test_predictions_G1_DT))

#To get min mae and it's index
if(mae_G1_DT <min_mae_G1_DT){
  min_mae_G1_DT<-mae_G1_DT
  index_mae_G1_DT<-i
}
#-----G2-------

#Subset train and test data using only the selected features for G2
train_subset_G2_DT <- train_data[, c("G2", top_features_G2_without_G_DT)]
test_subset_G2_DT <- test_data[, c("G2", top_features_G2_without_G_DT)]

#Fit a decision tree model for G2
tree_model_G2 <- rpart(G2 ~ ., data = train_subset_G2_DT, method = "anova")

#Predict on test data for G2
test_predictions_G2_DT <- predict(tree_model_G2, newdata = test_subset_G2_DT)

#Calculate Mean Absolute Error (MAE) for G2 
mae_G2_DT <- mean(abs(test_subset_G2_DT$G2 - test_predictions_G2_DT))

#To get min mae and it's index
if(mae_G2_DT <min_mae_G2_DT){
  min_mae_G2_DT<-mae_G2_DT
  index_mae_G2_DT<-i
}
#-----G3-------

#Subset train and test data using only the selected features for G3
train_subset_G3_DT <- train_data[, c("G3", top_features_G3_without_G_DT)]
test_subset_G3_DT <- test_data[, c("G3", top_features_G3_without_G_DT)]

#Fit a decision tree model for G3
tree_model_G3 <- rpart(G3 ~ ., data = train_subset_G3_DT, method = "anova")

#Predict on test data for G3
test_predictions_G3_DT <- predict(tree_model_G3, newdata = test_subset_G3_DT)

#Calculate Mean Absolute Error (MAE) for G3 
mae_G3_DT <- mean(abs(test_subset_G3_DT$G3 - test_predictions_G3_DT))

#To get min mae and it's index
if(mae_G3_DT <min_mae_G3_DT){
  min_mae_G3_DT<-mae_G3_DT
  index_mae_G3_DT<-i
}
}
#Print min mae and it's index 
cat(min_mae_G1_DT, " ", index_mae_G1_DT,"\n")
cat(min_mae_G2_DT, " ", index_mae_G3_DT,"\n")
cat(min_mae_G3_DT, " ", index_mae_G3_DT,"\n")

#step8-------Use the prediction of G1_DT as an feature------------

#Compare between linear regression and decision tree
cat(min_mae_G1_LR, " ", min_mae_G1_DT,"\n")
cat(min_mae_G2_LR, " ", min_mae_G2_DT,"\n")
cat(min_mae_G3_LR, " ", min_mae_G3_DT,"\n")

#Sort attributes by absolute correlation values without G1, G2, G3 
top_features_G1_without_G_DT <- names(sort(cor_with_G1[!names(cor_with_G1) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:7]

#Subset train and test data using only the selected features for G1
train_subset_G1_DT <- train_data[, c("G1", top_features_G1_without_G_DT)]
test_subset_G1_DT <- test_data[, c("G1", top_features_G1_without_G_DT)]

#Fit a decision tree model for G1
tree_model_G1 <- rpart(G1 ~ ., data = train_subset_G1_DT, method = "anova")

#Predict on test data for G1
train_predictions_G1_DT <- predict(tree_model_G1, newdata = train_subset_G1_DT)
test_predictions_G1_DT <- predict(tree_model_G1, newdata = test_subset_G1_DT)

#Create a data frame with the columns G1 and Predictions for G1 
result_df_G1_DT <- data.frame(Actual = test_subset_G1_DT$G1, Predictions = test_predictions_G1_DT)

#View the resulting data frame for G1 
print(result_df_G1_DT)

train_data_edit<-train_data
train_data_edit$predictions <- train_predictions_G1_DT

test_data_edit<-test_data
test_data_edit$predictions <- test_predictions_G1_DT

combined_predictions <- rbind(train_data_edit, test_data_edit)

cor_matrix_edit <- cor(combined_predictions)
#Correlation with G1, G2, and G3
cor_with_G2_edit <- abs(cor_matrix_edit["G2", ])
cor_with_G3_edit <- abs(cor_matrix_edit["G3", ])

#--------new LR------ 

min_mae_G2_LR<-Inf
min_mae_G3_LR<-Inf

index_mae_G2_LR<-0
index_mae_G3_LR<-0
for (i in 1:13) {
  #Sort attributes by absolute correlation values without G1, G2, G3 
  top_features_G2_without_G_LR <- names(sort(cor_with_G2_edit[!names(cor_with_G2_edit) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]
  top_features_G3_without_G_LR <- names(sort(cor_with_G3_edit[!names(cor_with_G3_edit) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]
  
  
  #----G2---
  #Subset train and test data using only the selected features for G2
  train_subset_G2_LR <- train_data_edit[, c("G2", top_features_G2_without_G_LR)]
  test_subset_G2_LR <- test_data_edit[, c("G2", top_features_G2_without_G_LR)]
  
  #Fit a linear regression model for G2
  lm_model_G2 <- lm(G2 ~ ., data = train_subset_G2_LR)
  
  #Predict on test data for G2
  test_predictions_G2_LR <- predict(lm_model_G2, newdata = test_subset_G2_LR)
  
  #Calculate Mean Absolute Error (MAE) for G2 
  mae_G2_LR <- mean(abs(test_subset_G2_LR$G2 - test_predictions_G2_LR))
  
  #To get min mae and it's index
  if(mae_G2_LR <min_mae_G2_LR){
    min_mae_G2_LR<-mae_G2_LR
    index_mae_G2_LR<-i
  }
  
  #----G3---
  #Subset train and test data using only the selected features for G2
  train_subset_G3_LR <- train_data_edit[, c("G3", top_features_G3_without_G_LR)]
  test_subset_G3_LR <- test_data_edit[, c("G3", top_features_G3_without_G_LR)]
  
  #Fit a linear regression model for G3
  lm_model_G3 <- lm(G3 ~ ., data = train_subset_G3_LR)
  
  #Predict on test data for G3
  test_predictions_G3_LR <- predict(lm_model_G3, newdata = test_subset_G3_LR)
  
  #Calculate Mean Absolute Error (MAE) for G3 
  mae_G3_LR <- mean(abs(test_subset_G3_LR$G3 - test_predictions_G3_LR))
  
  #To get min mae and it's index
  if(mae_G3_LR <min_mae_G3_LR){
    min_mae_G3_LR<-mae_G3_LR
    index_mae_G3_LR<-i
  }
}

cat(min_mae_G2_LR, " ", index_mae_G3_LR,"\n")
cat(min_mae_G3_LR, " ", index_mae_G3_LR,"\n")

#-----------new DT------------

min_mae_G2_DT <- Inf
min_mae_G3_DT <- Inf

index_mae_G2_DT <- 0
index_mae_G3_DT <- 0

for (i in 1:13) {
  # Sort attributes by absolute correlation values without G1, G2, G3 
  top_features_G2_without_G_DT <- names(sort(cor_with_G2_edit[!names(cor_with_G2_edit) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]
  top_features_G3_without_G_DT <- names(sort(cor_with_G3_edit[!names(cor_with_G3_edit) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]
  
  # ----G2---
  # Subset train and test data using only the selected features for G2
  train_subset_G2_DT <- train_data_edit[, c("G2", top_features_G2_without_G_DT)]
  test_subset_G2_DT <- test_data_edit[, c("G2", top_features_G2_without_G_DT)]
  
  # Fit a linear regression model for G2
  tree_model_G2 <- rpart(G2 ~ ., data = train_subset_G2_DT, method = "anova")
  
  # Predict on test data for G2
  test_predictions_G2_DT <- predict(tree_model_G2, newdata = test_subset_G2_DT)
  
  # Calculate Mean Absolute Error (MAE) for G2 
  mae_G2_DT <- mean(abs(test_subset_G2_DT$G2 - test_predictions_G2_DT))
  
  # To get min mae and its index
  if (mae_G2_DT < min_mae_G2_DT) {
    min_mae_G2_DT <- mae_G2_DT
    index_mae_G2_DT <- i
  }
  
  # ----G3---
  # Subset train and test data using only the selected features for G3
  train_subset_G3_DT <- train_data_edit[, c("G3", top_features_G3_without_G_DT)]
  test_subset_G3_DT <- test_data_edit[, c("G3", top_features_G3_without_G_DT)]
  
  # Fit a linear regression model for G3
  tree_model_G3 <- rpart(G3 ~ ., data = train_subset_G3_DT, method = "anova")
  
  # Predict on test data for G3
  test_predictions_G3_DT <- predict(tree_model_G3, newdata = test_subset_G3_DT)
  
  # Calculate Mean Absolute Error (MAE) for G3 
  mae_G3_DT <- mean(abs(test_subset_G3_DT$G3 - test_predictions_G3_DT))
  
  # To get min mae and its index
  if (mae_G3_DT < min_mae_G3_DT) {
    min_mae_G3_DT <- mae_G3_DT
    index_mae_G3_DT <- i
  }
}

cat(min_mae_G2_DT, " ", index_mae_G3_DT, "\n")
cat(min_mae_G3_DT, " ", index_mae_G3_DT, "\n")

#step9-------Use the prediction of G1 and G2 as features------------

# Sort attributes by absolute correlation values without G1, G2, G3 
top_features_G2_without_G_DT <- names(sort(cor_with_G2_edit[!names(cor_with_G2_edit) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:13]


# ----G2---
# Subset train and test data using only the selected features for G2
train_subset_G2_DT <- train_data_edit[, c("G2", top_features_G2_without_G_DT)]
test_subset_G2_DT <- test_data_edit[, c("G2", top_features_G2_without_G_DT)]

# Fit a linear regression model for G2
tree_model_G2 <- rpart(G2 ~ ., data = train_subset_G2_DT, method = "anova")

# Predict on test data for G2
train_predictions_G2_DT <- predict(tree_model_G2, newdata = train_subset_G2_DT)
test_predictions_G2_DT <- predict(tree_model_G2, newdata = test_subset_G2_DT)

train_data_edit2<-train_data_edit
train_data_edit2$predictions2 <- train_predictions_G2_DT

test_data_edit2<-test_data_edit
test_data_edit2$predictions2 <- test_predictions_G2_DT

combined_predictions2 <- rbind(train_data_edit2, test_data_edit2)

cor_matrix_edit2 <- cor(combined_predictions2)
#Correlation with G1, G2, and G3
cor_with_G3_edit2 <- abs(cor_matrix_edit2["G3", ])

#-------------new LR-------------------
min_mae_G3_LR<-Inf

index_mae_G3_LR<-0
for (i in 1:14) {
  #Sort attributes by absolute correlation values without G1, G2, G3 
  top_features_G3_without_G_LR <- names(sort(cor_with_G3_edit2[!names(cor_with_G3_edit2) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]
  
  #----G3---
  #Subset train and test data using only the selected features for G2
  train_subset_G3_LR <- train_data_edit2[, c("G3", top_features_G3_without_G_LR)]
  test_subset_G3_LR <- test_data_edit2[, c("G3", top_features_G3_without_G_LR)]
  
  #Fit a linear regression model for G3
  lm_model_G3 <- lm(G3 ~ ., data = train_subset_G3_LR)
  
  #Predict on test data for G3
  test_predictions_G3_LR <- predict(lm_model_G3, newdata = test_subset_G3_LR)
  
  #Calculate Mean Absolute Error (MAE) for G3 
  mae_G3_LR <- mean(abs(test_subset_G3_LR$G3 - test_predictions_G3_LR))
  
  #To get min mae and it's index
  if(mae_G3_LR <min_mae_G3_LR){
    min_mae_G3_LR<-mae_G3_LR
    index_mae_G3_LR<-i
  }
}

cat(min_mae_G3_LR, " ", index_mae_G3_LR,"\n")

#------------new DT -----------

min_mae_G3_DT <- Inf
index_mae_G3_DT <- 0

for (i in 1:14) {
  # Sort attributes by absolute correlation values without G1, G2, G3 
  
  top_features_G3_without_G_DT <- names(sort(cor_with_G3_edit2[!names(cor_with_G3_edit2) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:i]
  
  # ----G3---
  # Subset train and test data using only the selected features for G3
  train_subset_G3_DT <- train_data_edit2[, c("G3", top_features_G3_without_G_DT)]
  test_subset_G3_DT <- test_data_edit2[, c("G3", top_features_G3_without_G_DT)]
  
  # Fit a linear regression model for G3
  tree_model_G3 <- rpart(G3 ~ ., data = train_subset_G3_DT, method = "anova")
  
  # Predict on test data for G3
  test_predictions_G3_DT <- predict(tree_model_G3, newdata = test_subset_G3_DT)
  
  # Calculate Mean Absolute Error (MAE) for G3 
  mae_G3_DT <- mean(abs(test_subset_G3_DT$G3 - test_predictions_G3_DT))
  
  # To get min mae and its index
  if (mae_G3_DT < min_mae_G3_DT) {
    min_mae_G3_DT <- mae_G3_DT
    index_mae_G3_DT <- i
  }
}

cat(min_mae_G3_DT, " ", index_mae_G3_DT, "\n")

#--------------------------

#Sort attributes by absolute correlation values without G1, G2, G3 
top_features_G3_without_G_LR <- names(sort(cor_with_G3_edit2[!names(cor_with_G3_edit2) %in% c("G1", "G2", "G3")], decreasing = TRUE))[1:14]

#----G3---
#Subset train and test data using only the selected features for G2
train_subset_G3_LR <- train_data_edit2[, c("G3", top_features_G3_without_G_LR)]
test_subset_G3_LR <- test_data_edit2[, c("G3", top_features_G3_without_G_LR)]

#Fit a linear regression model for G3
lm_model_G3 <- lm(G3 ~ ., data = train_subset_G3_LR)

#Predict on test data for G3
train_predictions_G3_LR <- predict(lm_model_G3, newdata = train_subset_G3_LR)
test_predictions_G3_LR <- predict(lm_model_G3, newdata = test_subset_G3_LR)

train_data_edit3<-train_data_edit2
train_data_edit3$predictions3 <- train_predictions_G3_LR

test_data_edit3<-test_data_edit2
test_data_edit3$predictions3 <- test_predictions_G3_LR

combined_predictions3 <- rbind(train_data_edit3, test_data_edit3)

#------------

#Create a data frame with the columns G1 and Predictions for G1 
result_G1 <- data.frame(Actual = combined_predictions3$G1, Predictions = combined_predictions3$predictions)

#View the resulting data frame for G1 
print(result_G1 )

# Calculate Mean Absolute Error (MAE) for G1 
mae_G1 <- mean(abs(combined_predictions3$G1 - combined_predictions3$predictions))
print(mae_G1 )


#Create a data frame with the columns G2 and Predictions for G2 
result_G2 <- data.frame(Actual = combined_predictions3$G2, Predictions = combined_predictions3$predictions2)

#View the resulting data frame for G2 
print(result_G2 )

# Calculate Mean Absolute Error (MAE) for G2 
mae_G2 <- mean(abs(combined_predictions3$G2 - combined_predictions3$predictions2))
print(mae_G2)


#Create a data frame with the columns G3 and Predictions for G3 
result_G3 <- data.frame(Actual = combined_predictions3$G3, Predictions = combined_predictions3$predictions3)

#View the resulting data frame for G3 
print(result_G3 )

# Calculate Mean Absolute Error (MAE) for G3 
mae_G3 <- mean(abs(combined_predictions3$G3 - combined_predictions3$predictions3))
print(mae_G3)
