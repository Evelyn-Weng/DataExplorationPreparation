## a) Identify which variables are categorical, discrete and continuous in the chosen data set and show
## using some visualization or plot. Explore whether there are missing values for any of the variables

## Data Cleaning

db <- read.csv('Crime_Data.csv')
dim(db)
names(db)

#[1] "Report.Number"-->discrete(value between 200800005 and 2011000011274624)           
#[2] "Occurred.Date"-->Categorical Data              
#[3] "Occurred.Time"-->discrete(value between 0 and 2359)
#[4] "Reported.Date"-->Categorical Data
#[5] "Reported.Time"-->discrete(value between 0 and 2359)
#[6] "Crime.Subcategory"-->categorical       
#[7] "Primary.Offense.Description"-->categorical
#[8] "Precinct"-->Categorical Data
#[9] "Sector"-->categorical                     
#[10] "Beat"-->categorical                       
#[11] "Neighborhood"-->categorical
#In this dataset there isn't any continuous data

## Before plot anything, check if there is any missing value
any(is.na(db))
## subdataset
db$Occurred.Date <- as.Date(db$Occurred.Date, format = "%m/%d/%Y")
db$Reported.Date <- as.Date(db$Reported.Date, format = "%m/%d/%Y")
View(db)
# Selecting 2018 year's dataset
data_2018 <- db[format(db$Occurred.Date, "%Y") == "2018", ]
crime2018 <- data_2018

# view the dataset
View(crime2018)
sum(is.na(crime2018$Sector))
## summary(crime2018)

## check each column if any empty value, or value not correct
unique(crime2018$Report.Number)
unique(crime2018$Occurred.Date)
unique(crime2018$Occurred.Time)
unique(crime2018$Reported.Date)
unique(crime2018$Crime.Subcategory)
unique(crime2018$Primary.Offense.Description)
unique(crime2018$Precinct)
unique(crime2018$Sector)
unique(crime2018$Neighborhood)
## there are empty value in Precinct and sector, and [precinct='M'] and [sector = 9512]
## replace them by using mode value


# use table to calculate in 'Sector' and 'precinct' each value's frequency
Precinct_frequency <- table(crime2018$Precinct)
sector_frequency <- table(crime2018$Sector)

# find out the mode value of precinct and sector
mode_Precinct <- names(Precinct_frequency)[Precinct_frequency == max(Precinct_frequency)]
mode_sector <- names(sector_frequency)[sector_frequency == max(sector_frequency)]

# use mode value to replace missing value and empty value, [precinct='M'] and [sector = 9512]
crime2018$Precinct <- ifelse(is.na(crime2018$Precinct), mode_Precinct, crime2018$Precinct)
crime2018$Precinct[crime2018$Precinct == ""] <- mode_Precinct
crime2018$Precinct[crime2018$Precinct == "M"] <- mode_Precinct

crime2018$Sector <- ifelse(is.na(crime2018$Sector), mode_sector, crime2018$Sector)
crime2018$Sector[crime2018$Sector == ""] <- mode_sector
crime2018$Sector[crime2018$Sector == "9512"] <- mode_sector
# check if still have missing and empty value
sum(is.na(crime2018$Sector))
unique(crime2018$Precinct)
unique(crime2018$Sector)


##write.csv(crime2018,'crime2018.csv', row.names = FALSE)

## Drop column 'Beat', just keep 10 columns, then have the final Cleaned dataset
Newcrime2018 <- crime2018[, -which(names(crime2018) == 'Beat')]
View(Newcrime2018)
## Drop last 2 NA rows, which is empty row
Newcrime2018 <- Newcrime2018[1:(nrow(Newcrime2018) -2),]
sum(is.na(Newcrime2018))
write.csv(Newcrime2018,'Newcrime2018.csv',row.names = FALSE)

## generate a plot of the new dataset
library(ggplot2)

#create a summary table of the precinct distribution
precinct_summary <- table(Newcrime2018$Precinct)

## convert the summarty table to a dataframe
precinct_summary_df <- as.data.frame(precinct_summary)
names(precinct_summary_df) <- c('Precinct', 'Count')

## create a pie chart of each precinct
ggplot(precinct_summary_df, aes(x= '' , y= Count, fill = Precinct)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y') +
  labs(title = 'Distribution of Incidents Across Precincts',
       fill = 'Precinct') +
  theme_minimal() +
  theme(legend.position = 'right')

# b) Calculate the statistical parameters (mean, median, minimum, maximum, and standard deviation)
# for each of the numerical variables.
names(Newcrime2018)
## [1] "Report.Number"               "Occurred.Date"               "Occurred.Time"              
## [4] "Reported.Date"               "Reported.Time"               "Crime.Subcategory"          
## [7] "Primary.Offense.Description" "Precinct"                    "Sector"                     
## [10] "Neighborhood"  

## In this case, "Report.Number"(200800005-2011000011274624),
## "Occurred.Time"(0-2359),"Reported.Time"(0-2359) are numerical varibale
## 'Report Number' in the real life that be something like Report Id,
## "Occurred.Time"(0-2359),"Reported.Time"(0-2359) should be in time format, not recomment to 
## calculate the statistical parameters, but since there isn't any better value to do,
## so I will still stick with those three to calculate.

## Using summary function to get all columns of mean, median, minimum, maximum
summary(Newcrime2018)

## by using psych library to get all columns' standard deviation
## install.packages('psych')
library(psych)

summary_Newcrime2018 <- describe(Newcrime2018)
print(summary_Newcrime2018)
## also can use sd() to get only one standard deviation

## c) Apply Min-Max Normalization, Z-score Standardization and Robust scalar on the numerical data
## variables.
## 1. Apply Min-Max Normalization on "Report.Number" 
mmnorm.ReportedNumber <- (Newcrime2018$Report.Number-min(Newcrime2018$Report.Number)) /(max(Newcrime2018$Report.Number)-min(Newcrime2018$Report.Number))
mmnorm.ReportedNumber## the value always between 0-1, and it is.

## 2. Apply Z-score Standardization on"Occurred.Time"
zscore.OccurredTime <- (Newcrime2018$Occurred.Time-mean(Newcrime2018$Occurred.Time))/sd(Newcrime2018$Occurred.Time)
zscore.OccurredTime

## 3. Apply Robust scalar on "Reported.Time"
## using summary to get Q3 and Q1 value again
summary(Newcrime2018$Reported.Time)

RobustScalarReportedTime <- (Newcrime2018$Reported.Time - mean(Newcrime2018$Reported.Time))/(1808 -  943)
RobustScalarReportedTime

## d) Line, Scatter and Heatmaps can be used to show the correlation between the features of the
## dataset.
##write.csv(Newcrime2018,'Newcrime2018.csv', row.names = FALSE) export new csv file
## 1.Line Chart
ggplot(Newcrime2018, aes(x = Precinct, group = Reported.Date, color = Reported.Date)) +
  geom_line(stat = "count") +
  labs(title = "Reported.Date Incidents Count Over Time for Each Precinct",
       x = "Precinct",
       y = "Incidents Count") +
  theme_minimal()

## 2.Scatter
ggplot(Newcrime2018, aes(x = Crime.Subcategory, y = Occurred.Time)) +
  geom_point() +
  labs(title = 'Scatter Plot of Crime Subcategory and Occurred Time',
       x = 'Crime.Subcategory',
       y = 'Occurred.Time') +
  ## theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5,size = 8, vjust = 1)) 
ggsave('scatter_plot.png', width = 18, height = 6)
  ## scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 0, "", x))  # Display every second label
  
  
## 3.Heatmaps
library(ggplot2)
library(dplyr)
library(scales)
View(Newcrime2018)
# change the order by occurred.Time
Newcrime2018$Occurred.Time <- factor(Newcrime2018$Occurred.Time, levels = unique(Newcrime2018$Occurred.Time))
# Create time period using cut function
Newcrime2018$TimePeriod <- cut(as.numeric(Newcrime2018$Occurred.Time), breaks = seq(0, 2400, by = 200), include.lowest = TRUE)
#check TimePeriod data type
##str(Newcrime2018$TimePeriod)

##Newcrime2018$TimePeriod <- as.numeric(as.character(Newcrime2018$TimePeriod), na.rm = TRUE)
##View(Newcrime2018)
##write.csv(Newcrime2018,'test.csv', row.names = FALSE)


# Create a heatmap with 'Neighborhood' on the x-axis, 'TimePeriod' on the y-axis, and color representing the count of incidents
ggplot(Newcrime2018, aes(x = Neighborhood, y = TimePeriod, fill = after_stat(x))) +
  geom_tile() +  
  scale_fill_gradient(name = "Incident Count", trans = "log", low = "green", high = "red") +
  labs(title = "Heatmap of Incidents by Occurred Time and Neighborhood",
       x = "Neighborhood",
       y = "Occurred Time") +
##  scale_y_discrete(labels = scales::number_format()) +
#  theme_minimal() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1,size = 5, vjust = 1))
##View(Newcrime2018)

##  e) Graphics and descriptive understanding should be provided along with Data Exploratory analysis
## (EDA). Identify subgroups of features that can explore some interesting facts.

## Visualizing Distributions

## descriptive statistics
summary(Newcrime2018)

# Histogram of Occurred.Time
ggplot(data = Newcrime2018) +
  geom_bar(mapping = aes(x = Occurred.Time, fill = "blue")) +
  labs(title = "Distribution of Occurred.Time")

## Newcrime2018$ReportedTimePeriod <- cut(as.numeric(Newcrime2018$Reported.Time), breaks = seq(0, 2400, by = 200), include.lowest = TRUE)

hist(Newcrime2018$Occurred.Date,
     freq = TRUE,
     xlab = 'Occurred.Date',
     main = 'Distribution of Occurred.Date',
     col = 'lightgreen',
     breaks = 'week',
    
     xlim = c(min(Newcrime2018$Occurred.Date), max(Newcrime2018$Occurred.Date)),

# Customize x-axis
    axis(1, at = seq(min(Newcrime2018$Occurred.Date), max(Newcrime2018$Occurred.Date), by = 'month'),
     cex.axis = 0.7))# las = 2 for vertical labels)

## overlay multiple histogram in the same plot
ggplot(Newcrime2018,mapping = aes(x = Occurred.Date, color = Sector)) +
  geom_freqpoly(binwidth = 0.1) +
  labs(title = 'Distribution of Occurred.Date and Sector')

## Apply dummy encoding to categorical variables (at least one variable used from the data set) and
## discuss the benefits of dummy encoding to understand the categorical data.
library(dummy)
## check how many variable in sector column
unique(Newcrime2018$Sector)
## 17 variables in sector
##"O","L","F","E","S","K","D","G","M","R","B","J","U","Q","W","N","C"
## more than 2 variables, will be k-1
library(fastDummies)
# Dummy encoding using fastDummies
Newcrime2018$Sectordummies <- as.data.frame(model.matrix(~ Newcrime2018$Sector -1))

# View the resulting data with the new dummy-encoded columns
View(Newcrime2018)

## the benefits of dummy encoding
## Good for machine learning to estimation models, such as linear regression
## easy to coding, improve efficiency


library(dplyr)
## install.packages('factoextra')
library(stats)

## select columns and create a new dataset named PCAData
## i dont have enough numeric value, so change Occurred.Time factor to numeric
Newcrime2018$Occurred.Time <- as.numeric(as.character(Newcrime2018$Occurred.Time))
selected_columns <- c('Report.Number','Occurred.Time')
PCAData <- Newcrime2018[, selected_columns]
View(PCAData)
##str(Newcrime2018$NewSector)
##summary(PCAData)
## check PCA eligibility -+0.3 correlation
## in ther is 0.5049047 which prety high
cor(PCAData)
mean(cor(PCAData))

## Principal component analysis
PCA = princomp(PCAData)

## PCA Loadings to how was oringal variable are loaded into these
PCA$loadings

## h) What is the purpose of dimensionality reduction? Explore the situations where you can gain the
## benefit of dimensionality reduction for data analysis#

#install.packages("randomForest")
library(randomForest)

# Create a Random Forest model
rf_model <- randomForest(NewSector ~ ., data = Newcrime2018, importance = TRUE)

# Extract feature importance scores
feature_importance <- rf_model$importance

# Order features by importance
ordered_features <- feature_importance[order(-feature_importance[, "MeanDecreaseGini"]), , drop = FALSE]

# Print the ordered features
print(ordered_features)


## The benefit of dimensionality reduction
## it help to understand easily, and request lower memory space easily to proccessing
## can help to remove noise, and help a better robust models