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

# use table to calculate in 'Sector' each value's frequency
sector_frequency <- table(crime2018$Sector)

# find out the mode value
mode_sector <- names(sector_frequency)[sector_frequency == max(sector_frequency)]

# use mode value to replace NA value in Sector
crime2018$Sector <- ifelse(is.na(crime2018$Sector), mode_sector, crime2018$Sector)
# check if still have NA value
sum(is.na(crime2018$Sector))

## Drop column 'Beat', just keep 10 columns, then have the final Cleaned dataset
Newcrime2018 <- crime2018[, -which(names(crime2018) == 'Beat')]
View(Newcrime2018)
## Drop last 2 NA rows
Newcrime2018 <- Newcrime2018[1:(nrow(Newcrime2018) -2),]

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
## 2.Scatter
ggplot(Newcrime2018, aes(x = Crime.Subcategory, y = Occurred.Time)) +
  geom_point() +
  labs(title = 'Scatter Plot of Crime Subcategory and Occurred Time',
       x = 'Crime.Subcategory',
       y = 'Occurred.Time')
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0, size = 10, vjust = 0))
  ## scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 0, "", x))  # Display every second label
  
  


## 3.Heatmaps









