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

# Selecting 2018 year's dataset
data_2018 <- db[format(db$Occurred.Date, "%Y") == "2018", ]
crime2018 <- data_2018
# view the dataset
View(crime2018)
sum(is.na(crime2018$Sector))
summary(crime2018)

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









