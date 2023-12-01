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