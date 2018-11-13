## libraries
library(dplyr)
library(tidry)

## Get the data
if (!file.exists('data')) {
  dir.create('data')
}
setwd('data')
deathsFile <- "https://docs.google.com/spreadsheets/d/1uBVH84OpkMsvQeSzc7oWuDnBhqmUt0Agid8eAT4ERi4/gviz/tq?tqx=out:csv&sheet=NCHS_-_Leading_Causes_of_Death__United_States"
incomeFile <- "https://docs.google.com/spreadsheets/d/1pcaOuokmGrSy4jc_VvAbB2SKh3AyCKMKngTYuSsZWtk/gviz/tq?tqx=out:csv&sheet=medianHouseholdIncom1999"
download.file(deathsFile, destfile = "deaths.csv", method = 'curl')
download.file(incomeFile, destfile = "income.csv", method = 'curl')
dateDownloaded <- date()
# data to data frame
deaths.df <- data.frame(read.csv("deaths.csv"))
income.df <- data.frame(read.csv("income.csv"))

## Summary of the Data
# deaths
head(deaths.df, 3)
summary(deaths.df) # year needs to be a factor
deaths.df$Year <- as.factor(deaths.df$Year) # convert year to factor
str(deaths.df)
quantile(deaths.df$Age.adjusted.Death.Rate, na.rm=TRUE)
# income
head(income.df, 3)
summary(income.df) #median.household.income needs to be converted to integer
income.df$median.household.income <- as.numeric(gsub("[\\$,]","", income.df$median.household.income)) #convert string to numeric
str(income.df)
quantile(income.df$median.household.income, na.rm=TRUE)
# check for missing values
sum(is.na(deaths.df))
sum(is.na(income.df))

## Subset data; 
# yr = 1999
deaths.99.df <- deaths.df[deaths.df$Year %in% 1999,]
deaths.99.df$Year <- factor(deaths.99.df$Year)
# remove "United States" from state variable
deaths99 <- filter(deaths.99.df, State != "United States")
# get rid of IDC code variable
deaths99 <- select(deaths99, -X113.Cause.Name)
# rename variables
deaths99 <- rename(deaths99, year = Year, causeName = Cause.Name, state = State,
                   deaths = Deaths, ageAdjDeathRate = Age.adjusted.Death.Rate)
# "all causes" should be a variable not value
df <- filter(deaths99, causeName == "All causes")
deaths99 <- filter(deaths99, causeName != "All causes")
deaths99$causeName <- factor(deaths99$causeName) # drops "All causes"
df <- spread(df, causeName, deaths)
df <- rename(df, totalDeaths = "All causes")
df <- select(df, c(state, totalDeaths))
deaths99 <- merge(deaths99, df)
# add a percent variable & sort the new table
deaths99 <- mutate(deaths99, percent = deaths / totalDeaths)
deaths99 <- arrange(deaths99, state, desc(percent))
# reorder columns
deaths99 <- deaths99[c('year', 'state', 'causeName', 'deaths', 
                       'percent', 'ageAdjDeathRate', 'totalDeaths')]
#merge income
deaths99 <- merge(deaths99, income.df)
# re-order columns and sort
deaths99 <- arrange(deaths99, desc(ageAdjDeathRate))

## Crostabs
xtabs(Age.adjusted.Death.Rate ~ State + Cause.Name, deaths.99.df)
