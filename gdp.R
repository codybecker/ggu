library(dplyr)
library(tidyr)
library(ggplot2)


## Download the data if needed---------------------------
# if (!dir.exists('data')) {dir.create('data')}
# fileURL <- "https://apps.bea.gov/regional/zip/SAGDP.zip"
# download.file(fileURL, "data/GDP.zip")
# unzip("data/GDP.zip", exdir = "data/GDPdata")
## ------------------------------------------------------

gdp <- read.csv("data/GDPdata/SAGDP2N__ALL_AREAS_1997_2017.csv")

df <- gdp 
df$IndustryId <- as.factor(df$IndustryId)

## Gather 'year' 
df[,10:30] <- lapply(df[,10:30], as.character) 
df[,10:30] <- lapply(df[,10:30], as.numeric) 
names(df) <- gsub("X", "", names(df))
df <- df %>% 
  gather(`1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`,
         `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`,
         `2015`, `2016`, `2017`, key = "year", value = "gdp")
df[, 'year'] <- as.factor(df[,'year'])


## Build the first cut
unique(df$GeoName)[2:52] ##drop "United States*"
wantedLevels<- unique(df$GeoName)[2:52]
wantedLevels<- droplevels(wantedLevels)
df <- df[df$GeoName %in% wantedLevels,]
df <- df[complete.cases(df),]
df <- df %>% 
  filter(gdp > 8000)
df <- droplevels(df)
df <- df %>% 
  filter(Description == "All industry total") 

## Second cut -- total GDP per state per year 1997-2017
df <- gdp 

unique(df$GeoName)[2:52]
wantedLevels<- unique(df$GeoName)[2:52]
wantedLevels<- droplevels(wantedLevels)
df <- df[df$GeoName %in% wantedLevels,]
df <- df[complete.cases(df),]
df <- df %>% 
  filter(gdp > 8000)
df <- droplevels(df)
df <- df %>% 
  filter(Description == "All industry total") 

df3 <- df %>%
  mutate(gdp = (gdp *10^6), 
         log_gdp = log(gdp),
         log2_gdp = log2(gdp)) %>%
  select(state = "GeoName", "year", "gdp", "log_gdp",
         "log2_gdp") %>%
  group_by(state) %>%
  mutate(mean_gdp = mean(gdp)) %>%
  arrange(mean_gdp)
df3$state <- factor(df3$state, unique(df3$state))
cutpoints <- quantile(df3$gdp, seq(0, 1, length = 4), na.rm = TRUE)
df3$gdpdec <- cut(df3$gdp, cutpoints)
df3 <- na.omit(df3) ##somwhere after line 25 an NA is introduced
levels(df3$gdpdec)
df3$hilow <- factor(df3$gdpdec, levels=rev(levels(factor(df3$gdpdec))))


## Third cut -- looking at industries
library(tidyverse)
temp <- df %>%
  select(state = "GeoName", naicsCode = "IndustryId", 
         description = "Description", "year", "gdp") %>%
  mutate(gdp = (gdp * 10^6),
         log_gdp = log(gdp))


