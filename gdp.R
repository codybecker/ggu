library(dplyr)
library(tidyr)
library(ggplot2)

# if (!dir.exists('data')) {dir.create('data')}
# fileURL <- "https://apps.bea.gov/regional/zip/SAGDP.zip"
# download.file(fileURL, "data/GDP.zip")
# unzip("data/GDP.zip", exdir = "data/GDPdata")
gdp <- read.csv("data/GDPdata/SAGDP2N__ALL_AREAS_1997_2017.csv")
df <- gdp 


df[,10:30] <- lapply(df[,10:30], as.character) 
df[,10:30] <- lapply(df[,10:30], as.numeric) 
names(df) <- gsub("X", "", names(df))
df <- df %>% 
  gather(`1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`,
         `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`,
         `2015`, `2016`, `2017`, key = "year", value = "gdp")
df[, 'year'] <- as.factor(df[,'year'])

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




