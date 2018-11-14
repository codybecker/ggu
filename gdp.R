library(dplyr)
library(ggplot2)

if (!dir.exists('data')) {dir.create('data')}
fileURL <- "https://apps.bea.gov/regional/zip/SAGDP.zip"
download.file(fileURL, "data/GDP.zip")
unzip("data/GDP.zip", exdir = "data/GDPdata")
gdp <- read.csv("data/GDPdata/SAGDP2N__ALL_AREAS_1997_2017.csv")
df <- gdp 

glimpse(df)
df[,10:30] <- lapply(df[,10:30], as.numeric) 
names(df) <- gsub("X", "", names(df))
                                                                                                                                                                                                             
