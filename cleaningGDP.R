#some random levels we don't want
nonWantedLevels<-c(1,2,4)
#just the filter part
df5 <- df4 %>% filter(!as.integer(df4$GeoName) %in% nonWantedLevels)
levels(df$GeoName)
droplevels(df5)
unique(df$GeoName)[2:52]
wantedLevels<- unique(df$GeoName)[2:52]
wantedLevels<- droplevels(wantedLevels)
df6 <- df[df$GeoName %in% wantedLevels,]
df7 <- df6[complete.cases(df6),]
df8 <- df7 %>% filter(gdp > 8000)
levels(df8$Description)




df10 <- df9 %>% filter(Description == "All industry total")
