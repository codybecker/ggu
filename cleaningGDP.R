
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


