library(ggplot2)
library(dplyr)
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

df2 <- df %>% mutate(aboveMedian = (df$gdp > 161512))

par(mfrow = c(1,1), mar = c(4,4,2,1))
hist(subset(df2, aboveMedian == "TRUE")$gdp, breaks = "Sturges", col = "green")
hist(subset(df2, aboveMedian == "FALSE")$gdp, breaks = "Sturges", col = "green")
qplot(log(gdp), data = df, binwidth = .1, fill = Region)
qplot(log(gdp), data = df, binwidth = .1, fill = year)

hist(df[df$GeoName == "California",]$gdp)

head(df[, c("GeoName", "year", "gdp")])


df$Region <- as.factor(df$Region)
levels(df$Region)

x <- rnorm(1000)




df2 <- df2 %>% mutate(quartile = ntile(gdp, 4))
qplot(log(gdp), data = df2, binwidth = .1, fill = quartile)
df2$quartile <- as.factor(df2$quartile)

qplot(log(gdp), data = df2, binwidth = .1, fill = Region)


df_years <- df %>% 
  group_by(year) %>%
  mutate(quartile_years = ntile(gdp, 5))
df_years$quartile_years <- as.factor(df_years$quartile_years)
qplot(log(gdp), data = df_years, binwidth = .1, fill = quartile_years)
## some states are in more than one "quartile_years"

df_states <- df %>% 
  group_by(GeoName) %>%
  mutate(quintile_states = ntile(gdp, 10))
df_states$quintile_states <- as.factor(df_states$quintile_states)
qplot(log(gdp), data = df_states, binwidth = .1, fill = quintile_states)

levels(df_years$quartile_years)[levels(df_years$quartile_years)=="10"] <- group1

df10 <- df_years[df_years$quartile_years %in% c("9","8"), ]
View(df10)

df_state_year <- df %>% 
  group_by(GeoName, year) %>%  
  mutate(quintiles = ntile(gdp, 4))
df_state_year$quintiles <- as.factor(df_state_year$quintiles)
qplot(log(gdp), data = df_state_year, binwidth = .1, fill = quintiles)

by_state <- df %>%
  group_by(GeoName) %>%
  mutate(mean_gdp = mean(gdp), median_gdp = median(gdp)) %>%
  arrange(desc(median_gdp), desc(gdp)) %>%
  select("GeoName", "year", "gdp", "mean_gdp", "median_gdp")







