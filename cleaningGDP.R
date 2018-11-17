library(ggplot2)
library(dplyr)
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


df4 <- df3 %>% 
  group_by(state) %>%
  