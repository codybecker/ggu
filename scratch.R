## Scratch
library(tidyverse)


# read the file
temp <- read_csv("data/GDPdata/SAGDP2N__ALL_AREAS_1997_2017.csv",
                 col_names = c("fips",
                               "state",
                               "region",
                               "tableName",
                               "componentName",
                               "unit",
                               "industryId",
                               "industryClassification",
                               "description",
                               "1997", "1998", "1999", "2000", "2001", "2002",
                               "2003", "2004", "2005", "2006", "2007", "2008",
                               "2009", "2010", "2011", "2012", "2013", "2014",
                               "2015", "2016", "2017"),
                 col_types = cols(
                   fips = col_factor(levels = NULL),
                   state = col_factor(levels = NULL),
                   region = col_factor(levels = NULL),
                   tableName = col_character(),
                   componentName = col_character(),
                   unit = col_character(),
                   industryId = col_factor(levels = NULL),
                   industryClassification = col_factor(levels = NULL),
                   description = col_character(),
                   `1997` = col_double(),
                   `1998` = col_double(),
                   `1999` = col_double(),
                   `2000` = col_double(),
                   `2001` = col_double(),
                   `2002` = col_double(),
                   `2003` = col_double(),
                   `2004` = col_double(),
                   `2005` = col_double(),
                   `2006` = col_double(),
                   `2007` = col_double(),
                   `2008` = col_double(),
                   `2009` = col_double(),
                   `2010` = col_double(),
                   `2011` = col_double(),
                   `2012` = col_double(),
                   `2013` = col_double(),
                   `2014` = col_double(),
                   `2015` = col_double(),
                   `2016` = col_double(),
                   `2017` = col_double()), 
                 skip = 1)
glimpse(temp)
levels(df$industryClassification)[x] <- "92"  

# Gather the year
temp <- temp %>% 
  gather(`1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`,
         `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`,
         `2015`, `2016`, `2017`, key = "year", value = "gdp")
temp$year <- parse_factor(temp$year, levels = NULL)
# Select only those industries that are not sub-industries
unique(temp$industryClassification)
wantedLevels <- unique(temp$industryClassification)[c(2,5,9,10,11,33,34,35,44,50, 
                                                    55,59,63,64,68,69,75,78,81)]
temp <- temp %>% 
  filter(industryClassification %in% wantedLevels)
temp$industryClassification <- droplevels(temp$industryClassification)
# Get rid of non-states (eg "United States*", "Midwest" etc)
wantedLevels <- unique(temp$state)[2:52]
wantedLevels <- droplevels(wantedLevels)
temp <- temp[temp$state %in% wantedLevels,]
# log transform gdp
temp <- temp %>%
  mutate(gpd = (gdp * 10^6),
         log_gdp = log(gdp))

## First cut (by industry, year)
by_industry <- temp %>%
  group_by(industryId, year)
# graph first cut
# g1 <- ggplot(by_industry, aes(gdp, industryId))
# p1 <- g1 + geom_point(aes(color = region))  
# print(p1)
# x <- which(by_industry$log_gdp < 0)
# by_industry <- by_industry[-x,]
# g1 <- ggplot(by_industry, aes(log_gdp, industryId))
# p1 <- g1 + geom_point(aes(color = region))  
# print(p1)
# g1 <- ggplot(by_industry, aes(log_gdp, state))
# p1 <- g1 + geom_point(aes(color = region))  
# print(p1)
# industry_summary <- by_industry %>%
#   summarize(total_gdp = sum(gdp))
# g1 <- ggplot(industry_summary, aes(total_gdp, industryId))
# p1 <- g1 + geom_point(aes(color = year))  
# print(p1)
industry_summary <- by_industry %>%
  summarize(total_gdp = sum(log_gdp))
g1 <- ggplot(industry_summary, aes(total_gdp, industryId))
p1 <- g1 + geom_point(aes(color = year))  
print(p1)

## Second cut; by industry, state
byIndustry_byState <- temp %>%
  na.omit() %>%
  group_by(industryId, state) 
indState_sum <- byIndustry_byState %>%
  summarize(total_gdp = sum(log_gdp),
            min = min(log_gdp),
            max = max(log_gdp),
            mean = mean(log_gdp),
            median = median(log_gdp),
            count = n(),
            blank = sum(is.na(log_gdp))) 
# Graph second cut
g2 <- ggplot(indState_sum, aes(total_gdp, industryId))
p2 <- g2 + geom_point()  
print(p2)

## Second cut; by state, industry
byIndustry_byState1 <- temp %>%
  na.omit() %>%
  group_by(state, industryId) 
indState_sum1 <- byIndustry_byState1 %>%
  summarize(total_gdp = sum(log_gdp),
            min = min(log_gdp),
            max = max(log_gdp),
            mean = mean(log_gdp),
            median = median(log_gdp),
            count = n(),
            blank = sum(is.na(log_gdp))) 

## Third cut: top industries
topIndustries_byState <- temp %>%
  na.omit() %>%
  group_by(state) 
indByState_sum <- topIndustries_byState %>%
  summarize(description = description[which.max(log_gdp == max(log_gdp))],
            industryNAICS = industryClassification[which.max(log_gdp == max(log_gdp))],
            total_gdp = sum(log_gdp),
            min = min(log_gdp),
            max = max(log_gdp),
            mean = mean(log_gdp),
            median = median(log_gdp),
            count = n(),
            blank = sum(is.na(log_gdp))) %>%
  arrange(median)
indByState_sum$state <- factor(indByState_sum$state, unique(indByState_sum$state))
# Graph third cut (top growssing industry by state)
g3 <- ggplot(indByState_sum, aes(median, state))
p3 <- g3 + geom_point(aes(color = description), size = 4)
print(p3)

g3 <- ggplot(topIndustries_byState, aes(state, gdp))
p3.1 <- g3 + geom_dotplot()
print(p3.1)


f <- ggplot(mpg, aes(class, hwy))
ptest <- f + geom_boxplot()
print(ptest)

# p2.1.2 <- g2.1 + facet_grid(hilow ~ .) +
#   geom_point(aes(color = year, size = gdp), alpha = 1/2) + 
#   labs(title = "Annual GDP by State 1997-2017") +
#   labs(x = "log GDP", y = "State Name") + 
#   theme_bw() + scale_y_discrete(breaks = c("California",
#                                            "South Carolina",
#                                            "Vermont"))
# 
# print(p2.1.2)
# 
# 
# 






