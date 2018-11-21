## Scratch
library(tidyverse)
library(maps)
library(ggplot2)
library(scales)


## Read the file
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
                   description = col_factor(levels = NULL),
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
 
## Clean the file
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
  mutate(gdp = (gdp * 10^6),
         log_gdp = log(gdp))

## Test some new shit out
# group by state and sum GDP
byState <- temp %>%
  na.omit() %>%
  group_by(state) 
byState_sum <- byState %>%
  filter(state != "District of Columbia") %>%
  summarize(total_gdp = sum(gdp))
byState_sum$state <- factor(byState_sum$state, unique(byState_sum$state))
data <- data.frame(total_gdp = byState_sum$total_gdp, 
                 state = tolower(byState_sum$state))
# graph a map!
map <- map_data("state")
k <- ggplot(data, aes(fill = total_gdp)) 
p <- k + geom_map(aes(map_id = state), map = map) +
  expand_limits(x = map$long, y = map$lat) + 
  scale_fill_gradient()
print(p)
# group by industry
i <- temp %>%
  na.omit() %>%
  group_by(description) %>%
  mutate(total_gdp_by_industry_1997_to_2017 = sum(gdp)) %>%
  arrange(desc(total_gdp_by_industry_1997_to_2017))
i$description <- factor(i$description, unique(i$description))
i_sum <- i %>%
  summarize(total_gdp_by_industry_1997_to_2017 = sum(gdp)) %>%
  arrange(desc(total_gdp_by_industry_1997_to_2017))
i_sum$description <- factor(i_sum$description, unique(i_sum$description))
g <- ggplot(i, aes(description, gdp))
p <- g + geom_boxplot(aes(fill = year)) + theme(axis.text.x = element_text(angle = 90,
                                   vjust = .25,
                                   hjust = 1))
print(p)
# by industry using 2015, 2016, 2017
i <- temp %>%
  na.omit() %>%
  mutate(log_gdp = log(gdp)) %>%
  filter(year == c("2014", "2015", "2016")) %>%
  group_by(description) %>%
  mutate(total_gdp_by_industry_1997_to_2017 = sum(gdp)) %>%
  arrange(desc(total_gdp_by_industry_1997_to_2017))
i$description <- factor(i$description, unique(i$description))
i_sum <- i %>%
  summarize(total_gdp_by_industry_1997_to_2017 = sum(gdp)) %>%
  arrange(desc(total_gdp_by_industry_1997_to_2017))
i_sum$description <- factor(i_sum$description, unique(i_sum$description))
g <- ggplot(i, aes(description, gdp))
p <- g + geom_boxplot(aes(fill = fct_rev(year))) + 
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1,
                                   hjust = 1)) +
  coord_cartesian(ylim = c(0, (1*10^11))) +
  scale_fill_brewer(palette = "Dark2")
                                                                           
print(p)
# industry == real estate; 1997-2017
re <- temp %>%
  na.omit() %>%
  mutate(log_gdp = log(gdp)) %>%
  # filter(description == "Real estate and rental and leasing") %>%
  group_by(description) %>%
  mutate(total_gdp_by_industry_1997_to_2017 = sum(gdp)) %>%
  arrange(desc(total_gdp_by_industry_1997_to_2017))
re$description <- factor(re$description, unique(re$description))

g <- ggplot(re, aes(year, gdp, fill = year))
p <- g + geom_boxplot() + facet_wrap(description ~ .) +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1,
                                   hjust = 1)) +
  coord_cartesian(ylim = c(0, (1*10^11))) +
  scale_y_continuous(labels = comma) +
  geom_smooth()
  # scale_fill_brewer(palette = "Blues")

print(p)

# adjust industryId to see densities of industry by state
ggplot(temp[temp$industryId == "12",], aes(log(gdp), 
                                           group = industryId, 
                                           fill = description)) + 
  geom_density(adjust = 2, alpha = .2) + facet_wrap(state ~ .)

























  byIndustry <- temp %>%
  na.omit() %>%
  group_by(description, state) %>%
  mutate(logTotal = log(sum(gdp)),
         total = sum(gdp),
         percent_of_industry_by_state = (gdp / total) * 100,
         p = percent_of_industry_by_state,
         mean = mean(p),
         median = median(percent_of_industry_by_state),
         min = min(percent_of_industry_by_state),
         max = max(percent_of_industry_by_state),
         gdpe6 = gdp / 10^6,
         totale6 = total / 10^6)
  
  
byIndustry[byIndustry$state == "California" &
             # byIndustry$year == "2015" &
             byIndustry$description == "Agriculture, forestry, fishing, and hunting",
           c(9,13,2,11,14,17)]  

g <- ggplot(df, aes(industryClassification, percent_of_industry_by_year))
p <- ggplot + geom_point()
print(p)

byIndustry_byState <- temp %>%
  group_by(description, state) %>%
  mutate(total)











  
# graph it
g <- ggplot(byIndustry, aes(state, gdp))
p <- g + geom_col() + facet_grid(description ~ .) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = .25,
                                   hjust = 1))
print(p)



data <- data.frame(murder = USArrests$Murder,
                   state = tolower(rownames(USArrests)))
map <- map_data("state")
k <- ggplot(data, aes(fill = murder)) 
p <- k + geom_map(aes(map_id = state), map = map) +
  expand_limits(x = map$long, y = map$lat)
print(p)



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


p <- ggplot(mpg, aes(class, hwy))
p + geom_boxplot()
p + geom_boxplot() + coord_flip()

p + geom_boxplot(notch = TRUE)
p + geom_boxplot(varwidth = TRUE)
p + geom_boxplot(fill = "white", colour = "#3366FF")
# By default, outlier points match the colour of the box. Use
# outlier.colour to override
p + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
# Remove outliers when overlaying boxplot with original data points
p + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2)

# Boxplots are automatically dodged when any aesthetic is a factor
p + geom_boxplot(aes(colour = drv))

# You can also use boxplots with continuous x, as long as you supply
# a grouping variable. cut_width is particularly useful
ggplot(diamonds, aes(carat, price)) +
  geom_boxplot()
ggplot(diamonds, aes(carat, price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.25)))
# Adjust the transparency of outliers using outlier.alpha
ggplot(diamonds, aes(carat, price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.25)), outlier.alpha = 0.1)



