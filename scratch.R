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


# Gather the year
temp <- temp %>% 
  gather(`1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`,
         `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`,
         `2015`, `2016`, `2017`, key = "year", value = "gdp")
temp$year <- parse_factor(temp$year, levels = NULL)


# First cut
by_industry <- temp %>%
  group_by(industryId, year) %>%
  mutate(median_ind_gpd = mean(gdp))
industry_sum <- by_industry %>%
  summarise(median = median(gdp))
# graph first cut
g <- ggplot(industry_sum[industry_sum$industryId != "1" &
                           industry_sum$industryId != "2" &
                           industry_sum$industryId != "91" &
                           industry_sum$industryId != "100" &
                           industry_sum$industryId != "101" &
                           industry_sum$industryId != "102" &
                           industry_sum$industryId != "103" &
                           industry_sum$industryId != "NA",], 
            aes(industryId, median))
p <- g + geom_point() #+ facet_grid(hilow ~ .)
print(p)

p2.1.2 <- g2.1 + facet_grid(hilow ~ .) +
  geom_point(aes(color = year, size = gdp), alpha = 1/2) + 
  labs(title = "Annual GDP by State 1997-2017") +
  labs(x = "log GDP", y = "State Name") + 
  theme_bw() + scale_y_discrete(breaks = c("California",
                                           "South Carolina",
                                           "Vermont"))

print(p2.1.2)









