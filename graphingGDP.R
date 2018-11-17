library(ggplot2)
library(dplyr)

df1 <- df %>%
  mutate(gdp = (gdp *10^6), 
         log_gdp = log(gdp),
         log2_gdp = log2(gdp)) %>%
  select(state = "GeoName", "year", "gdp", "log_gdp",
         "log2_gdp") %>%
  group_by(state) %>%
  mutate(mean_gdp = mean(gdp)) %>%
  arrange(desc(mean_gdp))
df1$state <- factor(df1$state, unique(df1$state))

g1 <- ggplot(df1, aes(state, log_gdp))
p1 <- g1 + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

p1.1 <- g1 + geom_point() + facet_grid(. ~ year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p1.1)

df2 <- df %>%
  mutate(gdp = (gdp *10^6), 
         log_gdp = log(gdp),
         log2_gdp = log2(gdp)) %>%
  select(state = "GeoName", "year", "gdp", "log_gdp",
         "log2_gdp") %>%
  group_by(state) %>%
  mutate(mean_gdp = mean(gdp)) %>%
  arrange((mean_gdp))
df2$state <- factor(df2$state, unique(df2$state))

g2 <- ggplot(df2, aes(log_gdp, state))

p2 <- g2 + geom_point(color = "steelblue", size = 4, alpha = 1/2)
p2.1 <- g2 + geom_point(aes(color = year), size = 4, alpha = 1/2)
p2.2 <- g2 + geom_point(aes(color = year, size = gdp), alpha = 1/2)
p3 <- g2 + geom_point() + labs(title = "Annual GDP by State 1997-2017") +
  labs(x = "log GDP", y = "State Name")
p3.2.1 <- g2 + geom_point(aes(color = year, size = gdp), alpha = 1/2) + 
  labs(title = "Annual GDP by State 1997-2017") +
  labs(x = "log GDP", y = "State Name") + theme_bw(base_family = "Times")


print(p3.2.1)


## using df3
g2.1 <- ggplot(df3, aes(log_gdp, state))

p3.2.1 <- g2.1 + geom_point(aes(color = year, size = gdpdec), alpha = 1/2) + 
  labs(title = "Annual GDP by State 1997-2017") +
  labs(x = "log GDP", y = "State Name") + theme_bw(base_family = "Times")



# p2 <- g2 + geom_point() + geom_smooth()
# print(p2)

g2.1 <- ggplot(df3, aes(log_gdp, state))
p2.1 <- g2.1 + geom_point() + facet_grid(hilow ~ .)
print(p2.1)

p2.1.2 <- g2.1 + facet_grid(hilow ~ .) +
  geom_point(aes(color = year, size = gdp), alpha = 1/2) + 
  labs(title = "Annual GDP by State 1997-2017") +
  labs(x = "log GDP", y = "State Name") + 
  theme_bw() + scale_y_discrete(breaks = c("California",
                                           "South Carolina",
                                           "Vermont"))
  
print(p2.1.2)






