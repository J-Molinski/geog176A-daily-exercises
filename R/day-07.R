# Jordan Molinski
# 08-12-2020
# Faceted Plots

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)
head(covid)

region = data.frame(state = state.name, region = state.region)
head(region)

left_join(covid, region, by = "state") %>%
  count(region) %>%
  mutate(tot = sum(n))

right_join(covid, region, by = "state") %>%
  count(region) %>%
  mutate(tot = sum(n))

covid %>%
  filter(!state %in% state.name) %>%
  filter(date == max(date)) %>%
  count(state)

covid %>%
  right_join(region, by = "state") %>%
  group_by(region, date) %>%
  summarize(cases  = sum(cases),
            deaths = sum(deaths)) %>%
  pivot_longer(cols = c('cases', 'deaths')) ->
  covid_region

ggplot(covid_region, aes(x = date, y = value)) +
  geom_line(aes(col = region)) +
  facet_grid(name~region, scale = "free_y") +
  theme_linedraw() +
  theme(legend.position = "bottom") +
  theme(legend.position = "NA") +
  labs(title = "Total Cases and Deaths by Region",
       y = "Daily Cumulative Count",
       x = "Date",
       caption = "Exercise 07",
       subtitle = "NY-Times COVID-19 Data" )
