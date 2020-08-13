#Jordan Molinski
#08-11-2020
#R script and plots

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)
head(covid)

covid %>%
  filter(date == max(date)) %>%
  group_by(state) %>%
  summarize(cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  slice_max(cases, n = 6) %>%
  pull(state) ->
  top_states

covid %>%
  filter(state %in% top_states) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_line(size = 2) +
  facet_wrap(~state) +
  labs(title = "Cummulative Cases",
       subtitle = "Source: NY-Times",
       x = "Date",
       y = "Cases",
       caption = "Exercise 06")

covid %>%
  group_by(date) %>%
  summarize(cases = sum(cases)) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_col(fill = "steelblue", color = "steelblue", alpha = .25) +
  geom_line(color = "steelblue", size = 3) +
  labs(title = "Cumulative Cases Nationally",
       x = "Date",
       y = "Cases",
       caption = "Daily Exercise 06")
