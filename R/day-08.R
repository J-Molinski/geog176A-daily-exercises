#  Jordan Molinski
# 08-17-2020
# Daily Cases and 7-day Rolling mean for Texas

install.packages("zoo")
library(zoo)
?rollmean

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)
head(covid)

state.of.interest = "Texas"
covid %>%
  filter(state == state.of.interest) %>%
  group_by(date) %>%
  summarize(cases = sum(cases)) %>%
  mutate(newCases = cases - lag(cases),
         roll7 = rollmean(newCases, 7, fill = NA, align="right")) %>%
  ggplot(aes(x=date)) +
  geom_col(aes(y = newCases), col = NA, fill = "#F5B8B5") +
  geom_line(aes(y = roll7), col = "darkred", size = 1) +
  ggthemes::theme_wsj()+
  labs(title = paste("New Cases by Day in", state.of.interest)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 16, face = 'bold')) +
  theme(aspect.ratio = .5)

