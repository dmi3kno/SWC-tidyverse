
library(tidyverse)

gapminder <- read_csv("data/gapminder-FiveYearData.csv")
gapminder


library("readxl")
raw_fert <- read_excel(path = "data/indicator undata total_fertility.xlsx", sheet = "Data")
raw_fert

raw_fert %>% 
  rename(country = `Total fertility rate`) %>% 
  gather(key = year, value = fert)


fert <- raw_fert %>% 
  rename(country = `Total fertility rate`) %>% 
  gather(key = year, value = fert, -country) %>% 
  mutate(year = as.integer(year))
fert

fert %>%
  filter(country == "Norway") %>% 
  ggplot() +
  geom_line(mapping = aes(x = year, y = fert))

#challenge 2
raw_infantMort <- read_excel(path = "data/indicator gapminder infant_mortality.xlsx", sheet = "Data")

infantMort <- raw_infantMort %>% 
  rename(country = `Infant mortality rate`) %>% 
  gather(key = year, value = infantMort, -country) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(infantMort = as.numeric(infantMort))

infantMort %>% 
  filter(country == "Norway") %>% 
  filter(year >= 2000) %>% 
  summarize(mean_infantMort = mean(infantMort))


gapminder %>% 
  inner_join(fert, by = c("year", "country"))


gapminder %>% 
  left_join(fert, by = c("year", "country"))

gapminder %>% 
  right_join(fert, by = c("year", "country"))

gapminder %>% 
  full_join(fert, by = c("year", "country"))

gapminder_plus <- gapminder %>% 
  left_join(fert, by = c("year", "country"))

gapminder_plus <- gapminder %>% 
  left_join(fert, by = c("year", "country"))
gapminder_plus

gapminder_plus %>% 
  is.na() %>% 
  colSums()


# Challenge 3

gapminder_plus <- gapminder %>% 
  left_join(fert, by = c("year", "country")) %>% 
  left_join(infantMort, by = c("year", "country"))
colSums(is.na(gapminder_plus))


gapminder_plus_summary <- gapminder_plus %>%
  group_by(continent) %>% 
  summarize(mean_fert = mean(fert),
            mean_infantMort = mean(infantMort)) 
gapminder_plus_summary


gapminder_plus_summary <- gapminder_plus %>%
  group_by(continent) %>% 
  summarize(mean_fert = mean(fert, na.rm = TRUE),
            mean_infantMort = mean(infantMort, na.rm = TRUE)) 
gapminder_plus_summary


gapminder_plus_summary <- gapminder_plus %>%
  group_by(country, continent) %>% 
  summarize(mean_fert = mean(fert, na.rm = TRUE),
            mean_infantMort = mean(infantMort, na.rm = TRUE)) 

map_data("world") %>% 
  rename(country = region) %>% 
  left_join(gapminder_plus_summary, by = "country") %>% 
  ggplot() +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = mean_fert)) +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_equal()

# FINAL PROJECT

gapminder_plus %>% filter(continent=="Africa", year==2007, infantMort/1000*pop/10^6>2) %>% select(country) %>% 
  left_join(gapminder_plus) %>% mutate(pop_mln=pop/10^6, gdp_mln=gdpPercap*pop/10^6) %>% select(-c(pop,continent)) %>% 
  gather(key=att, value=val, -c(year,country)) %>% 
  ggplot()+
  geom_line(mapping = aes(x=year, y=val, group=country, color=country))+
  geom_text(data=. %>% filter(year==2007) %>% group_by(att) %>% mutate(max_val=max(val)) %>% filter(val==max_val),
            mapping = aes(x=year, y=max_val, label=country, color=country), hjust="right")+
  facet_wrap(~att, scales = "free_y")+
  theme_bw()+
  labs(title="Key parameters for selected African countries",
       subtitle="with over 2 mln baby deaths in 2007",
       caption="Fertility and Infant mortality in # babies per 1000 population, GDP in USD mln, GDP per capita in USD, 
       Life Expectancy in years, Population in million people",
       x="Year") + theme(legend.position = "none") + ylab(NULL)
 