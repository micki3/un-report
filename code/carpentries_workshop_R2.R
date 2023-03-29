library(tidyverse)
read.csv("data/gapminder_data.csv") #don't forget quotations!

gapminder_data <- read.csv("data/gapminder_data.csv") #naming this to make it easier 



summarise(gapminder_data, averageLifeExp=mean(lifeExp)) #must name data before summarizing


# (%>% ) is the pipe function and can link things like the (+)

gapminder_data %>% summarise(averageLifeExp=mean(lifeExp)) 

gapminder_data_summarized <- gapminder_data %>% summarise(averageLifeExp=mean(lifeExp))

gapminder_data_summarized

gapminder_data %>% summarise(recent_year=max(year))


#filter()- subsets the rows in a dataframe - meaning filters out data that is not what you assigned

gapminder_data %>% filter(year == 2007) %>%  #filters out the data for that's not year 2007
  summarise(averageLifeExp=mean(lifeExp)) 

gapminder_data %>% summarise(first_year = min(year)) #get the lowest year

gapminder_data %>% filter(year == 1952) %>% 
summarise(average_GDP = mean(gdpPercap))


#group_by() -group values from a column

gapminder_data %>% 
  group_by(year) %>% 
  summarise(average_lifeexp = mean(lifeExp))


#calculating the average life expectancy by continent

gapminder_data %>% 
  group_by(continent) %>%
  summarise(average_lifeexp = mean(lifeExp))


#calculating the average life expectancy and minimum life expectancy by continent

gapminder_data %>% group_by(continent) %>%
  summarise(average_lifeexp = mean(lifeExp), min_lifeexp = min(lifeExp))


#mutate() - add or change a variable/column in a dataframe

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap) #added a new column called gdp that was a multiplication of 2 columns


#making a new column called pop in millions

gapminder_data %>% 
  mutate(pop_in_millions = pop / 1000000)


#saving changes to the data in the environment by assigning it to a variable called gapminder_data_mutated

gapminder_data_mutated <- gapminder_data %>% 
  mutate(pop_in_millions = pop / 1000000) 

view(gapminder_data)  #how to visualize small data frames


#select() - subsets columns in a dataframe

gapminder_data %>% select(pop, year)  #now it only shows the pop and year columns

gapminder_data %>% select(-continent) #now it shows all columns minus the continent column


#create a dataframe with only country, continent, year and lifeExp

gapminder_data %>% select(country, continent, year, lifeExp)

gapminder_data %>% select(-gdpPercap, -pop)


# pivot_wider() and pivot_longer() can widen or lengthen the dataframe

#for pivot_wider() you will need "names_from()" which will be what are the new columns
# and you will also need "values_from" which will be the what goes under the columns as the values in the boxes

gapminder_data %>%
  select(continent, year, lifeExp, country) %>%
  pivot_wider(names_from = year, values_from = lifeExp)


#subsetting the gapminder_data to only 2007 in Americas showing only country, lifeExp, and GDP

gapminder_data %>% filter(year == 2007, continent == "Americas") %>% 
  select(-year, -continent) %>% 
  mutate(GDP = pop * gdpPercap) %>%
  select(-pop, -gdpPercap)

#storing this

gapminder_data_mine <- gapminder_data %>% filter(year == 2007 & continent == "Americas") %>% #should use & in place of , in filters
  select(-year, -continent) %>% 
  mutate(GDP = pop * gdpPercap) %>%
  select(-pop, -gdpPercap)

gapminder_data_2007 <- gapminder_data %>% filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent) 

#is C02 emissions related to GDP?

read.csv("data/co2-un-data.csv", skip=1) #skip=1 means skip the first row

?read.csv #showing that read.csv is base R and read_csv is tidyverse so col.names and col_names is diff

read_csv("data/co2-un-data.csv", skip=2, col_names = c("region", "country","year", "series","value", "footnotes", "source"))

co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip=2, 
      col_names = c("region", "country","year", "series",
                    "value", "footnotes", "source"))

#recode is a way to rename things

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>%
  mutate(series= recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                        "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions" )) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year)

view(co2_emissions)
view (gapminder_data_2007)

# inner_join will join the data sets and drop things that are not in both of them (everything has to have exact same spelling of names)

#right join will keep all of the right data set and drop whats in the other set that isnt in the right

#left join keeps the left data set and drops excess of right

#another join will keep all of the data from both sets when joined

inner_join(gapminder_data_2007, co2_emissions)

anti_join(gapminder_data_2007, co2_emissions) #shows what was dropped when joined

#Some of the same countries were spelled differently in each dataset so they were dropped

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>%
  mutate(series= recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                        "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions" )) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country, "Bolivia (Plurin. State of)" = "Bolivia",  
                           "United States of America" = "United States", 
                           "Venezuela (Boliv. Rep. of)" = "Venezuela"))
view(co2_emissions)

inner_join(gapminder_data_2007, co2_emissions)

anti_join(gapminder_data_2007, co2_emissions)

#still dropping Puerto Rico because its separated in gapminder data but its part of US in co2 data

gapminder_data_2007 <- gapminder_data %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent) %>%
  mutate(country= recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%  #gets rid of duplicate countries by combining the data
  summarise(lifeExp = sum(lifeExp * pop) / sum(pop), 
            gdpPercap = sum(gdpPercap * pop)/ sum(pop), 
            pop = sum(pop))     #recalculating statistics

view(gapminder_data_2007)

inner_join(gapminder_data_2007, co2_emissions)
anti_join(gapminder_data_2007, co2_emissions) 

gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions)
view(gapminder_co2) 

# so our original question: is Co2 emissions related to GDP?

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) +
  geom_point() + #scatterplot
  geom_smooth() #adds a trendline but a curved one

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) +
  geom_point() + #scatterplot
  geom_smooth(method = "lm") + #makes it a straight trendline
labs(x = "GDP (per Capita)", 
     y = "C02 emitted (per Capita)")

write_csv(gapminder_co2, "data/gapminder_co2.csv") #how to save the dataframes you make as csv files! 
#the first is the data frame name, and then the path and filename
