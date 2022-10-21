library(tidyverse) #load library

#Load data
gapminder_data <- read_csv("data/gapminder_data.csv")

# Summarize our data

# get avg life exp across whole data set
summarise(gapminder_data, averageLifeExp=mean(lifeExp))

# get avg life exp across whole data set using the pipe (%>%), layering the analysis
gapminder_data %>% summarise(averageLifeExp=mean(lifeExp))

# store the summary in a variable
gapminder_data_summarized <- gapminder_data %>%
  summarise(averageLifeExp=mean(lifeExp))

### Filtering our data using filter()

# Filtere data to only contain data from year 2007
gapminder_data %>%
  filter(year == 2007)

# Get average life expectancy for the year 2007
gapminder_data %>%
  filter(year == 2007) %>%
  summarize(average=mean(lifeExp))

#Identify the earliest year in the data set
gapminder_data %>%
  summarise(first_year = min(year))

# Get average GDP per capita for the earliest year in the dataset
gapminder_data %>%
  filter(year == 1952) %>%
  summarise(average_gdp = mean(gdpPercap))

### Grouping Rows using group_by()

gapminder_data %>% #using this data
  group_by(year) %>% #group by year
  summarise(average = mean(lifeExp)) #give a summary oof the avg Life Exp/year

#Find the minimum
gapminder_data %>% #use this data
  group_by(continent) %>% #gruop by continent
  summarise(average = mean(lifeExp), min = min(lifeExp)) #give a summary of the avg Life Exp/year and min lifeExp/year


###Adding columns with mutate()

gapminder_data %>%
  mutate(gdp = pop * gdpPercap)  #create a new column called gdp

gapminder_data %>%
  mutate(gdp = pop * gdpPercap,
         popInMillions = pop / 1000000)  #create a new column and manipulated it

#Select columns of interest or change their order with select()

gapminder_data %>%
  select(pop, year)

colnames(gapminder_data) #view headings in our data

gapminder_data %>%
  select(continent, country) #reorder and select columns


###Changing the Shape of the data
#Long format data = every single row is a single observation
#Wide format data = observation is in columns rather than rows

# convert from long to wide format data with pivot_wider() and pivot_longer()
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

#Dataset for analysis
gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)

### Data cleaning
library(readr)
read_csv("data/co2-un-data.csv") #read in data, don't store in object yet

read_csv("data/co2-un-data.csv", skip = 1) #skip the first line


read_csv("data/co2-un-data.csv", skip = 2, #skip the first 2 lines
         col_names = c("region", "country", "year", "series", "value",
                       "footnotes", "source")) #create new column names

##always want to read in the data to see if it fits your expectations; take nothing for granted!!!

#also could have used rename() to rename column
read_csv("data/co2-un-data.csv", skip = 1) %>% #read data, skip line1
  rename(country = ...2)

read_csv("data/co2-un-data.csv", skip = 1) %>%
  rename_all(tolower)

co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2, 
                                col_names = c("region", "country", "year",
                                              "series", "value","footnotes",
                                              "source")) 

#We only want country, year, series, value
co2_emissions_dirty %>%
  select(country, year, series, value)

co2_emissions <- co2_emissions_dirty %>% #reassiign cleaned up data
  select(country, year, series, value) %>% #choose these headers
  mutate(series = recode(series, 
      "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
      "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"
      ) #rename "dirty" columns
    ) %>% 
  pivot_wider(names_from=series, values_from=value) %>% #restructure dataset
  filter(year == 2005) %>% #select only year 2005
  select(-year) #drop year since it was filtered in previous step
  

###Look at structure of data
  str(gapminder_data)
  
### Joining datasets
df <- inner_join(gapminder_data_2007, co2_emissions) #can select inidex column and only pull in data that exists in both datasets
view(df) #look at the data

anti_join(gapminder_data_2007, co2_emissions,
          by = "country") #what are the observations that are in one file but not another?
#we see that the naming of Bolivia and PR have the country names coded differently

co2_emissions <- read_csv("data/co2-un-data.csv",
                          skip = 2,
                          col_names = c("region", "country", "year",
                          "series", "value", "footnotes", "source")) %>% #change col names
  select(country, year, series, value) %>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"
  ) #rename "dirty" columns
  ) %>% 
  pivot_wider(names_from=series, values_from=value) %>% #restructure dataset
  filter(year == 2005) %>% #select only year 2005
  select(-year) %>% #drop year since it was filtered in previous step
  mutate(country = recode(country,
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))
  
#a second antijoin
anti_join(gapminder_data_2007, co2_emissions, by = "country")

#Recode PR to be part of the US because it only occurs in one dataset
gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States"))

#a second antijoin
anti_join(gapminder_data_2007, co2_emissions, by = "country")

#get weighted averages of categories
gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarise(lifeExp = sum(lifeExp * pop) / sum(pop),
            gdpPercap = sum(gdpPercap * pop) / sum(pop),
            pop = sum(pop))

#combine datasets again after the cleanup and tidying
inner_join(gapminder_data_2007, co2_emissions, by = "country")

#make a new dataset
gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by = "country")

#what if we want to compare north v south countries? Create new column called "region"
gapminder_co2 %>%
  mutate(region = if_else(country == "Canada" |
                          country == "United States" |
                          country == "Mexico", "north", "south")) 
#if country is listed, coded as north; if not, coded as "south"

#create a new .csv using write_csv()
write_csv(gapminder_co2, "data/gapminder_co2.csv")


#### Analyzing/plotting exercises
ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) +
  geom_point() +
  labs(x = "GDP (per capita)",
        y = "CO2 emitted (per capita)",
        title = "There is a strong association between a nation's GDP \nand the amount of CO2 it produces")

#Fit a line to our data
ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) +
  geom_point() +
  labs(x = "GDP (per capita)",
       y = "CO2 emitted (per capita)",
       title = "There is a strong association between a nation's GDP \nand the amount of CO2 it produces") +
  geom_smooth(method = "lm")
