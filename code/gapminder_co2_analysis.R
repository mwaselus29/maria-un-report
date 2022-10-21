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

