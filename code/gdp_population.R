# Load the tidyverse library
library(tidyverse)

#Read in the data file
read_csv("gapminder_1997.csv")

#Name the data file
gapminder_1997 <- read_csv("gapminder_1997.csv")

#Look at the data file
view(gapminder_1997)

#How to name objects
name <- "Ben"
name

age <- 26
age

#you can rewrite objects
name <- "Harry Potter"

bens_name <- "Ben"

#Which of these is the best name for an object?
1number <- 3 #Name starts with a number
Flower <- "marigold" #Name starts with a capital
flower <-  "rose" #Best choice!
favorite number <- 12 #Name contains a space

#How do we learn more about read_csv
?read_csv

#Look up the function round
?round

#Round pi to two digits
round(3.1415, digits=2)

gapminder_1997 <- read_csv("gapminder_1997.csv")

Sys.Date() #Outputs the current date
getwd() #outputs the current working directory
sum(5,6) #sum of these numbers

#Column names
colnames(gapminder_1997)

# Start plotting!
ggplot(data = gapminder_1997) #only gives you a gray box...need more info

ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap) # used aes() to add x-axis

#Both variables have continuous data

ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap) + # used aes() to add x-axis
  labs(x= "GDP Per Capita") + #change the x-axis name
  aes(y = lifeExp) + #Map lifeExp to the y-axis
  labs(y= "Life Expectancy") + #change the y-axis name
  geom_point() + # add data
  labs(title = "Do people in wealthy countrieis live longer?") + # Add title
  aes(color = continent) + #Add color to continents
  scale_color_brewer(palette = "Set1") + #Change color
  aes(size = pop) +
  aes(size = pop/1000000) +
  labs(size = "Population (in millions)")

ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000) + 
  geom_point() + # add data
  scale_color_brewer(palette = "Set1") + #Change color
  labs(x = "GDP Per Capita", y= "Life Expectancy", title = "Do people in wealthy countries live longer?", size = "Population (in millions)")

#read in GapMinder Data
gapminder_data <-read_csv("gapminder_data.csv")

#plot the gapminder_data
ggplot(data = gapminder_data) + 
  aes(x = year, y = lifeExp, color = continent) +
  geom_point()
# This plot geom_point colored by continent might not be the best way to represent the data!

#explore thet gapminder_data
str(gapminder_data)

#Explore geom_ that might better represent the data
ggplot(data = gapminder_data) + 
  aes(x = year, y = lifeExp, color = continent) +
  geom_line()
# also NOT GREAT!

# add grouping by country
ggplot(data = gapminder_data) + 
  aes(x = year, y = lifeExp, color = continent, group = country) +
  geom_line()
#Useful plots for numerical data

#What if we want to explore plotting a categorical variable?
ggplot(data = gapminder_data) + 
  aes(x = continent, y = lifeExp) +
  geom_boxplot()

#What if we want to explore plotting a categorical variable?
ggplot(data = gapminder_1997) + 
  aes(x = continent, y = lifeExp) +
  geom_violin() +
  geom_point() #Not ideal, point data stacked on top of each other

#What if we want to explore plotting a categorical variable?
ggplot(data = gapminder_1997) + 
  aes(x = continent, y = lifeExp) +
  geom_violin() +
  geom_jitter() #better for lots of point data

#What if we want to explore plotting a categorical variable?
ggplot(data = gapminder_1997) + 
  aes(x = continent, y = lifeExp) +
  geom_jitter() +
  geom_violin() #violin is laying over the jitter points

#We are building the plot in layers using geom_

#Can add aes to the ggplot function and pass aes to specific geom
ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin() +
  geom_jitter(aes(size = pop)) #passed size to population

#add color and fill to geom
ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin(color = "pink") + #controls outline of plot
  geom_jitter()

#add color and fill to geom
ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin(fill = "green") + #controls fill of plot
  geom_jitter()

#add color and fill to geom
ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  geom_violin(aes(fill = continent)) + 
  geom_jitter(alpha = 0.7) #points have a little transparency to them

sample(colors())

#geom_histogram
ggplot(gapminder_1997) +
  aes(x=lifeExp) + 
  geom_histogram() 
  
ggplot(gapminder_1997) +
  aes(x=lifeExp) + 
  geom_histogram (bins = 20) #changes the structure of the data

ggplot(gapminder_1997) +
  aes(x=lifeExp) + 
  geom_density() #changes the structure of the data

ggplot(gapminder_1997) +
  aes(x=lifeExp) + 
  geom_freqpoly() #changes the structure of the data


#### ggplot2 Themes

ggplot(gapminder_1997) +
  aes(x=lifeExp) + 
  geom_histogram() +
  theme_classic()

ggplot(gapminder_1997) +
  aes(x=lifeExp) + 
  geom_histogram() +
  theme_minimal()

ggplot(gapminder_1997) +
  aes(x=lifeExp) + 
  geom_histogram() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust=.5))
  #vjust=vertical adjustment; hjust=horizontal adjustment of labels

axis.text #will change all the font in your text

# how to facet a plot
ggplot(gapminder_1997) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point() +
  facet_wrap(vars(continent)) #use the variables in our data to separate plots

ggplot(gapminder_1997) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point() +
  facet_grid(rows = vars(continent)) #can control the segmentation of your output; output will be in rows

ggplot(gapminder_1997) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point() +
  facet_grid(vars(continent)) #you don't always need to name the argument

# we ultimately want to save this data
ggsave("awesome_plot.jpg", width = 6, height = 4) 

#default is to plot the last plot we ran...might need to specify which plot you want by assigning it to an object

violin_plot <- ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin(aes(fill = continent))

#if you want to see your plot
violin_plot
View(violin_plot)

violin_plot + theme_bw() #this only changes the viewer

#if you like it, you need to resave your object
violin_plot <- violin_plot + theme_bw()

ggsave("awesome_violin_plot.jpg", width = 6, height = 4) #default is in inches

#if you want too include your plot in final manuscript, check guidelines to include parameters in your code to simplify things!


#### BONUS EXERCISE - ANIMATED PLOTS
install.packages(c("gganimate", "gifski")) #install packages
library(gganimate) #load library gganimate
library(gifski) #load library gifski

ggplot(data = gapminder_data) +
  aes(x = log(gdpPercap), y = lifeExp, size = pop, color = continent) +
  geom_point()

staticHansPlot <-ggplot(data = gapminder_data) +
  aes(x = log(gdpPercap), y = lifeExp, size = pop/1000000, color = continent) +
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy", color = "Continent", size = "Population (in millions)") +
  theme_classic()

staticHansPlot #view plot

animatedHansPlot <- staticHansPlot +
  transition_states(year, transition_length = 1, state_length = 1) +
  ggtitle("{closest_state}") #give plot a title; alternative to labs()

animatedHansPlot #view animated plot; it will take time

anim_save("hansAnimatedPlot.gif",
          plot = animatedHansPlot,
          renderer = gifski
)
          
          