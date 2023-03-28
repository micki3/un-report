library(tidyverse)
gapminder_1997 <- read_csv("gapminder_1997.csv") 
str(gapminder_1997)
?read_csv
sum(5,6)
round(3.1415)
?round
round(3.1415,3)
round(x=3.1415, digits = 2)

# Making Plot in layers

ggplot(data = gapminder_1997) +  # begins the plots, + connects these commands
  aes(x=gdpPercap) +  # deciding which variable in the data will plot as X
labs(x= "GDP Per Capita") + # labeling the x axis with a title
  
  aes(y=lifeExp) +
  labs(y= "Life Expectancy") +
  
geom_point() +   # plotting the data as points / scatterplot
  
labs(title = "Do people in wealthy countries live longer?") + # overall plot title
  
  aes(color= continent) +
  
  scale_color_brewer(palette = "Set1") +
  
  aes(size = pop/1000000) +
  
  labs(size = "Population (in millions)")

# Summarizing/reducing the previous code

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  labs(size = "GDP Per Capita", y = "Life Expectany",
  title= "Do people in wealthy contries live longer?", size = "Population (in millions)")

#Plotting for data exploration
# Loading in a larger dataset

gapminder_data <- read_csv("gapminder_data.csv") 

dim(gapminder_data) #gives the dimensions of the data

ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_line() + #makes linear plot
  aes(group = country) # groups data to make a spaghetti plot/ countries are colored by continent and is now continuous

# Discrete plots

ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_boxplot() 

ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
geom_violin() + 
  geom_point() 

ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin() + 
  geom_jitter() # another kind of point/ dot plot

#Master Aesthetics
ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp) ) + #putting it here means its impacting all layers
geom_violin() + 
  geom_jitter(aes(size=pop), color= "green") #putting it here means its just impacting the point layer of the plot 

ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp) ) + 
  geom_violin(fill = "grey") # just fills in the violins with a color you want

ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp) ) + 
  geom_violin(aes(fill = continent)) # maps the color of the violin to a variable and generates a legend

#Univariate plots

ggplot(gapminder_1997) + 
  aes(x = lifeExp) +
  geom_histogram(bins = 2)

#Plot themes

ggplot(gapminder_1997) + 
  aes( x = lifeExp) + 
  geom_histogram(bin=20) +
  theme_minimal() #changes themes, minimal=gives gridlines

ggplot(gapminder_1997) + 
  aes( x = lifeExp) + 
  geom_histogram(bin=20) +
  theme_linedraw() #darker gridlines

ggplot(gapminder_1997) + 
  aes( x = lifeExp) + 
  geom_histogram(bin=20) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle=90, vjust= 0.5, hjust= 1)) #must come after the theme_ layer
# angle=90 - this adjusts the angle of the x data labels 90 degrees, vjust and hjust makes sure its centered on a line

ggplot(gapminder_1997) +
  aes (x = gdpPercap, y = lifeExp) +
  geom_point() + 
  facet_wrap(vars(continent)) # compacting/separating large data by making sub plots based on one variable 

ggplot(gapminder_1997) +
  aes (x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(row=vars(continent)) #the subplots now share an x axis 

ggsave("awesomeplot.jpg", width= 6, height= 4) #save the last plot you created as a jpeg with the dimensions specified

awesome_plot <- ggplot(gapminder_1997) + #now you named it and made it an object in your environment
  aes (x = gdpPercap, y = lifeExp) +
  geom_point() + 
  facet_wrap(vars(continent))

ggsave(awesome_plot, 
       file = "awesome_plot.jpg",
       width = 6, height = 4)        #saves any plot you created by name - must name plot and run it first


  
violin_plot  <- ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp) ) +
  labs( x = "Continent", y = "Life Expectancy", title = "Awesome Violin Plot" ) +
  geom_violin(aes(fill = continent)) + 
  scale_color_brewer(palette = "Set1") +
  theme_bw() 

violin_plot 
#previous was my version

ggsave(violin_plot, 
       file = "awesome_violin_plot.jpg",
       width = 6, height = 4)

violin_plot  <- ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp) ) +
  labs( x = "Continent", y = "Life Expectancy") +
  geom_violin(aes(color = continent)) + 
  theme_bw() 

violin_plot 

#teacher version

