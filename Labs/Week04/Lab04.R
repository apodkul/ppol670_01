
library(dplyr)
library(ggplot2)

### Loading data (via URL)
life <- read.csv("https://raw.githubusercontent.com/apodkul/ppol670_01/main/Labs/Week04/life_expect.csv")

life %>% 
  glimpse()

### ggplot2 
#### Working with geoms 
##### Bar Plots
ggplot(life) + 
  geom_bar(aes(x = Continent))

life %>% 
  group_by(Continent) %>%
  summarize(Count = n()) %>%
  ggplot() + 
  geom_bar(aes(x = Continent, y = Count), stat = 'identity')

##### Histograms
ggplot(life) + 
  geom_histogram(aes(x = life_expectancy))

ggplot(life) + 
  geom_histogram(aes(x = life_expectancy), 
                 color = 'white', fill = 'black')

ggplot(life) + 
  geom_histogram(aes(x = life_expectancy), 
                 color = 'white', fill = 'black', 
                 bins = 50)

##### Density
ggplot(life) + 
  geom_density(aes(x = life_expectancy))


##### Ridge plots
#install.packages("ggridges")
library(ggridges)
ggplot(life) + 
  geom_density_ridges(aes(x = life_expectancy, 
                          y = Continent))


##### Scatter plots
ggplot(life) + 
  geom_point(aes(x = GDP_per_capita, 
                 y = life_expectancy))

ggplot(life) + 
  geom_point(aes(x = GDP_per_capita, 
                 y = life_expectancy, 
                 color = Continent))


#### Adjusting Scales 
ggplot(life) + 
  geom_point(aes(x = GDP_per_capita, 
                 y = life_expectancy, 
                 color = Continent)) + 
  scale_x_log10('GDP per capita')


ggplot(life) + 
  geom_point(aes(x = GDP_per_capita, 
                 y = life_expectancy, 
                 color = Continent)) + 
  scale_x_log10('GDP per capita', 
                labels = scales::dollar)

ggplot(life) + 
  geom_point(aes(x = GDP_per_capita, 
                 y = life_expectancy, 
                 color = Continent)) + 
  scale_x_log10('GDP per capita', 
                labels = scales::dollar) + 
  scale_y_continuous('Life Expectancy', 
                     limits = c(45, 90))


#### Adding titles
ggplot(life) + 
  geom_point(aes(x = GDP_per_capita, 
                 y = life_expectancy, 
                 color = Continent)) + 
  scale_x_log10('GDP per capita', 
                labels = scales::dollar) + 
  scale_y_continuous('Life Expectancy', 
                     limits = c(45, 90)) + 
  labs(title = 'Life Expectancy v. GDP Per Capita', 
       subtitle = 'Broken out by Continent')

#### Custom theme work 
#install.packages("ggthemes")

library(ggthemes)

ggplot(life) + 
  geom_point(aes(x = GDP_per_capita, 
                 y = life_expectancy, 
                 color = Continent)) + 
  scale_x_log10('GDP per capita', 
                labels = scales::dollar) + 
  scale_y_continuous('Life Expectancy', 
                     limits = c(45, 90)) + 
  labs(title = 'Life Expectancy v. GDP Per Capita', 
       subtitle = 'Broken out by Continent') + 
  theme_clean()
  

ggplot(life) + 
  geom_point(aes(x = GDP_per_capita, 
                 y = life_expectancy, 
                 color = Continent)) + 
  scale_x_log10('GDP per capita', 
                labels = scales::dollar) + 
  scale_y_continuous('Life Expectancy', 
                     limits = c(45, 90)) + 
  labs(title = 'Life Expectancy v. GDP Per Capita', 
       subtitle = 'Broken out by Continent') + 
  theme_clean() + 
  theme(legend.position = 'top')

ggplot(life) + 
  geom_point(aes(x = GDP_per_capita, 
                 y = life_expectancy, 
                 color = Continent)) + 
  scale_x_log10('GDP per capita', 
                labels = scales::dollar) + 
  scale_y_continuous('Life Expectancy', 
                     limits = c(45, 90)) + 
  labs(title = 'Life Expectancy v. GDP Per Capita', 
       subtitle = 'Broken out by Continent') + 
  theme_clean() + 
  theme(legend.position = 'top', 
        axis.title.y = element_text(angle = 0, 
                                    hjust = 0.5, 
                                    vjust = 0.5))

ggplot(life) + 
  geom_point(aes(x = GDP_per_capita, 
                 y = life_expectancy, 
                 color = Continent)) + 
  scale_x_log10('GDP per capita', 
                labels = scales::dollar) + 
  scale_y_continuous('Life Expectancy', 
                     limits = c(45, 90)) + 
  labs(title = 'Life Expectancy v. GDP Per Capita', 
       subtitle = 'Broken out by Continent') + 
  theme_clean() + 
  theme(legend.position = 'top', 
        axis.title.y = element_text(angle = 0, 
                                    hjust = 0.5, 
                                    vjust = 0.5)) + 
  scale_color_tableau()



#### Saving plots locally 
ggplot(life) + 
  geom_point(aes(x = GDP_per_capita, 
                 y = life_expectancy, 
                 color = Continent)) + 
  scale_x_log10('GDP per capita', 
                labels = scales::dollar) + 
  scale_y_continuous('Life Expectancy', 
                     limits = c(45, 90)) + 
  labs(title = 'Life Expectancy v. GDP Per Capita', 
       subtitle = 'Broken out by Continent') + 
  theme_clean() + 
  theme(legend.position = 'top', 
        axis.title.y = element_text(angle = 0, 
                                    hjust = 0.5, 
                                    vjust = 0.5)) + 
  scale_color_tableau()

ggsave(filename = 'plotname.png', 
         width = 7, height = 5, units = 'in')