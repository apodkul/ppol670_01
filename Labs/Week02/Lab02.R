### Lab session .r file 


## Manually Write Data (in multiple steps)
x <- c(1, 10, 2, 4,
       5, 2, 8, 9, 
       3, 4)
y <- c("Red", "Green", "Blue", "Blue", 
       "Yellow", "Orange", "Red", "Blue", 
       "Green", "Blue")
z <- c(1:10)
test_df <- data.frame(x, y, z)

test_df
test_df[1,]
test_df[,1]
test_df$x


## Load a .csv file
data <- read.csv('https://raw.githubusercontent.com/apodkul/ppol670_01/main/Labs/Week02/census_demo.csv')


## Explore the data file 
dim(data)
nrow(data)
ncol(data)

head(data)
str(data)
summary(data)

View(data) #note capitalization of the command

## Access variables
names(data)
colnames(data)
data$COUNTY

## Create a new variable 
data$Male_Percent <- data$TOT_MALE/data$TOT_POP
head(data$Male_Percent)
summary(data$Male_Percent)

data$Male_Percent <- data$Male_Percent*100
head(data$Male_Percent)
summary(data$Male_Percent)


## Explore a categorical variable
table(data$STNAME)
prop.table(table(data$STNAME))

## Explore a numeric variable
summary(data$TOT_POP)
sd(data$TOT_POP)


## Working with Packages 
install.packages('haven') # How often do we have to install it?
library(haven)

## Export data 
write.csv(x = data, 'file_name.csv', 
          row.names = F)

write_dta(data, 'file_name.dta')

## Where did the files go? 
getwd() 
setwd('')
### R Projects
