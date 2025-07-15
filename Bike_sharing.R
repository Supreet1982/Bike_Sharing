library(tidyverse)

#drop features

df <- hour

df$instant <- NULL
df$dteday <- NULL
df$casual <- NULL
df$registered <- NULL

str(df)

################################################################################

#EDA

#Target Variable

df %>%
  ggplot(aes(cnt)) +
  geom_histogram(binwidth = 30, fill = 'steelblue') + 
  labs(title = 'DIstribution of Bike Rentals', x = 'Number of Rentals',
       y = 'Frequency')

df %>%
  ggplot(aes(y = cnt)) + 
  geom_boxplot(fill = 'steelblue')

#Univariate Analysis

#Convert relevant predictors to factors

df <- df %>%
  mutate(across(c('season', 'yr', 'mnth', 'hr', 'holiday', 'weekday', 'workingday',
           'weathersit'), as.factor))

#Check continuous predictors

df %>%
  select(temp, atemp, hum, windspeed) %>%
  summary()

df %>%
  select(temp, atemp, hum, windspeed) %>%
  gather(variable, value) %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(fill = 'skyblue') +
  labs(title = 'Boxplot of continuous predictors')

#Check for missing values

colSums(is.na(df))

#Check categorical predictors

table(df$season)
table(df$yr)
table(df$mnth)
table(df$hr)
table(df$holiday)
table(df$weekday)
table(df$workingday)
table(df$weathersit)

#Combine levels

df$weathersit <- as.character(df$weathersit)
df$weathersit[df$weathersit == 4] <- '3'
df$weathersit <- as.factor(df$weathersit)

################################################################################

#Multivariate Analysis














