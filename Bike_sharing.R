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

#Convert to factors

df <- df %>%
  mutate(across(c('season', 'yr', 'mnth', 'hr', 'holiday', 'weekday', 'workingday',
           'weathersit'), as.factor))



























