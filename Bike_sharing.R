library(tidyverse)
library(car)
library(GGally)

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
  mutate(across(c('season', 'yr', 'mnth', 'hr', 'holiday', 'weekday', 
                  'workingday','weathersit'), as.factor))

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

#Rename factor levels to descriptive names

df$season <- factor(df$season, levels = c(1, 2, 3, 4),
                  labels = c('Spring', 'Summer', 'Fall', 'Winter'))
df$yr <- factor(df$yr, levels = c(0, 1), labels = c('2011', '2012'))
df$holiday <- factor(df$holiday, levels = c(0, 1), 
                     labels = c('No', 'Yes'))
df$workingday <- factor(df$workingday, levels = c(0, 1),
                        labels = c('No', 'Yes'))
df$weathersit <- factor(df$weathersit, levels = c(1, 2, 3),
              labels = c('Clear', 'Misty_Cloudy', 'Light_Rain_Snow'))

#Relevel categorical variables

cat_vars <- c('season', 'yr', 'mnth', 'hr', 'holiday', 'weekday',
              'workingday', 'weathersit')

for (i in cat_vars) {
  table2 <- as.data.frame(table(df[,i]))
  max <- which.max(table2[,2])
  level_name <- as.character(table2[max, 1])
  df[,i] <- relevel(df[,i], ref = level_name)
}

df %>%
  ggplot(aes(weathersit)) +
  geom_bar(fill='blue')

################################################################################

#Multivariate Analysis

#Numeric predictors vs Numeric target

df %>%
  ggplot(aes(temp, cnt)) +
  geom_point(alpha=0.6) +
  geom_smooth(method = 'loess', color='blue') +
  labs(title = 'Bike Rentals vs Temperature', x = 'Normalized Temp',
       y = 'Rentals (cnt)')

df %>%
  ggplot(aes(hum, cnt)) +
  geom_point(alpha=0.6) +
  geom_smooth(method = 'loess', color='blue') +
  labs(title = 'Bike Rentals vs Humidity', x = 'Normalized Humidity',
       y = 'Rentals (cnt)')

df %>%
  ggplot(aes(windspeed, cnt)) +
  geom_point(alpha=0.6) +
  geom_smooth(method = 'loess', color='blue') +
  labs(title = 'Bike Rentals vs Windspeed', x = 'Normalized Windspeed',
       y = 'Rentals (cnt)')

#Categorical predictors vs Numeric target

df %>%
  group_by(hr) %>%
  summarize(avg_cnt = mean(cnt)) %>%
  ggplot(aes(x = as.numeric(as.character(hr)), y = avg_cnt)) +
  geom_line() +
  labs(title = 'Average Rentals by Hour', x = 'Hour', y = 'Average Rentals')

df %>%
  group_by(hr, workingday) %>%
  summarize(avg_cnt = mean(cnt)) %>%
  ggplot(aes(x = as.numeric(as.character(hr)), y = avg_cnt,
             color = factor(workingday))) +
  geom_line() +
  labs(title = 'Hourly Rentals: Working Day vs Non-working Day', x = 'Hour', 
       y = 'Average Rentals', color = 'Working Day')

df %>%
  ggplot(aes(season, cnt)) +
  geom_boxplot(fill = 'skyblue') +
  labs(title = 'Bike Rentals by Season', x = 'Season', y = 'Rentals (cnt)')

df %>%
  ggplot(aes(x = hum, y = cnt, color = weathersit)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)


#Multicollinearity check

num_vars <- df %>% select(temp, atemp, hum, windspeed)
cor(num_vars)

ggpairs_plot <- ggpairs(df %>% select(cnt, temp, atemp, hum, windspeed))

ggsave('corr_plot.png', plot = ggpairs_plot, width = 10, height = 8, dpi = 300,
       units = 'in')

vif(lm(cnt ~ temp + hum + windspeed, data = df))

df$atemp <- NULL

################################################################################











