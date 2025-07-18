library(tidyverse)
library(car)
library(GGally)
library(caret)
library(MASS)

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
  ggplot(aes(weathersit, cnt)) +
  geom_boxplot(fill = 'skyblue') +
  labs(title = 'Bike Rentals by Weather', x = 'Weather', y = 'Rentals (cnt)')

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

#Clustering

hourly_data <-df %>%
  group_by(hr) %>%
  summarize(
    avg_temp = mean(temp),
    avg_hum = mean(hum),
    avg_ws = mean(windspeed),
    avg_cnt = mean(cnt)
  )

hourly_scaled <- scale(hourly_data %>% select(-hr))

set.seed(123)

km <- kmeans(hourly_scaled, centers = 3)

hourly_data$cluster <- factor(km$cluster)

hourly_data %>%
  ggplot(aes(hr, avg_cnt, color = cluster)) +
  geom_point(size = 3) +
  geom_line(aes(group = cluster)) +
  labs(title = 'Hourly Rentals clustered by Weather and Demand')

df$workingday <- NULL

################################################################################

set.seed(4769)

partition <- createDataPartition(df$cnt, p = 0.75, list = FALSE)
df_train <- df[partition,]
df_test <- df[-partition,]

mean(df_train$cnt)
mean(df_test$cnt)

#Fitting Poisson GLM

poisson_glm <- glm(cnt ~ ., data = df_train, family = poisson(link = 'log'))
summary(poisson_glm)

#Check for overdispersion

dispersion_ratio <- deviance(poisson_glm) / df.residual(poisson_glm)
dispersion_ratio

#Fitting Negative Binomial GLM

nb_glm <- glm.nb(cnt ~ ., data = df_train)
summary(nb_glm)

#Predicted vs Actual plot

df_test$pred_nb <- predict(nb_glm, newdata = df_test, type = 'response')
df_test$pred_pois <- predict(poisson_glm, newdata = df_test, type = 'response')

df_test %>%
  ggplot(aes(cnt, pred_nb)) +
  geom_point(alpha=0.4) +
  geom_abline(color='red', linetype='dashed')+
  labs(title = 'Predicted vs Actual Rentals (NB GLM)',
       x = 'Actual Rentals', y = 'Predicted Rentals')

df_test %>%
  ggplot(aes(cnt, pred_pois)) +
  geom_point(alpha=0.4) +
  geom_abline(color='red', linetype='dashed')+
  labs(title = 'Predicted vs Actual Rentals (Poisson)',
       x = 'Actual Rentals', y = 'Predicted Rentals')

RMSE(df_test$pred_nb, df_test$cnt)
RMSE(df_test$pred_pois, df_test$cnt)

res <- residuals(nb_glm, type = 'deviance')
fitted_vals <- fitted(nb_glm)

plot(res, main = 'Deviance Residuals', ylab = 'Residual', xlab = 'index')

cooksD <- cooks.distance(nb_glm)
plot(cooksD, type = 'h', main = "Cook's Distance", ylab = 'Distance')
abline(h = 4 / length(cooksD), col='red', lty=2)

influential <- which(cooksD > 4/length(cooksD)) 
df_train[influential,]

influential_df <- data.frame(
  Index = 1:length(cooksD),
  CooksDistance = cooksD
)

top10_influential <- influential_df[order(-influential_df$CooksDistance),][1:10,]
top10_influential

################################################################################

nb_glm_interact <- glm.nb(cnt ~ . + hr * weekday, data = df_train)
summary(nb_glm_interact)
AIC(nb_glm)
summary(nb_glm)

df_test$pred_nb_int <- predict(nb_glm_interact, newdata = df_test, 
                               type = 'response')

RMSE(df_test$pred_nb_int, df_test$cnt)

cooksD_int <- cooks.distance(nb_glm_interact)
plot(cooksD_int, type = 'h', main = "Cook's Distance after interaction term", 
     ylab = 'Distance')
abline(h = 4 / length(cooksD_int), col='red', lty=2)

df_test %>%
  ggplot(aes(cnt, pred_nb_int)) +
  geom_point(alpha=0.4) +
  geom_abline(color='red', linetype='dashed')+
  labs(title = 'Predicted vs Actual Rentals (NB GLM with interaction)',
       x = 'Actual Rentals', y = 'Predicted Rentals')

################################################################################