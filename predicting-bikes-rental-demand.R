# Predicting Bike Shares Demand with Ridge and Lasso

## Importing required packages

# install.packages("tidyverse")
# install.packages("tidymodels")

library(tidyverse)
library(tidymodels)

bikes_hours <- read_csv("bike-sharing-dataset/hour.csv")

glimpse(bikes_hours)


# theme set

theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))


# first six observations of the datasets

head(bikes_hours)

# checking missing values
sum(is.na(bikes_hours))

# summary statistics
summary(bikes_hours)

# Constructing a 'bike_data' dataframe with organized data attributes

bike_data <-  data.frame(count = bikes_hours$cnt,
                         season = as.factor(bikes_hours$season), 
                         year = as.factor(bikes_hours$yr),
                         month = as.factor(bikes_hours$mnth),
                         hour = as.factor(bikes_hours$hr),
                         holi = as.factor(bikes_hours$holiday),
                         week = as.factor(bikes_hours$weekday),
                         work = as.factor(bikes_hours$workingday),
                         weather = as.factor(bikes_hours$weathersit),
                         env.temp = bikes_hours$temp,
                         feel.temp = bikes_hours$atemp,
                         humidity = bikes_hours$hum,
                         windspeed = bikes_hours$windspeed,
                         registered = bikes_hours$registered,
                         casual = bikes_hours$casual)

# Column names
names(bike_data)

# Glimpse of new data
glimpse(bike_data)

# Distribution of response variable.

bike_data |> 
  ggplot(aes(x = count)) +
  geom_histogram() +
  labs(title = "Distriution of Count")


# Fit a Poisson regression model
poisson_model <- glm(count ~ ., 
                     data = bike_data, 
                     family = "poisson")

summary(poisson_model)

# Checking over dispersion

# install.packages("AER")

library(AER)

dispersion_test <- dispersiontest(poisson_model)
print(dispersion_test)


# install.packages("MASS")

library(MASS)

model_nb <- glm.nb(count ~ ., data = bike_data)

summary(model_nb)


# Calculating the AIC
aic_value <- AIC(model_nb)

# AIC value
aic_value