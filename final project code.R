library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(furniture)
library(Hmisc)
set.seed(143)

airbnb <- read_csv("listings.csv")

airbnb <- airbnb %>% filter(price > 0)
airbnb <- airbnb %>% filter(price < 1000)
airbnb$room_type <- as.factor(airbnb$room_type)
airbnb$neighbourhood_group <- as.factor(airbnb$neighbourhood_group)
airbnb[,c(10,11,12,16)] = sapply(airbnb[,c(10,11,12,16)], as.numeric)

table1(airbnb, price, room_type, minimum_nights, number_of_reviews, availability_365, splitby = ~ neighbourhood_group)

sample <- sample_n(airbnb, 10000)

ggplot(data=sample, aes(sample$price)) + 
  geom_histogram(col="red", fill="pink", alpha = .9, bins=50) + 
  xlab("price($)") +
  ggtitle("Distribution of Airbnb Price") +
  theme_economist()

res <- cor(sample[,c(10,11,12,16)])
round(res, 2)

model_add <- lm(price ~ room_type + number_of_reviews, data = sample)
summary(model_add)

model_int <- lm(price ~ room_type * number_of_reviews, data = sample)
summary(model_int)

anova(model_add, model_int)