################################################
# Data Science Essentials in R: Linear Models
#
# UVA Library Research Data Services
# Feburary 12, 2019
# Michele Claibourn
# 
# 1. Load libraries, read in data
# 2. Before you model, explore
# 3. Linear model estimation, extraction, inference
# 4. Model Specification, factors, interactions, nonlinearity
# 5. Model evaluation
# 6. Regression as Machine Learning
################################################


################################################
# 1. Load libraries, read in data ----

library(tidyverse)
library(GGally)
library(broom)

# Get data from github
github_url <- "https://github.com/uvastatlab/phdplus/raw/master/data/albemarle_homes.rds"
homes <- readRDS(url(github_url))


################################################
# 2. Before you model, explore ----

# one more variable: age of home
homes <- homes %>% 
  mutate(age = 2016 - YearBuilt)

ggplot(homes, aes(x = age)) + geom_histogram(bins = 100)

city_age <- homes %>% 
  filter(age < 2016) %>% 
  group_by(city) %>% 
  summarize(med_age = median(age))

homes <- left_join(homes, city_age)

homes <- homes %>% 
  mutate(age = if_else(age == 2016, med_age, age))
ggplot(homes, aes(x = age)) + geom_histogram(bins = 100)

# scatterplots and smoothers
ggplot(homes, aes(x = FinSqFt, y = TotalValue)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue") +
  geom_smooth(color = "orange")

# scatterplot matrix
homes_reduced <- homes %>% 
  select(TotalValue, age, FinSqFt, LotSize, Bedroom, FullBath, city) %>% 
  sample_n(5000)

# from GGally
ggpairs(homes_reduced, columns = 1:6)
ggpairs(homes_reduced, columns = 1:6, 
        lower = list(continuous = "smooth"))

# facets as conditioning plots
ggplot(homes, aes(y = TotalValue, x = FinSqFt)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ city)
# note: filter TotalValue < 2e6; log FinSqFt; add remodel as color

# YOUR TURN: create a graph of the relatonship between TotalValue and 
#   another numeric variable of your choice, conditioning/faceting by city


################################################
# 3. Linear model estimation, extraction, inference ----
lm_value <- lm(TotalValue ~ age + FinSqFt + LotSize + Bedroom + FullBath, 
                  data = homes)

# extract model summary
summary(lm_value)

# targeted extractors
coef(lm_value)
confint(lm_value) # 95%
head(fitted(lm_value))
head(residuals(lm_value))

# save summary information
ms_value <- summary(lm_value)
# and extract quantitites
ms_value$coefficients
ms_value$adj.r.squared
ms_value$sigma

# YOUR TURN: estimate your own model of TotalValue --
#   add at least one predictor, save it as a new model object,
#   and summarize the model; 
#   are the newly added variables significantly related?
#   do the effects of the initially included variables change?


################################################
# 4. Model Specification, factors, interactions, nonlinearity ----

# factors
table(homes$city)

lm_value <- lm(TotalValue ~ city + age + FinSqFt + LotSize + Bedroom + FullBath, 
                  data = homes)
summary(lm_value)

# to use a different factor level as the reference, relevel
lm_value <- lm(TotalValue ~  relevel(city, ref = "KESWICK") + age + FinSqFt + LotSize + Bedroom + FullBath, 
               data = homes)
summary(lm_value)

# interactions: a factor and numeric variable
lm_value1 <- lm(TotalValue ~ city*FinSqFt + age + LotSize + Bedroom + FullBath, 
                  data = homes)
summary(lm_value)

# interactions: a numeric and numeric variable
lm_value <- lm(TotalValue ~ city + FinSqFt*LotSize + age + Bedroom + FullBath, 
               data = homes)
summary(lm_value)

# nonlinearity: polynomials with I()
lm_value2 <- lm(TotalValue ~ city + FinSqFt + I(FinSqFt^2) + age + LotSize + Bedroom + FullBath, 
               data = homes)
summary(lm_value2)

# nonlinearity: polynomials with poly()
lm_value3 <- lm(TotalValue ~ city*poly(FinSqFt, 2, raw = TRUE) + age + LotSize + Bedroom + FullBath, 
               data = homes)
summary(lm_value3)

# nonlinearity: logged values
lm_value4 <- lm(log(TotalValue) ~ city*poly(FinSqFt, 2, raw = TRUE) + age + LotSize + Bedroom + FullBath,
                data = homes)
summary(lm_value4)

# YOUR TURN: estimate your own model of TotalValue -- 
#   with a different interaction and/or polynomial,
#   save it as a new model object and summarize the model


################################################
# 5. Model evaluation, diagnostic plots, F-tests, AIC, visualization ----

# Diagnostic plots
plot(lm_value1)

# or make all plots fit in one graph
op <- par(mfrow=c(2,2)) 
plot(lm_value2)
par(op) # restore to previous setting

# or pick a plot
plot(lm_value3, which = 1)
plot(lm_value3, which = 2)
plot(lm_value3, which = 3)
plot(lm_value3, which = 5)

# what are the high leverage points
homes %>% 
  select(TotalValue, city, age, FinSqFt, LotSize, Bedroom, FullBath) %>% 
  slice(c(17148, 31932))


# Model specification and model fit: F-test
# does inclusion of city interactions significantly improve the model?
anova(lm_value2, lm_value3)


# Model specification and model fit: AIC
# is the model with city interactions better?
extractAIC(lm_value3) # df AIC
extractAIC(lm_value2)

# YOUR TURN: using one of the new models you created earlier
#   compare it to model 3 based on AIC -- which is better?


# Visualization
# tidy the summary output of a linear model into a data frame (from broom)
tidy_val3 <- tidy(lm_value3, conf.int = TRUE)
tidy_val3

# or augment the original data with fitted values, SE of fitted values, residuals, etc.
tidy_val3_aug <- augment(lm_value3)
tidy_val3_aug

# then work with ggplot2
# create residuals vs fitted plot using augmented data
ggplot(tidy_val3_aug, aes(x = .fitted, y = .resid)) +  
  geom_point() +
  geom_smooth(se=F)

# create a coefficient plot to visualize magnitude and uncertainty of coefficients
ggplot(tidy_val3, aes(x = estimate, y = term, 
                 xmin = conf.low, 
                 xmax = conf.high)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh()

# coefficient plot for intercept/factors 
ggplot(tidy_val3[1:10,], aes(x = estimate, y = term, 
                     xmin = conf.low, 
                     xmax = conf.high)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh() +
  labs(x = "Coefficient Estimate", y = "Intercept and Factors",
       title = "Effect of City on Intercept")

# coefficient plot for slopes 
ggplot(tidy_val3[11:16,], aes(x = estimate, y = term, 
                            xmin = conf.low, 
                            xmax = conf.high)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh() + 
  labs(x = "Coefficient Estimate", y = "Slopes Coefficients",
       title = "Effect of Square Feet, Lot Size, Age, and Rooms")

# coefficient plot for slopes with interactions 
ggplot(tidy_val3[c(11:12, 17:34),], aes(x = estimate, y = term, 
                              xmin = conf.low, 
                              xmax = conf.high)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh() + 
  labs(x = "Coefficient Estimate", y = "Slope and Interactions",
       title = "Effect of City/Square Feet Interactions")

# come back next week for more on visualizing model effects!!!


################################################
# 6. Regression as Machine Learning ----
# RMSE, average model prediction error, via cross-validation

# a. divide data into two sets: training and test
train <- sample(nrow(homes), round(0.75*nrow(homes)))

# b. build model with training data
# use model 3: city*poly(FinSqFt, 2, raw = TRUE) + age + LotSize + Bedroom + FullBath
lm_train <- lm(TotalValue ~ city*poly(FinSqFt, 2, raw = TRUE) + age + LotSize + Bedroom + FullBath, 
               data = homes, subset = train)

# c. predict response with test data and generate RMSE
sqrt(mean((homes$TotalValue - predict(lm_train, homes))[-train]^2))

# repeat these steps 1000 times (thanks, Clay!)
# this takes about 2 minutes
rmse3 <- numeric(1000) # empty vector to store RMSE
for(i in 1:1000){
  # randomly sample row numbers
  train <- sample(nrow(homes), round(0.75*nrow(homes)))
  # build model using only "train" data
  lm_train <- lm(TotalValue ~ city*poly(FinSqFt, 2, raw = TRUE) + age + LotSize + Bedroom + FullBath, 
                 data = homes, subset = train)
  # test error estimate by using test data to predict
  rmse3[i] <- sqrt(mean((homes$TotalValue - predict(lm_train, homes))[-train]^2))
}
# this generates a more realistic measure of average prediction error
mean(rmse3) 

# here's the RMSE of this model using the full data
sqrt(mean(residuals(lm_value3)^2))

# RMSE is most useful for comparing models
# compare to model 2: TotalValue ~ city*FinSqFt + age + LotSize + Bedroom + FullBath
rmse2 <- numeric(1000) # empty vector to store MSE
for(i in 1:1000){
  # randomly sample row numbers
  train <- sample(nrow(homes), round(0.75*nrow(homes)))
  # build model using only "train" data
  lm_train <- lm(TotalValue ~ city*FinSqFt + age + LotSize + Bedroom + FullBath, 
                 data = homes, subset = train)
  # test error estimate by using test data to predict
  rmse2[i] <- sqrt(mean((homes$TotalValue - predict(lm_train, homes))[-train]^2))
}
mean(rmse2)
# model 3 is better


# RMSE via k-fold cross validation

# a. fit the model using glm()
# using model 3
glm_value3 <-  glm(TotalValue ~ city*poly(FinSqFt, 2, raw = TRUE) + age + LotSize + Bedroom + FullBath, 
                   data=homes)
summary(glm_value3)

# b. use cv.glm() function (from boot)
library(boot) 

# 5-fold cross validation
cv_mse <- cv.glm(homes, glm_value3, K=5)$delta 
cv_mse # the first number is mean MSE
sqrt(cv_mse[1]) # RMSE

# compare to a model with a three-way interaction
# let's call it model 5: TotalValue ~ city*poly(FinSqFt, 2, raw = TRUE)*LotSize + age + Bedroom + FullBath
glm_value5 <-  glm(TotalValue ~ city*poly(FinSqFt, 2, raw = TRUE)*LotSize + age + Bedroom + FullBath, 
                   data=homes)
cv_mse5 <- cv.glm(homes, glm_value5, K=5)$delta 
sqrt(cv_mse5[1])
# model 3 is better

