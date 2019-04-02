################################################
# Data Science Essentials in R: Linear Models
#
# UVA Library Research Data Services
# March 21, 2019
# Michele Claibourn
# 
# 1. Load libraries, read in data
# 2. Before you model, explore
# 3. Linear model estimation, extraction, inference
# 4. Model Specification, factors, interactions, nonlinearity
# 5. Model evaluation, diagnostic plots, F-tests, AIC
# 6. Model visualization
# 7. Regression as Machine Learning
################################################


################################################
# 1. Load libraries, read in data ----

# install.packages("tidyverse")
library(tidyverse)
library(GGally)
library(broom)

# Get data from github
github_url <- "https://github.com/uvastatlab/phdplus/raw/master/data/albemarle_homes.rds"
homes <- readRDS(url(github_url))
str(homes)

################################################
# 2. Before you model, explore ----

# scatterplots and smoothers
ggplot(homes, aes(x = finsqft, y = totalvalue)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue") +
  geom_smooth(color = "orange")

# scatterplot matrix
homes_reduced <- homes %>% 
  select(totalvalue, age, finsqft, lotsize, bedroom, fullbath, city) %>% 
  sample_n(5000)

# from GGally
ggpairs(homes_reduced, columns = 1:6)
ggpairs(homes_reduced, columns = 1:6, 
        lower = list(continuous = "smooth"))

# facets as conditioning plots
ggplot(homes, 
       aes(y = totalvalue, x = finsqft)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ city)
# note: filter totalvalue < 2e6; log finsqft; add remodel as color


# YOUR TURN: create a graph of the relatonship between totalvalue and 
#   another numeric variable of your choice, faceting by city



################################################
# 3. Linear model estimation, extraction, inference ----
lm_value <- lm(totalvalue ~ age + finsqft + lotsize + bedroom + fullbath, 
                  data = homes)

# extract model summary
summary(lm_value)

# What's up with the missing age variable? Need to go back to the
# data prep script and fix this. In the meantime...
homes <- homes %>% filter(!is.na(age))

lm_value <- lm(totalvalue ~ age + finsqft + lotsize + bedroom + fullbath, 
               data = homes)
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

# YOUR TURN: estimate your own model of totalvalue --
#   add at least one predictor, save it as a new model object,
#   and summarize the model; 
#   are the newly added variables significantly related?
#   do the effects of the initially included variables change?



################################################
# 4. Model Specification, factors, interactions, nonlinearity ----

# factors
table(homes$city)

lm_value <- lm(totalvalue ~ city + age + finsqft + lotsize + bedroom + fullbath, 
                  data = homes)
summary(lm_value)

# to use a different factor level as the reference, relevel
lm_value <- lm(totalvalue ~  relevel(city, ref = "KESWICK") + age + finsqft + lotsize + bedroom + fullbath, 
               data = homes)
summary(lm_value)

# back to Cville as baseline
lm_value <- lm(totalvalue ~ city + age + finsqft + lotsize + bedroom + fullbath, 
               data = homes)

# is city factor significant?
anova(lm_value)


# interactions
# interacting a factor and numeric variable: city and square feet
lm_value1 <- lm(totalvalue ~ city*finsqft + age + lotsize + bedroom + fullbath, 
                  data = homes)
summary(lm_value1)

# is city-square foot interaction significant?
anova(lm_value1)


# interacting a numeric and numeric variable: square feet and lotsize
lm_value2 <- lm(totalvalue ~ city + finsqft*lotsize + age + bedroom + fullbath, 
               data = homes)
summary(lm_value2)


# nonlinearity
# including polynomials with I()
lm_value3 <- lm(totalvalue ~ city + finsqft + I(finsqft^2) + age + lotsize + bedroom + fullbath, 
               data = homes)
summary(lm_value3)

# including polynomials with poly()
lm_value4 <- lm(totalvalue ~ city*poly(finsqft, 2) + age + lotsize + bedroom + fullbath, 
               data = homes)
summary(lm_value4)


# including logged values (of y)
lm_value5 <- lm(log(totalvalue) ~ city*finsqft + age + lotsize + bedroom + fullbath,
                data = homes)
summary(lm_value5)

# YOUR TURN: estimate your own model of totalvalue -- 
#   with a different interaction and/or polynomial,
#   save it as a new model object and summarize the model



################################################
# 5. Model evaluation, diagnostic plots, F-tests, AIC ----

# Diagnostic plots
plot(lm_value1)

# or make all plots fit in one graph
op <- par(mfrow=c(2,2)) 
plot(lm_value4)
par(op) # restore to previous setting

# or pick a plot
plot(lm_value5, which = 1)
plot(lm_value5, which = 2)
plot(lm_value5, which = 3)
plot(lm_value5, which = 5)

# what are the high leverage points
homes %>% 
  select(totalvalue, city, age, finsqft, lotsize, bedroom, fullbath) %>% 
  slice(c(23153, 3804))


# Model specification and model fit: F-test
# does inclusion of city-square foot interaction 
#   significantly improve the model?
anova(lm_value1, lm_value)


# Model specification and model fit: AIC
# is the model with the city-square foot interacted or 
#   the model with city-square foot^2 interacted a better fit?
extractAIC(lm_value1) # df AIC
extractAIC(lm_value4)

# is the model with logged y or squared square feet a better fit?
extractAIC(lm_value)
extractAIC(lm_value5)
# you can't use AIC (or most model fit statistics) to compare 
#   models with different y values! 
# you could transform the AIC for the logged y model to compare...
extractAIC(lm_value5)[2] + 2*sum(log(homes$totalvalue))


# YOUR TURN: generate a two competing models (you might adapt one from above)
# and compare the models with an anova test (if nested)
# or with AIC (if non-nested)



################################################
# 6. Model visualization ----
# broom

# tidy the summary output of a linear model into a data frame (from broom)
tidy_mod5 <- tidy(lm_value5, conf.int = TRUE)
tidy_mod5

# or augment the original data with fitted values, SE of fitted values, residuals, etc.
tidy_mod5_aug <- augment(lm_value5)
tidy_mod5_aug

# then work with ggplot2

# use the augmented data frame to create diagnostic plots, e.g., 
# residuals vs fitted plot 
ggplot(tidy_mod5_aug, aes(x = .fitted, y = .resid)) +  
  geom_point() +
  geom_smooth(se=F)

# or create your own diagnostic plot, e.g., 
# residuals vs variables
ggplot(tidy_mod5_aug, aes(x = finsqft, y = .resid)) +
  geom_point() + 
  geom_smooth()

ggplot(tidy_mod5_aug, aes(x = city, y = .resid)) + 
  geom_boxplot()


# use the tidied summary output to create a coefficient plot
# to visualize magnitude and uncertainty of coefficients
ggplot(tidy_mod5, aes(x = estimate, y = term, 
                 xmin = conf.low, 
                 xmax = conf.high)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh()

# coefficient plot just for intercept/factors 
ggplot(tidy_mod5[1:10,], aes(x = estimate, y = term, 
                     xmin = conf.low, 
                     xmax = conf.high)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh() +
  labs(x = "Coefficient Estimate", y = "Intercept and Factors",
       title = "Effect of City on Intercept")

# coefficient plot just for fixed slopes
ggplot(tidy_mod5[11:15,], aes(x = estimate, y = term, 
                            xmin = conf.low, 
                            xmax = conf.high)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh() + 
  labs(x = "Coefficient Estimate", y = "Slopes Coefficients",
       title = "Effect of Square Feet, Lot Size, Age, and Rooms")

# coefficient plot for varying slopes (interactions) 
ggplot(tidy_mod5[c(11, 16:24),], aes(x = estimate, y = term, 
                              xmin = conf.low, 
                              xmax = conf.high)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_errorbarh() + 
  labs(x = "Coefficient Estimate", y = "Slope and Interactions",
       title = "Effect of City/Square Feet Interactions")

# come back next week for more on visualizing model effects!!!


################################################
# 7. Regression as Machine Learning ----
# RMSE, average model prediction error, via cross-validation

# a. divide data into two sets: training and test
train <- sample(nrow(homes), round(0.75*nrow(homes)))

# b. build model with training data
# use lm_value4: city*poly(FinSqFt, 2) + age + lotsize + bedroom + fullbath
lm_train <- lm(totalvalue ~ city*poly(finsqft, 2) + age + lotsize + bedroom + fullbath, 
               data = homes, subset = train)

# c. predict response with test data and generate RMSE
sqrt(mean((homes$totalvalue - predict(lm_train, homes))[-train]^2))

# repeat these steps 1000 times (thanks, Clay!)
# this took about 2 minutes
# so I'm loading prior results
# setwd("~/Box Sync/AC_homes/modelR/")
# load("homes_rmse.Rdata")
rmse4 <- numeric(1000) # empty vector to store RMSE
for(i in 1:1000){
  # randomly sample row numbers
  train <- sample(nrow(homes), round(0.75*nrow(homes)))
  # build model using only "train" data
  lm_train <- lm(totalvalue ~ city*poly(finsqft, 2) + age + lotsize + bedroom + fullbath, 
                 data = homes, subset = train)
  # generate error estimate by using test data to predict
  rmse4[i] <- sqrt(mean((homes$totalvalue - predict(lm_train, homes))[-train]^2))
}
mean(rmse4)
# this generates a more realistic measure of average 
# prediction error; compare to the RMSE using the full data
sqrt(mean(residuals(lm_value4)^2))

# RMSE is most useful for comparing models
# compare to lm_value1: totalvalue ~ city*finsqft + age + lotsize + bedroom + fullbath
rmse1 <- numeric(1000) # empty vector to store MSE
for(i in 1:1000){
  # randomly sample row numbers
  train <- sample(nrow(homes), round(0.75*nrow(homes)))
  # build model using only "train" data
  lm_train <- lm(totalvalue ~ city*finsqft + age + lotsize + bedroom + fullbath, 
                 data = homes, subset = train)
  # test error estimate by using test data to predict
  rmse1[i] <- sqrt(mean((homes$totalvalue - predict(lm_train, homes))[-train]^2))
}
mean(rmse1)
mean(rmse4) 
# model 4 is better


# RMSE via k-fold cross validation
# a. fit the model using glm()
# using model 4
glm_value4 <-  glm(totalvalue ~ city*poly(finsqft, 2) + age + lotsize + bedroom + fullbath, 
                   data=homes)
summary(glm_value4)

# b. use cv.glm() function (from boot)
library(boot) 

# 5-fold cross validation
cv_mse <- cv.glm(homes, glm_value4, K=5)$delta 
cv_mse # the first number is mean MSE
sqrt(cv_mse[1]) # RMSE

# compare to a model with a three-way interaction
# let's call it model 6: TotalValue ~ city*poly(FinSqFt, 2, raw = TRUE)*LotSize + age + Bedroom + FullBath
glm_value6 <-  glm(totalvalue ~ city*poly(finsqft, 2)*lotsize + age + bedroom + fullbath, 
                   data=homes)
cv_mse6 <- cv.glm(homes, glm_value6, K=5)$delta 
sqrt(cv_mse6[1])
# model 4 is better

# setwd("~/Box Sync/AC_homes/modelR/")
# save.image("homes_rmse.Rdata")
