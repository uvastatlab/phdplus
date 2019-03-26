################################################
# Data Science Essentials in R: Linear Models
#
# UVA Library Research Data Services
# March 21, 2019
# Michele Claibourn
# 
# 1. Load libraries, read in data
# 2. Regression illustration on webpage
################################################

# 1. Load libraries, read in data ----

# install.packages("tidyverse")
library(tidyverse)

# Get data from github
github_url <- "https://github.com/uvastatlab/phdplus/raw/master/data/albemarle_homes.rds"
homes <- readRDS(url(github_url))


# 2. Regression illustration on webpage ----
# data frame with square foot bins
sqft_break <- c(0, 250, 500, 750, 1250, 1500, 1750, 2000, 2250, 
                2500, 2750, 3000, 3250, 3500, 3750, 4000, 4250, 
                4500, 4750, 5000, 5250, 5550, 5750, 6000, 6250,
                6500, 6750, 7000, 7250, 7500, 7750, 8000, Inf)
homes_tmp <- homes %>% 
  mutate(sqft_bins = cut(finsqft, breaks = sqft_break)) %>% 
  group_by(sqft_bins) %>% 
  mutate(bin_meany = mean(improvementsvalue),
         bin_medx = max(finsqft)) %>% 
  ungroup()

table(homes_tmp$sqft_bins)
table(round(homes_tmp$bin_medx, 0))

# plot with bins
p <- ggplot(homes_tmp, aes(x = bin_medx, y = improvementsvalue)) 
p + geom_point(alpha = 1/10) 

# plot with conditional means
p + geom_point(alpha = 1/10) + 
  geom_point(aes(x=bin_medx, y=bin_meany), color = "orange", size = 3) 

# plot with line connecting conditional means
p + geom_point(alpha = 1/10) + 
  geom_point(aes(x=bin_medx, y=bin_meany), color = "orange", size = 3) +
  geom_line(aes(x=bin_medx, y=bin_meany), color = "orange")

# plot with regression line
p + geom_point(alpha = 1/10) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_point(aes(x = bin_medx, y = bin_meany), color = "orange", size = 3) +
  geom_line(aes(x = bin_medx, y = bin_meany), color = "orange") 

# bivariate regression model
lm_impvalue <- lm(improvementsvalue ~ bin_medx, data = homes_tmp)

# add predicted values and residuals to df
homes_tmp$predicted <- predict(lm_impvalue)   
homes_tmp$residuals <- residuals(lm_impvalue) 

# plot (sample) with residual lines...
# sample 500 observations
set.seed(121)
homes_tmp_samp <- homes_tmp %>% sample_n(size = 500)

# plot
ggplot(homes_tmp_samp, aes(x = bin_medx, y = improvementsvalue)) +
  geom_point(aes(color = residuals)) +
  geom_line(aes(y = predicted)) +
  geom_segment(aes(xend = bin_medx, yend = predicted)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE)

