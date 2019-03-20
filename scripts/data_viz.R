# Exploratory data visualization with ggplot2
# phdplus - Spring 2019
# Clay Ford: clayford@virginia.edu


# Helpful RStudio commands ------------------------------------------------

# Description       Windows & Linux       Mac 
#
# Run current line  Ctrl+Enter            Command+Enter
# Previous plot 	  Ctrl+Alt+F11 	        Command+Option+F11
#                   Ctrl+Shift+PageUp
# Next plot 	      Ctrl+Alt+F12 	        Command+Option+F12
#                   Ctrl+Shift+PageDown

# Packages ----------------------------------------------------------------

library(tidyverse)
library(scales)


# Data --------------------------------------------------------------------

# Albemarle County real estate data.

# Downloaded from Office of Geographic Data Services, 26-Sept-2018

# See Michele Claibourn's workshop for how data were obtained and prepared:
# https://uvastatlab.github.io/phdplus/dataprep.html

# Read in the data:

# The readRDS() function allows us to read in an R object that was saved with saveRds().
# saveRDS(homes, file = "albemarle_homes.rds")

github_url <- "https://github.com/uvastatlab/phdplus/raw/master/data/albemarle_homes.rds"
homes <- readRDS(file = url(github_url))

# Let's review what we have.
str(homes) 
names(homes)
summary(homes)

# levels of city
levels(homes$city)

# Today we'll look at homes in the top 6 cities by number of homes
vars <- c("CHARLOTTESVILLE", "CROZET", "EARLYSVILLE", 
          "KESWICK", "SCOTTSVILLE", "NORTH GARDEN")

homes <- homes %>% 
  filter(city %in% vars)

# drop the empty city levels
levels(homes$city)
table(homes$city)
homes$city <- droplevels(homes$city)
  

# One Variable - Continuous -----------------------------------------------


# The histogram helps us see how a numeric variable is distributed
ggplot(homes, aes(x=finsqft)) + geom_histogram()

# note the message regarding "stat_bin".
# "stat_bin" is the stat for geom_histogram.

# Says the documentation: "You should always override this value, exploring
# multiple widths to find the best way to illustrate the stories in your data."

# Try a new binwidth or bins.
# bins: number of bins into which data is assigned
# binwidth: width of the bins (eg, 0-100, 101-200, etc)
ggplot(homes, aes(x=finsqft)) + geom_histogram(bins = 40)
ggplot(homes, aes(x=finsqft)) + geom_histogram(binwidth=250)
# try some others! Beware, setting small binwidths can tax your computer if you
# have a lot of data

# A frequency polygon is like a histogram, but counts are presented with lines
# instead of bars.
ggplot(homes, aes(x=finsqft)) + geom_freqpoly(bins = 40)
ggplot(homes, aes(x=finsqft)) + geom_freqpoly(binwidth = 250)

# We can map city to color to get freq polygon for all cities
ggplot(homes, aes(x=finsqft, color = city)) + geom_freqpoly(bins = 40)


# To create a "true" histogram with density instead of count, (ie, the area
# under the histogram is 1), set y = stat(density) in the aes() function. By
# default, y = stat(count)
ggplot(homes, aes(x=finsqft, y=stat(density))) + 
  geom_histogram(binwidth=250)
ggplot(homes, aes(x=finsqft, y=stat(density))) + 
  geom_freqpoly(binwidth=250)


# The reason we might want a "True" histogram is to compare distributions with
# different counts.
ggplot(homes, aes(x=finsqft, y=stat(density), color = city)) + 
  geom_freqpoly(binwidth=500) 

# facet by city
ggplot(homes, aes(x=finsqft, y = stat(density))) +
  geom_histogram(binwidth = 200) +
  facet_wrap(~ city)

# facet by city and zoom in on x-axis
ggplot(homes, aes(x=finsqft, y = stat(density))) +
  geom_histogram(binwidth = 200) +
  facet_wrap(~ city) +
  coord_cartesian(xlim = c(0,5000))

# see also geom_density for smooth estimates of a distribution


# YOUR TURN #1 ------------------------------------------------------------

# (1) Create a "true" histogram of totalvalue, the total value of the home. That
# is, set y = stat(density). Set bins = 150.


# (2) repeat #1 but now also facet by zip


# (3) repeat #3 but now also zoom in on x-axis from $0 to $1,000,000




# One variable - Discrete -------------------------------------------------


# Bar Plots help us visualize how discrete values are distributed. Map x to a
# categorical or discrete value.
ggplot(homes, aes(x=condition)) + geom_bar()
ggplot(homes, aes(x=bedroom)) + geom_bar()

# Notice the geom_bar() function did the counting for us. If we already had a
# data frame with counts, we could use geom_col() with a y aesthetic.

# Example: create a data frame of condition counts
hc <- homes %>% 
  group_by(condition) %>% 
  count()
hc

# want the heights of the bars to represent values in the data
ggplot(hc, aes(x = condition, y = n)) + geom_col()

# A nice benefit of a data frame with counts is that we can sort the categories
# on the x-axis using reorder()
ggplot(hc, aes(x = reorder(condition, n), y = n)) + geom_col()

# Or the other way...
ggplot(hc, aes(x = reorder(condition, -n), y = n)) + geom_col()

# The x-axis now has a funny label. We can fix with labs()
ggplot(hc, aes(x=reorder(condition, -n), y = n)) + geom_col() +
  labs(x = "Condition")

# back to the homes data frame...

# the coord_flip() function allows us to flip the coordinate axis; 
ggplot(homes, aes(x=condition)) + 
  geom_bar() +
  coord_flip() 

# Let's map the fp indicator to fill to get counts of homes with and without
# fireplaces by condition. Notice we had to wrap fp in factor()
ggplot(homes, aes(x=condition, fill = factor(fp))) + geom_bar()

# Note the "stack" position. We can change that by setting position = "dodge"
# in geom_bar():
ggplot(homes, aes(x=condition, fill=factor(fp))) + 
  geom_bar(position = "dodge")

# Setting position = "fill" shows relative proportions at each category
ggplot(homes, aes(x=condition, fill=factor(fp))) + 
  geom_bar(position = "fill")

# what are those proportions exactly?
table(homes$fp, homes$condition) %>% 
  prop.table(margin = 2) %>%           # margin = 2 means "column proportions"
  round(2)

# What if we wanted the plot displayed in order of proportion? It's tricky.
# Here's how I did it but there may be a better way.

# Get character vector of condition levels in ascending order of proportion for
# fp==1
nl <- table(homes$fp, homes$condition, dnn = c("fp","condition")) %>% 
  prop.table(margin = 2) %>%           
  round(2) %>% 
  as.data.frame(responseName = "prop") %>% 
  arrange(fp, prop) %>% 
  filter(fp == 1) %>% 
  pull(condition) %>%    # pull out the condition vector
  as.character()         # convert to character

# Then use forcats:fct_relevel in ggplot to relevel condition by the nl vector
ggplot(homes, aes(x=fct_relevel(condition, nl), fill=factor(fp))) + 
  geom_bar(position = "fill")


# How can we fix the legend title?
# Recall that is a by-product of the fill scale. So we need to use a scale
# function.
ggplot(homes, aes(x=fct_relevel(condition, nl), fill=factor(fp))) + 
  geom_bar(position = "fill") +
  scale_fill_discrete(name="Fireplaces", labels = c("None","At least one"))

# We could also modify the data
# homes <- homes %>%
#   mutate(fp = factor(fp, labels = c("None","At least one")))

# An example with manually defined colors
ggplot(homes, aes(x=fct_relevel(condition, nl), fill=factor(fp))) + 
  geom_bar(position = "fill") +
  scale_fill_manual("Fireplaces", values = c("Blue","Red"))

ggplot(homes, aes(x=fct_relevel(condition, nl), fill=factor(fp))) + 
  geom_bar(position = "fill") +
  scale_fill_manual("Fireplaces", values = c("grey80","grey40"))

# enter colors() at console to see all available colors
# run demo("colors") to see all colors
# see Appendices for defining your own color palettes

# YOUR TURN #2 ------------------------------------------------------------

# (1) Create a barplot of counts for zip. Tip: make zip a factor!



# (2) Create a stacked proportional bar chart (ie, set position = "fill") for
# zip using cooling as the fill variable.



# (3) The following code attempts to show a proportional bar plot of number of
# bedrooms (1-5) by city, but it doesn't work. Can you fix it?
ggplot(filter(homes, bedroom %in% 1:5), 
       aes(x=city, fill = factor(bedroom))) + 
  geom_bar(position = fill) +
  scale_fill_discrete("Bedrooms")



# Two numeric variables ---------------------------------------------------

# The scatterplot allows you to visualize the relationship between two
# numeric variables.

# plot finsqft vs totalvalue:
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point()

# Lots of overplotting!
# What can we do about that? One solution is the alpha aesthetic. "alpha=1/10"
# means 10 points overplotted adds to a solid color
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(alpha=1/10)

# Can also try smaller points using the shape aesthetic
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(shape=".")
# open circles ala Base R
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(shape = 1)

# see ?points for shape codes

# geom_density2d() can also be helpful as can zooming in on data
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(shape = 1) +
  geom_density2d() +
  coord_cartesian(xlim = c(0, 3000), ylim = c(0, 1e6))


# Another approach is to use facets: break the data into subsets 
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point() +
  facet_wrap(~ city)

# We can also zoom in on plot
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(shape = 1) +
  facet_wrap(~ city) +
  coord_cartesian(xlim = c(0,3000),ylim = c(0,1e6))

# We can also map the color of points to a variable in our data.
ggplot(homes, aes(x=finsqft, y=totalvalue, color=cooling)) + 
  geom_point(shape = 1) +
  facet_wrap(~ city) +
  coord_cartesian(xlim = c(0,3000),ylim = c(0,1e6))



# YOUR TURN #3 ------------------------------------------------------------

# (1) Plot age vs. finsqft with geom_point(). Put finsqft on the y axis. 



# (2) Repeat 1 but now also zoom in on the x and y axis. Look at the last 100
# years for houses with less than 5000 finsqft. Also, set shape = 1.



# (3) Repeat 2 but now also map color of points to fp




# Saving ggplot objects ---------------------------------------------------

# ggplot2 objects can be saved in our global environment
p <- ggplot(homes, aes(x=finsqft, y=totalvalue)) +
  geom_point()

# nothing is plotted. To see plot, call p
p


# We can work with ggplot objects as if they were ggplot2 code
p + coord_cartesian(xlim = c(0,2000), ylim = c(0,5e5))
p + facet_wrap(~condition)

# This can also help us save some typing. For example, instead of always typing
# ggplot(homes) we could do this:
h <- ggplot(homes)

# Now just use h instead of ggplot(homes) and call aes() by itself
h + aes(x=finsqft, y=totalvalue) + 
  geom_point()

h + aes(x = condition) + 
  geom_bar()

rm(h, p)

# Scales and smooths ------------------------------------------------------


# Let's look again at totalvalue vs finsqft:
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point() +
  facet_wrap(~ city)

# The y-axis scale would look better formatted as dollar amounts. The scales
# package can help with this. It has functions designed for this type of
# situation. When you install ggplot2, scales is installed as well.

# Three functions that are very handy: percent, comma, dollar;
# typically used in a scale function with the labels argument.

p2 <- ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(alpha=1/6) +
  facet_wrap(~ city) + 
  scale_y_continuous(labels=dollar) + 
  scale_x_continuous(labels=comma)
p2

# We can add a smooth line through our scatterplots with geom_smooth()
p2 + geom_smooth()

# We can fit a straight linear regression line setting method="lm"
p2 + geom_smooth(method="lm")

# We can add both smooth and straight lines
p2 + 
  geom_smooth(se=FALSE, color="red") +
  geom_smooth(method="lm", se=FALSE)

# Worth mentioning we can plot smooth lines without data 
ggplot(homes, aes(x = finsqft, y = totalvalue)) +
  geom_smooth() +
  facet_wrap(~city)

# Let's zoom in on the plots to get a closer look at what's going on in the 
# lower left corner. We can zoom in using coord_cartesian(). Below we set the 
# x-axis limits between 0 and 5000, and the y-axis limits between 0 and
# 1,000,000.
p2 + 
  geom_smooth(se=FALSE, color="red") +
  geom_smooth(method="lm", se=FALSE) +
  coord_cartesian(xlim = c(0, 5000), ylim = c(1,1e6))

# To add a title we can use labs()
p2 + 
  geom_smooth(se=FALSE, color="red") +
  geom_smooth(method="lm", se=FALSE) +
  coord_cartesian(xlim = c(0, 5000), ylim = c(1,1e6)) +
  labs(title = "Total Values vs Finished Sq Ft by city") 

# Notice the title is left-aligned by default. Here's how we can center it using
# the theme() function.
p2 + 
  geom_smooth(se=FALSE, color="red") +
  geom_smooth(method="lm", se=FALSE) +
  coord_cartesian(xlim = c(0, 5000), ylim = c(1,1e6)) +
  labs(title = "Total Values vs Finished Sq Ft by city") +
  theme(plot.title = element_text(hjust = 0.5))


# YOUR TURN #4 ------------------------------------------------------------

# Plot totalvalue vs lotsize, with totalvalue on y axis. 
# - add a smooth
# - zoom in on plot: x-axis (0, 10) y-axis (0, 1e6)
# - Fix the y-axis to show the amount in dollars. 






# Boxplots and stripcharts ------------------------------------------------


# Boxplots are good for visualizing a numeric variable conditional on a
# categorical variable. Let's look at totalvalue by number of fullbaths:
ggplot(homes, aes(x=fullbath, y=totalvalue)) + geom_boxplot()

# Not what we wanted! fullbath is not categorical; can fix by setting group =
# fullbath
ggplot(homes, aes(x = fullbath, y = totalvalue, group = fullbath)) + 
  geom_boxplot()

# and so does this (convert fullbath to factor):
ggplot(homes, aes(x=factor(fullbath), y=totalvalue)) + 
  geom_boxplot() + 
  scale_y_continuous(labels=dollar)


# The two most expensive homes have only 2 full baths
homes %>% 
  filter(totalvalue > 9e6) %>% 
  View()

# The expensive homes have rendered this boxplot practically useless. Another
# option is to create a stripchart, which is basically a scatterplot in one
# dimension. We can do that with geom_jitter() which is geom_point() but with
# random noise added to the coordinates. Below we jitter left and right, but not
# up and down.
ggplot(homes, aes(x=fullbath, y=totalvalue, group = fullbath)) + 
  geom_jitter(width = 0.2, height = 0, alpha = 1/5) + 
  scale_y_continuous(labels=dollar) 

# zoom in 
ggplot(homes, aes(x=fullbath, y=totalvalue, group = fullbath)) + 
  geom_jitter(width = 0.2, height = 0, alpha = 1/5) + 
  scale_y_continuous(labels=dollar) +
  coord_cartesian(ylim = c(0,1e6), xlim = c(0,5))

# investigate the line of homes with 4 fullbaths less than $250,000
filter(homes, fullbath == 4 & totalvalue < 250000) %>% 
  arrange(totalvalue) %>% 
  mutate(diff = c(NA, diff(totalvalue))) %>%  
  filter(diff == 0) %>% 
  View()


# Another stripchart of fireplaces by finished square feet:
ggplot(homes, aes(x = finsqft, y = fp)) +
  geom_jitter(width = 0, height = 0.05, shape = ".")

# Once a house is over 2500 finsqft in size, there appears to be a high
# probability it will have a fireplace. We can add a smooth line to represent
# this probability. Use method = "glm" and then pass family = binomial to
# method.args as a list.
ggplot(homes, aes(x = finsqft, y = fp)) +
  geom_jitter(width = 0, height = 0.05, shape = ".") +
  geom_smooth(method = "glm", method.args = list(family = binomial))


# YOUR TURN #5 ------------------------------------------------------------


# (1) Make a boxplot totalvalue by zip. Tip: use factor(zip)



# (2) Make a stripchart of totalvalue by zip. Tip: use factor(zip)
# - in geom_jitter() set width = 0.4 and alpha = 1/5
# - format y axis as dollar and set limits to 0 - $1,000,000




# Plotting two discrete integer variables ---------------------------------


# Bedroom vs. fullbath
ggplot(homes, aes(x=bedroom,y=fullbath)) + geom_point()

# geom_jitter() can help with this
ggplot(homes, aes(x=bedroom,y=fullbath)) + geom_jitter()

# scales could be better; minor_breaks=NULL turns off the minor grid lines 
ggplot(homes, aes(x=bedroom,y=fullbath)) + 
  geom_jitter(alpha=1/5) +
  scale_x_continuous(breaks=0:20, minor_breaks=NULL) +
  scale_y_continuous(breaks=0:14, minor_breaks=NULL)



# Line Graphs -------------------------------------------------------------


# line graphs are nice for connecting dots and showing a trend over time.

# plot number of houses built per year; 
# need to count up number of homes by yearbuilt
years <- homes %>% 
  filter(yearbuilt > 0) %>% 
  group_by(yearbuilt) %>% 
  count()
years


# now use geom_line()
ggplot(years, aes(x=yearbuilt, y=n)) + 
  geom_line()

# Let's change the x-axis to show years in increments of 50 years
ggplot(years, aes(x=yearbuilt, y=n)) + 
  geom_line() +
  scale_x_continuous(breaks=seq(1700,2000,50)) +
  labs(x="Year", y="Number of Homes")


# what years were the spikes?
filter(years, n > 750)

# Using ggplotly from the plotly package. The ggplotly() function makes a ggplot
# interactive. The plotly::ggplotly syntax allows us to use ggplotly without
# loading the plotly package.
plotly::ggplotly(ggplot(years, aes(x=yearbuilt, y=n)) + 
           geom_line() +
           scale_x_continuous(breaks=seq(1700,2000,50)) +
           labs(x="Year", y="Number of Homes"))


# YOUR TURN #6 ------------------------------------------------------------

# The following counts number of remodeled homes by year.
years2 <- homes %>%
  filter(yearremodeled > 2) %>%  # why?
  group_by(yearremodeled) %>% 
  count() 
years2

# (1) create a line plot using the years2 dataframe that shows number of
# remodels by year.



# (2) What year saw a spike in homes remodeled?



# (3) What year did the big decline begin?




# Plotting with two data frames -------------------------------------------

# ggplot allows us to use multiple data frames when creating a plot. This can be
# helpful for combining raw and summary data.

# For example, consider the following plot: totalvalue vs yearbuilt for the
# years 1950 - 2018 for homes valued at $1,000,000 or less. Notice we are zoomed
# in.

ggplot(homes, aes(x=yearbuilt, y=totalvalue)) + 
  geom_point(position=position_jitter(w=0.2,h=0), shape=".") +
  coord_cartesian(xlim=c(1950,2018), ylim=c(0,1e6))

# Let's say we wanted to plot the median totalvalue by year on top of the raw
# data. First we need to calculate the median by year. Here's one way to do it
# using dplyr:

homeValues <- homes %>% 
  group_by(yearbuilt) %>% 
  summarize(median_totalvalue = median(totalvalue, na.rm=TRUE))
homeValues

# Each geom_xxx() function can use a separate data frame and mappings. Below
# notice the geom_line() function references the homeValues data frame and
# provides a different mapping to the y aesthetic. Also notice we have swap the
# order of data and aesthesics.

ggplot(homes, aes(x=yearbuilt, y=totalvalue)) + 
  geom_point(position=position_jitter(w=0.2,h=0), shape=".") +
  geom_line(aes(y = median_totalvalue), homeValues, color = "red", size = 1) +
  coord_cartesian(xlim=c(1950,2016), ylim=c(0,1e6))

# Another example

# stripchart of totalvalue by zip
ggplot(homes, aes(x = factor(zip), y = totalvalue)) +
  geom_jitter(width = 0.3, height = 0, shape = 1, color = "grey80") +
  coord_cartesian(ylim = c(0,1e6))

# Let's plot the 25th, 50th and 90th percentiles on top of this data. First we
# need to calculate the percentiles and create a "tidy" (ie, long) data frame.
pDF <- homes %>% 
  group_by(zip) %>% 
  summarize(p25 = quantile(totalvalue, probs = 0.25),
            p50 = quantile(totalvalue, probs = 0.50),
            p90 = quantile(totalvalue, probs = 0.90)) %>% 
  gather(key = "percentile", value = "totalvalue", -zip)

# Now add geom_point() and geom_line() to our previous plot using the pDF data
# frame. Notice we can keep the original x and y aesthetics. We just need to
# define color and group.
ggplot(homes, aes(x = factor(zip), y = totalvalue)) +
  geom_jitter(width = 0.3, height = 0, shape = 1, color = "grey80") +
  geom_point(aes(color = percentile), pDF) +
  geom_line(aes(color = percentile, group = percentile), pDF) +
  coord_cartesian(ylim = c(0,1e6)) 

# Reverse order of legend using guides function
ggplot(homes, aes(x = factor(zip), y = totalvalue)) +
  geom_jitter(width = 0.3, height = 0, shape = 1, color = "grey80") +
  geom_point(aes(color = percentile), pDF) +
  geom_line(aes(color = percentile, group = percentile), pDF) +
  coord_cartesian(ylim = c(0,1e6)) +
  guides(color = guide_legend(reverse = TRUE))




# Saving images -----------------------------------------------------------

# Let's say we want to save the following plot that shows count of home sales by
# month:
library(lubridate)
# create month variable
homes <- homes %>% mutate(lastsaledateM = month(lastsaledate, label = TRUE),
                          lastsaledateD = wday(lastsaledate, label = TRUE))

homes %>% 
  select(lastsaledate, lastsaledateM, lastsaledateD) %>%
  head()

ggplot(homes, aes(x = lastsaledateM)) + 
  geom_bar() +
  labs(x = "Month of Sale")

# Click the export button in the plot pane to manually save an image with a
# dialog.

# We can also use ggsave. By default it will save the last plot created to your
# working directory. File type is determined by file extension you provide.
ggsave(filename = "barPlot.jpg")

# Can specify size using height, width and unit arguments
ggsave(filename = "barPlot.jpg", height = 5, width = 5, units = "in")
ggsave(filename = "barPlot.jpg", height = 15, width = 15, units = "cm")

# delete file from computer using link() function
unlink("barPlot.jpg")

# Can save plot objects and then save to file

# Example
pM <- ggplot(homes, aes(x = lastsaledateM)) + 
  geom_bar() +
  labs(x = "Month of Sale")
pD <- ggplot(homes, aes(x = lastsaledateD)) + 
  geom_bar() +
  labs(x = "Day of Sale")
pM
pD

ggsave("barPlotM.jpg", pM)
ggsave("barPlotD.jpg", pD)

# delete files
unlink(x = c("barPlotM.jpg", "barPlotD.jpg"))




# Appendix: Multiple plots in one window ----------------------------------

# In base R, we usually use par(mfrow=c(i,j)), like so:
par(mfrow=c(1,2))
hist(homes$finsqft)
plot(totalvalue ~ finsqft, data=homes)
par(mfrow=c(1,1))

# We cannot use this approach for ggplot2. The easiest solution is to use the 
# grid.arrange() function in the gridExtra package. To use it, you have to save
# your plots and then call them using grid.arrange().

# install.packages("gridExtra")
library(gridExtra)
p1 <- ggplot(homes, aes(x=finsqft)) + geom_histogram() 
p2 <- ggplot(homes, aes(x=finsqft, y=totalvalue)) + geom_point()
grid.arrange(p1, p2, nrow=1) # can also use ncol




# Appendix: Themes --------------------------------------------------------


# Using built-in themes such as theme_bw()
# Before
ggplot(homes, aes(x=finsqft)) + geom_histogram(bins = 40)
# After
ggplot(homes, aes(x=finsqft)) + geom_histogram(bins = 40) + 
  theme_bw()
# another: theme_light
ggplot(homes, aes(x=finsqft)) + geom_histogram(bins = 40) + 
  theme_light()

# To permanently change the theme, use theme_set:
prevTheme <- theme_set(theme_bw())

ggplot(homes, aes(x=finsqft)) + geom_histogram(bins = 40)

ggplot(homes, aes(x=city, y = finsqft)) + geom_boxplot()


# To restore
theme_set(prevTheme)

# verify
ggplot(homes, aes(x=city, y = finsqft)) + geom_boxplot()

# see package ggthemes for more themes!
# install.packages("ggthemes")
library(ggthemes)

# Wall Street Journal theme
ggplot(homes, aes(x=city, y = finsqft)) + 
  geom_boxplot() + 
  theme_wsj()

# fivethirtyeight.com theme
ggplot(homes, aes(x=city, y = finsqft)) + 
  geom_boxplot() + 
  theme_fivethirtyeight()

# Tufte's theme (based on The Visual Display of Quantitative Information)
ggplot(homes, aes(x=city, y = finsqft)) + 
  geom_boxplot() + 
  theme_tufte()

# base R theme
ggplot(homes, aes(x=city, y = finsqft)) + 
  geom_boxplot() + 
  theme_base()



# Appendix: Labelling facets ----------------------------------------------


# We can use the labeller argument and associated labeller functions to modify
# the labels of facets.

# Let's create a 3 x 6 grid of finsqft vs Total Value by city and Condition.
# First we'll subset the data.
homes2 <- subset(homes, totalvalue < 1e6 & condition %in% c("Average","Good","Excellent"))

ggplot(homes2, aes(x = finsqft, y = totalvalue)) +
  geom_point(alpha = 1/5) +
  facet_grid(condition ~ city)

# labeller = label_both adds the variable name and value
ggplot(homes2, aes(x = finsqft, y = totalvalue)) +
  geom_point(alpha = 1/5) +
  facet_grid(condition ~ city, labeller = label_both)

# We use the labeller function with the labeller argument to control labelling
# for each facet. For example, wrap city name after 10 characters and show both
# variable and value label for Condition.
ggplot(homes2, aes(x = finsqft, y = totalvalue)) +
  geom_point(alpha = 1/5) +
  facet_grid(condition ~ city, labeller = labeller(city = label_wrap_gen(10),
                                                   condition = label_both))


# Appendix: Using RColorBrewer for color palettes -------------------------


# The RColorBrewer package provides color schemes for maps (and other graphics)
# designed by Cynthia Brewer as described at http://colorbrewer2.org

# ggplot2 provides functions for easily incorporating these color schemes into
# your plots.

# There are three types of color schemes:

# 1. sequential - suited to ordered data that progress from low to high

# 2. qualitative - suited for nominal or categorical data

# 3. diverging -  put equal emphasis on mid-range critical values and extremes
#                 at both ends of the data range

# See all color schemes
# sequential, qualitative, diverging
# Use the names on the left of the colors to specify a color scheme
# install.packages("RColorBrewer")
library(RColorBrewer)
# sequential color palettes
display.brewer.all(type = "seq")
# qualitative color palettes
display.brewer.all(type = "qual")
# divergent color palettes
display.brewer.all(type = "div")

# To modify color or fill aesthetic, use scale_color_brewer or
# scale_fill_brewer, respectively, and specify the palette

# Example 1: sequential color scheme using Blues palette
# View the palette (optional)
display.brewer.all(type = "seq", select = "Blues")
# Use the palette in a plot
ggplot(subset(homes, totalvalue < 1e6), 
       aes(x = totalvalue, fill = condition)) + 
  geom_histogram() +
  scale_fill_brewer(palette = "Blues")

# Example 2: qualitative color scheme using Accent palette
# View the palette (optional)
display.brewer.all(type = "qual", select = "Accent")
# Use the palette in a plot
ggplot(subset(homes, totalvalue < 1e6 & yearbuilt > 0), 
       aes(x = yearbuilt, fill = city)) + 
  geom_histogram() +
  scale_fill_brewer(palette = "Accent")

# Example 3: diverging color scheme using PiYG palette
# View the palette (optional)
display.brewer.all(type = "div", select = "PiYG")
# Create a divergent variable: scaled finsqft
# first scale then cut into categories
homes <- mutate(homes, finsqftZ = scale(finsqft)[,1]) %>% 
  mutate(finsqftZ = cut(finsqftZ, quantile(finsqftZ,
                                           probs = seq(0, 1, length.out = 6)),
                        labels = c("Very Small", "Below Average", "Average", "Above Average", "Very Large"),
                        include.lowest = TRUE))
# Use the palette in a plot
ggplot(subset(homes, totalvalue < 1e6), 
       aes(x = city, fill = finsqftZ)) +
  geom_bar(position = "fill") +
  scale_fill_brewer("Size of House", type = "div", palette = "PiYG")


# Appendix: defining your own color palette -------------------------------

# There are many ways to define your own color palettes in R. Here are a few
# relatively easy methods.

# Below we use HEX values, but you can also use color names. Enter colors() at
# the console to see all 657 available.

# Example 1: Using scale_color_manual with specific values for a categorical
# variable
ggplot(subset(homes, totalvalue < 1e6 & 
                condition %in% c("Average","Good","Excellent","Fair")), 
       aes(x = finsqft, y = totalvalue, color = condition)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  # 2016 color palettes: https://www.w3schools.com/colors/colors_palettes.asp
  scale_color_manual(values = c("#92a8d1", "#034f84", "#f7cac9", "#f7786b"))

# Example 2: Using scale_color_gradientn with continuous data to create a custom
# 4-color gradient.
ggplot(filter(homes, totalvalue < 1e6 & 
                condition %in% c("Average","Good","Excellent","Fair")), 
       aes(x = condition, y = totalvalue, color = finsqft)) +
  geom_jitter(height = 0) +
  scale_y_log10(labels = scales::dollar) +
  # 2016 color palettes: https://www.w3schools.com/colors/colors_palettes.asp
  scale_color_gradientn(colours = c("#92a8d1", "#034f84", "#f7cac9", "#f7786b"))

# Example 3: Using scale_color_gradient with continuous data to create a custom
# 2-color, low-high gradient
ggplot(filter(homes, totalvalue < 1e6 & 
                condition %in% c("Average","Good","Excellent","Fair")), 
       aes(x = condition, y = totalvalue, color = finsqft)) +
  geom_jitter(height = 0) +
  scale_y_log10(labels = scales::dollar) +
  # 2016 color palettes: https://www.w3schools.com/colors/colors_palettes.asp
  scale_color_gradient(low = "#d5f4e6", high = "#618685")


# Example 4: Using scale_color_gradient2 with continuous data to create a custom
# diverging color gradient. Note we scale finsqft so average is 0.
ggplot(filter(homes, totalvalue < 1e6 & 
                condition %in% c("Average","Good","Excellent","Fair")), 
       aes(x = condition, y = totalvalue, color = scale(finsqft))) +
  geom_jitter(height = 0) +
  scale_y_log10(labels = scales::dollar) +
  # 2016 color palettes: https://www.w3schools.com/colors/colors_palettes.asp
  scale_color_gradient2("Scaled\nfinsqft", low = "#4040a1", mid = "white", high = "#bc5a45")


# Appendix: adding uncertainty to graphs ----------------------------------

# Let's sample 100 homes from each city.
set.seed(1)
homes_sample <- homes %>% 
  group_by(city) %>% 
  sample_n(100)

# Now let's calculate the mean totalvalue for each city and the respective
# standard error.
sDF <- homes_sample %>% 
  group_by(city) %>% 
  summarize(mean = mean(totalvalue),
            se = sd(totalvalue)/sqrt(length(totalvalue)))
sDF

# Now plot the mean with +/- 2*SE bars using geom_errorbar. Notice we have to set
# the ylim and ymax aesthetics.
ggplot(sDF, aes(x = city, y = mean, 
                ymin = mean - 2*se, 
                ymax = mean + 2*se)) +
  geom_point() +
  geom_errorbar() +
  labs(title = "mean with 2*SE bars")

# The stat_summary function allows us to quickly plot error bars without having
# to make any calculations. Notice we have to call it twice: once for the mean,
# and again for the confidence intervals.
ggplot(homes_sample, aes(x=city, y=totalvalue)) + 
  stat_summary(fun.data = "mean_cl_normal", color="red", geom="errorbar", width=0.1) +
  stat_summary(fun.y = "mean", geom="point", color="red", size=3) +
  labs(title="Mean totalvalue by city with 95% error bars")

# The same confidence intervals plotted with the data.
ggplot(homes_sample, aes(x=city, y=totalvalue)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.data = "mean_cl_normal", color="red", geom="errorbar", width=0.1) +
  stat_summary(fun.y = "mean", geom="point", color="red", size=3) +
  labs(title="Mean totalvalue by city with 95% error bars")

# Another approach is to use bootstrapped confidence intervals. Bootstrapping
# means resampling the data many times (with replacement) and calculating the
# mean each time. And then using the many bootstrapped means to estimate the
# standard error. Setting fun.data = "mean_cl_boot" does this for us. 
ggplot(homes_sample, aes(x=city, y=totalvalue)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.data = "mean_cl_boot", color="red", geom="errorbar", width=0.1) +
  stat_summary(fun.y = "mean", geom="point", color="red", size=3) +
  labs(title="Mean totalvalue by city with 95% error bars")

# Due to the skewness of totalvalue, the median would be a better measure of
# center. Setting fun.data = "median_hilow" returns the 0.025 and 0.97 quantiles
# of the data (instead of +/- 1.96*SE). Notice the error bars are now skew and
# extend into the upper ranges of totalvalue. But they're NOT true error bars
# for the median. They're just summary statistics.
ggplot(homes_sample, aes(x=city, y=totalvalue)) + 
  geom_point(position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.data = "median_hilow", color="red", geom="errorbar", width=0.1) +
  stat_summary(fun.y = "median", geom="point", color="red", size=3) +
  labs(title="Mean totalvalue by city with bootstrapped 95% error bars")

# We could also bootstrap the median and then use the 0.025 and 0.97 quantiles
# of the bootstrapped medians to create our confidence intervals. We need to do
# this ourselves to get a data frame to plot with ggplot.

# Load the boot package; comes with R
library(boot)

# split data by city; returns a list
cities <- split(homes_sample$totalvalue, homes_sample$city)

# write a function that bootstraps data (totalvalue) and returns the median and
# 95% percentile confidence intervals.
boot_median_ci <- function(data){
  b.out <- boot(data, function(x,i)median(x[i]), R = 100)
  ci.out <- boot.ci(b.out, type = "perc")
  data.frame(median = b.out$t0, lower = ci.out$percent[4], upper = ci.out$percent[5])
}

# map (or apply) the function to the cities list (do it for each city)
median_ci <- map(cities, boot_median_ci)

# collapse the result into a single data frame
median_DF <- bind_rows(median_ci, .id = "city")

# generate the plot
ggplot(median_DF, aes(x = city, y = median)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_y_continuous(labels = scales::dollar)

# generate the plot with the original data
ggplot(median_DF, aes(x = city, y = median)) +
  geom_point(size = 3, color = "red") +
  geom_jitter(data = homes_sample, mapping = aes(y = totalvalue), 
              width = 0.1, height = 0, alpha = 0.3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "red") +
  scale_y_continuous(labels = scales::dollar)


# Appendix: Log Transformations in scatter plots --------------------------



# Skewed positive data are often log transformed. It helps "squeeze together"
# the large values and "spread out" the small values.

# before log transformation:
ggplot(homes, aes(x=finsqft, y=totalvalue)) + geom_point()

# Maybe try log base 10 transformation directly:
ggplot(homes, aes(x=log10(finsqft), y=log10(totalvalue))) + geom_point(alpha = 1/5)

# The scale of the axes is on the log10 scale because the data has been
# transformed.

# We can use scale functions to both transform the data and map the scale of the
# axes to the original data.
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(alpha=1/5) +
  scale_x_log10(labels=comma) + 
  scale_y_log10(labels=dollar)

# We can see the non-linear nature of the scales by manually defining the breaks
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(alpha=1/5) +
  scale_x_log10(labels=comma, breaks = seq(0, 1e4, 2e3)) +  # 0 to 10,000 in steps of 2,000
  scale_y_log10(labels=dollar, breaks = seq(0, 10e6, 2e6))  # 0 to 10,000,000 in steps of 2,000,000

# To do the same with the natural log scale, we can use the log_trans() function
# from the scales package. I prepended scales:: to the function to indicate the
# function is from the scales package, though technically we don't need it since
# we loaded the scales package at the top of the script.
ggplot(homes, aes(x=finsqft, y=totalvalue)) + 
  geom_point(alpha=1/5) +
  scale_x_continuous(labels=comma, trans = scales::log_trans()) + 
  scale_y_continuous(labels=dollar, trans = scales::log_trans())




# Appendix: plotting dates and date-times ---------------------------------

# The scale_*_date and scale_*_datetime functions provide extra control over
# scales that involve dates and date-times.

# Read in some stock price data and format date column
library(lubridate)
stock_prices <- read_csv("http://people.virginia.edu/~jcf2d/data/foxa.csv")
stock_prices <- stock_prices %>% mutate(Date = dmy(Date))

# default plot of closing price over time
ggplot(stock_prices, aes(x = Date, y = Close)) + geom_line()

# Examples of using scale_x_date to change scale of x axis

# Every 2 months
ggplot(stock_prices, aes(x = Date, y = Close)) + 
  geom_line() +
  scale_x_date(date_breaks = "2 months")

# Every 2 months with formatted labels as Month Year
# see ?strptime for date codes such as %B and %Y
ggplot(stock_prices, aes(x = Date, y = Close)) + 
  geom_line() +
  scale_x_date(date_breaks = "2 months", 
               date_labels = "%B %Y")

# Every 6 weeks with minor breaks set to 1 week and labels formatted as
# Abbreviated Month
ggplot(stock_prices, aes(x = Date, y = Close)) + 
  geom_line() +
  scale_x_date(date_breaks = "6 weeks", 
               minor_breaks = "1 week", 
               date_labels = "%b")



# Appendix: GGally --------------------------------------------------------

# 'GGally' extends 'ggplot2' by adding several functions, including a pairwise
# plot matrix. Here's a quick demo.

# install.packages("GGally")
library(GGally)

# Learn more: http://ggobi.github.io/ggally/
# Beware: these functions can be slow

# the ggpairs function produces a pairwise comparison of multivariate data. 
homes %>% select(finsqft, lotsize, totalvalue, bedroom, fp) %>% ggpairs()

# ggscatmat is similar to ggpairs but only works for purely numeric multivariate
# data.
homes %>% select(finsqft, lotsize, totalvalue, bedroom, fp) %>% 
  ggscatmat(columns = 1:3, color = "fp")



# Appendix: gganimate -----------------------------------------------------

# install.packages("gganimate")
# install.packages("gifski")
library(gganimate)

# The gganimate package allows you to create animations in ggplot. Below is a
# simple example. We first sample 1000 records from the homes data. Next we
# create a plot of finsqft versus totalvalue and save it as "p".

samp <- homes %>% sample_n(1000) 
p <- ggplot(samp, aes(x = finsqft, y = totalvalue, color = city)) +
  geom_point()

# Now to create the animation. The following says to transition states between
# city letting each "state" last 1 second and the transition to last 2 seconds.
# The last two functions cause the point to fade in and then to shrink away.
p + transition_states(city, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_shrink()

# Beware: these animations can take a while to render. Hence the reason we
# sampled 1000 records instead of using all 29,000+.

