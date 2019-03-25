# Visualizing Models, Communicating Results
# phdplus - Spring 2019
# Clay Ford: clayford@virginia.edu

library(tidyverse)
library(effects)
library(ggeffects)
library(scales)
library(stargazer)


# read in homes data ------------------------------------------------------

github_url <- "https://github.com/uvastatlab/phdplus/raw/master/data/albemarle_homes.rds"
homes <- readRDS(url(github_url))

# Today we'll look at homes in the top 6 cities by number of homes
vars <- c("CHARLOTTESVILLE", "CROZET", "EARLYSVILLE", 
          "KESWICK", "SCOTTSVILLE", "NORTH GARDEN")

# filter for our cities of interest and homes with at least 1 bedroom and 1 bathroom
homes <- homes %>% 
  filter(city %in% vars & bedroom > 0 & fullbath > 0)

# drop unusued levels for city
homes$city <- droplevels(homes$city)



# Visualizing a linear model ----------------------------------------------

# We presented effect plots for helping understand non-linear effects and
# interactions, but they are useful for standard linear models as well.

# model home totalvalue by city and finsqft
mod_lm <- lm(totalvalue ~ city + finsqft + fullbath, data = homes)
summary(mod_lm)

# Interpretation: 

# Expected value of home increases about $259 for each additional square foot.

# Expected value of home increases about $26,517 for each additional full bath.

# The coefficients of the other cities represent the difference in expected home
# value from Charlottesville.

# Intercept is the expected value of a home in Charlottesville with 0 finished
# square feet and 0 full baths. Not very useful. 

# Let's look at the default effect plots
plot(allEffects(mod_lm), rug = FALSE)

# An effect plot for "city" can help us visualize and understand the differences
# in expected home values between cities.
plot(Effect("city", mod_lm))

# It appears expected home values in Crozet and Earlysville may be significantly
# different. How could we "test" that? What about all pairise differences?
# See "Appendix: Comparisons of factors in model"


# An effect plot for "finsqft" and "fullbath" is just a straight line since the
# effects are linear. The model assumes the effects of finsqft and fullbath are
# the same for each city. rug = FALSE suppreses the "rug" of data points on the
# x-axis.
plot(Effect("finsqft", mod_lm), rug = FALSE)
plot(Effect("fullbath", mod_lm), rug = FALSE)

# Obviously we would want to review model diagnostics before putting too much
# stock in this model and its effect plots.


# Visualizing non-linear effects ------------------------------------------

# model log(totalvalue) as a function of fullbath and finsqft

# Why log(totalvalue)? totalvalue is highly skewed; log transformation makes it
# more symmetric, helps with non-constant variance.
hist(homes$totalvalue)
hist(log(homes$totalvalue))

mod_le <- lm(log(totalvalue) ~ fullbath + finsqft, data = homes)
summary(mod_le)

# Let's check for non-linearity. Partial residual plots can help with this. The
# base R function termplot creates these. 
termplot(mod_le, terms = "finsqft", partial.resid = TRUE, smooth = panel.smooth)

# The solid line corresponds to the coefficient in the model. The dotted line is
# a smooth trend line through the partial residuals. Partial residuals are the
# remaining variability unexplained by the term. If the smooth line differs
# substantially from the straight line, the assumption of linearity may be
# violated.

# Perhaps we should try a 3rd-degree polynomial to capture the non-linearity.
mod_nle <- lm(log(totalvalue) ~ fullbath + poly(finsqft, 3), data = homes)
summary(mod_nle)

# partial residual plot
termplot(mod_nle, terms = "poly(finsqft, 3)", 
         partial.resid = TRUE, smooth = panel.smooth)

# Another approach might involve splines. See Appendix: using natural splines
# for non-linearity. But we'll stick with the polynomial.

# The extra terms appear justified
anova(mod_le, mod_nle) # Null: smaller model is as good as larger model
AIC(mod_le, mod_nle) # lower AIC is "better"

# And now to Effect Plots!

# basic effect plots for all predictors; rug = FALSE suppreses the "rug" of data
# points on the x-axis.
plot(allEffects(mod_nle), rug = FALSE)

# we can also see the values being plotted if we like
allEffects(mod_nle)

# use transformation = list(link = log, inverse = exp) to place the y-axis on
# the scale of the original data. Good to use any time you log transform your
# response variable.
plot(allEffects(mod_nle, transformation = list(link = log, inverse = exp)), rug = FALSE)

# Looking at just finsqft
plot(Effect("finsqft", mod_nle, transformation = list(link = log, inverse = exp)), 
     rug = FALSE)

# Quick look at the ggeffects package
plot(ggeffect(mod_nle))
plot(ggeffect(mod_nle, "finsqft"))

# I'm not sure how to update axis to original scale using ggeffects plot method.
# See Appendix for more on using ggeffects

# Extract effects data to data frame and make our own plot;

# Using effects package;
# including transformation = list(link = log, inverse = exp) does not transform
# fit to original scale. That only works for plot()
eff.out <- Effect("finsqft", mod_nle)
effDF <- as.data.frame(eff.out)
head(effDF)

# now create effect plot as generated by effect package
ggplot(effDF, aes(x = finsqft, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1/5) 

# Update y-axis to be labeled on original scale.

# First define "pretty" breaks for y axis. This takes all the lower and upper CI
# values, transforms back to original scale, finds the largest and smallest
# values, and creates a "pretty" range of values
yaxs <- c(effDF$lower, effDF$upper) %>% exp() %>% range() %>% pretty()

# now create same effect plot but with update y-axis
ggplot(effDF, aes(x = finsqft, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1/5) +
  scale_y_continuous(breaks = log(yaxs),     # place breaks at log(yaxs)
                     labels = dollar(yaxs))  # label the breaks with dollar(yaxs)



# Visualizing interaction effects -----------------------------------------


# Fit a model that allows effect of finsqft to interact with City
mod_int <- lm(log(totalvalue) ~ fullbath + poly(finsqft, 3) * city, data = homes)
summary(mod_int)

# is the interaction significant?
anova(mod_int)

# default all effects
plot(allEffects(mod_int), rug = FALSE) # not what we want

# specify we want just interaction between finsqft and city and that we want
# "finsqft" on the x-axis: x.var = "finsqft"
plot(Effect(c("finsqft", "city"), mod_int), rug = FALSE, x.var = "finsqft")

# extract data frame so we can make our own plot
effDF2 <- as.data.frame(Effect(c("finsqft", "city"), mod_int))
head(effDF2)

# define "pretty" breaks for y axis
yaxs <- c(effDF2$lower, effDF2$upper) %>% exp() %>% range() %>% pretty()

# and create plot
ggplot(effDF2, aes(x = finsqft, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1/5) +
  facet_wrap(~city) +
  scale_y_continuous(breaks = log(yaxs),
                     labels = dollar(yaxs))


# with free scales
ggplot(effDF2, aes(x = finsqft, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1/5) +
  facet_wrap(~city, scales = "free_y") +
  scale_y_continuous(breaks = log(yaxs),
                     labels = dollar(yaxs))


# These effect plots need some work.

# YOUR TURN #1 ------------------------------------------------------------

# The following models log(totalvalue) as a function of fullbath, finsqft,
# lotsize and the interaction between lotsize and finsqft
mod_int2 <- lm(log(totalvalue) ~ fullbath + poly(finsqft, 3) * lotsize, data = homes)
summary(mod_int2)


# (1) Create an effect plot for the lotsize:finsqft interaction using the Effects
# package. TIP: set rug = FALSE. 

# What do you notice about the y-axis?
# What do you think of the values chosen for the panels?


# (2) Try recreating the plot using ggplot; What do you notice about the y-axis?



# BACK TO PRESENTATION!

# Customizing focal predictor values --------------------------------------

# recall our first model
formula(mod_nle)
plot(Effect("finsqft", mod_nle), rug = FALSE)

# Take a look at the default values being used for the "focal predictor" FinSFt:
eff.out <- Effect("finsqft", mod_nle)
eff.out$x

# Let's modify the focal predictor values. In other words, let's specify which
# finsqft values we want to run through the model. Let's say we want to use 1000
# to 5000 in steps of 500.
eff.out <- Effect("finsqft", mod_nle, 
                  xlevels = list(finsqft = seq(1000,5000,500)))
eff.out$x
plot(eff.out, rug = FALSE)

# Let's make our own plot using same code as before
effDF <- as.data.frame(eff.out)
head(effDF)

# define "pretty" breaks for y axis
yaxs <- c(effDF$lower, effDF$upper) %>% exp() %>% range() %>% pretty()

# now create effect plot as generated by effect package
ggplot(effDF, aes(x = finsqft, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1/5) +
  scale_y_continuous(breaks = log(yaxs),
                     labels = dollar(yaxs))


# recall our second model
formula(mod_int)
plot(Effect(c("finsqft", "city"), mod_int), rug = FALSE, x.var = "finsqft")

# let's take a look at the values used for the focal predictors
eff.out2 <- Effect(c("finsqft", "city"), mod_int)
eff.out2$x
sapply(eff.out2$x, unique)

# We can set finsqft the same as we just did
eff.out2 <- Effect(c("finsqft", "city"), mod_int, 
                   xlevels = list(finsqft = seq(1000,5000,500)))
plot(eff.out2, rug = FALSE)

# How do we modify city? What if we wanted to only compare KESWICK and CROZET?
# The easiest way to filter the data frame we create from the effects object and
# then make our own plot.

effDF2 <- as.data.frame(eff.out2)
yaxs <- c(effDF2$lower, effDF2$upper) %>% exp() %>% range() %>% pretty()

# notice we filter the data frame
ggplot(filter(effDF2, city %in% c("KESWICK","CROZET")), 
       aes(x = finsqft, y = fit, color = city, fill = city)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1/5) +
  scale_y_continuous(breaks = log(yaxs),
                     labels = dollar(yaxs))



# Customizing fixed predictor values --------------------------------------

# recall our first model
eff.out$formula
plot(eff.out, rug = FALSE)

# Our focal predictor values
eff.out$x

# Our fixed predictor values for fullbath (mean)
eff.out$model.matrix
mean(homes$fullbath)

# Let's set it to 3
eff.out <- Effect("finsqft", mod_nle, 
                  xlevels = list(finsqft = seq(1000,5000,500)),
                  fixed.predictors = list(given.values = c(fullbath = 3)))
eff.out$model.matrix

# doesn't change the shape of plot, just shifts it up
plot(eff.out, rug = FALSE)

# We could also use the median value
eff.out <- Effect("finsqft", mod_nle, 
                  xlevels = list(finsqft = seq(1000,5000,500)),
                  fixed.predictors = list(typical = median))
eff.out$model.matrix

# doesn't change the shape of plot, just shifts it down
plot(eff.out, rug = FALSE)



# YOUR TURN #2 ------------------------------------------------------------

# YOUR TURN #1 gave us this plot
e.out <- Effect(c("lotsize","finsqft"), mod_int2)
eDF <- as.data.frame(e.out)
yaxs <- c(eDF$lower, eDF$upper) %>% exp() %>% range() %>% pretty()
ggplot(eDF, aes(x = lotsize, y = fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 1/4) + 
  facet_wrap(~finsqft, labeller = "label_both") +
  scale_y_continuous(breaks = log(yaxs),
                     labels = dollar(yaxs))


# Modify the code above so finsqft ranges from 1000 - 6000 (in steps of 1000)
# and lotsize ranges from 0 - 15. Then re-run the plot code. 




# BACK TO PRESENTATION!



# Appendix: comparisons of factors in model -------------------------------

# Recall the following model and plot
summary(mod_lm)
plot(Effect("city", mod_lm))

# Are the expected home values between Crozet and Earlysville significantly
# different? One way to answer that using the existing model is the
# linearHypothesis function in the car package.

# We simply set the coefficient names equal to one another. The null of the
# hypothesis test is that coefficients for CROZET and EARLYSVILLE are the same.
library(car)
linearHypothesis(mod_lm, "cityCROZET = cityEARLYSVILLE")

# We could also test the null that both CROZET and EARLYSVILLE, and CROZET and
# KESWICK, have the same coefficients.
linearHypothesis(mod_lm, c("cityCROZET = cityEARLYSVILLE",
                           "cityCROZET = cityKESWICK"))

# For all pairwise comparisons, we can use Tukey's test available in the
# multcomp package. Use the glht() function which stands for General Linear
# Hypotheses Test. The first argument is our model. The second argument is the
# linear function to be tested. We set city equal to "Tukey" and wrap in the
# mcp() function, which stands for multiple comparisons.
library(multcomp)
comp.out <- glht(mod_lm, linfct = mcp(city = "Tukey"))

# The summary reveals that 7 out of the 15 possible comparisons appear to be
# significantly different. Notice these comparisons use adjusted p-values that
# take into account the multiple hypothesis tests. 
summary(comp.out)

# Wheresas previously Crozet and Earlysville appeared significantly different,
# they no longer appear significantly different when considered 1 of 15
# different hypothesis tests.

# multcomp provides a plotting method to visualize the comparisons along with
# the confidence intervals of the differences.
plot(comp.out)

# The labels are too big for the margin. An easy fix is to just create the plot
# ourselves using ggplot. First get the confidence intervals and then convert to
# a data frame.
c.out <- confint(comp.out)
ciDF <- as.data.frame(c.out$confint)
ciDF$city <- rownames(ciDF)
ggplot(ciDF, aes(x = reorder(city, Estimate), y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-60000,60000,20000)) +
  coord_flip()



# Appendix: using natural splines for non-linearity -----------------------

# We can model nonlinear effects with natural splines. Splines are piecewise
# cubic polynomials used in curve fitting. The splines package provides the ns()
# function. The 2nd argument, df, corresponds to how many cubic polynomials we
# want to use. Almost always use 3 - 5. (Harrell, 2015) When sample is large, 5
# is good choice.

library(splines)
mod_splines <- lm(log(totalvalue) ~ fullbath + ns(finsqft, 5), data = homes)
summary(mod_splines)
termplot(mod_splines, terms = "ns(finsqft, 5)", partial.resid = TRUE, 
         smooth = panel.smooth)

# Polynomials are not well-behaved at the "turns" or in the extremes. Let's see
# how polynomials and splines compare with some fake data
n <- 100
x <- seq(0, 3*pi, length.out = n)
set.seed(2019)
y <- sin(x) + rnorm(n, sd = 0.3)
plot(x, y)

# There is obvious non-linearity. Let's try polynomials and splines. Since the
# line seems to change "directions" 4 times we'll compare the fit of a
# 4th-degree polynomial with a 4 df natural spline
m1 <- lm(y ~ poly(x, 4))
m2 <- lm(y ~ ns(x, 4))
lines(x, sin(x), col = "black")                     # the "true" line
lines(x, fitted(m1), col = "red", lty = 2)          # spline
lines(x, fitted(m2), col = "blue", lty = 2)         # polynomial
legend("bottomright", legend = c("poly", "ns"), 
       col = c("red", "blue"), lty = 2)

# Notice how the polynomial deviates from the true line at the turns and in the
# extremes.

# polynomials and splines are transformations of our original data. We took one
# column of data and transformed it into 4 columns of data.

# original data
plot(x)
# polynomial-transformed data
p.out <- poly(x, 4)
matplot(p.out)
# spline-transformed data
ns.out <- ns(x, 4)
matplot(ns.out)

# create data frames of transformed data and include y
pDF <- as.data.frame(cbind(y,p.out))
nsDF <- as.data.frame(cbind(y,ns.out))
names(pDF)[2:5] <- paste0("x",1:4)
names(nsDF)[2:5] <- paste0("x",1:4)

# re-fit models using the transformed data
m1a <- lm(y ~ ., pDF)
m2a <- lm(y ~ ., nsDF)
# Note that these are same as previous models
all(coef(m1) == coef(m1a))
all(coef(m2) == coef(m2a))

# Look at partial residual plots for the coefficients in both models
op <- par(mfrow = c(2,2))
termplot(m1a, partial.resid = TRUE, smooth = panel.smooth)
termplot(m2a, partial.resid = TRUE, smooth = panel.smooth)
par(op)

# On the whole, the natural spline coefficients exhibit more linearity than
# their polynomial counterparts. Recall the original variable x was transformed
# into 4 variables (x1 - x4) since we could not assume linearity. But the
# linearity assumption still applies when we fit the transformed data. 


# Appendix: effects vs. marginal effects ----------------------------------

# In the slides we saw the following example of an effects plot for
# interactions.
mod.cowles <- glm(volunteer ~ sex + neuroticism + extraversion +
                    neuroticism:extraversion,
                  data=Cowles, family=binomial)
plot(Effect(c("neuroticism","extraversion"), mod.cowles), rug = F)

# The effect of neuroticism on volunteering changes depending on the level of
# extraversion. What is that effect exactly at each level of extraversion? Those
# are marginal effects.  We can get these using the margins package, which is a
# port of Stata's margins command.
library(margins)

# The following returns the marginal effect of neuroticism at the specified
# values of extraversion.
summary(margins(mod.cowles, variables = "neuroticism", 
                at = list(extraversion = c(2, 7.2, 12, 18, 23))))

# The values under AME are the average marginal effects. These are the slopes of
# the lines in the effect plot. Take note they are on the log-odds (or logit)
# scale. Notice the output also returns hypothesis tests of whether or not the
# slopes are 0 and confidence intervals of the marginal effects. 

# When extraversion = 2, the estimated marginal effect of neuroticism is 0.0175.
# If we exponentiate, we get an odds ratio. 
exp(0.0175)

# The odds (not probability) of volunteering increase by about 2% for every
# one-unit increase in neuroticism, when extraversion = 2.

# Returning to our homes data, here's a simple model with finsqft interacted
# with city
mod <- lm(totalvalue ~ fullbath + finsqft * city, data = homes)
summary(mod)
# The interaction appears significant
anova(mod)

# The effect plot reveals subtle differences in the slopes for each city. In
# other words, the significant interaction means the relationship between
# finsqft and totalvalue is different for each city, or at least some cities.
plot(Effect(c("finsqft", "city"), mod), x.var = "finsqft", rug = F)

# What are the slopes of those lines exactly? Those are the marginal effects.

# The following takes our model and returns the marginal effect of finsqft for
# each city. It takes a few seconds to run.
summary(margins(mod, variables = "finsqft", 
                at = list(city = levels(homes$city))))

# The output is confusing. The city column labels the cities 1 - 6. This
# corresponds to the order of the city factor.
levels(homes$city)

# The slope for North Garden is about 304 with a 95% CI of (285, 323). It has
# the steepest slope of the six cities.

# Here's another model that looks at the effect of having a fireplace on the
# totalvalue of a home within each city. (ie, we allow fp and city to interact)

# First we make fp a factor, then fit the model.
homes$fp <- factor(homes$fp)
mod_fp <- lm(totalvalue ~ fullbath + fp * city, data = homes)
summary(mod_fp)
anova(mod_fp)

# The interaction appears significant the effect plot confirms this. Homes with
# a fireplace appear to have a higher total value, but the difference a
# fireplace(s) makes differs between cities.
plot(Effect(c("fp", "city"), mod_fp), x.var = "fp")

# What are those differences? Those are marginal effects. The following takes
# our model and returns the marginal effect of fp for each city. It takes a few
# seconds to run.
summary(margins(mod_fp, variables = "fp", 
                at = list(city = levels(homes$city))))

# The biggest difference appears to be for North Garden. In that city a home
# with one or more fireplaces is expected to have a totalvalue of about
# $148,875 more than a home without a fireplace. 95% CI ($103,988, $193,763)



# Appendix: interpreting log transformations ------------------------------

# It's faily common to log transform positive data that is skewed, particular
# things like income or home values.

# When data is log transformed in a linear model, the interpretation of
# coefficients changes.

# Three scenarios:
# 1. response is log transformed
# 2. predictor(s) is/are log transformed
# 3. both response and predictors log-transformed


# 1. response is log transformed
mod_1 <- lm(log(totalvalue) ~ fullbath + finsqft, data = homes)
summary(mod_1)

# How to interpret coefficients:

# Exponentiate the coefficient. This is the multiplicate change in the response
# for every one-unit increase.

exp(coef(mod_1)["finsqft"])  # 1.000494

# For every additional square foot, totalvalue = totalvalue*1.000494

# We can also subtract one from this number and multiply by 100. This gives the
# percent increase (or decrease) in the response for every one-unit increase in
# the independent variable.

# finsqft:
(exp(coef(mod_1)["finsqft"]) - 1) * 100  # 0.0494125

# For every additional finished square foot, totalvalue increases by about
# 0.05%. Or for every additional 100 square feet, totalvalue increases by about
# 5%.


# 2. predictor(s) is/are log transformed
mod_2 <- lm(totalvalue ~ fullbath + log(finsqft), data = homes)
summary(mod_2)

# How to interpret coefficients:

# Divide the coefficient by 100. This tells us that a 1% increase in the
# independent variable increases (or decreases) the dependent variable by
# (coefficient/100) units.

coef(mod_2)["log(finsqft)"]/100  # 4217.411 

# For every 1% increase in finsqft, totalvalue increases by about $4217


# 3. both response and predictors log-transformed
mod_3 <- lm(log(totalvalue) ~ fullbath + log(finsqft), data = homes)
summary(mod_3)
# How to interpret coefficients:

# The coefficient is the percent increase in the dependent variable for every 1%
# increase in the independent variable.

coef(mod_3)["log(finsqft)"] # 1.103296

# For every 1% increase in finsqft, totalvalue increases by about 1.10%.


# More information:
# https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/



# Appendix: more on ggeffects ---------------------------------------------

# The following applies to ggeffects version 0.9.0

# Recall this model:
formula(mod_nle)

# all effect plots for the model, each plotted separately
plot(ggeffect(mod_nle))

# Just for finsqft
plot(ggeffect(mod_nle, terms = "finsqft"))

# ggeffects returns a data frame by default
geff.out <- ggeffect(mod_nle, "finsqft")
geff.out

# The data frame is similar to the data frame returned by the effects package,
# with different column names.

# We can use this data frame to create our own plots.

# define "pretty" breaks for y axis
yaxs <- c(geff.out$conf.low, geff.out$conf.high) %>% exp() %>% range() %>% pretty()

# now create similar effect plot as generated by ggeffect package
ggplot(geff.out, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 1/5) +
  scale_y_continuous(breaks = log(yaxs),
                     labels = dollar(yaxs)) +
  theme_ggeffects()


# Modifying predictor values with ggeffects is easy - up to a point. We simply
# indicate the levels in square brackets, like so:
ggeffect(mod_nle, "finsqft [1000,2000,3000,4000,5000]")

# However it does not accommodate functions such as seq(1000,5000,500).

# To specify a certain number of levels, use n = 
ggeffect(mod_nle, "finsqft [n=10]")

# We can also randomly sample values
ggeffect(mod_nle, "finsqft [sample=10]")


# Customizing fixed predictor values with ggeffects is actually done the same
# way as we did with the Effect function in the effects package. We use the
# fixed.predictors argument. That's because ggeffect() computes effects by
# internally calling Effect!

# To set fullbaths to 3:
ggeffect(mod_nle, "finsqft [1000,2000,3000,4000,5000]",
         fixed.predictors = list(given.values = c(fullbath = 3)))

# To set fullbaths to the median:
ggeffect(mod_nle, "finsqft [1000,2000,3000,4000,5000]",
         fixed.predictors = list(typical = median))


# Working with interactions
# returns a data frame but has a special print method
ggeffect(mod_int, c("finsqft", "city"))
geff.out2 <- ggeffect(mod_int, c("finsqft", "city"))

# The default plot is multi-line
plot(geff.out2)

# set facets = TRUE to see separate plot for each city. Notice the color
# remains, which is redundant in a facet plot.
plot(geff.out2, facets = TRUE)

# To turn off the colors, set colors = "bw"
plot(geff.out2, facets = TRUE, colors = "bw")

# Notice the column names in the data frame...
head(as.data.frame(geff.out2))

# ...instead of "finsqft" and "city" we have "x" and "group"

# The ggeffects package has several friendly vignettes. Go to the help page for
# the package and click "User guides, package vignettes and other documentation"
# to review them.
help(package = "ggeffects")


