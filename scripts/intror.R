# Intro to R workshop
# Spring 2019
# Jennifer Huck
# UVA Library

# This R Script will introduce you to scripts, installing and loading pacakages,
# using tidyverse, keeping R up to date, viewing and changing your working 
# directory, importing data, evaulating data, and graphing Albemarle County real
# estate data. 

# Basic R operations------------------------------------------------------------

# Arithmetic
# Tip: put the cursor anywhere in the line below and press CTRL+Enter/CMD+Enter 
# to run the command
2+8/2
(2+8)/2  #R does order of operations

# Let's make an object
# R statements where you create an object take this form:
# object_name <- value aka "object gets value"

# Tip: Keyboard shortcut ALT+- for <- (with spaces!)
# Tip: More cool shortcuts: Help...Keyboard Shortcuts Help (Alt+Shift+K)
# Tip: Include spaces around your operators.  Spaces make your script easier to 
# read.

# Let's make some objects for housing sq. ft. 
cottage <- 1000   # See how it gets saved in your Environment
mansion <- 10000  # aka "mansion gets ten thousand"

# Mini-Your Turn: take advantage of RStudio autocomplete facilty: 
# type the first 3 letters of cottage, press Tab, then CTRL+Enter/CMD+Enter


CoTtage <- 5000   # R is cAse sEnsiTive

cottage / 2       # We can do arithmetic with R objects
cottage + mansion # Prints to console, we aren't saving this as an object in the
                  # Environment

cottage <- 2000   # We can reassign objects
cottage           # Verify by looking in the Environment or printing to console

two_homes <- cottage + mansion  # Save our arithmetic as a new object
# Tip: use snake_case by separating lowercase words with _.  
# it_is_easy_to_read

rm(CoTtage)       # Remove an object
CoTtage           # Now we get an error, b/c the object no longer exists

# Your turn #1------------------------------------------------------------------

# 1. A cottage costs $100.  Create an object labeled cottage_cost 
# (You do not need to include the dollar sign.)


# 2. You are European, and in the market for a cottage.  How much does the 
# cottage cost in Euros?  Use cottage_cost, multiply by 0.88, and 
# save as cottage_euros.


# Data Types: Part 1------------------------------------------------------------

# scalar: holds one value at a time
# number
x <- 1
y <- 2.5
class(x)

# logical
m <- x > y  # Is x larger than y?
n <- x < y  # Is x smaller than y?
m
n

# character (string)
d <- "cottage"
a <- "1"; b <- "2.5" # Are they different from x and y we used earlier?
x + y
a + b

# vector - a sequence of data elements of the same basic type
o <- c(1,2,5.3,6,-2,4)                             # Numeric vector
p <- c("one","two","three","four","five","six")    # Character vector
q <- c(TRUE,TRUE,FALSE,TRUE,FALSE,TRUE)            # Logical vector
# Note that we use the c() function (combine or concatenate) to make a vector
# Have questions about a function? Put a ? in front to get the R documentation:
?c

# indexing vectors - use brackets to reference elements in a vector
o[3]                       # pulls 3rd element
p[4:6]                     # pulls 4th-6th elements
q[c(1,3)]                  # pulls non-sequential elements

# matrix - a two-dimensional rectangular layout consisting of the same basic type
t <- matrix(
     1:12,                 # the data components
     nrow=4,               # number of rows
     ncol=3,               # number of columns
     byrow = FALSE)        # fill matrix by columns
t                          # print the matrix

# indexing matrices - use brackets to reference elements in a matrix
t[2,3]                    # component at 2nd row and 3rd column
t[,3]                     # 3rd column of matrix
t[4,]                     # 4th row of matrix

# list - an ordered collection of objects 
w <- list(name="Jenn", mynumbers=o, mymatrix=t, age=5.3)

# use double brackets to identify elements within list
w[[2]]

# data frame - similar to a matrix, except columns can be different data types 
# (i.e., can be numeric, string, logical, factor, etc.)

# factor - tell R that a variable is nominal - good for categories

# Let's hold on to the idea of data frames and factors, and open up the Homes 
# data first.

# Your turn #2------------------------------------------------------------------
# 1. Try using some logical operators with m and n: & (and), | (or), ! (negation).


# 2. When might you want numeric digits to be a string? 


# 3. Within RStudio, look up the R Documentation for the matrix function. 


# 4. Create a string vector consisting of 3 compenents. Remember the keyboard
# shortcut for the assignment <-  operator: ALT+-



# (Jump back to Intro to R website.)

# Loading tidyverse, reading conflicts------------------------------------------

# Let's clear the objects we just made from our workspace since we don't need 
# them anymore.  Select the broom icon in the Environment window. 

# Let's load the tidyverse package now 
library(tidyverse) 
# Remember, you only install a package once, but you must load it every new R session. 

# Note the message that we see in the Console.  You should see what packages
# were loaded (readr, dplyr, ggplot2, etc.) and conflicts.  You should see 2 conflicts:
# x dplyr::filter() masks stats::filter()
# x dplyr::lag()    masks stats::lag()
# Those are okay! 

# If you see a Warning Message in red text below the conflicts, that is error
# you need to fix.

# Note the message we got about maksing.  You can read that as:
# The filter command in the dplyr package masks the filter command in the stats
# package.  

# View and Change Your Working Directory----------------------------------------

# get your working directory
getwd()

# Set working the working directory to wherever you saved this script and
# albemarle_real_estate.csv via point-and-click: 
# Session...Set Working Directory...Choose Directory

# Verify the directory is correct
getwd()

# You can also set the working directory in the console.  Copy the file path.
# This is an example of how I set MY working directory:
# setwd("C:/Users/jah2ax/Box Sync/_R/workshops/intro_to_R_spring_2019") 
# Windows users: note the FORWARD slashes

# Paste YOUR working directory file path here, between the quotes:
setwd("") 

# Verify the directory is correct
# Note that you can see the working directory listed at the top of the Console
getwd() 


# Import homes data-------------------------------------------------------------

# You have several options for importing data.  let's try a few!

# 1. Save the file to your working directory, and simply pull the file from 
# there. Since you already saved the file, and you just set your working 
# directory, just put the file name in quotes. 
homes <- read.csv("albemarle_real_estate.csv")

# homes is now saved into memory as a data object.  Look for it in the 
# Environment pane. 

# 2. Read from a website.  Copy and paste the URL in quotes.
# For this example, let's try read.csv
homes_url <- read.csv("http://people.virginia.edu/~jah2ax/intro_to_R_spring_2019/albemarle_real_estate.csv")

# 3. Point and click: Look in the Environment window for the "Import Dataset: 
# From Text (base)" button.  There is a slight but important difference in how 
# this method imports a dataset versus the other methods (we will see this in 
# the next section).  Save as "homes_point_click".

# 4. We have been relying on read.csv from the utils package. There are 
# other options.  Let's try the read_csv function from the readr package that
# comes with tidyverse.
homes_readr <- read_csv("albemarle_real_estate.csv")

# These are examples of data frames - tabular data made up of different data types.
# Tabular in that you have rows and columns.  Rows are generally observations. 
# Columns are generally variables.  The data type of each column can vary: numeric
# string, factor, etc.


# Evaluate Data-----------------------------------------------------------------

View(homes)  # view spreadsheet-style data viewer
             # You can also click on "homes" in your Environment pane
class(homes) # what type of data this is

head(homes)  # prints first 6 rows
tail(homes)  # prints last 6 rows 

# We got 6 rows back, but we only want 3
?head               # read Arguments section
head(homes, n = 3)  # Add an argument to change your results
head(homes, 3)      # note that you get the same results as the above line since 
                    # arguments are in the right order

dim(homes)  # dimensions
nrow(homes) # number of rows
ncol(homes) # number of columns

names(homes) # column headings (variable names)
summary(homes)

str(homes) # View structure of the homes data
# Notice the non-numeric/integer data is "Factor." Factor basically means a 
# categorical variable. "Factor w/ 6 levels" for City means there are 6 
# unique functional areas in this data. But notice the numbers. Factors are 
# actually stored as integers, and the integers have associated "labels". 

# You have the option to import or store the data as character data, if you 
# want. Pointing-and-clicking in the Environment window will do this, as will 
# using readr::read_csv
str(homes_readr) # View structure of the homes_readr data

summary(homes) # Get a quick statistical summary of the dataset

# indexing dataframes - use $ to reference elements in a data frame
summary(homes$LotSize)  # Get a quick statistical summary of a single variable

# Save values by assigning objects
lot_size_mean <- mean(homes$LotSize)

# Your Turn #3------------------------------------------------------------------

# 1. Print the last 8 lines of 'homes'


# 2. Take advantage of Rstudio autocomplete by typing in summary(homes$) below 
# and pick a variable that you think looks interesting.


# 3. Save the median total value as an object.  


# 4. What is the range of years represented by Year Built? (Hint: `range()`)


# 5. Fix these commands so they run correctly:
median(homes$Bedrooms)
Mean(homes$FullBath)


# Filter data: A Taste of dplyr-------------------------------------------------

# It can be helpful to filter data based on values that you are interested in.
# Let's use the dplyr package (part of Tidyverse, which we already installed).

# We can subset observations based on their values
filter(homes, YearBuilt > 2000)       # returns only those observations where
                                      # YearBuilt is greater (more recent) than 2000
filter(homes, City == "SCOTTSVILLE")  # returns only those observations where
                                      # city is Scottsville
                                      # Note string in quotes, no quotes for numeric


scottsville <- filter(homes, City == "SCOTTSVILLE") # save Scottsville subset
View(scottsville)                                   # view Scottsville subset in
                                                    # spreadsheet viewer

# Wrap in () to print to console
# Save results as an object
# Layer arguments
# R accepts all the usual comparison operators.
(large_homes_small_plot <- filter (homes, FinSqFt > 3500, LotSize < 1.0))

# Homes where sq ft is greater than 3500 OR lot size is greater than 1 acre
large_homes_or_plot <- filter (homes, FinSqFt > 3500 | LotSize > 1.0)
View(large_homes_or_plot)

# Combine filtered data with summary stats -
# Filter Charlottesville homes built in 2016, save as new object
# Median Total Value of 2016 Cville homes
new_cville <- filter(homes, City == "CHARLOTTESVILLE" & YearBuilt == 2016)
median(new_cville$TotalValue)

# RStudio Cheat sheets: dplyr
# e.g., ggplot: https://www.rstudio.com/resources/cheatsheets/#dplyr 

# Your Turn #4------------------------------------------------------------------

# 1A. View the help file for filter:
?filter
# Make sure to choose the right filter command. Recall the package our filter() 
# is from.  (Hint: masking message from Tidyverse.)  How are the arguments 
# combined, and what is kept?
# 1B. Would you expect to get the same results for these two commands?
large_homes_small_plot <- filter (homes, FinSqFt > 3500, LotSize < 1.0)
large_homes_small_plot_alt <- filter (homes, FinSqFt > 3500 & LotSize < 1.0)

# 2. Fix these commands so they run correctly:
crozet <- filter(homes, City == "Crozet")
keswick <- filter(homes, City == KESWICK)

# 3A. Find all homes built before 1900.  Save the results as a new object.


# 3B. Using the object you made in 3A, what is the mean Finished Square Feet 
# of those homes? 


# Visualize data: A Taste of ggplot2--------------------------------------------

# It can be helpful to visualize data.  Let's use the ggplot2 package (part of 
# Tidyverse, which we already installed).

# histogram
ggplot(data = homes, mapping = aes(x = TotalValue)) +
  geom_histogram()
# Note data, mapping, and geom_function -- the grammar of graphics (the gg in ggplot2)

# add an argument to the histogram: binwidth
ggplot(data = homes, mapping = aes(x = TotalValue)) +
  geom_histogram(binwidth = 10000)

# scatterplot
ggplot(data = homes, mapping = aes(x = TotalValue, y = FinSqFt)) +
  geom_point()

# bar chart
ggplot(homes, aes(City)) +  # note that you can drop "data", "mapping", "x", "y"
  geom_bar()                # as long as they are in the right order 
                            
# save parts of a plot, and re-use later
d <- ggplot(homes, aes(City))

d + geom_bar()  # note that we get the same bar chart as above

d + geom_bar(aes(fill = Condition))  # layer more arguments to make a more 
                                     # complex graph

ggsave("homes.png")                  # save figure

# RStudio Cheat sheets: ggplot2
# e.g., ggplot: https://www.rstudio.com/resources/cheatsheets/#ggplot2 

# Exercise----------------------------------------------------------------------
# 1. Create a scatterplot where x is FinSqFt and Y is LotSize


# Jump to "Using R Projects" on accompanying documentation. 

# Projects----------------------------------------------------------------------

# Save this R script.  It should already be in a folder with 
# albemarle_real_estate.csv.  Create a project by selecting the R Project icon 
# or File...New Project...Existing Directory...select the directory (folder) 
# where this R script is saved.

# Once you start working with a project, you will notice that opening a project
# will provide you with the same working directory, the command history, and 
# easy access to your files. 

# You also get a fresh environment, so you should reload libraries and data. 

# Re-load your libraries (~line 138, loads tidyverse)
# Re-load you data (~line 186, calls data from within your working directory)

# Use projects to keep input data, R scripts, history, analytical results, and
# figures together in the same directory!