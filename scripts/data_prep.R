################################################
# Data Science Essentials in R: Data Preparation
#
# UVA Library Research Data Services
# February 28, 2019
# Michele Claibourn
# 
# 1. Data source
# 2. Load library, read in data sets: read_csv, select, mutate
# 3. Merge data sets: join, filter, select
# 4. Initial look at key variables: filter, select, arrange, (very little) ggplot
# 5. Munge data: filter, select, mutate, if_else, factors
# 6. Clean up and save: rm, save.image, saveRDS, write_csv
# 7. Explore data further: group_by, summarize, filter, arrange, etc.
################################################

################################################
# # 1. Data source
# # Data located and acquired by Clay Ford
# # You don't need to run this... 
# # ...the resulting data files are part of our workshop materials.
# #
# # Albmarle county Office of Geographic Data Services
# # http://www.albemarle.org/department.asp?department=gds&relpage=3914#Parcels
# 
# # Real Estate Information - Primary Card Level Data 
# # http://www.albemarle.org/gds/gisdata/CAMA/CAMA_CardLevelData_TXT.zip
# 
# link <- "http://www.albemarle.org/gds/gisdata/CAMA/CAMA_CardLevelData_TXT.zip"
# download.file(link, destfile = basename(link))
# unzip(basename(link), list = TRUE) # list files, but don't extract
# unzip(basename(link), "CAMA_CardLevelData.txt") # extract file to working directory
# card_level <- read.csv("CAMA_CardLevelData.txt", stringsAsFactors = FALSE)
# names(card_level)
# 
# # Real Estate Information - Parcel Level Data.  
# # http://www.albemarle.org/gds/gisdata/CAMA/CAMA_ParcelInfo_TXT.zip
# 
# link2 <- "http://www.albemarle.org/gds/gisdata/CAMA/CAMA_ParcelInfo_TXT.zip"
# download.file(link2, destfile = basename(link2))
# unzip(basename(link2), list = TRUE) # list files, but don't extract
# unzip(basename(link2), "CAMA_ParcelInfo.txt") # extract file to working directory
# parcel_level <- read.csv("CAMA_ParcelInfo.txt", stringsAsFactors = FALSE)


################################################
# 2. Load library, read in data sets ----

library(lubridate)
library(tidyverse)

# a.  read in card level data and have a peek
card_level <- read_csv("CAMA_CardLevelData.txt")
str(card_level)
summary(card_level)

# list of variables to keep
vars <- c("TMP", "CardNum", "CardType", "YearBuilt", "YearRemodeled", "UseCode", 
          "Condition", "NumStories", "FinSqFt", "Cooling", "FP_Num", 
          "Bedroom", "FullBath", "HalfBath", "TotalRooms"
          )
# select listed variables, add variable for source
card <- card_level %>% 
  select(vars) %>% 
  mutate(source = "card")


# b. read in parcel level data and have a peek
parcel_level <- read_csv("CAMA_ParcelInfo.txt")
str(parcel_level)
summary(parcel_level)

# list of variables to keep
vars <- c("ParcelID", "Owner", "City", "Zip", "LotSize", "PropName",
          "LandValue", "LandUseValue", "ImprovementsValue", 
          "TotalValue", "LastSalePrice", "LastSaleDate", 
          "Subdivision", "Cards"
          )

# select listed variables, add variable for source = parcel
parcel <- parcel_level %>% 
  select(vars) %>% 
  mutate(source = "parcel")

rm(card_level, parcel_level)



################################################
# 3. merge primary card level data with parcel level data ---- 

n_distinct(card$TMP)
n_distinct(parcel$ParcelID)

homes <- full_join(card, parcel, by = c("TMP" = "ParcelID"))

# check the result
# records in parcels only
in_parcel <- homes %>% 
  filter(is.na(source.x)) %>% 
  select(Owner:Cards)
View(in_parcel)

# many with 0 improvements (or missing), how many?
in_parcel %>% 
  filter(ImprovementsValue == 0 | is.na(ImprovementsValue)) %>% 
  tally()

# records in cards only
in_cards <- homes %>% 
  filter(is.na(source.y)) %>% 
  select(TMP:TotalRooms)
View(in_cards)
# mostly vacant residential land or use is missing

# keep only records in both cards and parcels
# drop source
homes <- homes %>% 
  filter(!is.na(source.y) & !is.na(source.x)) %>% 
  select(-c(source.y, source.x))

# fix names
names(homes) <- tolower(names(homes))

# and make a copy in case we mess something up or want to reference the full set
homes_copy <- homes
rm(in_parcel, in_cards)



################################################
# 4. Initial look at key variables ----

# a. keep only residential home records (e.g., not businesses, apartment complexes)
#    ideally, home properties which individual households own, and with which they 
#    can accrue wealth, or properties that can be rented to individual households 

table(homes$usecode)

# identify residential home records with usecode 
res <- c("Doublewide", "Duplex", "Mobile Homes", 
         "Rental House", "Single Family", "Single Family-Rental"
         )

homes <- homes %>% 
  filter(usecode %in% res)


# b. examine key variables: totalvalue, finsqft, lotsize
# totalvalue
summary(homes$totalvalue) # 2 NAs, some 0s, and more than 200M at max

homes %>%  # check NAs
  filter(is.na(totalvalue)) %>% 
  select(cardtype, usecode, finsqft, lotsize, landvalue, improvementsvalue, totalvalue)

homes %>% # check 0s
  filter(totalvalue == 0) %>% 
  select(cardtype, usecode, finsqft, lotsize, landvalue, improvementsvalue) 

# remove the rows with TotalValue 0 or NA
homes <- homes %>% 
  filter(!is.na(totalvalue) & totalvalue > 0)

ggplot(homes, aes(x = totalvalue)) + geom_histogram()
ggplot(homes, aes(log(totalvalue))) + geom_histogram()


# finsqft
summary(homes$finsqft) # 9 NAs, some 0s, and up to nearly 20K sqft

tmp <- homes %>% # check NAs (I want to see the full Owner/Name so save in tmp data frame)
  filter(is.na(finsqft)) %>% 
  select(yearbuilt, usecode, finsqft, totalrooms, owner, lotsize, propname, landvalue, improvementsvalue, totalvalue)
# some commercial, all duplex/rental (remove below)
 
tmp <- homes %>% # check 0s
  filter(finsqft == 0) %>% 
  select(yearbuilt, usecode, finsqft, totalrooms, owner, lotsize, propname, landvalue, improvementsvalue, totalvalue) %>% 
  arrange(desc(improvementsvalue))
# outside of the first 100+, most don't seem to have improvements valued highly enough to be a home (remove below)
# how many with home-priced improvementsvalue?
tmp %>% 
  filter(improvementsvalue > 75000) %>% 
  tally()

# check the high end
tmp <- homes %>% 
  filter(finsqft > 5000) %>% 
  select(yearbuilt, usecode, finsqft, totalrooms, owner, city, lotsize:totalvalue, cards) %>% 
  arrange(desc(finsqft)) # limit this, e.g, < 10K

# remove rows with FinSqFt >= 10000
homes <- homes %>% 
  filter(!is.na(finsqft) & finsqft > 0 & finsqft < 10000)

ggplot(homes, aes(x = finsqft)) + geom_histogram(binwidth = 100) 


# lotsize
summary(homes$lotsize)

tmp <- homes %>% # check 0s
  filter(lotsize == 0) %>% 
  arrange(finsqft)
# mostly condos/townhouses, keep

tmp <- homes %>%  # check high end
  filter(lotsize > 250) %>% 
  arrange(desc(lotsize))
# mostly farms (also golf course, school, ashlawn, etc.)
# ... many with mutiple properties on one assessment

# remove records with 2 or more cards associated with parcel
homes <- homes %>% 
  filter(cards < 2)

ggplot(homes, aes(lotsize)) + geom_histogram()
ggplot(filter(homes, lotsize > 0), aes(log(lotsize))) + geom_histogram()



################################################
# 5. Munge remaining data ---- 

# a. examples to highlight: yearbuilt/age, condition, city

# yearbuilt
summary(homes$yearbuilt) # check 0s
tmp <- homes %>% 
  filter(yearbuilt == 0) %>% 
  arrange(lastsaledate)
# just missing on these; use with caution
ggplot(filter(homes, yearbuilt > 0), aes(x = yearbuilt)) + geom_histogram()

# more likely to use this as age than year, create age of home
homes <- homes %>% 
  mutate(age = 2016 - yearbuilt)

ggplot(homes, aes(x = age)) + geom_histogram(bins = 100)

# impute median value within city for missing
city_age <- homes %>% 
  filter(age < 2016) %>% 
  group_by(city) %>% 
  summarize(med_age = round(median(age)))

homes <- left_join(homes, city_age, by = "city")

homes <- homes %>% 
  mutate(age = if_else(age == 2016, med_age, age))
ggplot(homes, aes(x = age)) + geom_histogram(bins = 100)


# condition
table(homes$condition) # reformat as factor
cond_levels <- c("Substandard", "Poor", "Fair", "Average", "Good", "Excellent") # define levels/order
homes <- homes %>% 
  mutate(condition2 = factor(condition, levels = cond_levels)) # I created a new variable here
summary(homes$condition2)

# redo - recode missing to None and use as a level
cond_levels <- c("None", "Substandard", "Poor", "Fair", "Average", "Good", "Excellent")
homes <- homes %>% 
  mutate(condition = if_else(is.na(condition), "None", condition),
         condition = factor(condition, levels = cond_levels)) %>% 
  select(-condition2) # remove prior variable
summary(homes$condition)


# city
table(homes$city) 
sum(is.na(homes$city)) # 256 missing
# based on mailing address, not jurisdictional boundaries,
# let's keep labels for those with > ~400 records and combine the remainder
homes <- homes %>% mutate(city = fct_explicit_na(city),
                          city = fct_lump(city, n = 9, other_level = "OTHER"),
                          city = fct_infreq(city),
                          city = fct_relevel(city, "OTHER", after = Inf))
table(homes$city)



# b. remaining cleaning (run, don't discuss): 
#   improvementsvalue, landvalue, landusevalue, cardtype, usecode, cooling, 
#   fp_num, numstories, bedroom, fullbath, halfbath, totalrooms, zip, 
#   subdivision, lastsaleprice, lastsaledate

# improvementsvalue
summary(homes$improvementsvalue) # no missing, some 0s

# create a tmp file with ImprovementsValue == 0, arrange the file by FinSqFt
tmp <- homes %>% 
  filter(improvementsvalue == 0) %>% 
  arrange(finsqft)
# hmmm, keep for now


# landvalue
summary(homes$landvalue)

homes %>% # check 0s
  filter(landvalue == 0) %>% 
  select(usecode, owner, finsqft, lotsize, improvementsvalue, totalvalue)
# all but one are owned by communication companies; related to towers (rather than land), I think...

# remove the rows with landvalue = 0
homes <- homes %>% 
  filter(landvalue > 0)


# landusevalue
summary(homes$landusevalue)
homes <- homes %>% # create binary indicator for land use
  mutate(landuse = if_else(landusevalue > 0, 1, 0)) %>% 
  select(-landusevalue) # remove variable
table(homes$landuse)


# cardtype
table(homes$cardtype)
homes %>% 
  filter(cardtype == "C") %>% 
  select(yearbuilt, usecode, finsqft, totalrooms, owner, improvementsvalue) 
homes <- homes %>% filter(cardtype == "R") %>% # keep only R
  select(-cardtype) # and remove column


# usecode
table(homes$usecode) # consider reducing to just single family...


# cooling
table(homes$cooling) # make a factor
homes <- homes %>% 
  mutate(cooling = factor(cooling, levels = c("No Central Air", "Central Air")))


# fp_num
table(homes$fp_num) # make a binary indicator
homes <- homes %>% 
  mutate(fp = if_else(fp_num > 0, 1, 0))


# numstories
table(homes$numstories) # realize I don't know what this means; was expecting 1, 2, 3, etc.. Let's drop it
homes <- homes %>% select(-numstories)


# bedroom, fullbath, halfbath, totalrooms 
table(homes$bedroom) # 228 homes with no bedroom seems high, use with caution
table(homes$fullbath) # 263 homes with no full bath seems high, use with caution
table(homes$halfbath) # ok
table(homes$totalrooms) # 1310 homes with no rooms is definitely a coding error, use with caution


# zip 
summary(homes$zip) # 256 missing
table(homes$city, homes$zip) # basically replicates City


# subdivision 
table(homes$subdivision) # would need more work and understanding to be useful
sum(is.na(homes$subdivision)) # 9,393 missing; is it reasonable to consider these more rural?
homes <- homes %>% 
  mutate(insub = if_else(is.na(subdivision), 0, 1)) %>% 
  select(-subdivision)


# lastsaleprice, lastsaledate
summary(homes$lastsaledate)

summary(homes$lastsaleprice)
tmp <- homes %>% 
  filter(lastsaleprice == 0) %>% 
  arrange(lastsaledate) # last sale date is prior to year built for many

homes %>% 
  mutate(datecheck = if_else(yearbuilt > as.integer(year(lastsaledate)), 1, 0)) %>% 
  filter(datecheck == 1 | yearbuilt == 0) %>% 
  tally()
# so yearbuilt (or lastsaledate) is wrong for at least 3472 records


## YOUR TURN
# 1. How many homes were remodeled (yearremodeled)


# 2. Create a binary variable (remodel) that indicates if a home has been remodeled




################################################
# 6. Clean up and save ----
# remove owner, propname, souce.x, source.y -- these were for examining the data
homes <- homes %>% 
  select(-c(cardnum, owner, propname, cards))


# remove unneeded objects from environment
rm(tmp, city_age, res, vars, cond_levels)


# save everything to working directory
save.image("albemarle_homes.Rdata") 
# load("albemarle_homes.Rdata")

# save just the homes data frame 
saveRDS(homes, file = "albemarle_homes.rds") 
# readRDS("albemarle_homes.rds")

# save a csv file of the homes data
write_csv(homes, path = "albemarle_homes.csv") 
# homes <- read_csv("albemarle_homes.csv")



################################################
# 7. Explore data further ----

# Does average house size (finsqft) appear related to assessed condition (condition)?
homes %>% 
  group_by(condition) %>% 
  summarize(mean(finsqft))


## YOUR TURN
# 3. Does average property value (improvementsvalue) appear related to assessed condition (condition)?


# 4. Do median property value (improvementsvalue), land value (landvalue), or 
# ... total assesed value (totalvalue) appear related to to city?


# 5. Find the proportion of homes remodeled in each city 
# ... and sort the cities by this proportion


# 6a. Find the number of homes remodeled by year (yearremodeled)
# ... save this to a data frame and call the count of remodeled homes "year_count"


# Plot the result
ggplot(remodels, aes(x = yearremodeled, y = year_count)) + geom_line()

# 6b. What are the peak years?
