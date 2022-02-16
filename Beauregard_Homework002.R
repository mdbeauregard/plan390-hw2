## Assignment 002 = PLAN 390
## Restaurant Inspections
## 16 February 2022

## First, load the tidyverse and lubridate libraries.
library(tidyverse)
library(lubridate)

# First, read and create an object for the restaurant inspections data.

restaurant_inspections = read_csv("restaurant_inspections.csv")

# 1. Visualize the overall distribution of inspection scores.

# By creating a histogram, we can see the distrubtion of sanitation scores in a visually accessible format.

hist(restaurant_inspections$SCORE,
     main="Facility Sanitation Scores in Wake County, NC",
     xlab="Facility Sanitation Scores",
     )

# This distribution shows that the vast majority of scores are concentrated in the 80-90 and 90-100
# brackets with some apparent outliers in the 0-10 bracket. According to the data from restaurant_inspections,
# this outlier appears to be Bru's Public House, which has a sanitation score of 0.0.
       
# 2. Some restaurants have been in business much longer than others. Is there any trend in
# terms of how highly older vs. newer restaurants score on their inspections?

# Because the dates are just characters at this point, we have to convert them into actual, usable dates
# using lubridate functions.

restaurant_inspections = mutate(restaurant_inspections, RESTAURANTOPENDATE=date(RESTAURANTOPENDATE))

# then we use the group_by function to group the data points by date and sanitation score, so we can see if
# there is any trend between the two.

plot(restaurant_inspections$RESTAURANTOPENDATE, restaurant_inspections$SCORE,
     ylim = c(80,100),
     xlab = "Restaurant Opening Date",
     ylab = "Restaurant Sanitation Score"
     )

# According to this graph, there appears to be little, if only a slight negative correlation between
# a restaurant's opening date and its sanitation score. There seems to be a trend of more newer restaurants
# having lower sanitation scores, but it is by no means the majority of establishments.

# I limited the y axis to only include scores between 80 and 100 points because this made the
# potential trends more visible. 

# 3. Wake County is the most populous county in North Carolina, and there are many cities
# in it. Do the inspection scores vary by city? Note that the city column contains some
# differently spelled city names; make sure to clean those up so that there is only one
# estimated value per city. The recode function that we used for creating a
# weekend/weekday variable in the SFpark exercise will be useful here, and you may also
# be interested in the str_to_upper function.

# By using the 'unique()' function, we we can see that a number of cities are listed 
# in non-capitalized and fully capitalized forms. 
# Restaurants within the town of Holly Springs are listed as being in "HOLLY SPRINGS",
# "HOLLY SPRING" and "Holly Springs". Restaurants in Fuquay-Varina are listed as being
# in "FUQUAY VARINA", "Fuquay Varina", FUQUAY-VARINA", and "Fuquay-Varina". There are some
# other mispellings that need to be replaced. By using the recode function, we can replace
# these instances with the correct spellings

restaurant_inspections$CITY = recode(restaurant_inspections$CITY, "Cary"="CARY",
                                     "Holly Springs"="HOLLY SPRINGS",
                                     "HOLLY SPRING" = "HOLLY SPRINGS",
                                     "FUQUAY VARINA" = "FUQUAY-VARINA",
                                     "Fuquay Varina" = "FUQUAY-VARINA",
                                     "Fuquay-Varina" = "FUQUAY-VARINA",
                                     "Zebulon" = "ZEBULON",
                                     "MORRISVILE" = "MORRISVILLE",
                                     "Morrisville" = "MORRISVILLE",
                                     "RESEARCH TRIANGLE PARK" = "RTP",
                                     "Raleigh" = "RALEIGH",
                                     "Apex" = "APEX",
                                     "Garner" = "GARNER",
                                     "Wake Forest" = "WAKE FOREST"
                                     )

# With the data cleaned up we can now see how scores vary by city. Using the group_by function,
# we can create a dataset that contains the cities with their associated average scores.

city_averages = group_by(restaurant_inspections, CITY) %>%
        summarize(SCORE=mean(SCORE))

# And now we can create a table that can display this data in an accessible format.

View(averages)

# According to these data, all cities in Wake County have similar mean sanitation scores.

# 4. Wake County employs a whole team of inspectors. It is possible that some inspectors
# may be more thorough than others. Do inspection scores vary by inspector?

# Using roughly the same code from the previous question, we can find out the average
# scores for each inspector.

inspector_averages = group_by(restaurant_inspections, INSPECTOR) %>%
        summarize(SCORE=mean(SCORE))

View(inspector_averages)

# Most inspectors have mean scores that range from 94.0 to 100. However, one inspector,
# Thomas Jumalon, seems to have issued a mean score of 89.0. 

# 5. It is possible that some extreme results from the previous questions are due to small
# sample sizes in a particular city, for a particular inspector, or in a particular time period.
# Look at the sample sizes in each of your groups. Do you think this is an explanation for
# the results you came to above? 

# The cities average scores were fairly similar. In the original data, however, there was a notable
# outlier that appeared to be a score of 0, whereas most scores ranged between 80 and 100.
# Looking through the data, we can find that there was indeed one score of 0.0, given to
# Bru's Public House in 2021. 

# Thomas Jumalon, acknowledged in question 4, had a much lower average sanitation score than
# his fellow inspectors. Looking through the data, it appears that Mr. Jumalon had only
# inspected three facilities, whereas most other inspectors had visited far more.

## 6. The data file contains records for many types of food-service facility (e.g. restaurants,
## food trucks, etc.). Are the scores for restaurants higher than other types of facility?

facility_averages = group_by(restaurant_inspections, FACILITYTYPE) %>%
        summarize(SCORE=mean(SCORE))

View(facility_averages)

# Using code similar to that which we used in questions 3 and 4, we can see that there is little
# variation in the scores of each facility type. The average scores for each facility lie between
# 96 and 100, and none is notably lower or higher than the rest. The average score for restaurants,
# however, is lower than the rest. That being said, the average score is still upwards of 96.

## 7. Since restaurants are where the general public is most likely to interact with the food-
## service system, Wake County Public Health is particularly interested in sanitation in
## restaurants. Repeat the analyses above (1-5) for restaurants specifically.

# In order to answer this question, we first want to filter the data so that we are only
# working with the data relevant to restaurants' sanitation scores.

restaurant_data = filter(restaurant_inspections, FACILITYTYPE == "Restaurant")

# Then, we simply repeat the code from the earliers steps, edited to use this new object.

hist(restaurant_data$SCORE,
     main="Facility Sanitation Scores in Wake County, NC",
     xlab="Facility Sanitation Scores",
)

# This looks very similar to the distribution of scores overall.

plot(restaurant_inspections$RESTAURANTOPENDATE, restaurant_inspections$SCORE,
     ylim = c(80,100),
     xlab = "Restaurant Opening Date",
     ylab = "Restaurant Sanitation Score"
)

# The restaurant plot showing sanitation scores vs opening dates is also very similar.

city_r_averages = group_by(restaurant_data, CITY) %>%
        summarize(SCORE=mean(SCORE))

View(city_r_averages)

# The average scores for each city are also fairly similar. New Hill has an average score
# of 100, which is likely the result of there only being one data point. 

inspector_r_averages = group_by(restaurant_data, INSPECTOR) %>%
        summarize(SCORE=mean(SCORE))

View(inspector_r_averages)

# The inspectors' averages are also quite similar. As in the regular data set, inspector
# Thomas Jumalon's mean score is noticeably lower than that of the other inspectors due
# to the small number of inspections he has carried out.