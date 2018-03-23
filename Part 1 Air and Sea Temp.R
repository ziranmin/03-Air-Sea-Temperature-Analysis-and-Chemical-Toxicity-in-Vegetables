
# MA415/615
# Assignment 5, Porject 2
# 03/19/2018
# Name: Ziran Min, Xi Cheng, ShihChing Huang

library(tidyverse)
library(stringr)
options(warn = -1)
########################################################################
#Step 1: Import the data of 30 years from website and combine to one set
########################################################################

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1988:2017)

# We don't have the data for 2013, so we delete 2013 from the years list
years <- years[-26]

urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("mr", years, sep = "")

N <- length(urls)


for (i in 1:N){
  suppressMessages(assign(filenames[i], read_table(urls[i], col_names = TRUE)))
  file <- get(filenames[i])
  colnames(file)[1] <-"YYYY"
  
  # before 2005 the data don't have minute(mm) column, we add them back
  if (i <= 17) {file <- file %>% mutate(mm = "00")}
  
  # since 2007, the data has a redundant first row about unit, we delet them 
  if (i >= 20) {file <- file[-1,]}
  
  # we only need column about time and air & sea temperature
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  # put '19' in front of 2 digit years
  if (i <= 11) {file[1] <- i + 1987}
  
  if(i == 1){Whole_Set <- file}
  else{Whole_Set <- rbind.data.frame(Whole_Set, file)}
  
}

#############################################################
# Step 2: Clean the data and select the data recorded at noon
#############################################################

# Before 2009, every time of record took place at mm = 00
# Since 2009, every time of record took place at mm = 50
# So the following is how we get the data that only recorded at noon from the whole data set

Noon_Data <- Whole_Set %>% filter((hh == "11" & mm == "50") | (hh == "12" & mm == "00"))

# Change sea temp and air temp to numeric form
Noon_Data$ATMP <- as.numeric(Noon_Data$ATMP)
Noon_Data$WTMP <- as.numeric(Noon_Data$WTMP)

# Convert all missing data (which is 999 or 99) into NA form
Noon_Data$ATMP <- ifelse(Noon_Data$ATMP > 90, NA, Noon_Data$ATMP)
Noon_Data$WTMP <- ifelse(Noon_Data$WTMP > 90, NA, Noon_Data$WTMP)

# Combine year, month, and day to one Data column
Noon_Data <- unite(Noon_Data, Date, YYYY, MM, DD, sep = "-")
Noon_Data$Date <-as.Date(Noon_Data$Date)


#############################################################
# Step 3: Data Visualization and Answering to Questions
#############################################################


# Time Series for Air Temperature
ggplot(Noon_Data, aes(Date, ATMP)) + geom_line() +
  geom_line(col = "red") +
  labs(x = "Date (Shown Year Here)", y = "Temperature (Celcius Degree)",
        title = "Time Series of Air Temperature (Daily Noon Data)")
  
# Time Series for Sea Temperature
ggplot(Noon_Data, aes(Date, WTMP)) + geom_line() +
  geom_line(col = "blue") +
  labs(x = "Date (Shown Year Here)", y = "Temperature (Celcius Degree)",
        title = "Time Series of Sea Temperature (Daily Noon Data)")

# Now we notice that not only the data in whole year of 2013 is missing, the data in 2012 is missing
# as well. To deal with this "big gap" in the time series chart. One solution we came up with, is to
# find the data in 2012 and 2013 of another buoy which is the closest to buoy 46035. 
# For example buoy 46070 is close to buoy 46035, we can add the data of buoy 46070 in 2012 and 2013
# into our whole data set by slightly changing the code, like adding an another url variables and 
# assigning the file of 2012 and 2013 into the new links 


# Put them together
ggplot(Noon_Data, aes(Date)) + 
  geom_line(aes(y = ATMP, col = "ATMP")) + 
  geom_line(aes(y = WTMP, col = "WTMP")) +
  scale_colour_manual(values=c("red", "blue")) +
  labs(x = "Date (Shown Year Here)", y = "Temperature (Celcius Degree)",
       title = "Time Series of Air & Sea Temperature (Daily Noon Data)")

# The combined time series chart, we think air temp and sea temp are correlated
# We want to comfirm our observation is true

# By looking the plot between ATMP and WTMP, we assume a strong correlation again
ggplot(Noon_Data) + 
  geom_point(mapping = aes(x = ATMP, y = WTMP)) +
  labs(x = "Daily Noon Air Temp (Celcius)", 
       y = "Daily Noon Sea Temp (Celcius)",
       title = "Scatter Plot to See the Correlation between ATMP and WTMP")

ggplot(Noon_Data) + 
  geom_smooth(mapping = aes(x = ATMP, y = WTMP)) +
  labs(x = "Daily Noon Air Temp (Celcius)", 
       y = "Daily Noon Sea Temp (Celcius)",
       title = "Smooth Line to See the Correlation between ATMP and WTMP")


# By conducting Pearson Correlation Test, 
# we find ATMP and WTMP have coefficient of correlation of 0.8774953
# Therefore, we prove that ATMP and WTMP do have strong correlation
cor.test(Noon_Data$ATMP, Noon_Data$WTMP, method = "pearson")


# To see whether the mean air temp changed over 30 years, we need to do t-test
# Creating datasets which are only for 1988 and 2017
Noon_1988 <- separate(Noon_Data, Date, into = c("YYYY", "MM", "DD"), sep = "-")
Noon_1988 <- filter(Noon_1988, YYYY == 1988)

Noon_2017 <- separate(Noon_Data, Date, into = c("YYYY", "MM", "DD"), sep = "-")
Noon_2017 <- filter(Noon_2017, YYYY == 2017)

t.test(Noon_1988$ATMP, Noon_2017$ATMP)
# Because the p-value = 5.153e-13 < 0.01, we reject the null hypothesis that the means of 
# ATMP in 1988 and in 2017 are equal and conclude that air temp indeed changed over 30 years
# means in two years: 2.330748  4.645179

# To see whether the mean sea temp changed over 30 years, we need to do t-test
# Creating datasets which are only for 1988 and 2017
t.test(Noon_1988$WTMP, Noon_2017$WTMP)
# Because the p-value = 9.035e-13 < 0.01, we reject the null hypothesis that the means of 
# WTMP in 1988 and in 2017 are equal and conclude that sea temp indeed changed over 30 years
# means in two years: 4.572981  6.141525

# However, above we only used the daily noon data.
# Let's go back to the whole data set and do the t-test again to see if we get different result


# By using the whole dataset for each year, we do t-test for the mean of ATMP again
mr2017 <- mr2017[-1,]
mr1988$ATMP <- as.numeric(mr1988$ATMP)
mr2017$ATMP <- as.numeric(mr2017$ATMP)

mr1988$ATMP <- ifelse(mr1988$ATMP > 90, NA, mr1988$ATMP)
mr2017$ATMP <- ifelse(mr2017$ATMP > 90, NA, mr2017$ATMP)

t.test(mr1988$ATMP, mr2017$ATMP)
# Because the p-value p-value < 2.2e-16 < 0.01, we reject the null hypothesis that the means of 
# ATMP in 1988 and in 2017 are equal and conclude that air temp indeed changed over 30 years.
# So my sampling doesn't affect my evaluation of air temperature change.
# means in two years: 2.438602  4.714333 

# By using the whole dataset for each year, we do t-test for the mean of WTMP again
mr1988$WTMP <- as.numeric(mr1988$WTMP)
mr2017$WTMP <- as.numeric(mr2017$WTMP)

mr1988$WTMP <- ifelse(mr1988$WTMP > 90, NA, mr1988$WTMP)
mr2017$WTMP <- ifelse(mr2017$WTMP > 90, NA, mr2017$WTMP)

t.test(mr1988$WTMP, mr2017$WTMP)
# Because the p-value p-value < 2.2e-16 < 0.01, we reject the null hypothesis that the means of 
# WTMP in 1988 and in 2017 are equal and conclude that sea temp indeed changed over 30 years.
# So my observed result of sampling sea temerature changing tallies with the real result of population.
# means in two years: 4.608660  6.156853 

# The reason why choosing daily noon data and whole hourly data gave us the same result of "changing mean"
# is that getting one noon data every day has already created a big dataset for a year (365 samples > 30 samples).
# Therefore, the results from daily noon data shows the situation under the normal distribution, 
# which approches to the population data (Whole Set).
# Plus, each temperature recorded is correlacted with the last and next temperaature recorded, 
# and it shows the concept of time series.












