#Loading data in a dataframe uber_cars
uber_cars <- read.csv("Uber Request Data.csv", stringsAsFactors = F)

#1. Data Cleaning
#Checking the the uber_cars dataframe
str(uber_cars)
summary(uber_cars)

#Converting request.id, pickup.point, driver.id & status of the columnes from nnumeric to factor
uber_cars[, c("Request.id", "Pickup.point", "Driver.id", "Status")] <- lapply(uber_cars[, c("Request.id", "Pickup.point", "Driver.id", "Status")], factor)

#Confirming that the columns has been converted to factors
str(uber_cars)
summary(uber_cars)

#Looking for total number of NA Values & then column wise NA values
sum(is.na(uber_cars))
colSums(is.na(uber_cars))
# The NA Values are for two columns only driver.id & drop.timestamp. The NA values are legitimate as status no cars available results to no drivers and not completed trips results to no values in drop.timestamp

#Checking for empty values
sapply(uber_cars, function(x) length(which(x == "")))
sapply(uber_cars, function(x) length(which(x == " ")))
#No empty values

#Copying the data time columns & converting / to - for the date time columns for uniformity
uber_cars$Request_timestamp <- uber_cars$Request.timestamp
uber_cars$Drop_timestamp <- uber_cars$Drop.timestamp
uber_cars$Request_timestamp <- gsub("/", "-", uber_cars$Request_timestamp)
uber_cars$Drop_timestamp <- gsub("/", "-", uber_cars$Drop_timestamp)

#Converting the date time values in standard format
uber_cars$Request_timestamp <- as.POSIXlt(uber_cars$Request_timestamp, tz = "", format = "%d-%m-%Y %H:%M")
uber_cars$Drop_timestamp <- as.POSIXlt(uber_cars$Drop_timestamp, tz = "", format = "%d-%m-%Y %H:%M")

#Delimit date time columns to date & time in separte columns
uber_cars$Request.Time <- format(uber_cars$Request_timestamp, "%H:%M:%S")
uber_cars$Drop.Time <- format(uber_cars$Drop_timestamp, "%H:%M:%S")
write.csv(uber_cars, "uber_Clean.csv")

#EDA
#Checking what is the count of cancellation and no cars available throughout the data
library(ggplot2)
library(scales)
require(gridExtra)

freq_plot <- ggplot(as.data.frame(table(uber_cars$Status)), aes(x = Var1, y = Freq)) + 
  geom_bar(fill = "#FF6666", stat = "identity") + 
  ggtitle("Number of Rides By Status") + xlab("Status") + ylab("Number of Rides") + 
  geom_text(aes(label = Freq))

percent_plot <- ggplot(uber_cars, aes(x = Status)) +  
  geom_bar(fill = "#0112FF", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + 
  ggtitle("Percentage of Rides By Status") + xlab("Status") + ylab("Percentage of Rides")

cowplot::plot_grid(freq_plot, percent_plot, labels = "AUTO")
#Observations
#1. 58% of the time the trip is either cancelled or there is a shortage of supply. This means uber is losing out of revenue on 58% of the times
#2. Out of these 58%, 39% of the times there is shortage of cabs. This means there is a huge lag in supply vs demand

#Checking the loss of the trips by hours to analyze the hours when the problem is highest
ggplot(uber_cars, aes(format(Request_timestamp, "%H"), fill = Status)) + 
  geom_histogram(stat = "count") +
  ggtitle("Hour of the Day vs Number of Rides") + 
  xlab("Hours of the Day") + 
  ylab("Number of Rides")


#Observations: There are three obervations:
#1. Number of cancellations starts increase between 4 AM in morning to 10 AM. Between 5 AM to 9 AM the number of cancellations are the highest
#2. There is always a lack of supply of cars throughout the day 
#3. The supply shortage starts to peak in the evening and night hours from 5 PM to 12 AM

#Observing the number of request from city or airport in day
ggplot(uber_cars, aes(format(Request_timestamp, "%H"), fill = Pickup.point)) + 
  geom_histogram(stat = "count") + 
  ggtitle("Number of request in a Day") + 
  xlab("Hours of the Day") + 
  ylab("Number of Rides")

#Observations: The number of request from city is higher in morning hours and number of request from Airport is higher in evening and late night hours

#Checking the loss of trips on basis of pick-up points
airport_stats <- subset(uber_cars, Pickup.point == "Airport")
city_stats <- subset(uber_cars, Pickup.point == "City")

require(gridExtra)
p1 <- ggplot(airport_stats, aes(format(Request_timestamp, "%H"), fill = Status)) + 
  geom_histogram(stat = "count") + 
  ggtitle("Pickup point: Airport") + 
  xlab("Hours of the Day") + 
  ylab("Number of Rides")

p2 <- ggplot(city_stats, aes(format(Request_timestamp, "%H"), fill = Status)) + 
  geom_histogram(stat = "count") + 
  ggtitle("Pickup point: City") + 
  xlab("Hours of the Day") + 
  ylab("Number of Rides")
cowplot::plot_grid(p1, p2, labels = "AUTO")

#Observations: When the Pick-up point is City
#1. There is a lack of supply vs demand in the city throughout the day
#2. The number of cancellations peaks from 4 AM to 10 AM

#Observations: When the Pick-up point is Airport
#1. There is a sudden rise in demand of cab from 5 PM to 11 PM
#2. As a result the supply of cabs low resulting in no cabs available status during this time period

#Conclusion
#The increase in the cancellations in city in morning hours is may be due to the fact that there are more outgoing flights in the morning hours which surges the demand of trip from City to Airport and very less incoming flights at the same time which results in less demand for trips to city
#In the morning hours, the non-availability of cabs is very low at Airport which means there are sufficient number of cabs at the Airport to meet the demands of the morning
#Therefore if no drivers cancels trips from city and completes the trip to Airport, there will be lack of demand which will result in long waiting hours before the get the next trip
#This results into the drivers cancelling the trips in the morning hours

#2.The increase in the non-availability of cabs at the airport in evening hours is may be due to the fact that there are more incoming flights in the evening hours which surges the demand of trip from Airport to City and less outgoing flights at the same time which results in less demand for trips to Airport from city
#In the evening hours, the number of rides from City to Airport is low which results in less cabs going for Airport from City in the evening hours
#While at the Airport the due to more number of incoming flights the is a surge in demand of cabs back to city
#This results in non-availability of cabs at the Airport in the evening hours

#Recommendations:
#1. Offering incentives like 25% more per KM for trips to & from Airport
#2. Increasing the request of trips from Airport and nearby areas in the morning hours. This can be done by offering discounts when they book cabs to city from Airport or nearby areas in the morning hours. This is based on assuming the fact that all customers do not opt for Uber from Airport or nearby locations.
#3. Offering incentives like 25% more per KM for night trips from Airport
