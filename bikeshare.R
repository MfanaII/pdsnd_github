```{r}
getwd()
list.files()

library(ggplot2)
library(lubridate)

nyc = read.csv('new-york-city.csv')
was = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(nyc)
head(was)
head(chi)

# anylized all 3 datasets - adding 2 columns on Washington so the layout is the same ('Gender' and 'Birth.Year')
was$Gender <- NA
was$Birth.Year <-NA

# As all the 3 datsets are in the same layout - to combine them and make as 1 dataset - adding a column 'Location' on each dataset.
nyc$City <- 'New York City'
was$City <- 'Washington'
chi$City <- 'Chicago'


# Concatenating all three datasets together as "Location"
Location <- rbind(nyc,chi,was)

head(Location)

table(Location$Gender)

# QUESTION 1
ggplot(aes(x = Gender, fill = City), data = subset(Location, !is.na(Gender)) +
  geom_bar(position = 'dodge', colour="black") +
  ggtitle('Counts of each gender') +
  scale_x_discrete(labels = c('gender not specified', 'Female', 'Male')) +
  labs(y = 'Number of Riders', x = 'Gender') +
  scale_fill_manual("legend", values = c("Chicago" = "green", "New York City" = "blue"))

# QUESTION 2
# Which City takes long trips?

#let's the summary of all the trips duration
summary(Location$Trip.Duration)

# here separate trips duration per city
ggplot(aes(x = Trip.Duration, y = City), data = Location) +
  geom_histogram(position = 'dodge', stat = "summary", fun.y = "mean", fill = c("blue","white", "red"), colour="black") + 
  ggtitle('The travel duration in different cities') +
  labs(y = 'Travel per City', x = 'duration count') +
  coord_flip()


ggplot(aes(x = Trip.Duration, y = City), data = Location) +
  geom_histogram(position = 'dodge', stat = "summary", fun.y = "mean", fill = c("blue","white", "red"), colour="black") + 
  ggtitle('The travel duration in different cities') +
  labs(y = 'Travel per City', x = 'duration count') +
  coord_flip()

#QUESTION 3
#Are male riders younger or older than female in in all the 3 Cities?  

#Running a descriptive summary to compare distributions across the two groups
by(Location$Birth.Year, Location$Gender, summary)

## A first initial Plot of the distribution of birth year by gender in New York, Chicago & Washington
qplot(x = Gender, y = Birth.Year, 
      data = subset(Location, !is.na(Gender)), 
      geom = 'boxplot') + 
  ggtitle('Distribution of Birth Year of riders by Gender in All cities')

##Plotting again the distribution of birth year by gender by zooming (Birth year between 1962 and 1990)
qplot(x = Gender, y = Birth.Year, 
      data = subset(Location, !is.na(Gender)), 
      geom = 'boxplot') + 
  coord_cartesian(ylim = c(1962,1990))+
  ggtitle('Distribution of Birth Year of riders by Gender in all cities (Birth year between 1962 and 1990)')

# or we can use this for question 3

QUESTION 3 
How old are the people renting bikes?
  To answer this question - Washington dataset is excluded as it does not have Birth information

calcAge <- function(Location_) {
  # Calculate user age by taking the difference between year when they rode vs their birth year.
  Location_$Age <- strtoi(strftime(Location_$Start.Time, format="%Y")) - Location_$Birth.Year
  Location_
}
ny <- calcAge(nyc)
chi <- calcAge(chi)

# Remove the previously created Weekday field for `chi`.
chi <- subset(chi, select = -c(Weekday))


City2 <- rbind(ny, chi)

ggplot(aes(x=Age, fill=City), data=City2) +
  geom_bar(position='dodge') +
  theme(text = element_text(size = 20)) +
  ggtitle("Age of riders") +
  scale_x_continuous(breaks = seq(0, 150, by = 5)) +
  labs(x = "Age (years)")

summary(City2$Age)

**Summary of your question 3 results goes here.**
  This indicates that there are invalid birth information
Looking at the Median - it is showing that riders around the age of 36 are main customers


```