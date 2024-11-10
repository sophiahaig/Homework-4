
# I used read.csv to upload the given dataset from my Plan372 folder
data <- read.csv("/Users/sophiahaig/Desktop/PLAN372/hw 4/airport_pairs.csv")

#Question 1: 

# I filter the data to include only rows where RDU is either the origin or destination airport,
# then grouped the data by origin and destination to organize by each route including RDU.
# I summarize the data to keep only the passengers column for each route, 
# and finally filter to show only routes with more than 10,000 passengers.
rdu = data %>%
  filter(origin == "RDU" | dest == "RDU") %>% 
  group_by(origin, dest) %>% 
  summarise(passengers = passengers) %>% 
  filter(passengers > 10000)
print(rdu)
write.csv(rdu, "rdu_routes.csv", row.names = FALSE)

# Question 2:
# First, I used install.packages("tidyverse") and install.packages("tidycensus"). 
library(tidycensus)
library(tidyverse)

# Then, I used census_api_key and copied and pasted the API key I got from online. 
census_api_key("3d88e6ed357f2a2743b6db04c64e5b5e1fc831f6")

# I then set the variables I wanted to use of: gender, income, and population to retrieve specific demographic data from the Census API. 
# This is done by assigning each variable a unique Census code.
variables <- c(
  population = "B01003_001",
  median_income = "B19013_001",
  poverty_population = "B17001_002",
  male_population = "B01001_002",
  female_population = "B01001_026"
)

# I retrieved the CBSA-level data from the Census API for the year 2020, using the ACS 5-year survey and the variables I just defined.
cbsa_data <- get_acs(
  geography = "cbsa",
  variables = variables,
  year = 2020,
  survey = "acs5"
)


# I reshaped the CBSA data to prepare it for merging by selecting only the GEOID, NAME, variable, and estimate columns,
#The spread(key = variable, value = estimate) function converts the variable column values into new columns, filled with the corresponding estimate values.
cbsa_data <- cbsa_data %>%
  select(GEOID, NAME, variable, estimate) %>%
  spread(key = variable, value = estimate)

# I created a dataset for origin CBSA data by selecting key demographic columns (GEOID, population, median_income, poverty_ population, male/ female population),
# then rename these columns to indicate that they refer to origin CBSA data, with "origin_" prefixes for clarity.
cbsa_origin <- cbsa_data %>%
  select(GEOID, population, median_income, poverty_population, male_population, female_population) %>%
  rename(
    origin_cbsa = GEOID,
    origin_population = population,
    origin_median_income = median_income,
    origin_poverty_population = poverty_population,
    origin_male_population = male_population,
    origin_female_population = female_population
  )


# I created a dataset for destination CBSA data by selecting the same key demographic columns as before (GEOID, population, median_income, etc.),
# then renamed these columns to indicate they refer to destination CBSA data, using "dest_" prefixes for clarity.
cbsa_dest <- cbsa_data %>%
  select(GEOID, population, median_income, poverty_population, male_population, female_population) %>%
  rename(
    dest_cbsa = GEOID,
    dest_population = population,
    dest_median_income = median_income,
    dest_poverty_population = poverty_population,
    dest_male_population = male_population,
    dest_female_population = female_population
  )


# I merged the main dataset "data" with the origin CBSA dataset (cbsa_origin) using "origin_cbsa" as the key column,
# keeping all rows in the main dataset by setting all.x = TRUE, which ensures a left join.
data <- merge(data, cbsa_origin, by = "origin_cbsa", all.x = TRUE)

# I merged the main datase "data" with the destination CBSA dataset (cbsa_dest) using "dest_cbsa" as the key column,
# keeping all rows from the main dataset by setting all.x = TRUE.
data <- merge(data, cbsa_dest, by = "dest_cbsa", all.x = TRUE)

# I grouped the data by both the origin and destination CBSA, then calculated the total number of passengers 
# for each origin-destination pair by summing the passenger counts. 
# I set any missing values with na.rm = TRUE.
cbsa_volumes <- data %>%
  group_by(origin_cbsa, dest_cbsa) %>%  # Group by origin and destination CBSA
  summarize(total_passengers = sum(passengers, na.rm = TRUE))  # Sum the volume for each CBSA-to-CBSA pair

# I created an empty column for population and then cycled through the other data set to grab each place’a population and add it to the new data set

cbsa_volumes$pop = NA

# I looped through each row of the 'cbsa_volumes' data frame. 
# I checked  if there are any matching 'dest_cbsa' values in the 'data' data frame
# If there is a match, I assign the corresponding 'origin_population' value from 'data' 
# to the 'pop' column of 'cbsa_volumes' based on the matching 'origin_cbsa'
for(i in 1:nrow(cbsa_volumes)){
  if(length(which(cbsa_volumes$dest_cbsa[i] == data$dest_cbsa)) > 0){
  cbsa_volumes$pop[i] = data$origin_population[which(cbsa_volumes$origin_cbsa[i] == data$origin_cbsa)]
  }
}


# I created an empty column for destination population and then cycled through the other data set to grab each places population and add it to the new data set
cbsa_volumes$dest_pop = NA

# I looped through each row in the 'cbsa_volumes' data frame
# I checked if there are any matching 'dest_cbsa' values in the 'data' data frame
# If a match is found, I assigned the corresponding 'dest_population' value from 'data'
# to the 'dest_pop' column of 'cbsa_volumes' based on the matching 'dest_cbsa'

for(i in 1:nrow(cbsa_volumes)){
  if(length(which(cbsa_volumes$dest_cbsa[i] == data$dest_cbsa)) > 0){
  cbsa_volumes$dest_pop[i] = data$dest_population[which(cbsa_volumes$dest_cbsa[i] == data$dest_cbsa)]
}
}

# I loaded the ggplot2 library.
library(ggplot2)

# Scatterplot: Origin Population vs Total Passengers
# I created a scatterplot to visualize the relationship between origin population and total passengers
# I then plotted the points.
# I added a title and axis labels.
# I used minimal theme for a clean look 

ggplot(data, aes(x = origin_population, y = passengers)) +
  geom_point() + 
  labs(title = "Origin Population vs Total Passengers", 
       x = "Origin Population", 
       y = "Total Passengers") +
  theme_minimal()

# Scatterplot: Destination Population vs Total Passengers
# I created a scatterplot to visualize the relationship between origin population and total passengers
# I then plotted the points.
# I added a title and axis labels.
# I used minimal theme for a clean look 
ggplot(data, aes(x = dest_population, y = passengers)) +
  geom_point() + 
  labs(title = "Destination Population vs Total Passengers", 
       x = "Destination Population", 
       y = "Total Passengers") +
  theme_minimal()

# Scatterplot: Flight Distance vs Total Passengers
# I created a scatterplot to visualize the relationship between origin population and total passengers
# I then plotted the points.
# I added a title and axis labels.
# I used minimal theme for a clean look 
ggplot(data, aes(x = distancemiles, y = passengers)) +
  geom_point() + 
  labs(title = "Flight Distance vs Total Passengers", 
       x = "Flight Distance (miles)", 
       y = "Total Passengers") +
  theme_minimal()



# Question 3

#I used mod =lm to create a linear regression model to predict the number of passengers based on various demographic and geographic factors listed after the lm(. 

mod = lm(passengers ~ origin_population + dest_population + distancemiles + origin_poverty_population +origin_male_population + origin_population + origin_median_income + dest_population + dest_median_income + dest_poverty_population + dest_male_population, data = data)
# I used summary to view the coefficients. 
summary(mod)



# Question 4 
# I extracted the population data for specific origin airports from the dataset. 
# Each line finds the rows where the origin airport is "RDU" or other and selects the first match from the origin_population column.

raleigh_pop = data$origin_population[which(data$origin == "RDU")][1]
elp_pop = data$origin_population[which(data$origin == "ELP")][1]
portland_pop = data$origin_population[which(data$origin == "PDX")][1]
th_pop = data$origin_population[which(data$origin == "TLH")][1]
sac_pop = data$origin_population[which(data$origin == "SMF")][1]

#I created a new data frame called new_routes from RDU to selected destinations. 
new_routes <- data.frame(
  origin_cbsa_name = rep("Raleigh-Durham, NC", 4),
  dest_cbsa_name = c("Portland, OR", "El Paso, TX", "Tallahassee, FL", "Sacramento, CA"),
 # I set distance in miles from Raleigh-Durham (RDU) to each destination
   distancemiles = c(2363, 1606, 496, 2345),
 # Population of Raleigh-Durham, using the pre-assigned variable raleigh_pop from the cbsa data. 
  origin_population = c(raleigh_pop),  
 #I set the populations for each destination, using the pre-made variables for each destination's population.
  dest_population = c(portland_pop, elp_pop, th_pop, sac_pop),
 # I set the population in poverty at RDU, for each destination's data, using values in the cbsa dataset. 
  origin_poverty_population = c(data$origin_poverty_population[which(data$origin == "PDX")][1],data$origin_poverty_population[which(data$origin == "ELP")][1], data$origin_poverty_population[which(data$origin == "TLH")][1], data$origin_poverty_population[which(data$origin == "SMF")][1]),
# Male population for each destination 
 origin_male_population = c(data$origin_male_population[which(data$origin == "PDX")][1],data$origin_male_population[which(data$origin == "ELP")][1], data$origin_male_population[which(data$origin == "TLH")][1], data$origin_male_population[which(data$origin == "SMF")][1]),
  
# I set median income for each destination 
  origin_median_income = c(data$origin_median_income[which(data$origin == "PDX")][1],data$origin_median_income[which(data$origin == "ELP")][1], data$origin_median_income[which(data$origin == "TLH")][1], data$origin_median_income[which(data$origin == "SMF")][1]), 
# I set poverty population for each destination 
dest_poverty_population = c(data$dest_poverty_population[which(data$dest == "PDX")][1],data$dest_poverty_population[which(data$dest == "ELP")][1], data$dest_poverty_population[which(data$dest == "TLH")][1], data$dest_poverty_population[which(data$dest == "SMF")][1]),
# I set destination male population and destination median income usinf cbsa data. 
dest_male_population = c(data$dest_male_population[which(data$dest == "PDX")][1],data$dest_male_population[which(data$dest == "ELP")][1], data$dest_male_population[which(data$dest == "TLH")][1], data$dest_male_population[which(data$dest == "SMF")][1]),
 dest_median_income = c(data$dest_median_income[which(data$dest == "PDX")][1],data$dest_median_income[which(data$dest == "ELP")][1], data$dest_median_income[which(data$dest == "TLH")][1], data$dest_median_income[which(data$dest == "SMF")][1])

)

#Then I made a prediction going one way
predictions <- predict(mod, newdata = new_routes)
new_routes$predictions <- predictions
print(new_routes)

#Then I made a new datset that was the same as new_routes but reversing origin and destinatoin to make predictions. 
routes_reversed <- new_routes

# I did the same thing above but just swapping origin and destination columns. 
routes_reversed$origin_cbsa_name <- new_routes$dest_cbsa_name
routes_reversed$dest_cbsa_name <- new_routes$origin_cbsa_name
routes_reversed$origin_population <- new_routes$dest_population
routes_reversed$dest_population <- new_routes$origin_population
routes_reversed$origin_poverty_population <- new_routes$dest_poverty_population
routes_reversed$dest_poverty_population <- new_routes$origin_poverty_population
routes_reversed$origin_male_population <- new_routes$dest_male_population
routes_reversed$dest_male_population <- new_routes$origin_male_population
routes_reversed$origin_median_income <- new_routes$dest_median_income
routes_reversed$dest_median_income <- new_routes$origin_median_income
routes_reversed$distancemiles <- new_routes$distancemiles  # distance remains the same

# made predications based on my model 
predictions_rev <- predict(mod, newdata = routes_reversed)
routes_reversed$predictions <- predictions_rev

# View the reversed routes with predictions
routes_reversed$predictions <- predictions_rev
print(routes_reversed)

# Lastly I made predictions using the existing model for a new dataset with reversed routes

predictions_rev <- predict(mod, newdata = routes_reversed)

routes_reversed$predictions <- predictions_rev
print(routes_reversed)
