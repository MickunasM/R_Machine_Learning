library(tidyverse)
library(tidymodels)
library(GGally)
library(sf)
library(leaflet)
library(janitor)
library(rpart.plot)
library(gt)
library(DataExplorer)

# for the figures
train_color <- "#1a162d"
test_color <- "#84cae1"
data_color <- "#CA225E"
assess_color <- data_color
splits_pal <- c(data_color, train_color, test_color)

# load the data set
california_housing <- read_csv("data/housing.csv")

# California Housing - median house prices for California districts derived from the 1990 census.

# Question - Can we predict whether the median house value in a district is above/below $150,000?

# inspect the data set
glimpse(california_housing)
# total_rooms in district is not very useful, could use number of rooms per household.
# total_bedrooms equally not useful.
# population in district could be useful, but also population_per_household.

# Data manipulation:
california_housing <- california_housing %>% 
  mutate(rooms_per_household = total_rooms/households,
         bedrooms_per_room = total_bedrooms/total_rooms,
         population_per_household = population/households)
glimpse(california_housing)

# Let's remove median house value - as we are building a categorical model predicting whether a price is above or below $150,000.
# Instead let's make categorical variables 'above' and 'below' and make these a factor.
housing_df <- california_housing %>%
  mutate(price_category = case_when(
    median_house_value < 150000 ~ "below",
    median_house_value >= 150000 ~ "above")) %>% 
  mutate(price_category = factor(price_category),
         ocean_proximity = factor(ocean_proximity)) %>% 
  select(-median_house_value)


# Lets make the table look nicer
housing_df %>% 
  count(price_category, 
        name ="districts_total") %>%
  mutate(percent = districts_total/sum(districts_total)*100,
         percent = round(percent, 2)) %>%
  gt() %>%
  tab_header(
    title = "California median house prices",
    subtitle = "Districts above and below 150K $"
  ) %>%
  cols_label(
    price_category = "Price",
    districts_total = "Districts",
    percent = "Percent"
  ) %>% 
  fmt_number(
    columns = c(districts_total),
    suffixing = TRUE
  ) 

# Check for missing values
DataExplorer::plot_missing(housing_df)
# The features bedrooms_per_room and total_bedrooms have 1% of the values missing, this is still classed in the "Good" band.

# Scatterplot - Correlation Matrix
housing_df %>% 
  select(
    housing_median_age,
    median_income,
    bedrooms_per_room,
    rooms_per_household,
    population_per_household,
    ocean_proximity,
    price_category) %>% 
  ggpairs()

# The correlation matrix: 
# Tell us that as median income decreases, the number of bedrooms_per_room also decreases (-0.616) as expected.
# Also tells us that rooms_per_household has a negative correlation with bedrooms_per_household (-0.417) again, as expected.
# Given this is a California data set and location may play a role in our model, can we try some maps?

# Read a shapefile with CA counties:
# Shapefiles are useful method of representing geospatial data (includes location on the Earth's surface).

california_sf <- read_sf("Data/CA_Counties_TIGER2016.shp")

# Check Coordinate Reference System (CRS) of the shapefile
st_geometry(california_sf)

# Convert the projected coordinates into longitute and latitude - multipolygon geometries from CRS become point geometries.
# They are then added to the dataframe as "geometry" column.
housing_map <- st_as_sf(california_housing,
                        coords = c("longitude", "latitude"),
                        crs = 4326)
st_geometry(housing_map)

# Now we can plot the map of the state of California, using lon/lat, population and median house value
ggplot()+
  # draw the shapefile of CA and its counties
  geom_sf(data = california_sf, 
          fill = "#fafafa",
          size = 0.125,
          colour = "grey10") +
  theme_void()+
  # add our shapefile with data on prices
  geom_sf(
    data = housing_map, 
    aes(size = population,
        colour = median_house_value),
    alpha = 0.4  )+
  scale_colour_gradientn(labels = comma, 
                         colours=rev(rainbow(5)))+
  labs(size = "Population",
       colour = "Median House Value")
# From the map we can see that prices seem to be related to location and population density
# ocean_proximity may be a useful predictor however, it's work noting that in northern CA, prices in coastal districts are not too high












