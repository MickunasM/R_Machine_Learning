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
# We also see that housing_median_age feature does not correkate with other features

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
# ocean_proximity may be a useful predictor however 
# it's worth noting that in northern CA, prices in coastal districts are not too high

# Next, we can explore the features of the numerical variables
# This can be helpful in identifying outliers which could skew the model
housing_df %>% 
  select(price_category, where(is.numeric), -longitude, -latitude) %>% 
  pivot_longer(cols = 2:10,
               names_to = "feature",
               values_to = "value") %>% 
  ggplot()+
  aes(x = price_category, y = value, fill = price_category)+
  coord_flip()+
  geom_boxplot()+
  facet_wrap(~ feature, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = NULL, y = NULL, title = "Feature Exploration - Numerical Variables")+
  theme(plot.title = element_text(hjust = 0.5))
# We can see that rooms_per_household, population_per_household & population have outliers, >100, >1200 & >30000 respectively

# We can re-run the code above but this time removing the outliers
housing_df %>% 
  select(price_category, where(is.numeric), -longitude, -latitude) %>% 
  filter(rooms_per_household < 50,
         population_per_household < 20,
         population < 20000) %>% 
  pivot_longer(cols = 2:10,
               names_to = "feature",
               values_to = "value") %>% 
  ggplot()+
  aes(x = price_category, y = value, fill = price_category)+
  coord_flip()+
  geom_boxplot()+
  facet_wrap(~ feature, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = NULL, y = NULL, title = "Feature Exploration - Numerical Variables (No Outliers)")+
  theme(plot.title = element_text(hjust = 0.5))

# We can also have a look at the relationships between the numerical variables (correlations)
housing_df %>% 
  select(price_category,
         median_income,
         bedrooms_per_room,
         rooms_per_household,
         population_per_household) %>% 
  ggscatmat(color = "price_category",
            corMethod = "spearman")+
  theme_bw()+
  labs(x = NULL, y = NULL, title = "Scatterplot - Correlation of Numerical Variables")+
  theme(plot.title = element_text(hjust = 0.5))
# We observe correlations between median_income vs rooms_per_household & vs bedrooms_per_room in both price categories
# We also observe a relationship between rooms_per_household vs bedrooms_per_room in both price categoris

# Having looked at the numerical variables,
# There are still the two categorical variables remaining
# We can have a brief look at these using the gt library as previously and make a display table

housing_df %>% 
  count(price_category, ocean_proximity) %>% 
  group_by(price_category) %>% 
  mutate(percent = n / sum(n) *100,
         percent = round(percent, 2)) %>% 
  gt() %>% 
  tab_header(
    title = "California median house prices",
    subtitle = "Districts above and below 150.000$"
  ) %>% 
  cols_label(
    ocean_proximity = "Ocean Proximity",
    n = "Districts",
    percent = "Percent"
  ) %>% 
  fmt_number(
    columns = vars(n),
    suffixing = TRUE
  ) 

# Or visualize these features as a heatmap
housing_df %>%
  ggplot(aes(x = price_category, 
             y = ocean_proximity)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis")+
  theme_light()+
  labs(x=NULL, y= NULL, title = "You will pay >$150k for a property <1hr from the Pacific")+
  theme(plot.title = element_text(hjust = 0.5))

## Up to this point, we have performed data cleaning and manipulation as a set up for our model
## We have also performed some EDA to assess which features would be useful in the model
## Features = logitute, latitude, price_category, median_income, ocean_proximity, bedrooms_per_room, rooms_per_household and population_per_household

## Moving on to the next step - building the model
# Feature Selection and Data Split



