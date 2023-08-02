library(tidyverse)
library(tidymodels)
library(GGally)
library(sf)
library(leaflet)
library(janitor)
library(rpart.plot)
library(gt)
library(DataExplorer)
library(ranger)
library(xgboost)
library(C50)
library(kknn)
library(vip)

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

housing_df_new <- housing_df %>% # select features
  select(longitude, latitude,
         price_category, median_income,
         ocean_proximity, bedrooms_per_room,
         rooms_per_household, population_per_household)

glimpse(housing_df_new)

# Split data
set.seed(123)

data_split <- initial_split(housing_df_new,
                            prop = 0.8,
                            strata = price_category)

train_data <- training(data_split)
test_data <- testing(data_split)


# Recipe
housing_recipe <- # has 2 arguments
  recipe(price_category ~ ., # 1 - a formula e.g. price_category, any variable on the left hand side of the ~ is the model outcome
         # anything on the right hand side of the ~ are the predictors. The dot (.) indicates all other variables are the predictors
         data = train_data) %>% # 2 - the data, which is always the training set
  update_role(longitude, latitude,
              new_role = "ID") %>% # update_role tells recipe that lon and lat have a custom role called "ID", recipe keeps these variables but does not use the as outcomes or predictors
  step_log(median_income, # - `step_log()` will log transform data (since some of our numerical variables are right-skewed). Cannot be performed on negative numbers.
           bedrooms_per_room, rooms_per_household,
           population_per_household) %>% 
  step_naomit(everything(), skip = TRUE) %>% # removes rows of data with NA's
  step_novel(all_nominal(), -all_outcomes()) %>% # converts all nominal variables to factors and takes care of other issues related to categorical variables.
  step_normalize(all_numeric(), -all_outcomes(),
                 -longitude, -latitude) %>% # normalizes (center and scales) the numeric variables to have a standard deviation of one and a mean of zero. (i.e., z-standardization).
  step_dummy(all_nominal(), -all_outcomes()) %>% # converts our factor column ocean_proximity into numeric binary (0 and 1) variables.
  step_zv(all_numeric(), -all_outcomes()) %>% # removes any numeric variables that have zero variance.
  step_corr(all_predictors(), threshold = 0.7, method = "spearman") # will remove predictor variables with high correlations with other predictor variables.
  
summary(housing_recipe)

## What does our processed data look like?
prepped_data <- 
  housing_recipe %>% # use the recipe object
  prep() %>% # perform the recipe on training data
  juice() # extract only the preprocessed dataframe 

glimpse(prepped_data)

prepped_data %>% 
  select(median_income, rooms_per_household, population_per_household, price_category) %>% 
  ggpairs()


# Validation Set
# Use k-fold cross validation to build a set of 10 validation folds
set.seed(123)

cv_folds <-
  vfold_cv(train_data, 
           v = 10, 
           strata = price_category) 

## Model Building (1/2)
# 1. Pick a `model type`
# 2. set the `engine`
# 3. Set the `mode`: regression or classification

# Logistic regression
log_spec <- logistic_reg() %>%
  set_engine(engine = "glm") %>% 
  set_mode("classification")
log_spec

# Decision tree
tree_spec <- decision_tree() %>% 
  set_engine(engine = "C5.0") %>% 
  set_mode("classification")
tree_spec

# Random Forest
rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")
rf_spec

# Boosted tree (XGBoost)
xgb_spec <-
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")
xgb_spec

# K-nearest neighbours (k-NN)
knn_spec <-
  nearest_neighbor(neighbors = 4) %>% # this can be adjusted
  set_engine("kknn") %>% 
  set_mode("classification")
knn_spec

## Bundle the recipe and the model with "workflows"

# Logistic regression workflow
log_wflow <- 
  workflow() %>% # use workflow function
  add_recipe(housing_recipe) %>% # use the new recipe
  add_model(log_spec) # add your model spec
log_wflow

# Decision tree workflow
tree_wflow <-
  workflow() %>%
  add_recipe(housing_recipe) %>% 
  add_model(tree_spec) 
tree_wflow

# Random forest workflow
rf_wflow <-
  workflow() %>%
  add_recipe(housing_recipe) %>% 
  add_model(rf_spec) 
rf_wflow

# Boosted tree workflow
xgb_wflow <-
  workflow() %>%
  add_recipe(housing_recipe) %>% 
  add_model(xgb_spec)
xgb_wflow

# K-nearest neighbours (k-NN) workflow
knn_wflow <-
  workflow() %>%
  add_recipe(housing_recipe) %>% 
  add_model(knn_spec)
knn_wflow


## Evaluate Models using the validation set cv_folds using fit_resamples()
# Logistic regression evaluation
log_res <- log_wflow %>% 
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(
      recall, precision, f_meas, accuracy,
      kap, roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE))

# Show average performance over all folds (note that we use log_res):
log_res %>%  collect_metrics(summarize = TRUE)

# Show performance for every single fold:
log_res %>%  collect_metrics(summarize = FALSE)

# collect_predictions() and get confusion matrix
log_pred <- log_res %>% collect_predictions()

log_pred %>%  conf_mat(price_category, .pred_class) 

log_pred %>% 
  conf_mat(price_category, .pred_class) %>% 
  autoplot(type = "mosaic") +
  geom_label(aes(
    x = (xmax + xmin) / 2, 
    y = (ymax + ymin) / 2, 
    label = c("TP", "FN", "FP", "TN")))

log_pred %>% 
  conf_mat(price_category, .pred_class) %>% 
  autoplot(type = "heatmap")

# ROC Curve
log_pred %>% 
  group_by(id) %>% # id contains our folds
  roc_curve(price_category, .pred_above) %>% 
  autoplot()


# Decision Tree evaluation
tree_res <-
  tree_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

tree_res %>%  collect_metrics(summarize = TRUE)


# Random Forest evaluation
rf_res <-
  rf_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

rf_res %>%  collect_metrics(summarize = TRUE)


# Boosted tree - XGBoost evaluation
xgb_res <- 
  xgb_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

xgb_res %>% collect_metrics(summarize = TRUE)


# K-nearest neighbour evaluation
knn_res <- 
  knn_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

knn_res %>% collect_metrics(summarize = TRUE)


## Model Comparison
log_metrics <- 
  log_res %>% 
  collect_metrics(summarise = TRUE) %>%
  # add the name of the model to every row
  mutate(model = "Logistic Regression") 

tree_metrics <- 
  tree_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Decision Tree")

rf_metrics <- 
  rf_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Random Forest")

xgb_metrics <- 
  xgb_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "XGBoost")

knn_metrics <- 
  knn_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Knn")

# create dataframe with all models
model_compare <- bind_rows(log_metrics,
                           tree_metrics,
                           rf_metrics,
                           xgb_metrics,
                           knn_metrics) 

#Pivot wider to create barplot
model_comp <- model_compare %>% 
  select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

# show mean are under the curve (ROC-AUC) for every model
model_comp %>% 
  arrange(mean_roc_auc) %>% 
  mutate(model = fct_reorder(model, mean_roc_auc)) %>% # order results
  ggplot(aes(model, mean_roc_auc, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    size = 3,
    aes(label = round(mean_roc_auc, 2), 
        y = mean_roc_auc + 0.08),
    vjust = 1
  )+
  theme_light()+
  theme(legend.position = "none")+
  labs(x = NULL, y = NULL, title = "XGBoost model performs with 93% accuracy")+
  theme(plot.title = element_text(hjust = 0.5))


## `last_fit()` on test set
# - `last_fit()`  fits a model to the whole training data and evaluates it on the test set. 
# - provide the workflow object of the best model as well as the data split object (not the training data). 
last_fit_xgb <- last_fit(xgb_wflow, 
                         split = data_split,
                         metrics = metric_set(
                           accuracy, f_meas, kap, precision,
                           recall, roc_auc, sens, spec))

last_fit_xgb %>% collect_metrics(summarize = TRUE)

#Compare to training
xgb_res %>% collect_metrics(summarize = TRUE)


## Variable importance using vip package
# vip visualizes variable importance scores for the top features. Note that we can’t create this type of plot for every model engine.
# Access the variable importance scores via the .workflow column: pluck out the first element in the workflow column, then pull out the fit from thr workflow

last_fit_xgb %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10) +
  theme_light()


# Final Confusion Matrix
last_fit_xgb %>%
  collect_predictions() %>% 
  conf_mat(price_category, .pred_class) %>% 
  autoplot(type = "heatmap")


# Final ROC curve
last_fit_xgb %>% 
  collect_predictions() %>% 
  roc_curve(price_category, .pred_above) %>% 
  autoplot()

