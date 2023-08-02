# Using the iris dataset, will perform a simple Neural Network implementation

# Install packages
install.packages(c('neuralnet','keras','tensorflow'),dependencies = T)

# Load the packages
library(tidyverse)
library(neuralnet)

# Convert character columns to factors
iris <- iris %>% 
  mutate_if(is.character, as.factor)

summary(iris) # Data is balanced, all 3 classes have 50 samples each 

# Train and Test split the dataset for model training and evaluation
# Set seed
set.seed(1234)

# 80:20 split, calculate the number of rows to be used for training data in iris
data_rows <- floor(0.80 * nrow(iris))

# Randomly sample the number of indices from 1 to the total number of rows in iris
train_indices <- sample(c(1:nrow(iris)), data_rows)

# Select the rows corresponding to the sample indices for training 
train_data <- iris[train_indices,]

# Select the remaining rows for testing
test_data <- iris[-train_indices,]

# Train our Neural Network using 'neuralnet'
model = neuralnet(
  # Formula for the model, Species = target variable, everything after '~' are the predictors
  Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
  # Using the training set
  data = train_data,
  # Specify the number of hidden layers & the number of neurons in each layer
  hidden = c(4,2),
  # The output should not be linear
  linear.output = FALSE
)

# View the model architecture
plot(model, 
     # 'rep' argument specifies the type of plot to be created
     # "best" tells 'plot' to show the best representation of the model
     rep = "best") 

# To evaluate the model we will build the confusion matrix

# Make predictions on the test data using the previously trained model with 'predict'
pred <- predict(model, test_data)

# Create a vector of labels for the 3 possible species of flower
labels <- c("setosa", "versicolor", "virginca")

# Create a column index of the maximum value of each row in the "pred" variable
prediction_label <- data.frame(max.col(pred)) %>% 
  # Add a new column with the labels for predicted species
  # The values are based on the maximum value in each row of the "pred" variable
  mutate(pred = labels[max.col.pred.]) %>% 
  # Keep only the "pred" column in the dataframe
  select(2) %>% 
  # Convert the dataframe into a vector
  unlist()

# Create a table that compares the actual species in the test data to the predicted species
# It shows how many flowers were correctly classified and how many were mis-classified
table(test_data$Species, prediction_label)

# Check the accuracy of the model
# Convert the categorical values to numerical  & compare to predicted values
check = as.numeric(test_data$Species) == max.col(pred)

# Calculate the accuracy of the predictions by summing the number of 'TRUE' values
# And dividing it by the total number of samples
accuracy = (sum(check)/nrow(test_data)) * 100

# Show the accuracy - in this case the model was 100% accurate
print(accuracy)






