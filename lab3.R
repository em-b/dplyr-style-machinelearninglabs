library(ISLR)
library(dplyr)
library(class)
data(Smarket)
str(Smarket)
microbenchmark({
###### Function Writing ################
knn_predictions <- function (k, df) {

# Note: KNN can take a matrix OR a data frame as input.

#Create Dataframe with lag1 and lag2 data from years before 2005
train_X <- df %>%
  filter(Year < 2005) %>%
  select(Lag1, Lag2)
# Create Dataframe with lag1 and lag2 data from 2004
test_X <- df %>%
  filter(Year == 2005) %>%
  select(Lag1, Lag2)
# Create data frame with direction label from years before 2005
train_direction <- df %>%
  filter(Year < 2005) %>%
  select(Direction)

##Note - classification argument must be a vector, NOT a dataframe with one column
knn.pred <- knn(train_X, test_X, train_direction$Direction, k = k)

## Find out how many were correct classifications

print(paste("Using k = ", k))
results <- df %>%
  filter(Year == 2005) %>%
  select(Direction) %>%
  mutate(Direction_Classification = knn.pred) %>%
  mutate(Accuracy = ifelse((Direction == "Up" & Direction_Classification == "Up"), "True UP", ifelse (
                           (Direction == "Down" & Direction_Classification == "Down"), "True Down", ifelse(
                             (Direction == "Down" & Direction_Classification == "Up"), "False Up", "False Down"
                           )))) %>%
  group_by(Accuracy) %>%
    summarize(total = n())
print(results)
return (results)
##Alternatively
##table(knn.pred,filter(Smarket, Year == 2005)$Direction)

}


### Analysis ######

knn_predictions(1, Smarket)
k_vector <- c(1:10)
k_results <- sapply(k_vector, knn_predictions, Smarket)

})
