# 3. nonparametric, non-statistical models
# (or, intro to machine learning)
# see https://bookdown.org/rehk/stm1001_dsm_introduction_to_machine_learning_in_r/overview.html
# https://lgatto.github.io/IntroMachineLearningWithR/index.html

# clear workspace
rm(list=ls())

library(tidyverse)
# 2D measurements of two flower traits for three plant species
data(iris)
str(iris)
# have a look
head(iris)

ggplot() +
  geom_point(data = iris,
             aes(x = Sepal.Length, y = Sepal.Width,
                 colour = Species))

ggplot() +
  geom_point(data = iris,
             aes(x = Petal.Length, y = Petal.Width,
                 colour = Species))

# k-means clustering using one dimension of two traits (sepal and petal length)
i <- grep("Length", names(iris))
x <- iris[, i] # df[row, column]
# fit clustering algorithm to identify 3 centers
cl <- kmeans(x, centers = 3, nstart = 10)
# model output (colour represents labels assigned by the model to each data point)
plot(x, col = cl$cluster)

length(iris$Species)
length(cl$cluster)

# check if predictions from the model match known species labels
check <- bind_cols(species = iris$Species,
                   # numeric labels for predicted values
                   predicted_species = cl$cluster)

# all setosa are in cluster 3
# 45 versicolor were labelled species 1, four were labelled 2, one labelled 3
# 13 viginica were mislabelled as 1's (rest are 3's)
check %>% 
  group_by(species, predicted_species) %>% 
  summarise(n())

# in-sample and out-of-sample prediction using supervised learning
# https://topepo.github.io/caret/ (vast resource for machine learning models)
# see also, Lucas 2020 Ecological Monographs
library(caret)
# valuable rock data
data(diamonds)
str(diamonds)

ggplot() +
  geom_boxplot(data = diamonds,
               aes(x = cut, y = price)) +
  scale_y_continuous(trans = 'log10')

# fit linear model with all available predictor variables (no interactions)
model <- lm(price ~ ., diamonds)

# make predictions with fitted model
p <- predict(model, diamonds)

# visualise model fit 
plot(p, diamonds$price);abline(c(0,1), lty = 2)

# calculate root mean squared error
error <- p - diamonds$price
rmse_in <- sqrt(mean(error^2)) ## in-sample RMSE
rmse_in

# set seed initialises the random number generator
set.seed(42)
# want to fit model to 80% of data
ntest <- nrow(diamonds) * 0.50
# get random row ids (80%) from full data set
test <- sample(nrow(diamonds), ntest)
# fit model with all predictors to reduced data set
# i.e., just the test rows
model <- lm(price ~ ., data = diamonds[test, ])
# want to predict prices for the data not included
# when we fit the model
p <- predict(model, diamonds[-test, ])
error <- p - diamonds$price[-test]
rmse_out <- sqrt(mean(error^2)) ## out-of-sample RMSE
rmse_out


# predicting penguin species using gender, and two traits (flipper length, 
# body mass)
# install.packages('palmerpenguins')
library(palmerpenguins)

head(penguins)

# reduce to variables of interest, and omit NAs
ml_penguins <- na.omit(penguins[, c(1,5:7)])

# visual inspection
featurePlot(x = ml_penguins[, -1], y = ml_penguins$species, 
            plot = "pairs", auto.key = list(columns = 3))


# Use the dummayVars function (from tibble package) to create a 
# full set of dummy variables for the ml_penguins data
dummy_penguins <- dummyVars(species ~ ., data = ml_penguins)

# Use the predict function to update our ml_penguins feature
# variables with sex dummy variables
ml_penguins_updated <- predict(dummy_penguins, newdata = ml_penguins) %>% 
  as_tibble

# Prepend the outcome variable to our updated data set, otherwise it will be lost
ml_penguins_updated <- cbind(species = ml_penguins$species, ml_penguins_updated)

# check variance of continous features (predictors)
nearZeroVar(ml_penguins_updated, saveMetrics = F)

# check correlation of continuous features
base_cor <-  cor(ml_penguins_updated[, 2:3])


# set seed for reproducibility
set.seed(16505050)
# use createDataPartition to create indices for training data set
train_index <- createDataPartition(ml_penguins_updated$species, 
                                   p = .8, # here p designates the split - 80/20
                                   list = FALSE, 
                                   times = 1) # times specifies how many splits to perform
head(train_index, n = 6)

# create two data sets: training, validation
ml_penguin_train <- ml_penguins_updated[train_index, ]
ml_penguin_validate  <- ml_penguins_updated[-train_index, ]

# use a gradient boosting machine model to predict penguin species
library(gbm)
set.seed(1650)
gbm_fit <- train(species ~ ., data = ml_penguin_train[,-2], 
                 method = "gbm", 
                 verbose = FALSE)
gbm_fit

# examine predictions in validation data set
ml_predictions <- predict(gbm_fit, newdata = ml_penguin_validate)
head(ml_predictions)
# check how many we got right
sum(ml_predictions == ml_penguin_validate$species)
# convert to percentage
sum(ml_predictions == ml_penguin_validate$species)/length(ml_predictions)
