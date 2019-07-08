# Building a recommendation system

# Classification with nearest neighbors

library(class)
library(caTools) # Used for data partitioning. 
library(dplyr)
library(DMwR)

knntest <- knnImputation(fifa[-1])

# Split data

set.seed(2019) # Set seed to always generate the same split

fifa <- data %>%
  dplyr::filter(Position != 'GK') %>%
  dplyr::select(Name, which(sapply(.,class) %in% c('numeric', 'integer')), 
        -ID, -Jersey.Number, -Age, -Special, -Value,
         -International.Reputation, -Weak.Foot, -(GKDiving:GKReflexes), -Wage, -Release.Clause)

fifa[,-1] <- scale(fifa[,-1])


sample <- sample.split(fifa[,-1], SplitRatio = 0.80) # Splits the data 80/20

# Create a vector of labels


train_labs <- subset(fifa[,1], sample == T)
test_labs <- subset(fifa[,1], sample == F)

# Training set
train1 <- subset(fifa[,-1], sample == T) 

test1 <- subset(fifa[,-1], sample == F)




# Use kNN to identify the player positions

position_pred <- FNN::knn(train = fifa[,-1], test = fifa[,-1], cl = fifa[,1], 
                     k = 4, algorithm = 'cover_tree')

indices <- attr(position_pred, "nn.index")
closest.labels <- apply(indices, 2, function(col) fifa[,1][col])


# Examine the structure of the fifa dataset

str(fifa)

# Count the number of players in each position

table(fifa$Position)

# Average rating per position

aggregate(Overall ~ Position, data = fifa, mean)


# Create confusion matrix of the predicted vs actual values

fifa[,35][is.na(fifa[,35])] <- 0
fifa[1:35] <- scale(fifa[1:35])
mean(position_pred == test_labels)




