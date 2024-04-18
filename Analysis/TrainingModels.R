#Importing Raw Data
data_2016 <- read.csv("2016Data.csv")
data_2020 <- read.csv("2020Data.csv")


#Dropping Na rows
data_2016 <- na.omit(data_2016)
data_2020 <- na.omit(data_2020)

#Random Forest
library(randomForest)
train.rf <- randomForest(as.factor(Outcome) ~., data = data_2016, importance = TRUE, proximity = TRUE)
print(train.rf)

#Bagging 
library(tidyr)
library(dplyr)
set.seed(8675309)
train.bag <- randomForest(as.factor(Outcome) ~ ., data = data_2016,
                            mtry = 20, ntree = 500, oob_score = TRUE)
print(train.bag)
importance(train.bag) %>% as_tibble() %>%
  mutate(var = rownames(importance(train.bag))) %>% 
  select(2, 1)

#Training predictions: RF
data_2016 %>% mutate(pred = train.rf$predicted) %>%
  select(Outcome, pred) %>% 
  mutate(pred = as.numeric(pred)-1) %>%
  mutate(category = "Training: Random Forest") -> predTrainRF

# Training predictions: Bag
data_2016 %>% mutate(pred = train.bag$predicted) %>%
  select(Outcome, pred) %>% 
  mutate(pred = as.numeric(pred)-1) %>%
  mutate(category = "Training: Bag") -> predTrainBag


bind_rows(predTrainBag, predTrainRF) %>%
  group_by(category, Outcome, pred) %>%
  count() %>%
  group_by(category) %>%
  mutate(totaln = sum(n), pred = ifelse(pred == 2, "R", "D"),
         correct = ifelse(Outcome == pred, 1, 0)) %>%
  filter(correct == 1) %>%
  summarize(accuracy = sum(n)/first(totaln)) %>%
  separate(category, c("Data", "Model"), sep=": ") -> accuracyTT

#Support Vector Machines
#Logistic Regression
#
