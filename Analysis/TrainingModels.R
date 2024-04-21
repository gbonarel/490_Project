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

#Support Vector Machines
#1 : Polynomial
library(e1071)
set.seed(1)
numerical_2016 <- data_2016[,c(-1,-2)]
numerical_2020 <- data_2020[,c(-1,-2)]
tune.out.poly <- tune(svm, as.factor(Outcome) ~ ., data = numerical_2016, 
                 kernel = "polynomial",
                 na.rm = TRUE,
                 ranges = list(
                   cost = c(0.1, 1, 10, 100, 1000),
                   degree = c(0.1, 0.5, 1, 2, 3, 4, 5)
                 )
)
pred.poly = predict(tune.out.poly$best.model, newdata = numerical_2016)
table(pred.poly, numerical_2016$Outcome)
misclassification_error_train_poly <- mean(pred.poly != numerical_2016$Outcome)
print(paste("Misclassification Error:", misclassification_error_train_poly))
#Misclassification Error: 0.0328502415458937
#Best Model:
# cost: 10
# degree: 2
poly.svm <- svm(as.factor(Outcome) ~., data = numerical_2016, cost=10, degree=2, probability = TRUE)
poly.svm.prob <- predict(poly.svm, type="prob", newdata = numerical_2020, probability = TRUE)

library(ROCR)
poly.svm.prob.rocr <- prediction(attr(poly.svm.prob, "probabilities")[,1], numerical_2020$Outcome)
poly.svm.perf <- performance(poly.svm.prob.rocr, "tpr","fpr")

#2: Radial
tune.out.radial <- tune(svm, as.factor(Outcome) ~ ., data = numerical_2016, 
                 kernel = "radial",
                 na.rm = TRUE,
                 ranges = list(
                   cost = c(0.1, 1, 10, 100, 1000),
                   gamma = c(0.25, 0.5, 2, 3)
                 )
)
pred.radial = predict(tune.out.radial$best.model, newdata = numerical_2016)
table(pred.radial, numerical_2016$Outcome)
misclassification_error_train_radial <- mean(pred.radial != numerical_2016$Outcome)
print(paste("Misclassification Error:", misclassification_error_train_radial))
#Misclassification Error: 0
#cost: 10
#gamma: 0.25

radial.svm <- svm(as.factor(Outcome) ~., data = numerical_2016, cost=10, gamma=0.25, probability = TRUE)
radial.svm.prob <- predict(radial.svm, type="prob", newdata = numerical_2020, probability = TRUE)

radial.svm.prob.rocr <- prediction(attr(radial.svm.prob, "probabilities")[,1], numerical_2020$Outcome)
radial.svm.perf <- performance(radial.svm.prob.rocr, "tpr","fpr")

#Lasso Regression
library(glmnet)
x <- model.matrix(Outcome ~ ., numerical_2016)[,-1]
y <- numerical_2016$Outcome
x.validation <- model.matrix(Outcome ~ ., numerical_2020)[,-1]
y.validation <- numerical_2020$Outcome
grid <- 10^seq(10, -2, length = 100)

lasso.mod <- glmnet(x, y, alpha = 1,
                    lambda = grid, family = "binomial")

plot(lasso.mod, label = TRUE)
set.seed(1)
cv.out <- cv.glmnet(x, as.factor(y), alpha = 1, family = "binomial")
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = x.validation, family = "binomial")




#ROC curves of different models
plot(poly.svm.perf, col = 1, main="ROC curves of different SVMs on validation set")
plot(radial.svm.perf,col = 2,  add = TRUE)
legend(0.6, 0.6, c('Polynomial', 'Radial'), 1:2)