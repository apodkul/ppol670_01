## Coding, Part 1: Regression (continued) 
life_expect <- read.csv('https://raw.githubusercontent.com/apodkul/ppol670_01/main/Labs/Week04/life_expect.csv')

trainingIdx <- createDataPartition(life_expect$life_expectancy, 
                                   p = .8, 
                                   list = F
)
trainingIdx

dset_train <- life_expect[trainingIdx,]
dset_test <- life_expect[-trainingIdx,]

dt_mod <- train(life_expectancy~log(GDP_per_capita)+Continent+log(Population),
                data = dset_train, 
                method = 'rpart'
)
dt_mod

plot(dt_mod)

plot(dt_mod$finalModel)
text(dt_mod$finalModel)

### Bagging 
db_mod <- train(life_expectancy~log(GDP_per_capita)+Continent+log(Population),
                data = dset_train, 
                method = 'treebag'
)
db_mod


dset_test$predict_tree <- predict(dt_mod, dset_test)
dset_test$predict_bag <- predict(db_mod, dset_test)

RMSE(dset_test$predict_tree, dset_test$life_expectancy)
RMSE(dset_test$predict_bag, dset_test$life_expectancy)

### Random Forest 
rf_mod <- train(life_expectancy~log(GDP_per_capita)+Continent+log(Population),
                data = dset_train, 
                method = 'rf'
)
rf_mod


dset_test$predict_tree <- predict(dt_mod, dset_test)
dset_test$predict_bag <- predict(db_mod, dset_test)
dset_test$predict_rf <- predict(rf_mod, dset_test)

RMSE(dset_test$predict_tree, dset_test$life_expectancy)
RMSE(dset_test$predict_bag, dset_test$life_expectancy)
RMSE(dset_test$predict_rf, dset_test$life_expectancy)


varImp(dt_mod)
varImp(db_mod)
varImp(rf_mod)

## Longer Example with CV
data("Sacramento")

set.seed(1789)
training_ind <- createDataPartition(Sacramento$price, 
                                    p = .75, 
                                    list = FALSE)

sac_train <- Sacramento[training_ind,] 
sac_test <- Sacramento[-training_ind,]

#5 fold 
trc <- trainControl(
  method = 'repeatedcv',
  number = 5
)

#LOOCV
trc2 <- trainControl(
  method = 'LOOCV'
)

#Fit a Linear Model 
fit1 <- train(price ~ beds + baths + sqft + type, 
              data = sac_train, 
              method = 'lm', 
              trControl = trc)
fit1

fit1$finalModel

fit1$finalModel %>%
  summary()

#Fit a Linear Model 
fit2 <- train(price ~ beds + baths + sqft + type, 
              data = sac_train, 
              method = 'lm', 
              trControl = trc2)
fit2

fit2$finalModel

fit2$finalModel %>%
  summary()


## Coding, Part 2: Classification 
library(dplyr)
library(caret)
library(purrr)
library(ggplot2)

### Loading Data 
heart <- read.csv(file = 'https://raw.githubusercontent.com/apodkul/ppol670_01/main/Labs/Week09/processed_cleveland.csv' 
                    )
heart %>% 
  glimpse()

heart %>% 
  purrr::modify_if(is.character, as.numeric) %>%
  glimpse()
  
heart <- heart %>% 
  purrr::modify_if(is.character, as.numeric) %>%
  glimpse()


### Estimating and working with Logit 
##### Compare with Linear
lin_mod <- lm(heart~chol+sex, data = heart)
summary(lin_mod)

heart$preds_line <- predict(lin_mod, heart) 

ggplot(heart) + 
  geom_histogram(aes(preds_line))


logit_mod <- glm(heart~chol+sex, data = heart, 
               family = binomial(link = 'logit'))
summary(logit_mod)

heart$preds_logit <- predict(logit_mod, heart) 

ggplot(heart) + 
  geom_histogram(aes(preds_logit))


heart$preds_logit <- predict(logit_mod, heart, type = 'response') 

ggplot(heart) + 
  geom_histogram(aes(preds_logit))

#Looking at metrics
confusionMatrix(
  as.factor(heart$preds_logit > .5), 
  as.factor(heart$heart == 1)
)

#install.packages("pROC")
library(pROC)

roc(heart$heart~heart$preds_logit, 
    plot = T, 
    print.auc = T)


### Using Logit with `caret`
set.seed(1789)
training_ind <- createDataPartition(heart$heart, 
                                    p = .8, 
                                    list = FALSE)

heart_train <- heart[training_ind,] 
heart_test <- heart[-training_ind,]

logit_mod_ct <- train(
  heart~chol+sex, 
  data = heart_train, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'glm',
  family = 'binomial'
)
#Note warning message here

logit_mod_ct <- train(
  as.factor(heart)~chol+sex, 
  data = heart_train, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'glm',
  family = 'binomial'
)

confusionMatrix(logit_mod_ct)

### KNN Classification 
heart$preds_line <- NULL
heart$preds_logit <- NULL

knn_mod_ct <- train(
  as.factor(heart)~chol+sex, 
  data = heart_train, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'knn', 
  tuneGrid = expand.grid(k = c(1, 3, 5, 9, 10, 15, 20, 25))
)

knn_mod_ct2 <- train(
  as.factor(heart)~., 
  data = heart_train, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'knn', 
  tuneGrid = expand.grid(k = c(1, 3, 5, 9, 10, 15, 20, 25))
)

confusionMatrix(knn_mod_ct2)

### SVM Modeling 
#will likely be prompted to install `kernlab`
heart_svml <- train(
  as.factor(heart)~., 
  data = heart_train, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'svmLinear',
  preProcess = c('center', 'scale')
)
heart_svml



### SVM Modeling with Varying Kernel Tricks 
heart_svmp <- train(
  as.factor(heart)~., 
  data = heart_train, 
  trControl = trainControl(method = 'cv', number = 5,
                           classProbs = TRUE), 
  method = 'svmPoly',
  preProcess = c('center', 'scale')
)
heart_svmp


### Decision Tree (Classification)
heart_Tree <- train(
  as.factor(heart)~., 
  data = heart_train, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'rpart'
)
heart_Tree

heart_Tree$finalModel

#install.packages("rpart.plot")
rpart.plot::rpart.plot(heart_Tree$finalModel)  
