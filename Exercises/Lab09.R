## In-Class Coding Notes 
library(dplyr)
library(caret)
library(ggplot2)

### Loading Data 
heart <- read.csv(file = 'https://github.com/apodkul/ppol670_01/raw/main/Data/processed_cleveland.csv' 
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

ggplot(heart, 
       aes(x = preds_line)) + 
  geom_histogram()


logit_mod <- glm(heart~chol+sex, data = heart, 
               family = binomial(link = 'logit'))
summary(logit_mod)

heart$preds_logit <- predict(logit_mod, heart) 
ggplot(heart) + 
  geom_histogram(aes(x = preds_logit))

heart$preds2_logit <- predict(logit_mod, heart, type = 'response') 
ggplot(heart) + 
  geom_histogram(aes(x = preds2_logit))

#Looking at metrics
confusionMatrix(
  as.factor(heart$preds_logit > .5), 
  as.factor(heart$heart == 1)
)

#install.packages("pROC")
library(pROC)

roc(heart$heart~heart$preds2_logit, 
    plot = T, 
    print.auc = T)

#install.packages("predtools")
library(predtools)
calibration_plot(data = heart, 
                 obs = 'heart', 
                 pred = 'preds2_logit')


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


heart_rf <- train(
  as.factor(heart)~., 
  data = heart_train, 
  trControl = trainControl(method = 'cv', number = 5), 
  method = 'rf'
)


heart_rf
varImp(heart_rf)


