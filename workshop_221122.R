library(tidyverse)
library(randomForest)

idp <- read_csv('ba_idp_2021.csv')
idp <- idp %>% select(-c(oxy, nitrate))

train_ids <- sample(4165, .7*4165)
train_idp <- idp[train_ids,]
test_idp <- idp[-train_ids,]

#rf <- randomForest(barium ~ temp + sal + 
#                     phosphate + silicate,
#                   data = train_idp)
train_pred <- predict(rf, train_idp)
test_pred <- predict(rf, test_idp)

holdout <- "GA02"

train_idp <- idp %>% filter(cruise != holdout)
test_idp <- idp %>% filter(cruise == holdout)
rf <- randomForest(barium ~ temp + sal + 
                     phosphate + silicate,
                   data = train_idp)
train_pred <- predict(rf, train_idp)
test_pred <- predict(rf, test_idp)

woa <- read_csv('woa_2_deg.csv')
woa_pred <- predict(rf, woa)
hist(woa_pred)