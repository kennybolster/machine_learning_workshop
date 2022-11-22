library(tidyverse)
library(randomForest)

idp = read_csv('ba_idp_2021.csv')
woa = read_csv('woa_2_deg.csv')

print(idp)

ggplot(idp) + geom_point(mapping = aes(longitude, latitude)) + 
  ggtitle('Stations in full IDP')
plot(last_plot())

n_na = apply(idp, 2, function(x) sum(is.na(x)))
print(n_na)

allvar_idp <- idp %>% drop_na()
ggplot(allvar_idp) + geom_point(mapping = aes(longitude, latitude)) +
  ggtitle('Stations with all variables measured')
plot(last_plot())

# split train and test set
set.seed(1551)
trainind = sample(1:nrow(allvar_idp), .7*nrow(allvar_idp))
train_idp = allvar_idp[trainind,]
test_idp = allvar_idp[-trainind,]

# run random forest on train set
allvar_rf = randomForest(barium ~ temp + sal + oxy + phosphate + nitrate + 
                           silicate,
                         data = train_idp)
train_idp$pred = predict(allvar_rf, train_idp)
# plot results for train set
ggplot(train_idp) + geom_point(aes(barium, pred)) + 
  ggtitle('All variables - training')
plot(last_plot())
print(cor(train_idp$barium, train_idp$pred))

# plot results for test set
test_idp$pred = predict(allvar_rf, test_idp)
p = ggplot(test_idp) + geom_point(aes(barium, pred)) +
  ggtitle('All variables - test')
plot(p)
print(cor(test_idp$barium, test_idp$pred))

# set up new dataset with fewer variables
allstation_idp = idp %>% select(-c(oxy, nitrate))
trainind = sample(1:nrow(allstation_idp), .7*nrow(allstation_idp))
train_idp = allstation_idp[trainind,]
test_idp = allstation_idp[-trainind,]

# redo random forest
allstat_rf = randomForest(barium ~ temp + sal + phosphate + silicate,
                         data = train_idp)
train_idp$pred = predict(allstat_rf, train_idp)
# plot results for train set
ggplot(train_idp) + geom_point(aes(barium, pred)) + 
  ggtitle('All stations - training')
plot(last_plot())
print(cor(train_idp$barium, train_idp$pred))

# plot results for test set
test_idp$pred = predict(allstat_rf, test_idp)
p = ggplot(test_idp) + geom_point(aes(barium, pred)) +
  ggtitle('All stations - test')
plot(p)
print(cor(test_idp$barium, test_idp$pred))

# write function to hold out one cruise at a time
onecruiseout <-  function(c){
  print(c)
  holdout = allstation_idp %>% filter(cruise != c)
  trainids = sample(nrow(holdout), .7*nrow(holdout))
  trainset = holdout[trainids,]
  testset = holdout[-trainids,]
  model = randomForest(barium ~ temp + sal + phosphate + silicate, 
                       data = trainset)
  preds = predict(model, testset)
  return(cor(testset$barium, preds))
}

# apply that function to each cruise and plot the new OOB R^2
holdout_corrs = sapply(unique(allstation_idp$cruise), onecruiseout)
print(holdout_corrs)

# predict woa concentrations using both models
allstat_woa <- predict(allstat_rf, woa)
allvar_woa <- predict(allvar_rf, woa)
p <- ggplot() + geom_point(mapping = aes(allvar_woa, allstat_woa))
plot(p)
