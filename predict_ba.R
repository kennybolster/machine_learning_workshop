library(tidyverse)
library(randomForest)

idp = read_csv('ba_idp_2021.csv')
woa = read_csv('woa_2_deg.csv')

print(idp)

ggplot(idp) + geom_point(mapping = aes(longitude, latitude))
plot(last_plot())

n_na = apply(idp, 2, function(x) sum(is.na(x)))
print(n_na)

idp <- idp %>% drop_na()
ggplot(idp) + geom_point(mapping = aes(longitude, latitude))
plot(last_plot())

set.seed(1551)
trainind = sample(1:nrow(idp), .7*nrow(idp))
train_idp = idp[trainind,]
test_idp = idp[-trainind,]



