library(tidyverse)

df = tibble(a = 1:50)
df$b <- df$a*.58 + .05
set.seed(80)
df$error <- runif(50) * 5
df$meas <- df$b + df$error
ggplot(df) + geom_point(aes(a, meas)) + xlab('x') + ylab('y')
plot(last_plot())

df$train <- sample(c(0,1), 50, replace = TRUE, prob = c(.3, .7))
df$train <- as.factor(df$train)
p <- ggplot(df) + geom_point(aes(a, meas, color = train)) + xlab('x') + ylab('y')
plot(p)

trainset <- df %>% filter(train == 1)
train_lm <- lm(meas ~ a, trainset)

whole_lm <- lm(meas ~ a, data = df)
df$pred_standard <- predict(whole_lm, df)
df$pred_train <- predict(train_lm, df)

p <- ggplot(df) + geom_point(aes(a, meas)) + geom_line(aes(a, pred_standard)) +
  xlab('x') + ylab('y')
plot(p)

p <- ggplot(df) + geom_point(aes(a, meas, color = train)) + xlab('x') + 
  ylab('y') + geom_line(aes(a, pred_train))
plot(p)

test <- df %>% filter(train == 0)
print(cor(test$meas, test$pred_train)^2)

trainset$pred_train <- predict(train_lm, trainset)
p <- ggplot(trainset) + geom_point(aes(a, meas)) + xlab('x') + ylab('y') +
  geom_line(aes(a, pred_train))
plot(p)
