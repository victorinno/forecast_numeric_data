Sys.setenv(JAVA_HOME="")
library(rpart)
library(rpart.plot)
library(RWeka)

wine <- read.csv('whitewines.csv')
hist(wine$quality)

wine_train <- wine[1:3750,]
wine_test <- wine[3751:4898,]

m.part <- rpart(quality ~ ., data = wine_train)

rpart.plot(m.part, digits = 3)
rpart.plot(m.part, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

p.raprt <- predict(m.part, wine_test)
summary(p.raprt)


cor(p.raprt, wine_test$quality)

MAE <- function(actual, predicted){
  mean(abs(actual - predicted))
}

MAE(p.raprt, wine_test$quality)

m.m5p <- M5P(quality ~ ., data = wine_train)

summary(m.m5p)

p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)
cor(p.m5p, wine_test$quality)

MAE(wine_test$quality, p.m5p)



Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.7.0_51")