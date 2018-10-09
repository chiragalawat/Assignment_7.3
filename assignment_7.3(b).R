#c. Visualize the dataset and make inferences from that 
#d. Perform any 3 hypothesis tests using columns of your choice, make conclusions 
#e. Create a linear regression model to predict the number of comments in the next 
#24 hours (relative to basetime)

library(ISLR) 
train_sg <- read.csv("ACADgILd\\Assignment\\train_sg.csv") 
test_sg <- read.csv("ACADgILd\\Assignment\\test_sg.csv") 
View(train_sg) 
View(test_sg) 
str(train_sg)
pairs(train_sg)
pairs(test_sg)

hist(train_sg$y,col='red')
hist(train_sg$meta_rf,col="purple")

library(tree) 
tree.train_sg<-tree(meta_knn~.-meta_rf,train_sg) 
summary(tree.train_sg)

hist(test_sg$meta_rf,col="yellow")

tree.test_sg<-tree(meta_knn~.-meta_rf,test_sg) 
summary(tree.test_sg)


plot(tree.train_sg) 
text(tree.train_sg,pretty = 0)

plot(tree.test_sg) 
text(tree.test_sg,pretty = 0)

t.test(train_sg$meta_knn,mu=0.6)

t.test(train_sg$meta_rf, mu=0.7)

t.test(train_sg$meta_rf,mu=0.5,conf.level =0.80 )

t.test(train_sg$y, mu=0.5)

t.test(test_sg$meta_knn,mu=0.6)

t.test(test_sg$meta_rf, mu=0.7)

t.test(test_sg$meta_rf,mu=0.5,conf.level =0.80 )

t.test(test_sg$y, mu=0.5)

plot(train_sg$meta_knn,train_sg$meta_rf)
cor(train_sg$meta_knn,train_sg$meta_rf) 
## [1] 0.6833799 

mod<-lm(train_sg$meta_knn~train_sg$meta_rf) 
summary(mod)

predict(mod)

cor(test_sg$meta_knn,test_sg$meta_rf) 
## [1] 0.8151615 

mod<-lm(test_sg$meta_knn~test_sg$meta_rf) 
summary(mod)

pred<-predict(mod) 
train_sg$predicted = NA 
train_sg$predicted = pred 
library(car) 
## Loading required package: carData 
dwt(mod) 
## lag Autocorrelation D-W Statistic p-value 
## 1 0.5028719 0.9942523 0 
## Alternative hypothesis: rho != 0 

plot(test_sg$meta_knn,test_sg$meta_rf)

pred<-predict(mod) 
test_sg$predicted = NA 
test_sg$predicted = pred 
library(car) 
dwt(mod)

plot(train_sg$meta_knn,train_sg$meta_rf,abline(lm(train_sg$meta_knn~train_sg$meta_rf), col="red"))










