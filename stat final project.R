library(ggplot2)
library(tidyverse)
library(ISLR)
library(caret)
library(corrplot)
library(e1071)

rr=read.csv("C:\\Users\\jaswa\\stat_final\\stat_final1.csv")
rr

#checking for zero variance variables
nearZeroVar(rr)



# data visualization

ggplot(rr, aes(x=month, y=temp)) + 
  geom_boxplot() +
  theme(legend.position = "none")+theme(axis.text.x = element_text(face="bold", color="#993333", 
  size=14, angle=45),axis.text.y = element_text(face="bold", color="#993333", 
  size=14, angle=45))+theme( axis.line = element_line(colour = "black", 
  size = 1, linetype = "solid"))+labs(x = "MONTH",y = "COUNT",title = "MONTH VS TEMPERATURE")+
  geom_jitter(alpha = 0.3, color = "tomato")



ggplot(rr, aes(x=month, y=wind)) + 
  geom_boxplot() +
  theme(legend.position = "none")+theme(axis.text.x = element_text(face="bold", color="#993333", 
  size=14, angle=45),axis.text.y = element_text(face="bold", color="#993333", 
  size=14, angle=45))+theme( axis.line = element_line(colour = "black", 
  size = 1, linetype = "solid"))+labs(x = "MONTH",y = "WIND",title = "MONTH VS WIND")+
  geom_jitter(alpha = 0.3, color = "tomato")





ggplot(rr,aes(x = rain, y = month, color = rain))+ scale_color_gradient(low = "green", high = "brown")+ layer(
  geom = "point", stat = "identity", position = "identity",
  params = list(na.rm = FALSE))+labs(x = "RAIN",y = "MONTH",title = "MONTH VS RAIN")




ggplot(rr,aes(x = month, y = RH, color = RH))+ scale_color_gradient(low = "yellow", high = "black")+ layer(
  geom = "point", stat = "identity", position = "identity",
  params = list(na.rm = FALSE))+coord_trans(y="log2")+coord_flip()+
  labs(x = "MONTH",y = "RELATIVE HUMIDITY",title = "MONTH VS RAIN")







library(tidyverse)
summary(rr)



data <- rr;

lm.all = lm(FFMC ~ DC+ISI+temp+RH+wind+rain+area , data =rr )



summary(lm.all)
confint(lm.all)
coef(lm.all)

par(mfrow=c(2,2))
plot(lm.all)




plot(rr$wind~rr$temp)
linmod= lm(rr$temp~rr$wind)
abline(linmod, col="red")
plot(rr$temp~rr$wind)
linmod = lm(rr$temp ~ rr$wind)
abline(linmod)
abline(linmod,col="red")
linmod




mm=read.csv("C:\\Users\\jaswa\\stat_final\\stat_final2.csv")
mm

library(psych)
lm.all = lm(temp ~ DC+wind , data =mm)
pairs.panels(rr,method="pearson",hist.col="olive drab",density=FALSE )
plot(mm)







library(backports)
library(ggpubr)
library(GGally)


ggpairs(
  
  data=mm,
  coloums=1,
  upper=list(continuous="cor"),
  lower=list(continuous="points"),
  diag=list(continuous="densityDiag")
  
)



# Libraries Needed
library(caret)
library(glmnet)
library(mlbench)
library(psych)

# Data

data <- read.csv("C:\\Users\\jaswa\\stat_final\\stat_final1.csv")
data
# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Custom Control Parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

# Linear Model
set.seed(1234)
lm <- train(temp ~ .,
            train,method="lm",trControl =custom)


# Results
lm$results
lm
summary(lm)
plot(lm$finalModel)

# Ridge Regression
set.seed(1234)
ridge <- train(FFMC ~ .,train,method='glmnet',
               tuneGrid=expand.grid(alpha=0,lambda=seq(0.0001,1,lenght=5)),trControl= custom)

# Plot Results
plot(ridge)
ridge
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=T))

# Lasso Regression
set.seed(1234)
lasso <- train(FFMC ~ DC+ISI+temp,train,method='glmnet',
               tuneGrid=expand.grid(alpha=0,lambda=seq(0.0001,1)),trControl= custom)

# Plot Results
plot(lasso)
lasso

plot(lasso$finalModel, xvar = 'lambda', label=T)
plot(lasso$finalModel, xvar = 'dev', label=T)
plot(varImp(lasso, scale=T))


# Elastic Net Regression
set.seed(1234)
en <- train(FFMC ~ FFMC+DMC+DC+ISI+temp+RH+wind+rain+area,train,method='glmnet',
            tuneGrid=expand.grid(alpha=seq(0,1,length=10),lambda=seq(0.0001,1,lenght=5)),trControl= custom)

# Plot Results
plot(en)
plot(en$finalModel, xvar = 'lambda', label=T)
plot(en$finalModel, xvar = 'dev', label=T)
plot(varImp(en))

# Compare Models
model_list <- list(LinearModel=lm,Ridge=ridge,Lasso=lasso,ElasticNet=en)
res <- resamples(model_list)
summary(res)
bwplot(res)
xyplot(res,metric = 'RMSE')

















# Logistic Regression

# Read data file
mydata <- read.csv('C:\\Users\\jaswa\\stat_final\\stat_final1.csv')
str(mydata)
mydata$temp <- as.factor(mydata$temp)
mydata$wind <- as.factor(mydata$wind)

# Two-way table of factor variables
xtabs(FFMC ~ FFMC+DMC+DC+ISI, data = mydata)

# Partition data - train (80%) & test (20%)
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8, 0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

# Logistic regression model
mymodel <- glm(temp ~ RH + wind, data = train, family = 'binomial')
summary(mymodel)



# Prediction
p1 <- predict(mymodel, train, type = 'response')
head(p1)
head(train)

# Misclassification error - train data
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$temp)
tab1
1 - sum(diag(tab1))/sum(tab1)

# Misclassification error - test data
p2 <- predict(mymodel, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$temp)
tab2
1 - sum(diag(tab2))/sum(tab2)

# Goodness-of-fit test
with(mymodel, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))



# Best Model
en$bestTune
best <- en$finalModel
coef(best, s = en$bestTune$lambda)

# Save Final Model for Later Use
saveRDS(en, "final_model.rds")
fm <- readRDS("final_model.rds")
print(fm)
















