# census-income-mld

train <- read.csv("C:/Users/patel/Desktop/project/project1/train.csv")

test <- read.csv("C:/Users/patel/Desktop/project/project1/test.csv")
summary(train)
str(train)

#checking the outpur variable 
unique(train$income_level)

unique(test$income_level)

train$income_level <- ifelse(train$income_level == -50000,0,1)
test$income_level <- ifelse(test$income_level == -50000,0,1)

factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)


train[,factcols] <-lapply(train[,(factcols)] , factor)
test[,factcols] <-lapply(test[,(factcols)] , factor)
 
str(train)


cat_train <- train[,factcols]
cat_test <- test[,factcols]

#subset numerical variables
num_train <- train[,numcols]
num_test <- test[,numcols]


library(ggplot2)
library(plotly)

ggplotly(ggplot(data = num_train, aes(x= age))+
           geom_histogram(fill="blue",color="red",alpha = 0.5,bins =91)+geom_density())


ggplotly(ggplot(data = train, aes(x= income_level,y= age))+
           geom_boxplot())


ggplotly(ggplot(cat_train,aes(x=class_of_worker,fill=income_level))
         +geom_bar(position = "dodge")+theme(axis.text = element_text(angle = 60,hjust = 1)))

library(dplyr)
table(train$income_level)

t <- train %>% select(age,income_level)%>%filter(age < 91 )
table(t)

ggplotly(ggplot(data=train,aes(x = age, y=wage_per_hour))+
  geom_point(aes(colour=income_level))+scale_y_continuous("wage per hour", breaks = seq(0,10000,1000)))

(ggplot(cat_train,aes(x=education,fill=income_level))
         +geom_bar(position = "dodge")+theme(axis.text = element_text(angle = 60,hjust = 1)))

ggplotly(ggplot(cat_train,aes(x=sex,fill=income_level))
+geom_bar(position = "dodge")+theme(axis.text = element_text(angle = 60,hjust = 1)))

f <- train %>% select(age,sex,education,income_level)%>%filter(education =="Doctorate degree(PhD EdD)" & income_level== 0)
table(f$sex)



(ggplot(cat_train,aes(x=,fill=income_level))+geom_bar(position = "dodge",  color="black")
  +scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10)))



table(cat_train$marital_status,cat_train$income_level)

# data cleaning
table(is.na(num_train))
table(is.na(num_test))


table(is.na(cat_train))
table(is.na(cat_test))

library(caret)
ax <-findCorrelation(x = cor(num_train), cutoff = 0.1)
ax


library(corrplot)

corrplot(cor(num_train), method = "number")
mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100
mvtr

mvte <- sapply(cat_test, function(x){sum(is.na(x))/length(x)})*100
mvte


cat_train <- subset(cat_train, select = mvtr < 5 )
cat_test <- subset(cat_test, select = mvte < 5)



cat_train[,names(cat_train)] <-lapply(cat_train[,(names(cat_train))] , as.character)

cat_train[is.na(cat_train)] <- "unavailable"

cat_train[,names(cat_train)] <-lapply(cat_train[,(names(cat_train))] ,factor)


cat_test[,names(cat_test)] <-lapply(cat_test[,(names(cat_test))] , as.character)

cat_test[is.na(cat_test)] <- "unavailable"

cat_test[,names(cat_test)] <-lapply(cat_test[,(names(cat_test))] ,factor)



#train
for(i in names(cat_train)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}

#test
for(i in names(cat_test)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_test[[i]])) < p))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}


library(mlr)

summarizeColumns(cat_train)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"]

num_train$age<- cut( num_train$age,breaks = c(0,25,65,90),include.lowest = TRUE,labels = c("young","adult","old"))
num_train$age <- factor(num_train$age)

num_test$age<- cut( num_test$age,breaks = c(0,25,65,90),include.lowest = TRUE,labels = c("young","adult","old"))
num_test$age <- factor(num_test$age)

num_train$wage_per_hour <- ifelse(num_train$wage_per_hour == 0, "Zero","MoreThanZero")
num_train$wage_per_hour <- factor(num_train$wage_per_hour)

num_train$capital_gains <- ifelse(num_train$capital_gains == 0, "Zero","MoreThanZero")
num_train$capital_gains <- factor(num_train$capital_gains)

num_train$capital_losses <- ifelse(num_train$capital_losses == 0, "Zero","MoreThanZero")
num_train$capital_losses <- factor(num_train$capital_losses)

num_train$dividend_from_Stocks <- ifelse(num_train$dividend_from_Stocks == 0, "Zero","MoreThanZero")
num_train$dividend_from_Stocks <- factor(num_train$dividend_from_Stocks)



num_test$wage_per_hour <- ifelse(num_test$wage_per_hour == 0, "Zero","MoreThanZero")
num_test$wage_per_hour <- factor(num_test$wage_per_hour)

num_test$capital_gains <- ifelse(num_test$capital_gains == 0, "Zero","MoreThanZero")
num_test$capital_gains <- factor(num_test$capital_gains)

num_test$capital_losses <- ifelse(num_test$capital_losses == 0, "Zero","MoreThanZero")
num_test$capital_losses <- factor(num_test$capital_losses)

num_test$dividend_from_Stocks <- ifelse(num_test$dividend_from_Stocks == 0, "Zero","MoreThanZero")
num_test$dividend_from_Stocks <- factor(num_test$dividend_from_Stocks)


#Machine Learning 
d_train <- cbind(num_train,cat_train)
d_test <- cbind(num_test,cat_test)


rm(num_train,num_test,cat_train,cat_test)

train.task <- makeClassifTask(data = d_train,target = "income_level")
test.task <- makeClassifTask(data=d_test,target = "income_level")

train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)


#install.packages("FSelector")
library(FSelector)
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)



train.under <- undersample(train.task,rate = 0.1)
table(getTaskTargets(train.under))


train.over <- oversample(train.task,rate=15) #make minority class 15 times
table(getTaskTargets(train.over))

train.smote <- smote(train.task,rate = 15,nn = 5)
table(getTaskTargets(train.smote))


naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
naive_learner$par.vals <- list(laplace = 1)
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)

fun_cv <- function(a){
  crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}
fun_cv (train.task)

fun_cv(train.smote)


nB_model <- train(naive_learner, train.smote)
 nB_predict <- predict(nB_model,test.task)
nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d_test$income_level,nB_prediction)
dCM
