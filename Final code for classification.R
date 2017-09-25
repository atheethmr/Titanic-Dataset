library(readxl)
library(caret)
library(e1071)
library(randomForest)
library(mass)
library(doSNOW)
dat <- read_excel("~/Desktop/KRAS_features_normalbyBoth.xlsx")
dat1<- read_excel("~/Desktop/Mayo Clinic data/Key_NoPHI.xlsx")

write.csv(a, file = "modified.csv")

dat2<- dat1[-c(1,101,102),]
a<- as.vector(NULL)
for(i in 1:nrow(dat2)) {
  if(str_detect(dat2[i,3], "Muta") == TRUE) {
    a <- c(a, "Mutant")
  } else if(str_detect(dat2[i,3], "Wild Type") == TRUE) {
    a<- c(a, "Wild")
  } else {
    a<- c(a,"Mutant")
  }
}

clean <- data.frame(dat[,-1], a)
colnames(clean)[43] <- "PType"

## remove extra columns not used for analysis
a <- clean[, -c(1,2,3,4,5)]

c1<- a[,c(2 ,27  ,4  ,8  ,9  ,1 ,29 ,10 ,11 ,12 ,33  ,7 ,35 ,28 ,31 ,25  ,3 ,17 ,24,  5, 22, 14, 19,23,6,30, 38)]
View(c1)
### Recursive feature Elimination method to select variables ---------> 4 features are efficient in predicting the results
### RFE == It eliminates features with less weight and constructs model iteratively.

tr.rfe <- rfeControl(functions = rfFuncs, method = 'repeatedcv', number = 20, repeats = 2)
varByRFE <- rfe(x = c1[,-27], y = c1[,27], sizes = c(4:20), rfeControl = tr.rfe)
varByRFE$variables[,c(4,5)]
predictors(varByRFE)
plot(varByRFE)

## selected variables
c<- a[,c(4,2,3,7,8,10,25,26,27,16,29,32,35,33,22,14,15,17,38)]
d <- c1[, c(1,2,3,4,7,9,11,12,13,15,16,17,20,21,22,24,27)]

cv.train<- trainControl(method = "LOOCV")

first.rf <- train(x = c[,-19], y = c[,19], method = "rpart", ntree = 1000, trControl = cv.train)
first.rf
first.rf$finalModel

## SVM with tuned parameters and variable selection ----------> 80.8%
svm_5 <- svm(PType ~ ., data = d , kernel = "radial" , cost = 1.5, gamma = 0.1)
summary(svm_5)
predi3 <- predict(svm_5, d[,-17])
table(d[,17],predi3)


## SVM with LOOCV and limited variables -----------> 76 %

cv.train<- trainControl(method = "LOOCV")

svm3 <- train(x = d[,-17], y = d[,17], method = "svmRadial", gamma = 0.1, cost = 1.5, trControl = cv.train)
svm3$results

pred <- predict(svm3, d[,-17])

table(d[,17], pred)

svm3$finalModel

## Graphs of each components

plot(d$Gabor_Std_1.0_0.5, d$Sum.of.Squares..Variance_Avg, col = d$PType)

## RFE algorithm
tr.rfe <- rfeControl(functions = rfFuncs, method = 'repeatedcv', number = 20, repeats = 2)
varByRFE <- rfe(x = a[,-38], y = a[,38], sizes = a(4:28), rfeControl = tr.rfe)
varByRFE$variables[,c(4,5)]
predictors(varByRFE)
plot(varByRFE)

### getting plot for each variable
for(i in 1:(ncol(d)-1)){
  plot(d[,i], d[,17], col = d$PType)
}