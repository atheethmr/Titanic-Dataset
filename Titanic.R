library(readxl)
library(caret)
library(e1071)
library(randomForest)
library(mass)
library(doSNOW)

set.seed(100)
trainingVec<- sample(1:nrow(dat), ceiling(nrow(dat)*0.75))
train<- dat[trainingVec, ]
Validation<- dat[-trainingVec, ]


# to take a part of name and create a vector with the part of the name

titles <- as.vector(NULL)
for (i in 1:nrow(train)) { 
  if( str_detect(train$Name[i], "Miss. ") == TRUE) {
    titles <- c(titles, "Miss. ")
  } else if( str_detect(train$Name[i], "Mrs. ") == TRUE) {
    titles <- c(titles, "Mrs")
  } else if(str_detect(train$Name[i], "Mr. ") == TRUE) {
    titles <- c(titles, "Mister")
  } else if(str_detect(train$Name[i], "Master. ") == TRUE) {
    titles<- c(titles, "Master")
  } else {
    titles <- c(titles, "Others")
  }
}
### To create a new column in an existing dataframe
train$titles <- as.factor(titles)

ggplot(train, aes(x= titles, fill = Survived) ) + geom_bar(stat = "count") + facet_wrap(~Pclass) + ggtitle("Pclass") + xlab("Titles") + ylab("Total Count") + labs(fill = "Survived")

# to remove the first letter from cabin column and create cabininfo column with blanks replaced with "U"
train$cabininfo <- c(substr(train$cabin, 1,1) # create a new column cabininfo with first letter of cabin column.
                     d<- as.vector(train$cabininfo)
                     for(i in 1:891){
                       if(d[i]== ""){
                         d[i] = "U"
                       }
                     }
                     ## OR ##
                     train[which(train$Cabin == ""), "Cabin"] <- "U"
                     
                     # build model using Random Forest 1st go  -> error rate 17%
                     e<- train[, c("titles","Pclass","familysize")]
                     f<- train[,"Survived"]
                     set.seed(100)
                     rf<- randomForest(x = e, y = f, improtance = TRUE, ntree = 1000)
                     
                     # build model using Random Forest 2st go --> error rate 16%
                     e<- train[, c("titles","Pclass","familysize","Fare")]
                     f<- train[,"Survived"]
                     set.seed(100)
                     rf<- randomForest(x = e, y = f, improtance = TRUE, ntree = 800)
                     rf
                     varImpPlot(rf)
                     
                     #re-engineering test dataset to the training dataset
                     test.name<- test$Name
                     test.namefn<- function(x){
                       if(str_detect(x, "Miss. ")){
                         return("Miss")
                       }else if(str_detect(x, "Mr. ")){
                         return("Mister")
                       }else if(str_detect(x, "Mrs. ")){
                         return("Mrs")
                       }else if(str_detect(x, "Master. ")){
                         return("Master")
                       }else {
                         return("Others")
                       }
                     }
                     titles<- as.vector(NULL)
                     for(i in 1: nrow(test)){
                       titles<- c(titles,test.namefn(test.name[i]))
                     }
                     test$titles<- titles
                     
                     test$familysize<- c(test$SibSp + test$Parch + 1)
                     
                     cabininfo<- as.character(substr(test$Cabin, 1, 1))
                     
# predict test dataset
t<- test[, c("titles","Pclass", "familysize","Fare")]
set.seed(100)
p<- predict(rf, t, type= "response", norm.votes= TRUE, predict.all = FALSE, proximity= FALSE, nodes= FALSE)
                     
# to handle NAs
na.roughfix(df)  ## it can handle only numeric and factor variables. replaces NAs with the median factor or numeric available in that column.
rfImpute(y~ ., dat)
                     
##### Cross Validation
library(caret)
library(doSNOW)
                     
##To create folds
set.seed(100)
cr.fold<- createMultiFolds(y = train$Survived, k = 10, times = 10)
                     
#to use traincontrol function
                     
cv.train<- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cr.fold)
                     
## To do parallel computing (Using doSNOW package)
cl<- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
                     
#train the data, preprocessing is available(Lot of awesome functions, please read help doc)
                     
set.seed(120)
cv.model<- train(x = e, y = f, method = "rf", tunelength = 2, ntree = 1000, trcontrol = cv.train)
                     
#to stop parallel computing
stopCluster(cl)
                     
                     
cv<- createMultiFolds(train$Survived, k = 10, times = 10) ----- forms list of 100sets of dataset
                     
cvtrain<- traincontrol(method = "repeatedcv", number = 10, repeats = 10, index = cv)
                     
cvmodel<- train(x = e, y = f, method = "rf", ntree = 1000, tunelength = 2, trcontrol = cvtrain)
                     