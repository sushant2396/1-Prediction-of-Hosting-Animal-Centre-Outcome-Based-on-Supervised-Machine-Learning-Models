install.packages("VIM")
library(VIM)
install.packages("tidyverse")
library(tidyverse)  
install.packages("mice")
library(mice)
install.packages("tidyr")
library(tidyr)  
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("lubridate")
library(lubridate)
install.packages("wesanderson")
library(wesanderson)
install.packages("caret")
library(caret)
install.packages("randomForest")
library(randomForest)
install.packages("ROCR")
library(ROCR)
install.packages("pROC")
library(pROC)
install.packages("xgboost")
library(xgboost)
install.packages("MLmetrics")
library(MLmetrics)
install.packages("glmnet")
library(glmnet)

convertToDays = function(n,num,age){
  y = vector(mode = "numeric", length = n)
  for (i in 1:n) {
    y[i] <- ifelse(grepl("year",age[i]),num[i] * 365,
                   ifelse(grepl("month", age[i]),num[i] * 30,
                          ifelse(grepl("week", age[i]),num[i] * 7,
                                 ifelse(grepl("day", age[i]),num[i], "" ))))
  }
  return (y)
}

convertToPeriod = function(n,t){
  y = vector(mode = "character", length = n)
  for (i in 1:n) {
    y[i] <- ifelse(t[i]>=6 && t[i] < 12,"Morning",
                   ifelse(t[i]>=12 && t[i] < 18,"Afternoon",
                          ifelse(t[i]>=18 || t[i] < 06,"Night-time", "" )))
  }
  return (y)
}

convertToColor = function(n,t){
  y = vector(mode = "character", length = n)
  for (i in 1:n) {
    y[i] <- ifelse(t[i]=="Lilac"||t[i]=="Lynx"||t[i]=="Silver"||t[i]=="Blue"||t[i]=="Gray","Gray",
                   ifelse(t[i]=="Seal"||t[i]=="Black","Black",
                          ifelse(t[i]=="Agouti"||t[i]=="Chocolate"||t[i]=="Liver"||t[i]=="Ruddy"||t[i]=="Brown","Brown",
                                 ifelse(t[i]=="Apricot"||t[i]=="Flame"||t[i]=="Fawn"||t[i]=="Gold"||t[i]=="Tan"||t[i]=="Red"||t[i]=="Yellow"||
                                          t[i]=="Orange","Orange",
                                        ifelse(t[i]=="Cream"||t[i]=="Buff"||t[i]=="Pink"||t[i]=="White","White",
                                               ifelse(t[i]=="Calico"||t[i]=="Sable"||t[i]=="Tortie"||t[i]=="Torbie"||
                                                        t[i]=="Tricolor","Multi",NA))))))
  }
  return (y)
}

createColorVar = function(n,t1,t2,color){
  y = vector(mode = "numeric", length = n)
  for (i in 1:n) {
    y[i] <- ifelse(t1[i]==color || t2[i]==color,1,0)
  }
  return (y)
}

animalRaw <- read.csv("C:/Users/Sushant Parte/Desktop/thesis/train.csv", stringsAsFactors = F)
test <- read.csv("C:/Users/Sushant Parte/Desktop/thesis/test1.csv",stringsAsFactors = F)
train <- animalRaw
head(train)
dim(train)
str(train)

# cleaning name
train$Name <- ifelse(nchar(train$Name) == 0, "Unknown", train$Name) #Change blank names to 'noname'
train$NameStatus <- if_else(train$Name == 'Unknown', 0, 1) #Add new variable indicating if name known or not

# cleaning sexuponoutcome
table(train$SexuponOutcome)  #'Neutered Male' most common
train$SexuponOutcome[train$SexuponOutcome == ""] = "Neutered Male"
train = tidyr::separate(train, SexuponOutcome, c("IntactStatus", "Sex"), sep = " ") #Separate 'SexuponOutcome' and add 'IntactStatus'
train$Sex[train$IntactStatus=="Unknown"] = "Unknown"

#cleaning date time
train$Year <- year(train$DateTime) #Add 'Year' predictor
train = train %>% mutate(Month = month.abb[month(train$Date)])
train$Weekday <- wday(train$DateTime, label=TRUE) #Add 'wday' predictor
train = tidyr::separate(train, DateTime , c("Date", "Time"), sep = " ", extra = "merge" ) #Separate 'DateTime' 
train$Date = as.Date(train$Date)
train = tidyr::separate(train, Time, c("timeHr", "timeMin"), sep = ":", remove = FALSE, extra = "merge") #Separate 'Time'
train$timeHr = as.numeric(train$timeHr)
train$timePeriod = convertToPeriod(nrow(train), train$timeHr) #Add 'timePeriod' based on Time
train = subset(train, select = -c(timeHr,timeMin))
 
#cleaning breed
train = tidyr::separate(train, Breed, c("Breed", "isMix"), sep = " Mix") #Separate Breed, adding new predictor 'isMix'
train$isMix <- if_else(train$isMix == "", 1, 0, missing = 0) #1 if mixed breed, 0 else

#cleaning ageuponoutcome
train <- tidyr::separate(train, AgeuponOutcome, c("num", "AgeInDays"), sep = " ")
train$num <- as.numeric(train$num)
train$AgeInDays <- convertToDays(nrow(train),train$num,train$AgeInDays)
train$num <- NULL

#cleaning Ageindays
train$AgeInDays[train$AgeInDays == ""] = NA
train$AgeInDays[train$AgeInDays==0] = NA  #22 records with '0 years' changed to NA

set.seed(42)
x1 = c("OutcomeType","NameStatus","AnimalType","IntactStatus","Sex","isMix","AgeInDays","Year","Month","Weekday","timePeriod")
miceDF = train[,x1]
miceDF[,x1] = lapply(miceDF[,x1],as.factor) #Change to correct class
miceDF$AgeInDays = as.numeric(as.character(miceDF$AgeInDays)) #Change Age to numeric
train$AgeInDays = mice::complete(mice(miceDF))$AgeInDays  #Perform mice 
 
# vectorization for color
temp = separate(train, Color, c("color1", "color2"), sep = "/", remove = FALSE) 
temp = separate(temp, color1, c("color1.1", "color1.2"), sep = " ", remove = FALSE, extra = "merge")
temp = separate(temp, color2, c("color2.1", "color2.2"), sep = " ", remove = FALSE, extra = "merge")
temp$RColor1 = convertToColor(nrow(temp),temp$color1.1)
temp$RColor2 = convertToColor(nrow(temp),temp$color2.1)
temp$RColor2[is.na(temp$RColor2)] = " "
train$isBlack = createColorVar(nrow(temp),temp$RColor1,temp$RColor2,"Black") #Black,Seal
train$isGray = createColorVar(nrow(temp),temp$RColor1,temp$RColor2,"Gray") #Gray,Blue,Lilac,Lynx,Silver
train$isBrown = createColorVar(nrow(temp),temp$RColor1,temp$RColor2,"Brown") #Brown,Agouti,Chocolate,Liver,Ruddy
train$isOrange = createColorVar(nrow(temp),temp$RColor1,temp$RColor2,"Orange") #Orange,Apricot,Flame,Fawn,Gold,Tan,Red,Yellow
train$isWhite = createColorVar(nrow(temp),temp$RColor1,temp$RColor2,"White") #White,Cream,Buff,Pink
train$isMulti = createColorVar(nrow(temp),temp$RColor1,temp$RColor2,"Multi") #Calico,Sable,Tortie,Torbie,Tricolor

train = subset(train,select = -c(OutcomeSubtype,AnimalID,Name,Date,Time,Breed,Color))

#Change predictors to correct classes
str(train)
x2 = c("OutcomeType","AnimalType","IntactStatus","Sex","isMix","NameStatus","Year","Month","timePeriod","isBlack","isGray","isBrown","isOrange","isWhite","isMulti")
train[,x2] = lapply(train[,x2],as.factor)
str(train)
write.csv(train, "C:/Users/Sushant Parte/Desktop/thesis/my_file.csv") 
inTr = createDataPartition(train$OutcomeType, p=.8, list = FALSE)
tr = train[inTr,]
te = train[-inTr,]

#caret used to tune Random Forest, Neural Networks, and XGBoosted
#Sets up 5-fold cross validation for train function of caret
set.seed(123)
cv.5.folds = createMultiFolds(tr$OutcomeType, k = 5, times = 5)
ctrl = trainControl(method="cv", number = 5, index = cv.5.folds, 
                    classProbs = TRUE, summaryFunction = multiClassSummary)
#nn
set.seed(456)
nn.model = train(OutcomeType ~ ., data = tr, method = "nnet", 
                 tuneLength = 5, trControl = ctrl, metric = "logLoss")
#The final values used for the model were size = 9 and decay = 0.1.
predNN = predict(nn.model, newdata = te, type = "raw")
confusionMatrix(predNN, te$OutcomeType) #confusion matrix
#Size & decay tuning plot
plot(nn.model, main = "NN Size & Decay Tuning",
     cex.axis = 1.5, cex.lab = 1.5)


#xgb
#Creates grid which parameters will be tuned for
#tuning for depth, gamma, colsample_bytree, and min_child_weight
xgb_grid = expand.grid(nrounds = 30, max_depth = c(3, 6, 10 ), eta = 0.2, gamma = c(0.1,.5,1), 
                       colsample_bytree = c(0.4, 0.7, 1.0), min_child_weight = c(0.5, 1, 1.5), subsample = 1)
set.seed(234)
xgb_model = train(OutcomeType ~ ., data = tr, method = "xgbTree",
                  tuneGrid = xgb_grid, trControl = ctrl, metric = "logLoss")
#optimal parameters
#nrounds max_depth eta gamma colsample_bytree min_child_weight
#     30         6 0.2   0.1                1            1
predXGB = predict(xgb_model, newdata=te, type = "raw")
confusionMatrix(predXGB, te$OutcomeType) #confusion matrix
#Get importance from optimal XGBoost model
Impor = varImp(xgb_model)$importance

#Logistic Regression
#Data preparation for glmnet
x <- model.matrix(OutcomeType~.,data = tr)
y <- tr$OutcomeType
train_rows <- sample(1:dim(x)[[1]], .70*dim(x)[[1]])
training.x <- x[train_rows,]
testing.x <- x[-train_rows,]

#glmnet with 5-fold cross validation
set.seed(567)
cvfit <- cv.glmnet(x, y, family="multinomial", type.multinomial = "grouped", parallel = TRUE, nfolds = 5)

plot(cvfit)  #cross validation curve
predGLM <- predict(cvfit, newx = testing.x, s = "lambda.min", type = "class")
confusionMatrix(factor(predGLM[,1]), factor(y[-train_rows])) #confusion matrix
# plot roc and auc value
roc.cv <- multiclass.roc(as.numeric(as.factor(predGLM)), as.numeric(y[-train_rows]))
rocb <- roc(as.numeric(as.factor(predGLM)), as.numeric(y[-train_rows]))
plot(rocb, print.auc=T, auc.polygon=T, max.auc.polygon=T,
     auc.polygon.col="yellow", print.thres=T, col = "green")

#Rf
set.seed(321)
rf.model = train(OutcomeType ~ ., data = tr, method = "rf",
                 tuneLength = 3, ntree = 500, trControl = ctrl,metric = "logLoss")
#The final value used for the model was mtry = 19. 
predRF = predict(rf.model, te, probability = TRUE)
confusionMatrix(predRF, te$OutcomeType) #confusion matrix
#Outcome type error rates & OOB plot
plot(rf.model$finalModel, main = "RF Outcome Type Error Rates",
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)
legend("topright",c("OOB","Adopt","Death","Euthanasia","Returned", "Transfer"),lty=1:6,fill = 1:6)

my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
#Store confusion matrices for each algorithm
glm.ConfMat = as.matrix.data.frame(confusionMatrix(predGLM[,1], y[-train_rows])$table)
rf.ConfMat = as.matrix.data.frame(confusionMatrix(predRF, te$OutcomeType)$table)
nn.ConfMat = as.matrix.data.frame(confusionMatrix(predNN, te$OutcomeType)$table)
xgb.ConfMat = as.matrix.data.frame(confusionMatrix(predXGB, te$OutcomeType)$table)

#GLM heat map
heatmap.2(glm.ConfMat, cellnote = glm.ConfMat, notecex = 2 ,  main = "Logistic Regression Confusion Matrix Heatmap",
          notecol="black", density.info="none", trace="none", margins =c(12,9), col=my_palette,revC = TRUE,  
          dendrogram="row", Colv="NA",Rowv = NULL, labRow = c("Adoption", "Died", "Euthanasia", "Returned", "Transfer"),
          labCol = c("Adoption", "Died", "Euthanasia", "Returned", "Transfer"), cexRow = 1.5, cexCol = 1.5, key = FALSE)
#RF heat map
heatmap.2(rf.ConfMat, cellnote = rf.ConfMat, notecex = 2 ,  main = "Random Forest Confusion Matrix Heatmap",
          notecol="black", density.info="none", trace="none", margins =c(12,9), col=my_palette,  
          dendrogram="row", Colv="NA", labRow = c("Adoption", "Died", "Euthanasia", "Returned", "Transfer"),
          labCol = c("Adoption", "Died", "Euthanasia", "Returned", "Transfer"), cexRow = 1.5, cexCol = 1.5, key = FALSE)

#NN heat map
heatmap.2(nn.ConfMat, cellnote = nn.ConfMat, notecex = 2 ,  main = "Neural Network Confusion Matrix Heatmap",
          notecol="black", density.info="none", trace="none", margins =c(12,9), col=my_palette,  
          dendrogram="row", Colv="NA", labRow = c("Adoption", "Died", "Euthanasia", "Returned", "Transfer"),
          labCol = c("Adoption", "Died", "Euthanasia", "Returned", "Transfer"), cexRow = 1.5, cexCol = 1.5, key = FALSE)
#XGB heat map
heatmap.2(xgb.ConfMat, cellnote = xgb.ConfMat, notecex = 2 ,  main = "XGBoost Confusion Matrix Heatmap",
          notecol="black", density.info="none", trace="none", margins =c(12,9), col=my_palette,  
          dendrogram="row", Colv="NA", labRow = c("Adoption", "Died", "Euthanasia", "Returned", "Transfer"),
          labCol = c("Adoption", "Died", "Euthanasia", "Returned", "Transfer"), cexRow = 1.5, cexCol = 1.5)
