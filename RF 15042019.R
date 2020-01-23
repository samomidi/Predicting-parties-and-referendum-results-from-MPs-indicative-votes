# Import data --------
data <- read.csv("data/mpdataFinal.csv", header=T)
data <- as.data.frame(unclass(data))
data$GroupedPercent <- as.factor(data$GroupedPercent)

# split for holdout
set.seed(5059)
train.indices <- createDataPartition(data$Party, p=0.8, list=FALSE)
train <- data[train.indices,]
holdout <- data[-train.indices,]

# Remove rows with NA in BetterLeavePercent and ConstituencyVote
train.rmNA <- na.omit(train)

# Random Forest Models Names ---------
# Train Models:
# rf.party2.omit
# rf.partygp2.omit
# rf.leave2.omit
# rf.vote2.omit
# rf.gpp22.omit

# # RF Models:
# model.party2
# model.partygp2
# model.leave2
# model.vote2
# model.gpp22

# library -------------
require(randomForest)
require(caret)
library(MLmetrics)
require(pROC)
source("ROC code.R")

## Run Parallel
require(doParallel)
require(foreach)

cores <- makeCluster(detectCores()-1)
registerDoParallel(cores = cores)

## Function -----------
# Random Forest with mtry
# Objective:
# to perform Random Forest with repeated 5-fold crossvalidation under caret package
# Input Arguments:
# DM - matrix ocontaining the exploratory variables
# y - vector containing the dependent variable
# naaction - na
# Output:
# model_kfCV - train class output

rf.cv <- function(DM, y, naaction){
  # set seed
  set.seed(5059)
  ## traincontrol
  cvcontrol <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                            classProbs = TRUE, verboseIter = FALSE, allowParallel = TRUE)
  # define TuneLength
  tune <- (length(DM)-1)

  # train model
  model_kfCV <- train(DM,y,method = "rf", trControl = cvcontrol,
    metric = ifelse(is.factor(y), "Accuracy", "RMSE"), ntree = 1000,
    tuneLength = tune, importance = TRUE, na.action=naaction)
  
  return(model_kfCV)
}

# OOB Error Plot
# Objective:
# to perform OOB Error Plot for each class under Random Forest 
# model - random forest model
# Output:
# OOB Error Plot
plot.er <- function(model){
  noclass <- length(model$classes)  
  layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
  par(mar=c(5,4,4,0)) #No margin on the right side
  plot(model, col=rainbow(noclass), main = "OOB Error rate for each class" )
  par(mar=c(5,0,4,2)) #No margin on the left side
  plot(c(0,1),type="n", axes=F, xlab="", ylab="")
  legend("top", colnames(model$err.rate),col=1:11,cex=0.8,fill=rainbow(noclass))
  par(mar=c(0,0,0,0))
}
# Analysis
# Party ========
rf.party2.omit <- rf.cv(train[,7:18], train[,1], na.omit)
rf.party2.omit$result
varImp(rf.party2.omit)
plot(varImp(rf.party2.omit), main="Variables Importance plot")
confusionMatrix(rf.party2.omit)
trellis.par.set(caretTheme())
plot(rf.party2.omit, main="Tuning Parameter plot")
model.party2 <- rf.party2.omit$finalModel
plot.er(model.party2)

train.rmNA$predictions <- predict(model.party2, train.rmNA, type = 'response')
multiclass.roc(as.numeric(train.rmNA$Party), as.numeric(train.rmNA$predictions))

# Party Group ===========
rf.partygp2.omit <- rf.cv(train[,7:18], train[,2], na.omit)
rf.partygp2.omit$result
varImp(rf.partygp2.omit)
plot(varImp(rf.partygp2.omit), main="Variables Importance plot")
confusionMatrix(rf.partygp2.omit)
trellis.par.set(caretTheme())
plot(rf.partygp2.omit, main="Tuning Parameter plot")
model.partygp2 <- rf.partygp2.omit$finalModel
plot.er(model.partygp2)

train.rmNA$predictions <- predict(model.partygp2, train.rmNA, type = 'response')
multiclass.roc(as.numeric(train.rmNA$PartyGroup), as.numeric(train.rmNA$predictions))

# Leave ==================
rf.leave2.omit <- rf.cv(train.rmNA[,7:18], train.rmNA[,3], na.omit)
rf.leave2.omit$result
varImp(rf.leave2.omit)
plot(varImp(rf.leave2.omit), main="Variables Importance plot")
trellis.par.set(caretTheme())
plot(rf.leave2.omit, main="Tuning Parameter plot")
model.leave2 <- rf.leave2.omit$finalModel
plot(model.leave2, main="OOB Error rate")
train.rmNA$predictions <- predict(model.leave2, train.rmNA, 
                                            type = 'response')

r <- multiclass.roc(as.numeric(train.rmNA$BetterLeavePercent), as.numeric(train.rmNA$predictions)) #run forever

# Constituency Vote ========
rf.vote2.omit <- rf.cv(train.rmNA[,7:18], train.rmNA[,5], na.omit)
rf.vote2.omit$result
varImp(rf.vote2.omit)
plot(varImp(rf.vote2.omit))
confusionMatrix(rf.vote2.omit, main="Variables Importance plot")
trellis.par.set(caretTheme())
plot(rf.vote2.omit, main="Tuning Parameter plot")
model.vote2 <- rf.vote2.omit$finalModel
plot.er(model.vote2)

train.rmNA$predictions <- predict(model.vote2, train.rmNA, 
                                            type = 'response')
multiclass.roc(as.numeric(train.rmNA$ConstituencyVote), as.numeric(train.rmNA$predictions))

# Grouppercent2========
rf.gpp22.omit <- rf.cv(train.rmNA[,7:18], train.rmNA[,6], na.omit)
rf.gpp22.omit$result
varImp(rf.gpp22.omit)
plot(varImp(rf.gpp22.omit))
confusionMatrix(rf.gpp22.omit, main="Variables Importance plot")
trellis.par.set(caretTheme())
plot(rf.gpp22.omit, main="Tuning Parameter plot")
model.gpp22 <- rf.gpp22.omit$finalModel
plot.er(model.gpp22)

train.rmNA$predictions <- predict(model.gpp22, train.rmNA, 
                                 type = 'response')
multiclass.roc(as.numeric(train.rmNA$ConstituencyVote), as.numeric(train.rmNA$predictions))

stopCluster(cores)
