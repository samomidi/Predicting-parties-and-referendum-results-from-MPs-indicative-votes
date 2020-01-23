#### Libraries ####

### Install all required libraries
# ipak <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, require, character.only = TRUE)
# }
# # install all packages at once
# packages <- c('tidyverse', 'maptools', 'rgdal', 'rgeos', 'ggplot2', 'mapproj', 'caret', 'nnet', 'kernlab', 'doParallel', 'foreach', 'randomForest', 'xgboost', 'skimr, ada', 'adabag', 'fastAdaboost', 'gbm', 'pROC', 'naivebayes', 'e1071', 'klaR', 'rpart', 'rattle', 'RColorBrewer')
# ipak(packages)

library(tidyverse)
library(maptools)
library(rgdal)
library(rgeos)
library(ggplot2)
library(mapproj)
library(caret)
library(nnet)
library(kernlab)
library(doParallel)
library(foreach)
library(randomForest)
library(xgboost)
library(skimr)
library(ada)
library(adabag)
library(fastAdaboost)
library(gbm)
library(pROC)
library(naivebayes)
library(e1071)
library(klaR)
library(rpart)
library(rattle)
library(RColorBrewer)

### set path to files directory ####
path <- "Submission/"
source(paste(path,"code_and_outputs/functions.R",sep=''))

#### Importing data ####
mpdata <- read_csv(paste(path,"data/mpdataFinal.csv",sep=''))

# Exploratory -------------------------------------------------------------
#### Choropleths ####
ukMap <- readOGR('Shapefiles/Westminster_Parliamentary_Constituencies_December_2017_Generalised_Clipped_Boundaries_in_the_UK.shp')
ukMapf <- fortify(ukMap, region = "pcon17nm") %>% rename(Geography = id) %>% arrange(Geography)
mpdata$Geography <- mpdata$Constituency

# Fixing geographies
ukMapf$Geography <- gsub('[[:punct:] ]+',' ',ukMapf$Geography)
mpdata$Geography <- replace(mpdata$Geography, mpdata$Name=="Albert Owen", "Ynys Mon")
geos.map <- sort(unique(ukMapf$Geography))
geos.map <- geos.map[!geos.map == "Newport West"]
geos.map <- geos.map[!geos.map == "Buckingham"]
mpdata <- mpdata[order(mpdata$Geography),]
mpdata$Geography <- geos.map

leavePercentMap <- UKChoroplethFunction(ukMapf, mpdata, 'BetterLeavePercent', "Leave Percent", "Leave Percent across UK Geographies")
leavePercentMap <- leavePercentMap + scale_fill_gradient(aes(fill = ""), low = 'green', high = 'red')
leavePercentMap

partyMap <- UKChoroplethFunction(ukMapf, mpdata, 'Party', "Party", "Party", FALSE)
party.color.scheme <- c("deepskyblue4", "lightgoldenrodyellow", "Green", "Black", "Red", "Orange", "Light Green", "Dark Green", "Yellow", "Dark Red")
partyMap <- partyMap + scale_fill_manual(values=party.color.scheme)
partyMap

#### Graphs ####
# All Parties
mpdata <- read_csv("data/mpdataFinal.csv")
mpdata.gathered <- mpdata %>% gather("VoteID", "Vote", -Party, -PartyGroup, -BetterLeavePercent, -GroupedPercent, -GroupedPercent2, -ConstituencyVote, -Name, -Constituency)
mpdata.gathered$VoteID <- factor(mpdata.gathered$VoteID, levels=c("Vote1", "Vote2", "Vote3", "Vote4", "Vote5", "Vote6", "Vote7", "Vote8", "SecondVote1", "SecondVote2", "SecondVote3", "SecondVote4"))
levels(mpdata.gathered$VoteID) <- c("Vote1.1", "Vote1.2", "Vote1.3", "Vote1.4", "Vote1.5", "Vote1.6", "Vote1.7", "Vote1.8", "Vote2.1", "Vote2.2", "Vote2.3", "Vote2.4")

party.nums <- data.frame(mpdata %>% group_by(Party) %>% summarise(mps = n()))
rownames(party.nums) <- party.nums$Party
summ.party <- data.frame(mpdata.gathered %>% group_by(Party, VoteID, Vote) %>% 
                           summarise(count = n(), meanleave = mean(BetterLeavePercent))) %>%
                           arrange(VoteID)
summ.party$proportion <- NA
for (i in 1:nrow(summ.party)) {
  summ.party$proportion[i] <- summ.party[i, 'count']/party.nums[summ.party[i,'Party'],2]
}

# reorder factors to get for,abstain,against order in plots
summ.party$Vote <- factor(summ.party$Vote, levels = c("Against","Abstain","For"))
# colours for votes
gucci.colours <- c("#A82B11", "goldenrod", "#0A6A56")

# summ.party$proportion <- apply(summ.party, 1, function(x) return(x[['count']]/party.nums[x[['Party']],2]))
party.proportions.plot <- ggplot(party.nums, aes(x=0,y=mps, fill=Party, group=mps)) + 
  geom_col() + 
  scale_fill_manual(values=party.color.scheme) + scale_y_continuous(breaks = c(0,314,559,594,648))
party.proportions.plot

# votes percentages
vote.labels <- c('1.1',	'1.2',	'1.3','1.4','1.5'	,'1.6'	,'1.7',	'1.8',	'2.1',	'2.2',	'2.3',	'2.4')

vote.percentages.plot <- ggplot(summ.party, aes(x=VoteID, y=proportion, fill=Vote)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual(values=gucci.colours) + scale_x_discrete(labels = vote.labels)
vote.percentages.plot

all.parties.plot <- ggplot(summ.party, aes(x=VoteID, y=proportion, fill=Vote)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual(values=gucci.colours) +
  facet_grid(rows=vars(Party)) + scale_y_continuous(breaks = c()) + scale_x_discrete(labels = vote.labels)
all.parties.plot

all.parties.vote.plot <- ggplot(summ.party, aes(x=Party, y=proportion, fill=Vote)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual(values=gucci.colours) +
  facet_grid(rows=vars(VoteID)) + scale_y_continuous(breaks = c())
all.parties.vote.plot

# Party Group
party.nums <- data.frame(mpdata.gathered %>% group_by(PartyGroup) %>% summarise(mps = n()/12))
rownames(party.nums) <- party.nums$PartyGroup
summ.party <- data.frame(mpdata.gathered %>% group_by(PartyGroup, VoteID, Vote) %>% summarise(count = n()))
summ.party$proportion <- NA
for (i in 1:nrow(summ.party)) {
  summ.party$proportion[i] <- summ.party[i, 'count']/party.nums[summ.party[i,'PartyGroup'],2]
}
# reorder factors to get for,abstain,against order in plots
summ.party$Vote <- factor(summ.party$Vote, levels = c("Against","Abstain","For"))

# summ.party$proportion <- apply(summ.party, 1, function(x) return(x[['count']]/party.nums[x[['Party']],2]))

parties.group.plot <- ggplot(summ.party, aes(x=VoteID, y=proportion, fill=Vote)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual(values=gucci.colours) +
  facet_grid(rows=vars(PartyGroup)) + scale_x_discrete(labels = vote.labels)
parties.group.plot

all.parties.group.vote.plot <- ggplot(summ.party, aes(x=PartyGroup, y=proportion, fill=Vote)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual(values=gucci.colours) +
  facet_grid(cols=vars(VoteID))
all.parties.group.vote.plot


# Party Leave Percents
parties.leave <- mpdata %>% group_by(Party) %>% summarise(mean = mean(BetterLeavePercent, na.rm = TRUE), sd=sd(BetterLeavePercent, na.rm=TRUE))
ggplot(parties.leave, aes(x=Party, y=mean, fill=Party)) + geom_col() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) +
  scale_fill_manual(values=party.color.scheme)

colfunc <- colorRampPalette(c("green", "red"))
ggplot() + geom_histogram(aes(x=mpdata$BetterLeavePercent),fill = colfunc(30)) + scale_x_continuous(limits = c(0,1)) + labs(x = "Constituency Leave %", y = "Count")


#### Descriptive stats ####
desc.data <- dplyr::select(mpdata, -Name, -Constituency)
skimmed.data <- skim_to_wide(desc.data) 
View(skimmed.data[, c(1:10, 16)])


# Modelling ---------------------------------------------------------------
#### Setting up data ####
# Import data
data <- mpdata
data <- as.data.frame(unclass(data))
data$GroupedPercent <- as.factor(data$GroupedPercent)

# Generate holdout split
set.seed(5059)
train.indices <- createDataPartition(data$Party, p=0.8, list=FALSE)
train <- data[train.indices,]
holdout <- data[-train.indices,]
train.no.NA <- na.omit(train)
holdout.no.NA <- na.omit(holdout)

# Generate dummy data
matrix <- dummyVars(~ Vote1 + Vote2 + Vote3 + Vote4 + Vote5 + Vote6 + Vote7 + Vote8 + 
                      SecondVote1 + SecondVote2 + SecondVote3 + SecondVote4,
                    data = data)
dummyData <- data.frame(predict(matrix,newdata=data))
dummyData <- cbind(data[c(1,2,3,4,5,6)],dummyData)
covars <- paste(c(colnames(dummyData[-c(1,2,3,4,5,6)])),collapse="+")
train.dummy <- dummyData[train.indices,]
train.dummy.no.NA <- na.omit(train.dummy)
holdout.dummy <- dummyData[-train.indices,]
holdout.dummy.no.NA <- na.omit(holdout.dummy)

# Generate grid for NN and SVMs
nn.grid <- expand.grid(.decay = c(0.5,0.1), .size = c(2,4,8,12))
svm.grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

# Setting up parralellisation
cores <- makeCluster(detectCores()-2)
registerDoParallel(cores = cores)

# Covars for binary tree
bin.covars <- colnames(train)[7:18]


#### Party ####
party.models <- list()
# names(party.models) <- c("NN", "SVM", "Tree", "RF", "XGB", "GBM", "GLM", "NB")
### Neural Net ###
party.models$NN <- nn.cv("Party", covars, train.dummy, nn.grid)
### SVM ###
party.models$SVM <- svm.cv("Party", covars, train.dummy, svm.grid)
### Binary Tree ###
party.models$Tree <- binary.tree.cv(train[,"Party"], train[,7:18])
### Random Forest ###
party.models$RF <- rf.cv(train[,7:18], train[,1], na.omit)
### XGBoost Tree ###
party.models$XGB <- xg.cv2(1, train, na.omit)
### Generalized Boosted Regression Modelling ###
party.models$GBM <- gbm.caret.func(DM = train[, 7:18], y = train[, 1])
### GLM ###
party.models$GLM <- multinom.cv(train[,"Party"], train[,7:18])
### Naive Bayes ###
party.models$NB <- nb.cv(train[,"Party"], train[,7:18])

#### Party Group ####
party.grp.models <- list()
# names(party.grp.models) <- c("NN", "SVM", "Tree", "RF", "XGB", "GBM", "GLM", "NB")
### Neural Net ###
party.grp.models$NN <- nn.cv("PartyGroup", covars, train.dummy, nn.grid)
### SVM ###
party.grp.models$SVM <- svm.cv("PartyGroup", covars, train.dummy, svm.grid)
### Binary Tree ###
party.grp.models$Tree <- binary.tree.cv(train[,"PartyGroup"], train[,7:18])
### Random Forest ###
party.grp.models$RF <- rf.cv(train[,7:18], train[,2], na.omit)
### XGBoost Tree ###
party.grp.models$XGB <- xg.cv2(2, train, na.omit) 
### Generalized Boosted Regression Modelling ###
party.grp.models$GBM <- gbm.caret.func(DM = train[, 7:18], y = train[, 2])
### GLM ###
party.grp.models$GLM <- multinom.cv(train[,"PartyGroup"], train[,7:18])
### Naive Bayes
party.grp.models$NB <- nb.cv(train[,"PartyGroup"], train[,7:18])

#### Constituency Vote Result ####
ref.vote.models <- list()
# names(ref.vote.models) <- c("NN", "SVM", "RF", "XGB", "GBM", "ada", "adaboost", "GLM", "NB")
### Neural Net
ref.vote.models$NN <- nn.cv("ConstituencyVote", covars, train.dummy.no.NA, nn.grid, linout=FALSE)
### SVM ###
ref.vote.models$SVM <- svm.cv("ConstituencyVote", covars, train.dummy.no.NA, svm.grid)
### Binary Tree ###
ref.vote.models$Tree <- binary.tree.cv(train[,"ConstituencyVote"], train[,7:18])
### Random Forest ###
ref.vote.models$RF <- rf.cv(train.no.NA[,7:18], train.no.NA[,6], na.omit)
### XGBoost Tree ###
ref.vote.models$XGB <- xg.cv2(6, train.no.NA, na.omit)
### Generalized Boosted Regression Modelling ###
ref.vote.models$GBM <- gbm.caret.func(DM = train.no.NA[, 7:18], y = train.no.NA[, 6])
### Adaboost ###
ref.vote.models$ada <- ada.caret.func(DM = train.no.NA[ , 7:18], y = train.no.NA[, 6], Ada.method = 'ada')
ref.vote.models$adaboost <- ada.caret.func(DM = train.no.NA[ , 7:18], y = train.no.NA[, 6], Ada.method = 'adaboost')
### GLM ###
ref.vote.models$GLM <- multinom.cv(train.no.NA[,'ConstituencyVote'], train.no.NA[,7:18])
### Naive Bayes ###
ref.vote.models$NB <- nb.cv(train.no.NA[,'ConstituencyVote'], train.no.NA[,7:18])

#### Constituency Leave Percent - Discretised by Quantile ####
leave.quant.models <- list()
# names(leave.quant.models) <- c("NN", "SVM", "RF", "XGB", "GBM", "GLM", "NB")
### Neural Net ###
leave.quant.models$NN <- nn.cv("GroupedPercent", covars, train.dummy.no.NA, nn.grid)
### SVM ###
leave.quant.models$SVM <- svm.cv("GroupedPercent", covars, train.dummy.no.NA, svm.grid)
### Binary Tree ###
leave.quant.models$Tree <- binary.tree.cv(train[,"GroupedPercent"], train[,7:18])
### Random Forest ###
leave.quant.models$RF <- rf.cv(train.no.NA[,7:18], train.no.NA[,4], na.omit)
### XGBoost Tree ###
leave.quant.models$XGB <- xg.cv2(4, train.no.NA, na.omit) 
### Generalized Boosted Regression Modelling ###
leave.quant.models$GBM <- gbm.caret.func(DM = train.no.NA[, 7:18], y = train.no.NA[,4])
### GLM ###
leave.quant.models$GLM <- multinom.cv(train.no.NA[,"GroupedPercent"], train.no.NA[,7:18])
### Naive Bayes ###
leave.quant.models$NB <- nb.cv(train.no.NA[,"GroupedPercent"], train.no.NA[,7:18])

#### Constituency Leave Percent - Discretised by Remain/Leave/StrongLeave ####
leave.disc.models <- list()
# names(leave.disc.models) <- c("NN", "SVM", "RF", "XGB", "GBM", "GLM", "NB")
### Neural Net ###
leave.disc.models$NN <- nn.cv("GroupedPercent2", covars, train.dummy.no.NA, nn.grid)
### SVM ###
leave.disc.models$SVM <- svm.cv("GroupedPercent2", covars, train.dummy.no.NA, svm.grid)
### Binary Tree ###
leave.disc.models$Tree <- binary.tree.cv(train[,"GroupedPercent2"], train[,7:18])
### Random Forest ###
leave.disc.models$RF <- rf.cv(train.no.NA[,7:18], train.no.NA[,5], na.omit)
### XGBoost Tree ###
leave.disc.models$XGB <- xg.cv2(5, train.no.NA, na.omit) 
### Generalized Boosted Regression Modelling ###
leave.disc.models$GBM <- gbm.caret.func(DM = train.no.NA[, 7:18], y = train.no.NA[, 5])
### GLM ###
leave.disc.models$GLM <- multinom.cv(train.no.NA[,"GroupedPercent2"], train.no.NA[,7:18])
### Naive Bayes ###
leave.disc.models$NB <- nb.cv(train.no.NA[,"GroupedPercent2"], train.no.NA[,7:18])

#### Constituency Leave Percent - Continuous ####
leave.percent.models <- list()
# names(leave.percent.models) <- c("NN", "lm", "GBM")
### Neural Net ###
leave.percent.models$NN <- nn.cv("BetterLeavePercent", covars, train.dummy.no.NA, nn.grid)
### Linear Model ###
leave.percent.models$lm <- train(formulaMaker('BetterLeavePercent', covars), data = train.dummy.no.NA,
                                        method = "lm",
                                        trControl = trainControl(
                                          method = "repeatedcv", number = 5, repeats=10, verboseIter = T,
                                          allowParallel = TRUE)
)
### Generalized Boosted Regression Model ###
leave.percent.models$GBM <- gbm.caret.func(DM = train.no.NA[, 7:18], y = train.no.NA[, 3])

stopCluster(cores)

# Model Analysis ---------------------------------------------------------------
# Load workspace to save time - load("data/saved_models.RData")
# All classification - ROC curves, confusion matrices, varImp
# Cool plots to do with final model:
#   Choropleth based on predictions of best model, suggests a changing environment
#   Most important votes, and for what parties
#     Keep in mind 'importance' only means 'more different that other parties' 
#     This could be a good predictor for how Brexit could 'resolve'
#       Whichever votes had the least 'importance' could be more likely to be compromises/have defectors
all.class.models <- list("Party" = party.models,"PartyGroup" = party.grp.models,
                         "ConstituencyVote" = ref.vote.models, "GroupedPercent" = leave.quant.models,
                         "GroupedPercent2" = leave.disc.models)
all.continuous.models <- list("NN"= leave.percent.models$NN, "lm" = leave.percent.models$lm,
                              "GBM"=leave.percent.models$GBM)
accuracy <- matrix(ncol=10, nrow=5)
colnames(accuracy) <- c("NN", "SVM", "Tree", "RF", "XGB", "GBM", "GLM", "NB", "ada", "adaboost")
rownames(accuracy) <- c("Party", "PartyGroup", "VoteResult", "LeaveQuantile", "LeaveDiscrete")

kappa <- matrix(ncol=10, nrow=5)
colnames(kappa) <- c("NN", "SVM", "Tree", "RF", "XGB", "GBM", "GLM", "NB", "ada", "adaboost")
rownames(kappa) <- c("Party", "PartyGroup", "VoteResult", "LeaveQuantile", "LeaveDiscrete")

best.kappa <- matrix(ncol=10, nrow=5)
colnames(best.kappa) <- c("NN", "SVM", "Tree", "RF", "XGB", "GBM", "GLM", "NB", "ada", "adaboost")
rownames(best.kappa) <- c("Party", "PartyGroup", "VoteResult", "LeaveQuantile", "LeaveDiscrete")

auc <- matrix(ncol=10, nrow=5)
colnames(auc) <- c("NN", "SVM", "Tree", "RF", "XGB", "GBM", "GLM", "NB", "ada", "adaboost")
rownames(auc) <- c("Party", "PartyGroup", "VoteResult", "LeaveQuantile", "LeaveDiscrete")

for (i in 1:length(all.class.models)) {
  for (j in 1:length(all.class.models[[i]])) {
    response.type <- names(all.class.models)[i]
    model.type <- names(all.class.models[[i]])[j]
    model <- all.class.models[[i]][[j]]
    
    if (model.type == "NN" | model.type == "SVM") {
      if (response.type != "Party" & response.type != "PartyGroup") {
        test.data <- holdout.dummy.no.NA[,7:42]
        response <- holdout.dummy.no.NA[,response.type]
      } else {
        test.data <- holdout.dummy[,7:42]
        response <- holdout.dummy[,response.type]
      }
    } else {
      if (response.type != "Party" & response.type != "PartyGroup") {
        test.data <- holdout[,7:18]
        response <- holdout[,response.type]
      } else {
        test.data <- holdout.no.NA[,7:18]
        response <- holdout.no.NA[,response.type]
      }
    }
    
    accuracy[i, model.type] <- max(na.omit(model$results[,"Accuracy"]))
    kappa[i, model.type] <-
      na.omit(model$results[,"Kappa"])[which.max(na.omit(model$results[,"Accuracy"]))]
    
    best.kappa[i, model.type] <- max(na.omit(model$results[,"Kappa"]))
    
    
    if (model.type != "SVM") {
      preds.probs <- predict(model, test.data, type="prob")
      auc[i, model.type] <- multiclass.roc(response, preds.probs)$auc[1]
    }
    
    analyse.model(model, test.data, response, response.type, model.type)
    
  }
}

leave.percent.stats <- matrix(nrow=5, ncol=3)
colnames(leave.percent.stats) <- c("NN", "lm", "GBM")
rownames(leave.percent.stats) <- c("RMSE", "Rsquared","Best.Rsquared", "MAE", "Best.MAE")

for (i in 1:length(all.continuous.models)) {
  model.type <- names(all.continuous.models)[i]
  model <- all.continuous.models[[i]]
  if (model.type != "GBM") {
    test.data <- holdout.dummy.no.NA[,7:42]
    response <- holdout.dummy.no.NA[,"BetterLeavePercent"]
  } else {
    test.data <- holdout.no.NA[,7:18]
    response <- holdout.no.NA[,"BetterLeavePercent"]
  }
  leave.percent.stats["RMSE",model.type] <- min(na.omit(model$results[,"RMSE"]))
  leave.percent.stats["Rsquared", model.type] <-
    na.omit(model$results[,"Rsquared"])[which.min(na.omit(model$results[,"RMSE"]))]
  leave.percent.stats["MAE", model.type] <-
    na.omit(model$results[,"MAE"])[which.min(na.omit(model$results[,"RMSE"]))]
  
  leave.percent.stats["Best.Rsquared",model.type] <- min(na.omit(model$results[,"Rsquared"]))
  leave.percent.stats["Best.MAE",model.type] <- min(na.omit(model$results[,"MAE"]))
  
  if (model.type != "lm") plot(model)
  preds <- predict(model, test.data)
  if (model.type == "lm") {
    par(mfrow=c(2,2))
    plot(model$finalModel)
    par(mfrow=c(1,1))
  } else {
    print(ggplot(mapping=aes(preds, response-preds)) + geom_point() + geom_smooth(method="loess") +
      labs(title=paste("Residual plot (", model.type, ", Leave Percent)", sep=""), x="Fitted values",
           y="Residuals"))
  }
}

## Renaming responses as row name to be more descriptive
# Party -> Party
# PartyGroup -> PartyGroup
# VoteResult -> ConstituencyVote
# LeaveQuantile -> GroupedPercent
# LeaveDiscrete -> GroupedPercent2

accuracy
kappa
auc
best.kappa # This is the best kappas if we had used kappa to choose rather than accuracy
           # (Btw, it's exactly the same, except for a TINY change in XGB for Party
leave.percent.stats

plot.metrics <- function(stat.matrices, stats) {
  all.stats <- data.frame("response"=NA, "stat"=NA, "Model"=NA, "value"=NA)
  
  for (i in 1:length(stat.matrices)) {
    matrix <- stat.matrices[[i]]
    stat <- stats[i]
    stat.df <- data.frame(matrix)
    responses <- rownames(matrix)
    stat.df$response <- rownames(matrix)
    stat.df$stat <- stat
    stat.df <- stat.df %>% gather("Model", "value", -response, -stat)
    all.stats <- rbind(all.stats, stat.df)
  }

  all.stats$type <- apply(all.stats, 1, function(x) return(paste(x[2], ", ", x[1], sep="")))
  all.stats <- all.stats[2:nrow(all.stats),]
  ggplot(na.omit(all.stats), aes(x=Model, y=value, fill=Model)) +
    geom_col() + facet_wrap(~type, ncol=5) + theme(axis.ticks.x=element_blank(),
                                                   axis.text.x=element_blank(),
                                                   strip.text=element_text(size=10))
}

plot.metrics(list(accuracy, kappa, auc), c("Accuracy","Kappa", "AUC"))


# Prediction Choropleths  -------------------------------------------------------------
#### Importing data ####
mpdata <- rbind(train, holdout)
mpdata$party.predict <- predict(all.class.models$Party$RF,mpdata[,7:18])
mpdata$vote.predict <- predict(all.class.models$ConstituencyVote$GBM, mpdata[,7:18])

#### Choropleths ####
ukMap <- readOGR('Shapefiles/Westminster_Parliamentary_Constituencies_December_2017_Generalised_Clipped_Boundaries_in_the_UK.shp')
ukMapf <- fortify(ukMap, region = "pcon17nm") %>% rename(Geography = id) %>% arrange(Geography)
mpdata$Geography <- mpdata$Constituency

# Fixing geographies
ukMapf$Geography <- gsub('[[:punct:] ]+',' ',ukMapf$Geography)
mpdata$Geography <- replace(mpdata$Geography, mpdata$Name=="Albert Owen", "Ynys Mon")
geos.map <- sort(unique(ukMapf$Geography))
geos.map <- geos.map[!geos.map == "Newport West"]
geos.map <- geos.map[!geos.map == "Buckingham"]
mpdata <- mpdata[order(mpdata$Geography),]
mpdata$Geography <- geos.map

vote.predict.map <- UKChoroplethFunction(ukMapf, mpdata, 'vote.predict', "Predicted votes", "Predicted Votes")
vote.predict.map <- vote.predict.map + scale_fill_manual(values=c("Green", "Red"))
vote.predict.map

party.predict.map <- UKChoroplethFunction(ukMapf, mpdata, 'party.predict', "Predicted Party", "Predicted Party")
party.predict.map <- party.predict.map + scale_fill_manual(values=party.color.scheme)
party.predict.map




#### OUR OWN VOTES ####
our.data <- data.frame(read_csv("data/ourvotes.csv"))
for (i in 2:13) {
  our.data[,i] <- factor(our.data[,i], levels=c("For", "Abstain", "Against"))
}
#our.data <- as.data.frame(unclass(our.data))
our.data$vote <- predict(all.class.models$ConstituencyVote$GBM, our.data[,2:13])
our.data$party <- predict(all.class.models$Party$RF, our.data[,2:13])
our.data$vote.continuous <- predict(all.continuous.models$GBM, our.data[,2:13])
our.data


write.csv(leave.percent.stats, file = "Leavepercent.csv")
