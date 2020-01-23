# Jack functions
UKChoroplethFunction <- function(map, database, variable, title, legendTitle, scale=TRUE) 
{
  require(ggplot2)
  require(maptools)
  
  choroData <- database %>% ungroup(GeoCode) %>% dplyr::select(Geography, variable) %>% arrange(Geography)
  ukMerge <- merge(map, choroData, by = 'Geography', all.x = T)
  ukMerge <- ukMerge[order(ukMerge$order), ]
  mapPlot <- ggplot(ukMerge)
  mapPlot <- mapPlot + geom_polygon( 
    aes(x = long, y = lat, group = group, fill = ukMerge[[variable]]), 
    color = "black", size = 0.25) 
  mapPlot <- mapPlot + xlim(c(min(map$long), max(map$long))) + ylim(c(min(map$lat), max(map$lat)))
  mapPlot <- mapPlot + labs(title = title, x = "Longitude", y = "Latitude") 
  
  if (scale) {
    mapPlot <- mapPlot + scale_fill_gradient(aes(fill = ""), low = 'red', high = 'green')
  }
  # mapPlot <- mapPlot + coord_map()
  
  return(mapPlot)
}

roc.plot <- function(roc) {
  require(ggplot2)
  df<-data.frame(t(coords(roc, "all")))
  best<-coords(roc, "best")
  p <- ggplot(df)
  p <- p + geom_path(aes(1-specificity, sensitivity, colour=threshold), size=3) +
    theme_bw()
  p <- p +
    geom_abline(intercept=0, slope=1) +
    geom_hline(yintercept=as.numeric(best[3]),
               colour='darkgrey', linetype='longdash') +
    geom_vline(xintercept = as.numeric(1-best[2]),
               colour='darkgrey', linetype='longdash') +
    geom_label(aes(x=1, y=0, label=paste("Threshold:", round(best[1], 4)), hjust=1, vjust=0)) +
    geom_label(aes(x=1, y=0, label=paste("AUC:", round(roc$auc, 4)), hjust=1, vjust=-1.2)) +
    scale_colour_gradient(high='red', low='blue') +
    geom_path(aes(1-specificity, sensitivity),
              colour='blue', alpha=1/3) +
    xlab('1-Specificity (False Positive Rate)') +
    ylab('Sensitivity (True Positive Rate)') +
    labs(colour='Threshold')
  return(p)
}


one.vs.all.multi.roc <- function(preds, observed) {
  rocs <- list()
  for (i in 1:length(colnames(preds))) {
    predictions <- preds[,i]
    response <- ifelse(observed==colnames(preds)[i], 1, 0)
    if (1 %in% response && 0 %in% response) {
      roc <- roc(response, predictions)
      rocs[[length(rocs)+1]] <- roc
      names(rocs)[length(rocs)] <- colnames(preds)[i]
    }
  }
  return(rocs)
}


multiple.roc.plot <- function(roc, from.multiclass.roc = FALSE, title=NULL) {
  # https://link.springer.com/content/pdf/10.1023/A:1010920819831.pdf
  # Roc is the rocs part of a list from multiclass.roc
  
  if (from.multiclass.roc) {
    roc <- lapply(roc$rocs, function(x) x[[1]] )
  }
  
  require(ggplot2)
  titles <- names(roc)
  df <- data.frame()
  best <- data.frame()
  for (i in 1:length(roc)){
    df.ind<-data.frame(t(coords(roc[[i]], "all")))
    df.ind$type <- titles[i]
    best.ind <- data.frame(t(coords(roc[[i]], 'best')))
    best.ind$type <- titles[i]
    best.ind$auc <- roc[[i]]$auc
    if (i == 1) {
      df <- df.ind
      best <- best.ind
    } else {
      df <- rbind(df, df.ind)
      best <- rbind(best, best.ind)
    }
  }
  best <- na.omit(best)
  df <- na.omit(df)
  p <- ggplot(df)
  p <- p + geom_path(aes(1-specificity, sensitivity, colour=threshold), size=2) +
    theme_bw()
  p <- p +
    geom_abline(intercept=0, slope=1) +
    geom_hline(data=best, aes(yintercept=sensitivity),
               colour='darkgrey', linetype='longdash') +
    geom_vline(data=best, aes(xintercept = 1-specificity),
               colour='darkgrey', linetype='longdash') +
    geom_label(data=best, aes(x=1, y=0, label=paste("Threshold:", round(threshold, 4)), hjust=1, vjust=0)) +
    geom_label(data=best, aes(x=1, y=0, label=paste("AUC:", round(auc, 4)), hjust=1, vjust=-1.2)) +
    scale_colour_gradient(high='red', low='blue') +
    geom_line(aes(1-specificity, sensitivity),
              colour='blue', alpha=0.1) +
    xlab('1-Specificity (False Positive Rate)') +
    ylab('Sensitivity (True Positive Rate)') +
    labs(title=title, colour='Threshold') +
    facet_wrap(~type) #+
  return(p)
}


get.confusion.table <- function(preds, observations) {
  # Makes a table if either the predictions or observations are missing levels
  preds <- as.factor(preds)
  observations <- as.factor(observations)
  levels <- levels(observations)
  
  conf.table <- table(preds, observations)
  for (i in 1:length(levels)) {
    if (colnames(conf.table)[i] != levels[i]) {
      conf.table <- cbind(conf.table[,1:(i-1)], rep(0, nrow(conf.table)), conf.table[,i:ncol(conf.table)])
    }
    if (rownames(conf.table)[i] != levels[i]) {
      conf.table <- rbind(conf.table[1:(i-1),], rep(0, ncol(conf.table)), conf.table[i:nrow(conf.table),])
    }
  }
  rownames(conf.table) <- levels
  colnames(conf.table) <- levels
  return(as.table(conf.table))
}

multinom.cv <- function(response, explanatory) {
  set.seed(5059)
  return(train(explanatory, response,
                method = "multinom",
                metric = "AUC",
                trControl = trainControl(
                  method = "repeatedcv", classProbs = TRUE, 
                  summaryFunction = multiClassSummary,
                  number = 5, repeats=1, verboseIter = T,
                  allowParallel = TRUE)))
}

nb.cv <- function(response, explanatory) {
  set.seed(5059)
  return(train(explanatory, response,
               method = "nb",
               trControl = trainControl(
                 method = "repeatedcv", number = 5, repeats=1, verboseIter = T,
                 allowParallel = TRUE)))
}


binary.tree.cv <- function(response, explanatory) {
  set.seed(5059)
  return(train(explanatory, response,
               method="rpart", 
               trControl = trainControl(
                 method="repeatedcv", number=5, repeats=1, verboseIter=T,
                 allowParallel=TRUE)))
}


nn.varImp <- function(object)
{
  # I THINK THIS ONLY WORKS FOR BINARY DATA
  beta <- coef(object)
  abeta <- abs(beta)
  nms <- names(beta)
  i2h <- array(NA, dim = object$n[2:1])
  h2o <- array(NA, dim = object$n[2:3])
  for (hidden in 1:object$n[2]) {
    for (input in 1:object$n[1]) {
      label <- paste("i", input, "->h", hidden, "$", sep = "")
      i2h[hidden, input] <- abeta[grep(label, nms, fixed = FALSE)]
    }
  }
  for (hidden in 1:object$n[2]) {
    for (output in 1:object$n[3]) {
      label <- paste("h", hidden, "->o", ifelse(object$n[3] == 
                                                  1, "", output), sep = "")
      h2o[hidden, output] <- abeta[match(label, nms)]
    }
  }
  if (FALSE) {
    i2h <- matrix(c(-1.67624, 3.29022, 1.32466, -0.51874, 
                    -0.22921, -0.25526, -4.01764, 2.12486, -0.08168, 
                    -1.75691, -1.44702, 0.58286), ncol = 3, byrow = TRUE)
    h2o <- matrix(c(4.57857, -0.48815, -5.73901, -2.65221), 
                  ncol = 1)
  }
  imp <- matrix(NA, nrow = object$n[1], ncol = object$n[3])
  for (output in 1:object$n[3]) {
    Pij <- i2h * NA
    for (hidden in 1:object$n[2]) Pij[hidden, ] <- i2h[hidden, 
                                                       ] * h2o[hidden, output]
    Qij <- Pij * NA
    for (hidden in 1:object$n[2]) Qij[hidden, ] <- Pij[hidden, 
                                                       ]/sum(Pij[hidden, ])
    Sj <- apply(Qij, 2, sum)
    imp[, output] <- Sj/sum(Sj) * 100
    rm(Pij, Qij, Sj)
  }
  colnames(imp) <- if (!is.null(colnames(object$residuals))) 
    colnames(object$residuals)
  else paste("Y", 1:object$n[3], sep = "")
  rownames(imp) <- if (!is.null(object$coefnames)) 
    object$coefnames
  else paste("X", 1:object$n[1], sep = "")
  return(imp)
}

nn.plot.var.imp <- function(model, title="Relative Variable Importance") {
  # I DON'T THINK THIS IS ACCURATE
  var.imp <- nn.varImp(model)[,1]
  var.imp <- data.frame(var.imp)
  var.imp$VoteID <- vapply(rownames(var.imp), function(x) return(strsplit(x, '[.]')[[1]][1]), c(""))
  var.imp$VoteID <- factor(as.character(var.imp$VoteID), levels=
                             c("Vote1", "Vote2", "Vote3", "Vote4", "Vote5", "Vote6", "Vote7", "Vote8", "SecondVote1", "SecondVote2", "SecondVote3", "SecondVote4"))
  levels(var.imp$VoteID) <- c("Vote1.1", "Vote1.2", "Vote1.3", "Vote1.4", "Vote1.5", "Vote1.6", "Vote1.7", "Vote1.8", "Vote2.1", "Vote2.2", "Vote2.3", "Vote2.4")
  var.imp$Vote <- vapply(rownames(var.imp), function(x) return(strsplit(x, '[.]')[[1]][2]), c(""))
  ggplot(var.imp, aes(x=VoteID, y=var.imp, fill=Vote)) + geom_col(position="dodge") + labs(title=title)
}

plot.var.imp.svm <- function(model, title="Relative Variable Importance") {
  var.imp <- varImp(model)$importance
  var.imp <- data.frame(var.imp)
  var.imp$VoteID <- vapply(rownames(var.imp), function(x) return(strsplit(x, '[.]')[[1]][1]), c(""))
  var.imp$VoteID <- factor(as.character(var.imp$VoteID), levels=
                             c("Vote1", "Vote2", "Vote3", "Vote4", "Vote5", "Vote6", "Vote7", "Vote8", "SecondVote1", "SecondVote2", "SecondVote3", "SecondVote4"))
  levels(var.imp$VoteID) <- c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "1.7", "1.8", "2.1", "2.2", "2.3", "2.4")
  var.imp$Vote <- vapply(rownames(var.imp), function(x) return(strsplit(x, '[.]')[[1]][2]), c(""))
  var.imp <- var.imp %>% gather("Class", "Importance", -Vote, -VoteID)
  ggplot(var.imp, aes(x=VoteID, y=Importance, fill=Vote)) + geom_col(position="dodge") + facet_wrap(~Class) + labs(title=title)
}

plot.var.imp.xgb <- function(model, title="Relative Variable Importance") {
  var.imp <- varImp(model)$importance
  var.imp <- data.frame(var.imp)
  var.imp$VoteID <- vapply(rownames(var.imp), function(x) {
    if (grepl("SecondVote", x)) {
      return(substr(x, 1, 11))
    } else {
      return(substr(x, 1, 5))
    }
  }, c(""))  
  var.imp$VoteID <- factor(as.character(var.imp$VoteID), levels=
                             c("Vote1", "Vote2", "Vote3", "Vote4", "Vote5", "Vote6", "Vote7", "Vote8", "SecondVote1", "SecondVote2", "SecondVote3", "SecondVote4"))
  levels(var.imp$VoteID) <- c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "1.7", "1.8", "2.1", "2.2", "2.3", "2.4")
  var.imp$Vote <- vapply(rownames(var.imp), function(x) {
    if (grepl("SecondVote", x)) {
      return(substr(x, 12, 10000))
    } else {
      return(substr(x, 6, 10000))
    }
  }, c("")) 
  var.imp <- var.imp %>% gather("Class", "Importance", -Vote, -VoteID)
  ggplot(var.imp, aes(x=VoteID, y=Importance, fill=Vote)) + geom_col(position="dodge") + labs(title=title)
}

plot.var.imp <- function(model, overall=TRUE, title="Relative Variable Importance") {
  var.imp <- varImp(model)$importance
  var.imp$VoteID <- rownames(var.imp)
  var.imp$VoteID <- factor(as.character(var.imp$VoteID), levels=
                             c("Vote1", "Vote2", "Vote3", "Vote4", "Vote5", "Vote6", "Vote7", "Vote8", "SecondVote1", "SecondVote2", "SecondVote3", "SecondVote4"))
  levels(var.imp$VoteID) <- c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "1.7", "1.8", "2.1", "2.2", "2.3", "2.4")
  var.imp <- var.imp %>% gather("Class", "Importance", -VoteID)
  if (overall) {
    ggplot(var.imp, aes(x=VoteID, y=Importance)) +
      geom_col(fill="darkslateblue") + labs(title=title)
  } else {
    if ("Lab" %in% var.imp$Class) {
      party.color.scheme <- c("deepskyblue4", "lightgoldenrodyellow", "Green", "Black", "Red", "Orange", "Light Green", "Dark Green", "Yellow", "Dark Red")
      ggplot(var.imp, aes(x=VoteID, y=Importance, fill=Class)) + geom_col(position="dodge") +
        scale_fill_manual(values=party.color.scheme) + facet_wrap(~Class) + labs(title=title)
    } else {
      ggplot(var.imp, aes(x=VoteID, y=Importance, fill=Class)) + geom_col(position="dodge") +
        facet_wrap(~Class) + labs(title=title)
    }

  }
}

analyse.model <- function(model, test.data, response, response.type, type="none") {
  preds.classes <- factor(predict(model, test.data, type="raw"), levels=levels(response))
  if (type != "SVM") { preds.probs <- predict(model, test.data, type="prob") }
  
  print(plot(model, main=paste("Parameter comparison (", type, ", ", response.type, ")", sep="")))
  if (type == "RF") { plot(model$finalModel, main=paste("Random Forest Error comparison (", response.type,")", sep=""))} 
  if (type == "Tree") { fancyRpartPlot(model$finalModel, main=paste("Binary Tree (", response.type, ")", sep="")) }
  print(confusionMatrix(preds.classes, response))
  if (type != "SVM") { print(multiclass.roc(response, preds.probs)) }
  if (type != "SVM") { print(multiple.roc.plot(one.vs.all.multi.roc(preds.probs, response), 
                                               title=paste("Multiclass ROC, One vs All (",
                                                           type, ", ", response.type, ")", sep=""))) }
  if (type == "SVM") { print(plot.var.imp.svm(model, paste("Variable importance (", type, ", ", response.type, ")", sep=""))) }
  if (type == "XGB" | type == "GLM") { print(plot.var.imp.xgb(model, paste("Variable importance (", type, ", ", response.type, ")", sep=""))) }
  if (type != "NN" && type != "SVM" && type != "XGB" && type != "GLM") {
    print(plot.var.imp(model, overall=ifelse(type=="NB",FALSE, TRUE), paste("Variable importance (", type, ", ", response.type, ")", sep="")))
  }
}

### Amir Functions ###
formulaMaker <- function(target, covars){
  form <- as.formula(paste(c(target,covars),collapse = '~'))
  return (form)
}

nn.cv <- function(response, covars, data, grid, linout=1) {
  form <- formulaMaker(response, covars)
  set.seed(5059)
  return(train(form, data = data,
               method = "nnet",
               maxit = 1000, trace = F, linout = linout,tuneGrid = grid,
               trControl = trainControl(
                 method = "repeatedcv", number = 5, repeats=1, verboseIter = T,
                 allowParallel = TRUE)))
}

svm.cv <- function(response, covars, data, grid) {
  form <- formulaMaker(response, covars)
  set.seed(5059)
  return(train(form, data = data,
        method = "svmLinear",
        tuneGrid = grid,
        trControl = trainControl(
          method = "repeatedcv", number = 5, repeats=1, verboseIter = T,
          allowParallel = TRUE)
  ))
}

rssR2 = function(model,test) {
  
  y <- test$BetterLeavePercent
  n <- length(y)
  # initate df
  results <- data.frame(rss = NA, R2 = NA)
  # rss
  results$rss <- sum(sapply(y-predict(model,test), function(r) { r^2 }))
  # R2
  results$R2 <- 1 - (results$rss/ (t(y)%*%y-(mean(y)**2*n)))
  return (results)
}

### Enoch Functions ###
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
  cvcontrol <- trainControl(method = "repeatedcv", number = 5, repeats = 1,
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


# XGBoost Tree
# Function ===========
# Objective:
# to perform XgBoost with repeated 5-fold crossvalidation under caret package
# Input Arguments:
# i - # of col vector containing the dependent variable
# data - data input
# naaction - na option

xg.cv2 <- function(i, data, naaction){
  # set seed
  set.seed(5059)
  ## traincontrol
  cvcontrol <- trainControl(method = "repeatedcv", number = 5, repeats = 1, 
                            classProbs = TRUE, verboseIter = FALSE, allowParallel = TRUE, 
                            savePredictions =TRUE)
  
  
  
  xgb.grid <- expand.grid(nrounds = 500, # the maximum number of iterations
                          max_depth = c(2,4,6,8), # the depth of the tree
                          eta = 0.1, # the learning rate
                          gamma = 0, #regularization (or prevents overfitting)
                          colsample_bytree = 0.75, # the number of features (variables) supplied to a tree
                          min_child_weight = 1, # the minimum number of instances required in a child node
                          subsample = 1) # the number of samples (observations) supplied to a tree
  
  f <- as.formula(paste(names(data)[i], "~", 
                        "Vote1 + Vote2 + Vote3 + Vote4 + Vote5 + Vote6 + Vote7 + Vote8 + 
                        SecondVote1 + SecondVote2 + SecondVote3 + SecondVote4")) 
  
  model_kfCV  <-train(f, data = data, method="xgbTree",
                      trControl=cvcontrol,
                      tuneGrid=xgb.grid,
                      verbose=T,
                      nthread =3,
                      na.action = naaction)
  
  # train model
  # model_kfCV <- train(DM,y,method = "rf", trControl = cvcontrol,
  #                     metric = ifelse(is.factor(y), "Accuracy", "RMSE"), ntree = 1000,
  #                     tuneLength = tune, importance = TRUE, na.action=naaction)
  # 
  return(model_kfCV)
}

### Ossian Functions ###
modelLookup('gbm')

gbm.caret.func <- function(DM, y) {
  
  # Set seed for reproducability
  set.seed(5059)
  
  ## 5-fold Repeated CV with parallel computing uding doParallel
  fitControl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 1,
    allowParallel = TRUE)
  
  ## Make a grid of tuning parameters to be optimized using train()
  gbmGrid.tune <-  expand.grid(interaction.depth = c(1, 3, 5), ## Depth of tree
                               n.trees = c(200, 500, 1000),    ## Number of trees
                               shrinkage = c(0.1,0.05, 0.01),  ## Learning rate (Shrinkage)
                               n.minobsinnode = 20)            ## Min. Terminal Node Size
  
  # DM is the a matrix containing covariate
  # y is a vector of the response variable given as a column index of the data set
  # method is the algorithm
  # Controlled using the controll measures specified above
  # metric used to evaluate - Accuracy for factor variables and RMSE for numeric
  # give the grid of tuning parameters as specified in tuneGrid
  gbm.caret <- train(DM, y, 
                     method = "gbm", 
                     trControl = fitControl, 
                     metric = ifelse(is.factor(y), "Accuracy", "RMSE"), 
                     verbose = FALSE, 
                     tuneGrid = gbmGrid.tune)
  
  return(gbm.caret) 
  
}

# These are the specifics of the caret method for different adaboosts
modelLookup('ada')
modelLookup('adaboost')

ada.caret.func <- function(DM, y, Ada.method) {
  
  # Set seed for reproducability
  set.seed(5059)
  
  ## 5-fold Repeated CV with parallel computing uding doParallel
  fitControl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 1,
    allowParallel = TRUE)
  
  ## depending on the specified method the grid is required to hold different variables
  if(Ada.method == 'ada') {
    AdaGrid<-  expand.grid(iter = c(200,500), 
                           maxdepth = c(1,3), 
                           nu = c(0.05))
  }
  else if(Ada.method == 'adaboost') {
    AdaGrid<-  expand.grid(nIter = c(200, 500, 1000), 
                           method = c(0.05))
  }
  
  
  # DM is the a matrix containing covariate
  # y is a vector of the response variable given as a column index of the data set
  # method is the algorithm
  # Controlled using the controll measures specified above
  # metric used to evaluate - Accuracy for factor variables and RMSE for numeric
  # give the grid of tuning parameters as specified in tuneGrid
  
  ada.caret <- train(DM, y, 
                     method = Ada.method, 
                     trControl = fitControl, 
                     metric = ifelse(is.factor(y), "Accuracy", "RMSE"), 
                     verbose = FALSE, 
                     tuneGrid = AdaGrid)
  
  return(ada.caret)
}
