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


multiple.roc.plot <- function(roc, from.multiclass.roc = FALSE) {
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
    labs(colour='Threshold') +
    facet_wrap(~type) #+
  return(p)
}


get.confusion.table <- function(preds, observations, levels) {
  # Makes a table if either the predictions or observations are missing levels
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

# Use pROC::roc() to get the roc data
# Then pass it as a parameter to the roc.plot functions