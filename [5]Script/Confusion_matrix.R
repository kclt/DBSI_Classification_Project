getinfo <- function(m, dat){
  tryCatch({
    prob <- predict(m, dat, type="response")
    thres <- 0.28929
    pred <- ifelse(prob>=thres,1,0)
    cmatrix <- table(dat$Churn[training==2],pred[training==2],dnn = c("observed","predicted")) # first variable indexes rows
    return(list(confusion_matrix = cmatrix, prediction = pred, accuracy = sum(diag(cmatrix)) / sum(cmatrix), sensitivity = cmatrix[4]/(cmatrix[2]+cmatrix[4]), precision = cmatrix[4]/(cmatrix[3]+cmatrix[4])))
  },
  error = function(decisiontree) {
      prob <- predict(m, dat, type="prob")
      thres <- 0.28929
      pred <- ifelse(prob>=thres,1,0)
      cmatrix <- table(dat$Churn[training==2],pred[training==2,2],dnn = c("observed","predicted")) # first variable indexes rows
      return(list(confusion_matrix = cmatrix, prediction = pred, accuracy = sum(diag(cmatrix)) / sum(cmatrix), sensitivity = cmatrix[4]/(cmatrix[2]+cmatrix[4]), precision = cmatrix[4]/(cmatrix[3]+cmatrix[4])))
  },
  warning = function(randomforest){
    tryCatch({
      prob <- predict(m, dat, type="prob")
      thres <- 0.28929 
      pred <- ifelse(prob>=thres,1,0)
      cmatrix <- table(dat$Churn[training==2],pred[training==2,2],dnn = c("observed","predicted")) # first variable indexes rows
      return(list(confusion_matrix = cmatrix, prediction = pred, accuracy = sum(diag(cmatrix)) / sum(cmatrix), sensitivity = cmatrix[4]/(cmatrix[2]+cmatrix[4]), precision = cmatrix[4]/(cmatrix[3]+cmatrix[4])))
    },
    error = function(SVM){
      pred <- predict(m, dat, decision.values = TRUE)
      cmatrix <- table(dat$Churn[training==2], pred[training==2], dnn= c("observed","predicted"))
      return(list(confusion_matrix = cmatrix, prediction = pred, accuracy = sum(diag(cmatrix)) / sum(cmatrix), sensitivity = cmatrix[4]/(cmatrix[2]+cmatrix[4]), precision = cmatrix[4]/(cmatrix[3]+cmatrix[4])))
    }
    )
    
  }
  )
} 