getinfo <- function(m, dat){
  tryCatch({
    prob <- predict(m, dat)
    avg_not_apply <- mean(prob[training==1 & df_clean$Churn=="Yes"]) # average of application probability for those who don't Churn (training sample)
    avg_apply <- mean(prob[training==1 & df_clean$Churn=="No"]) # average of application probability for those who do Churn (training sample)
    thres <- 0.5*(avg_apply+avg_not_apply) 
    pred <- ifelse(prob>=thres,1,0)
    cmatrix <- table(dat$Churn[training==2],pred[training==2],dnn = c("observed","predicted")) # first variable indexes rows
    return(list(confusion_matrix = cmatrix, prediction = pred, accuracy = sum(diag(cmatrix)) / sum(cmatrix), sensitivity = cmatrix[4]/(cmatrix[2]+cmatrix[4]), precision = cmatrix[4]/(cmatrix[3]+cmatrix[4])))
  },
  error = function(e) {
    prob <- predict(m, dat)
    avg_not_apply <- mean(prob[training==1 & df_clean$Churn=="Yes"]) # average of application probability for those who don't Churn (training sample)
    avg_apply <- mean(prob[training==1 & df_clean$Churn=="No"]) # average of application probability for those who do Churn (training sample)
    thres <- 0.5*(avg_apply+avg_not_apply) 
    pred <- ifelse(prob>=thres,1,0)
    cmatrix <- table(dat$Churn[training==2],pred[training==2,2],dnn = c("observed","predicted")) # first variable indexes rows
    return(list(confusion_matrix = cmatrix, prediction = pred, accuracy = sum(diag(cmatrix)) / sum(cmatrix), sensitivity = cmatrix[4]/(cmatrix[2]+cmatrix[4]), precision = cmatrix[4]/(cmatrix[3]+cmatrix[4])))
  })
} 