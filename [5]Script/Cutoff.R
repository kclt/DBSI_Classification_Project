threshold <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(predict(df_clean.bestlogit2,type = "response",newdata = df_clean2[training==2,]) >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, df_clean2$Churn[training==2], positive = "Yes")
  accuracy <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  out <- t(as.matrix(c(sensitivity, specificity, accuracy))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}