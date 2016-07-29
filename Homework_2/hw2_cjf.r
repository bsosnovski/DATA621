hw2 <- read.csv('classification-output-data.csv')

#check confusion matrix
table(hw2$class, hw2$scored.class)

#identify whether prediction was true or false
hw2$correct.pred <- hw2$class == hw2$scored.class

#classify TP/TN/FP/FN based on prediction veracity and predicted value
hw2$confusion[hw2$correct.pred == TRUE & hw2$scored.class == 1] <- "TP"
hw2$confusion[hw2$correct.pred == TRUE & hw2$scored.class == 0] <- "TN"
hw2$confusion[hw2$correct.pred == FALSE & hw2$scored.class == 1] <- "FP"
hw2$confusion[hw2$correct.pred == FALSE & hw2$scored.class == 0] <- "FN"

#helper function that identifies the count of TP/TN/FP/FN 
#in given column of dataframe
#returns a list
conf_mat_values <- function(df, conf_col_index) {
  conf_vector <- df[,conf_col_index]
  tp <- length(conf_vector[conf_vector =="TP"])
  tn <- length(conf_vector[conf_vector =="TN"])
  fp <- length(conf_vector[conf_vector =="FP"])
  fn <- length(conf_vector[conf_vector =="FN"])
  
  return(list(TP = tp, TN = tn, FP = fp, FN = fn))
}

#accuracy (TP+TN)/(TP+FP+TN+FN)

accuracy <- function(df, conf_col_index) {
  conf_vals <- conf_mat_values(df, conf_col_index)
  return((conf_vals$TP + conf_vals$TN) / 
           (conf_vals$TP + conf_vals$FP + conf_vals$TN + conf_vals$FN))
}

#classification error rate (FP + FN)/(TP + FP + TN + FN)
class_error <- function(df, conf_col_index) {
  conf_vals <- conf_mat_values(df, conf_col_index)
  return((conf_vals$FP + conf_vals$FN)/
           (conf_vals$TP + conf_vals$FP + conf_vals$TN + conf_vals$FN))
}

#precsion TP/(TP+FP)

precision <- function(df, conf_col_index) {
  conf_vals <- conf_mat_values(df, conf_col_index)
  return(conf_vals$TP / (conf_vals$TP + conf_vals$FP))
}

#sensitivity TP/(TP+FN)

sensitivity <- function(df, conf_col_index) {
  conf_vals <- conf_mat_values(df, conf_col_index)
  return(conf_vals$TP / (conf_vals$TP + conf_vals$FN))
}

#specificity TN/(TN+FP)

specificity <- function(df, conf_col_index) {
  conf_vals <- conf_mat_values(df, conf_col_index)
  return(conf_vals$TN / (conf_vals$TN + conf_vals$FP))
}

#f1 (2 * precsion * sensitivity)/(precision + sensitivity)

f1 <- function(df, conf_col_index) {
  prec <- precision(df, conf_col_index)
  sens <- sensitivity(df, conf_col_index)
  return((2 * prec * sens)/(prec + sens))
}


#run the functions
accuracy(hw2, 13)
class_error(hw2, 13)
accuracy(hw2, 13) + class_error(hw2, 13)