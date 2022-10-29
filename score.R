getScore <- function(results){
  TP <- results[1]
  TN <- results[4]
  FP <- results[2]
  FN <- results[3]
  
  print("CONFUSION MATRIX: ")
  print(results)
  accuracy <- (TP + TN) / ( TP+FP+TN+FN)
  print(paste("ACCURACY: ", accuracy))
  precision <- TP / ( TP+FP)
  print(paste("PRECISION: ", precision))
  recall <-  TP / (TP+FN)
  print(paste("RECALL:", recall))
  F1 <- (2*precision*recall)/(precision+recall)
  print(paste("F1:", F1))
  result_vector <- c(accuracy, precision, recall, F1)
  return(result_vector)
}
