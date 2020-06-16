conf_matrix_plot <- function(cm_input, class_label1="Class Negative",
                                  class_label2="Class Positive", quadrant_col1='#3F97D0', 
                                  quadrant_col2='#F7AD50', custom_title="Confusion matrix",
                                  text_col="black", round_dig=2){
  library(caret)
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  #n is specified in plot to indicate no plotting
  title(custom_title, cex.main=2)
  
  # Create the matrix visualisation using custom rectangles and text items on the chart 
  rect(150, 430, 240, 370, col=quadrant_col1)
  text(195, 435, class_label1, cex=1.2)
  rect(250, 430, 340, 370, col=quadrant_col2)
  text(295, 435, class_label2, cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=quadrant_col1)
  rect(250, 305, 340, 365, col=quadrant_col2)
  text(140, 400, class_label1, cex=1.2, srt=90)
  text(140, 335, class_label2, cex=1.2, srt=90)
  
  #Add the results of the confusion matrix - as these will be saved to cm$table
  result <- as.numeric(cm$table)
  text(195, 400, result[1], cex=1.6, font=2, col=text_col)
  text(195, 335, result[2], cex=1.6, font=2, col=text_col)
  text(295, 400, result[3], cex=1.6, font=2, col=text_col)
  text(295, 335, result[4], cex=1.6, font=2, col=text_col)
  
  #Add in other confusion matrix statistics
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "Confusion matrix statistics", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.6, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), round_dig), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.6, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), round_dig), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.6, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), round_dig), cex=1.2)
  text(65, 85, names(cm$byClass[6]), cex=1.6, font=2)
  text(65, 70, round(as.numeric(cm$byClass[6]), round_dig), cex=1.2)
  text(86, 85, names(cm$byClass['Balanced Accuracy']), cex=1.6, font=2)
  text(86, 70, round(as.numeric(cm$byClass['Balanced Accuracy']), round_dig), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 4), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), round_dig), cex=1.4)
}  