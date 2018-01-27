install.packages("pROC")
library(pROC)

#change to name of file to be loaded
myfile <- read.csv("ATR.csv")

#create original curve plot.roc(response, predictor) 
#
#response = a  factor,  numeric  or  character  vector  of  responses,  typically  encoded  with  0
#(controls) and 1 (cases)
#predictor =  a numeric vector, containing the value of each observation

myroc <- plot.roc(myfile$QuestionMLActual,myfile$X1,percent=TRUE)

#this displays statistical values for roc curve on the console
roc_info <-coords(myroc, "b", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                              "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                              "1-sensitivity", "1-accuracy", "1-npv", "1-ppv",
                              "precision", "recall"), best.method="youden") # default

#this adds threshold (cutoff value), precision (right), and accuracy (left) to the curve
curve_w_info <-plot(myroc, print.thres="best", print.thres.best.method="youden")

#Area Under the Curve value
auc(myroc)

#Extra stuff (unnecessary), can smooth the plot but it doesnt keep threshold value

extra <- plot(smooth(myroc))
smooth_curve_w_info <-plot(smooth(myroc), print.thres="best", print.thres.best.method="youden")
smooth_roc_info <-coords(smooth(myroc), "b", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                                    "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                                    "1-sensitivity", "1-accuracy", "1-npv", "1-ppv",
                                                    "precision", "recall"), best.method="youden") # default
