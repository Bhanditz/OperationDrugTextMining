
PredResults$Actual[1:100] <- 0

test <- plot.roc(PredResults$Actual,PredResults$`1`)

a1 <- coords(test,"b",ret="t",best.method = "youden")
a2 <- coords(test,"b",ret="t",best.method = "closest.topleft")

a1a <- plot(test,print.thres="best", print.thres.best.method = "youden")
a12 <- plot(test,print.thres="best", print.thres.best.method = "closest.topleft")

test2 <- plot(smooth(test))