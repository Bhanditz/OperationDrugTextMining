Questdata = read.csv("QuestionWithUrl.csv") 
Questdata <- subset(Questdata, select = -c(X))
names(Questdata)[names(Questdata) == 'V1'] <- 'QuestionNumber'
names(Questdata)[names(Questdata) == 'V2'] <- 'Question'
names(Questdata)[names(Questdata) == 'V3'] <- 'Url'
names(Questdata)[names(Questdata) == 'V4'] <- 'Type'
View(Questdata)
write.csv(Questdata,file = "QuestionTypes.csv")
NewQuestData = read.csv("QuestionTypes.csv")
NewQuestData <- subset(NewQuestData, select = -c(X))
QuestionMLTable1 <- cbind(Questdata,NewColumn = 1)
names(QuestionMLTable1)[names(QuestionMLTable1) == 'NewColumn'] <- 'Q/N'
names(QuestionMLTable1)[names(QuestionMLTable1) == 'V2'] <- 'Question'
QuestionMLTable1 <- cbind(QuestionMLTable1, Qlength = 1)
QuestionMLTable1 <- cbind(QuestionMLTable1, AveWordLen = 1)
QuestionMLTable1 <- cbind(QuestionMLTable1, my = 0)
QuestionMLTable1 <- cbind(QuestionMLTable1, I = 0)
QuestionMLTable1 <- cbind(QuestionMLTable1, take_verb = 0)
QuestionMLTable1 <- cbind(QuestionMLTable1, pill_words = 0)
QuestionMLTable1 <- cbind(QuestionMLTable1, feel_verb = 0)

length(QuestionMLTable)

sink("WarfarinTrainingData.txt")
print(QuestionMLTable)
sink()


for (i in rownames(QuestionMLTable1))
{
  #print(QuestionMLTable[i,"Url"])
 if(grepl("no-question",QuestionMLTable1[i,"Question"],fixed = TRUE))
   QuestionMLTable1[i,"Q/N"] <- 0
}



QuestionMLTable1$V3 <- NULL
QuestionMLTable$Type <- NULL
QuestionMLTable$Url <- NULL
names(QuestionMLTable)[names(QuestionMLTable) == 'NewColumn'] <- 'Q/N'

for (i in rownames(QuestionMLTable1))
{
  QuestionMLTable1[i,"my"] <- 0
  QuestionMLTable1[i,"I"] <- 0
  QuestionMLTable1[i,"take_verb"] <- 0
  QuestionMLTable1[i,"pill_words"] <- 0
  QuestionMLTable1[i,"feel_verb"] <- 0
  
}

for (i in rownames(QuestionMLTable1))
{
  x <- QuestionMLTable1[i,"Question"]
  x <- gsub("[^0-9A-Za-z///']"," ",x,ignore.case = TRUE)
  x <- gsub("yahoo answers|no question", "",x)
  QuestionMLTable1[i,"Qlength"] <- str_count(x,'\\w+')
  words <- strsplit(x, " ")
  j = 1
  word_length = NULL
  while (!(is.na(words[[1]][j])))
  {
    word_length <- rbind(word_length,nchar(words[[1]][j]))
    j = j + 1
  }
  word_length <- word_length[-which(word_length == 0)]
  QuestionMLTable1[i,"AveWordLen"] <- round(mean(word_length),2)
}

for (i in rownames(QuestionMLTable1))
{
  
  QuestCorp <- Corpus(VectorSource(QuestionMLTable1[i,"Question"]))
  QuestCorp <- tm_map(QuestCorp, stripWhitespace)
  QuestCorp <- tm_map(QuestCorp, removeNumbers)
  QuestCorp <- tm_map(QuestCorp, removePunctuation)
  ndtm <- DocumentTermMatrix(QuestCorp,control=list(wordLengths=c(1,Inf)))
  nfreq <- colSums(as.matrix(ndtm))
  neword <- order(nfreq,decreasing = TRUE)
  CorpWordList <- c("my","i","taking","take","taken","took","dose","pill","mg"
                    ,"feeling","feel","felt")
  
  for (j in 1:length(CorpWordList)) 
     if (is.na(nfreq[CorpWordList[j]]))
         nfreq[CorpWordList[j]] <- 0
  
  QuestionMLTable1[i,"my"] <- nfreq["my"] 
  QuestionMLTable1[i,"I"] <- nfreq["i"]
  QuestionMLTable1[i,"take_verb"] <- nfreq["taking"] + nfreq["take"] + nfreq["taken"] + nfreq["took"]
  QuestionMLTable1[i,"pill_words"] <- nfreq["dose"] + nfreq["pill"] + nfreq["mg"]
  QuestionMLTable1[i,"feel_verb"] <- nfreq["feeling"] + nfreq["feel"] + nfreq["felt"]
  
}
TrainingData <- as.data.frame(QuestionMLTable)
TrainingData$Question <- NULL
TrainingData$Classification <- as.factor(TrainingData$Classification)

sink("TrainingData.csv")
print(TrainingData)
sink()

QFit <- train(Classification ~ .,data = TrainingData, method = 'bayesglm')
TestData <- as.data.frame(QuestionMLTable1)
TestData$Question <- NULL
predict(QFit, newdata = TrainingData)
TestDataCat <- predict(QFit, newdata = TestData)

TestData <- cbind(TestData,Classification = TestDataCat)
for (i in rownames:TestData)
  TestData[i,] <- cbind(TestData[i,],Classification = TestDataCat[i])
sink("TestData.csv")
print(TestData)
sink()
PatientData <- sqldf("select QuestionNumber,Question,Type from NewQuestData where Type='P'")

DupPatientData <- sqldf("select Question,Type from NewQuestData where Type='P'")
DupPatientData <- unique(DupPatientData)
PatientData <- unique(PatientData)
write.csv(PatientData,file="PatientData.csv")
NewPatientData <- sqldf("select Question from DupPatientData")
NPD = NULL
QNumbers <- (unlist(PatientData$QuestionNumber))
QNumbers[3]
length(QNumbers)
SuperPatientData = NULL
for (i in 1:length(QNumbers))
  SuperPatientData[i] <- QuestionV[QNumbers[i]]
SuperPatientData <- unique(SuperPatientData)

SuperCorp[[1]]<- Corpus(VectorSource(QuestionMLTable[1,"Question"]))
writeLines(as.character(SuperCorp[[1]]))
Corp <- Corpus(VectorSource(SuperPatientData))
#Corp <- tm_map(Corp, removeWords, stopwords("SMART"))
Corp <- tm_map(Corp, stripWhitespace)
Corp <- tm_map(Corp, removeNumbers)
Corp <- tm_map(Corp, removePunctuation)
Corp <- tm_map(Corp, removeWords, c("yahoo","warfarin","coumadin"))
Corp <- tm_map(Corp, removeWords, c("answers","question","noquestion"))
ndtm <- DocumentTermMatrix(Corp,control=list(wordLengths=c(1,Inf)))
inspect(ndtm)

QuestionMLTable[2,"I"] <- nfreq[neword]["my"]

nfreq <- colSums(as.matrix(ndtm))
neword <- order(nfreq,decreasing = TRUE)
#Most frq
nfreq[head(neword)]
#Least frq
nfreq[tail(neword)]
sink("WordFreqWithStopwords.txt")
print(nfreq[neword])
sink()
nwf=data.frame(term=names(nfreq),occurrences=nfreq)
np <- ggplot(subset(nwf, nfreq>25), aes(term, occurrences))
np <- np + geom_bar(stat="identity")
np <- np + theme(axis.text.x=element_text(angle=45, hjust=1))
np

wordcloud(Corp,max.words = 100, random.order= FALSE,colors = brewer.pal(6,"Dark2"))

SuperPatientData <- unique(SuperPatientData)
length(NPD)
length(DupPatientData$Question)
PatientQuestions = NULL
for (i in 1:length(NPD))
  PatientQuestions[i] <- NPD[i]







QuestCorp <- Corpus(VectorSource(QuestionMLTable[1,"Question"]))
QuestCorp <- tm_map(QuestCorp, stripWhitespace)
QuestCorp <- tm_map(QuestCorp, removeNumbers)
QuestCorp <- tm_map(QuestCorp, removePunctuation)
ndtm <- DocumentTermMatrix(QuestCorp,control=list(wordLengths=c(1,Inf)))
nfreq <- colSums(as.matrix(ndtm))
neword <- order(nfreq,decreasing = TRUE)
QuestionMLTable[2,"my"] <- nfreq[neword]["my"] 
QuestionMLTable[1,"I"] <- nfreq[neword]["i"]
QuestionMLTable[2,"take_verb"] <- nfreq[neword]["taking"] + nfreq[neword]["take"] 
+ nfreq[neword]["take"] + nfreq[neword]["took"]
QuestionMLTable[2,"pill_words"] <- nfreq[neword]["dose"] + nfreq[neword]["pill"] 
+ nfreq[neword]["mg"]
QuestionMLTable[2,"feel_verb"] <- nfreq[neword]["feeling"] + nfreq[neword]["feel"] 
+ nfreq[neword]["felt"]