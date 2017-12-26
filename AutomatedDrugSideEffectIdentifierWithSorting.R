#*************************************************
#AutomatedDrug-SideEffectIdentifierwithML        *
#Authors: Luis Franco, Delroy Fong               *
#Supervisers: Dr. Feng Cheng, Dr. Yicheng Tu     *
#*************************************************

library(tm)
library(gtools)
library(stringi)
library(stringr)
library(SnowballC)
library(stringr)
library(rvest)
library(textreg)
library(ggplot2)
library(plyr)
library(tidytext)
library(dplyr)
library(XML)
library(xml2)
library(sqldf)
library(caret)
library(installr)


#R Crawling for Data
pg_alive = TRUE
user_url <- readline(prompt="Please enter the Yahoo! Answers search url:\n")
selection <- readline(prompt="Select Output,SideEffectsFreq, DrugFreq or both:\n")

firstpglist = NULL
allpglist = NULL
page <- read_html(user_url)
firstpglist<- page %>% html_nodes(".fz-m") %>% html_attr('href')
firstpglist <- firstpglist[-length(firstpglist)]
allpglist<- firstpglist

if (length(page) == 0){pg_alive=FALSE}
curr_pg <- page
while (pg_alive){
  #get url of next pg
  nextpg_url <- curr_pg %>% html_nodes("a.next") %>% html_attr('href')
  if (length(nextpg_url) != 0)
  {
    nextpg <- read_html(nextpg_url)
    # get urls listed on new pg and append to list
    nextpg_list <- nextpg %>% html_nodes(".fz-m") %>% html_attr('href')
    nextpg_list<- nextpg_list[-length(nextpg_list)]
    allpglist <- c(allpglist,nextpg_list)
    curr_pg <- nextpg
  }
  #checks if next button = 0, if true then no more next button left
  if (length(nextpg_url) == 0){pg_alive=FALSE}
} 
#Get only unique URLs and define title and real(question)
allpglist <- unique(allpglist)
titleV = NULL
realV = NULL

#Assure that yahoo will not timeout
if (length(allpglist) >= 1000)
{
  for (i in 1:999)
  {
    QuestPage <- read_html(allpglist[i])
    titleV[i] <- QuestPage %>% html_nodes("title") %>% html_text("title")
    QData <- QuestPage %>% html_node(".ya-q-text") %>% html_text("text")
    if (identical(QData,""))
    {
      realV[i] <- "no-question"
    }else{
      realv[i] <- QData
    }
  }
}else{
  for (i in 1:length(allpglist))
  {
    QuestPage <- read_html(allpglist[i])
    titleV[i] <- QuestPage %>% html_nodes("title") %>% html_text("title")
    QData <- QuestPage %>% html_node(".ya-q-text") %>% html_text("text")
    if (identical(QData,""))
    {
      realV[i] <- "no-question"
    }else{
      realV[i] <- QData
    }
  }
}
#The full question
QuestionV = NULL
QuestionV = paste(titleV,realV,sep = " | ")

#Modifies data to lower case 
for (i in 1:length(QuestionV))
  QuestionV[i] <- sapply(QuestionV[i],tolower)

QuestionV <- gsub("\\n","",QuestionV)
QuestionV <- gsub("\\r","",QuestionV)

QuestionWithUrl = NULL
for (i in 1:length(QuestionV))
{
  QuestionWithUrl=rbind(QuestionWithUrl,c(i,QuestionV[i],allpglist[i]))
}
write.csv(QuestionWithUrl,file="QuestionWithUrl.csv")

#Define test dataframe with machine learning criteria
Questdata = read.csv("QuestionWithUrl.csv") 
Questdata <- subset(Questdata, select = -c(X))
names(Questdata)[names(Questdata) == 'V1'] <- 'QuestionNumber'
names(Questdata)[names(Questdata) == 'V2'] <- 'Question'
names(Questdata)[names(Questdata) == 'V3'] <- 'Url'


QuestionMLTest <- cbind(Questdata,NewColumn = 1)
names(QuestionMLTest)[names(QuestionMLTest) == 'NewColumn'] <- 'Q/N'

QuestionMLTest <- cbind(QuestionMLTest, Qlength = 1)
QuestionMLTest <- cbind(QuestionMLTest, AveWordLen = 1)
QuestionMLTest <- cbind(QuestionMLTest, my = 0)
QuestionMLTest <- cbind(QuestionMLTest, I = 0)
QuestionMLTest <- cbind(QuestionMLTest, take_verb = 0)
QuestionMLTest <- cbind(QuestionMLTest, pill_words = 0)
QuestionMLTest <- cbind(QuestionMLTest, feel_verb = 0)
QuestionMLTest$QuestionNumber <- NULL
QuestionMLTest$Url <- NULL

#Getting the data for testing
for (i in rownames(QuestionMLTest))
{
  #Test Q/n
  if(grepl("no-question",QuestionMLTest[i,"Question"],fixed = TRUE))
    QuestionMLTest[i,"Q/N"] <- 0
  
  #Test Qlength and AveWordLen
  x <- QuestionMLTest[i,"Question"]
  x <- gsub("[^0-9A-Za-z///']"," ",x,ignore.case = TRUE)
  x <- gsub("yahoo answers|no question", "",x)
  QuestionMLTest[i,"Qlength"] <- str_count(x,'\\w+')
  words <- strsplit(x, " ")
  j = 1
  word_length = NULL
  while (!(is.na(words[[1]][j])))
  {
    word_length <- rbind(word_length,nchar(words[[1]][j]))
    j = j + 1
  }
  word_length <- word_length[-which(word_length == 0)]
  QuestionMLTest[i,"AveWordLen"] <- round(mean(word_length),2)
  
  QuestCorp <- Corpus(VectorSource(QuestionMLTest[i,"Question"]))
  QuestCorp <- tm_map(QuestCorp, stripWhitespace)
  QuestCorp <- tm_map(QuestCorp, removeNumbers)
  QuestCorp <- tm_map(QuestCorp, removePunctuation)
  ndtm <- DocumentTermMatrix(QuestCorp,control=list(wordLengths=c(1,Inf)))
  nfreq <- colSums(as.matrix(ndtm))
  CorpWordList <- c("my","i","taking","take","taken","took","dose","pill","mg"
                    ,"feeling","feel","felt")
  
  for (k in 1:length(CorpWordList)) 
    if (is.na(nfreq[CorpWordList[k]]))
      nfreq[CorpWordList[k]] <- 0
  
  QuestionMLTest[i,"my"] <- nfreq["my"] 
  QuestionMLTest[i,"I"] <- nfreq["i"]
  QuestionMLTest[i,"take_verb"] <- nfreq["taking"] + nfreq["take"] + nfreq["taken"] + nfreq["took"]
  QuestionMLTest[i,"pill_words"] <- nfreq["dose"] + nfreq["pill"] + nfreq["mg"]
  QuestionMLTest[i,"feel_verb"] <- nfreq["feeling"] + nfreq["feel"] + nfreq["felt"]
  
}

#Analyzing Test Data to get patient Data
TestData <- as.data.frame(QuestionMLTable1)
TestData$Question <- NULL
TestDataCat <- predict(QFit, newdata = TestData)

TestData <- cbind(TestData,Classification = TestDataCat)
TestData <- cbind(TestData,Question = QuestionV)
PatientQuestions <- sqldf("select Question from TestData where Classification = 1")


#Reads in possible sideeffects
sideeffectsV <- readLines("C:/Users/Luisman/Desktop/USF FOLDERS/Fall 2017/CIS 4900 Independent Study in CS/DrugTextAnalysisFiles/BigDataSideEffects.txt")                                 
sideeffectsV <- unique(sideeffectsV)

#Reads in genericdrugs
genericdrugsV <- readLines("C:/Users/Luisman/Desktop/USF FOLDERS/Fall 2017/CIS 4900 Independent Study in CS/DrugTextAnalysisFiles/GenericDrugNames.txt")                                 
genericdrugsV <- unique(genericdrugsV)

#Reads in branddrugs
branddrugsV <- readLines("C:/Users/Luisman/Desktop/USF FOLDERS/Fall 2017/CIS 4900 Independent Study in CS/DrugTextAnalysisFiles/BrandNames.txt")                                 
branddrugsV <- unique(branddrugsV)

#Merge both drug vectors into one
drugsV <-c(genericdrugsV,branddrugsV)

#Lower Case Sideeffects
for (i in 1:length(sideeffectsV))
  sideeffectsV[i] <- sapply(sideeffectsV[i],tolower)

#Remove unnecesarry entries in Sideeffects Vector
sideeffectsV <- sideeffectsV[-which(sideeffectsV=="tic")]

#Lower Case Drugs Vector
for (i in 1:length(drugsV))
  drugsV[i] <- sapply(drugsV[i],tolower)

#Removes unnecesarry entries in Drugs Vector

drugsV <- drugsV[-which(drugsV=="na")]
drugsV <- drugsV[-which(drugsV=="")]
drugsV <- drugsV[-which(drugsV=="water")]


if ((identical(selection,"SideEffectsFreq")) || (identical(selection,"both")))
{
  #Getting the matching sideffects
  SideEffectList = NULL
  for (i in 1:length(PatientQuestions[,]))
  {
    
    for (j in 1:length(sideeffectsV))
    {
      if (grepl(sideeffectsV[j],PatientQuestions[i,],fixed = TRUE))
      {
        tmp<-c(i,sideeffectsV[j])
        SideEffectList=rbind(SideEffectList,tmp)
      }
    }
  }
  write.csv(SideEffectList,file="SideEffectList.csv")
  mydata = read.csv("SideEffectList.csv") 
  CleanData <- subset(mydata, select = -c(X))
  names(CleanData)[names(CleanData) == 'V1'] <- 'Question'
  names(CleanData)[names(CleanData) == 'V2'] <- 'SideEffect'
  SideEffectFreq <- sqldf("select SideEffect,Count(*)as Frequency from CleanData group by SideEffect order by Frequency desc")
  TopTenSideEffects <- sqldf("select * from SideEffectFreq limit 10")
  barplot(TopTenSideEffects$Frequency, main = "Top Ten Side Effects",ylab= "Frequency",names.arg=TopTenSideEffects$SideEffect,col="darkblue",las=2)
  sink("SideEffectFreq.txt")
  print(SideEffectFreq)
  sink()
  dev.copy(png,"TopTenSideEffectsChart.jpg")
  dev.off()
}
if ((identical(selection,"DrugFreq")) || (identical(selection,"both")))
{
  #Getting the matching drugs
  DrugList = NULL
  for (i in 1:length(PatientQuestions[,]))
  {
    for (j in 1:length(drugsV))
    {
      if (grepl(drugsV[j],PatientQuestions[i,],fixed = TRUE))
      {
        tmp<-c(i,drugsV[j])
        DrugList=rbind(DrugList,tmp)
      }
    }
  }
  write.csv(DrugList,file="DrugList.csv")
  mydata2 = read.csv("DrugList.csv")
  CleanData2 <- subset(mydata2, select = -c(X))
  names(CleanData2)[names(CleanData2) == 'V1'] <- 'Question'
  names(CleanData2)[names(CleanData2) == 'V2'] <- 'Drug'
  DrugFreq <- sqldf("select Drug,Count(*)as Frequency from CleanData2 group by Drug order by Frequency desc")
  sink("DrugFreq.txt")
  print(DrugFreq)
  sink()
}
