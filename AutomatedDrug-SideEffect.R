#*************************************************
#AutomatedDrug-SideEffectIdentifier              *
#Authors: Luis Franco, Delroy Fong               *
#Supervisers: Dr. Feng Cheng, Dr. Yicheng Tu     *
#*************************************************

library(tm)
library(gtools)
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

titleV = NULL
realV = NULL

if (length(allpglist) >= 1000)
{
  for (i in 1:999)
  {
    QuestPage <- read_html(allpglist[i])
    titleV[i] <- QuestPage %>% html_nodes("title") %>% html_text("title")
    realV[i] <- QuestPage %>% html_node(".ya-q-full-text") %>% html_text("text")
  }
}else{
  for (i in 1:length(allpglist))
  {
    QuestPage <- read_html(allpglist[i])
    titleV[i] <- QuestPage %>% html_nodes("title") %>% html_text("title")
    realV[i] <- QuestPage %>% html_node(".ya-q-full-text") %>% html_text("text")
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

#Reads in possible sideeffects
sideeffectsV <- readLines("C:/Users/Luisman/Desktop/BigDataSideEffects.txt")                                 
sideeffectsV <- unique(sideeffectsV)

#Reads in genericdrugs
genericdrugsV <- readLines("C:/Users/Luisman/Desktop/GenericDrugNames.txt")                                 
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
RemoveEffect1=which(sideeffectsV=="tic")
sideeffectsV=sideeffectsV[-RemoveEffect1]

#Lower Case Drugs Vector
for (i in 1:length(drugsV))
  drugsV[i] <- sapply(drugsV[i],tolower)

#Removes unnecesarry entries in Drugs Vector
RemoveDrug1=which(drugsV=="na")
drugsV=drugsV[-RemoveDrug1]
RemoveDrug2=which(drugsV=="")
drugsV=drugsV[-RemoveDrug2]
RemoveDrug3=which(drugsV=="water")
drugsV=drugsV[-RemoveDrug3]

if ((identical(selection,"SideEffectsFreq")) || (identical(selection,"both")))
{
  #Getting the matching sideffects
  SideEffectList = NULL
  for (i in 1:length(QuestionV))
  {
  
    for (j in 1:length(sideeffectsV))
    {
      id=grepl(sideeffectsV[j],QuestionV[i],fixed = TRUE)
      if (id)
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
  for (i in 1:length(QuestionV))
  {
  
    for (j in 1:length(drugsV))
    {
      id=grepl(drugsV[j],QuestionV[i],fixed = TRUE)
      if (id)
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
