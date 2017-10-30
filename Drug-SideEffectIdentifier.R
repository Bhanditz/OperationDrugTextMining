#*************************************************
#Drug/SideEffectIdentifier                       *
#Authors: Luis Franco, Delroy Fong               *
#Supervisers: Dr. Feng Cheng, Dr. Yicheng Tu     *
#*************************************************

library(tm)
library(gtools)
library(stringr)
library(wordcloud)
library(SnowballC)
library(stringr)
library(rvest)
library(textreg)
library(ggplot2)
library(plyr)
library(tidytext)
library(dplyr)
require(sqldf)

#Read in the question subject and full question
titleV <- readLines("C:/Users/Luisman/Desktop/USF FOLDERS/Fall 2017/CIS 4900 Independent Study in CS/DrugTextAnalysisFiles/Questions.txt")  
realV <- readLines("C:/Users/Luisman/Desktop/NewTest.txt")  

#The full question
QuestionV = NULL
QuestionV = paste(titleV,realV,sep = " | ")

#Modifies data to lower case 
for (i in 1:length(QuestionV))
    QuestionV[i] <- sapply(QuestionV[i],tolower)


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

#Create csv files
write.csv(SideEffectList,file="SideEffectList.csv")
write.csv(DrugList,file="DrugList.csv")
#Convert vectors to lists
mydata = read.csv("SideEffectList.csv")  
mydata2 = read.csv("DrugList.csv")

#Remove unnecessary rows
CleanData <- subset(mydata, select = -c(X))
CleanData2 <- subset(mydata2, select = -c(X))

#Rename columns in lists
names(CleanData)[names(CleanData) == 'V1'] <- 'Question'
names(CleanData)[names(CleanData) == 'V2'] <- 'SideEffect'
names(CleanData2)[names(CleanData2) == 'V1'] <- 'Question'
names(CleanData2)[names(CleanData2) == 'V2'] <- 'Drug'


SideEffectFreq <- sqldf("select SideEffect,Count(*)as Frequency from CleanData group by SideEffect order by Frequency desc")
SideEffectFreq


DrugFreq <- sqldf("select Drug,Count(*)as Frequency from CleanData2 group by Drug order by Frequency desc")
DrugFreq




