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

#The actual data
titleV <- readLines("C:/Users/Luisman/Desktop/USF FOLDERS/Fall 2017/CIS 4900 Independent Study in CS/DrugTextAnalysisFiles/Questions.txt")  
#reads in text as string, if blank lines, will pull in as list
#realV <- readLines("NewTest.txt")
titleV <- (str_replace_all(titleV, "[[:punct:]]", ""))   #removes special characters

realV <- readLines("C:/Users/Luisman/Desktop/NewTest.txt")  
#reads in text as string, if blank lines, will pull in as list
#realV <- readLines("NewTest.txt")
realV <- (str_replace_all(realV, "[[:punct:]]", ""))   #removes special characters

for (i in 1:length(titleV))
{
  titleV[i] <- sapply(titleV[i],tolower)
  titleV[i] <- sapply(titleV[i],removeNumbers)
}
for (i in 1:length(realV))
{
  realV[i] <- sapply(realV[i],tolower)
  realV[i] <- sapply(realV[i],removeNumbers)
}


sideeffectsV <- readLines("C:/Users/Luisman/Desktop/BigDataSideEffects.txt")                                 
#similar to above, modified to resemble read.table()
#sideeffectsV <- readLines("BigDataSideEffects.txt")
sideeffectsV <- (str_replace_all(sideeffectsV, "[[:punct:]]", ""))
sideeffectsV <- unique(sideeffectsV)

genericdrugsV <- readLines("C:/Users/Luisman/Desktop/GenericDrugNames.txt")                                 
genericdrugsV <- (str_replace_all(genericdrugsV, "[[:punct:]]", ""))
genericdrugsV <- unique(genericdrugsV)


for (i in 1:length(sideeffectsV))
{
  sideeffectsV[i] <- sapply(sideeffectsV[i],tolower)
  #sideeffectsV[i] <- gsub("\\s","++",sideeffectsV[i])
}
id=which(sideeffectsV=="tic")
sideeffectsV=sideeffectsV[-id]

for (i in 1:length(genericdrugsV))
{
  genericdrugsV[i] <- sapply(genericdrugsV[i],tolower)
  #sideeffectsV[i] <- gsub("\\s","++",sideeffectsV[i])
}
id=which(genericdrugsV=="na")
genericdrugsV=genericdrugsV[-id]

#realV
#sideeffectsV
#h<- grep(sideeffectsV[3279],realV[1],value = TRUE)
#h

#Getting  the results from this loop
SideEffectList = NULL
for (i in 1:length(realV))
{

  for (j in 1:length(sideeffectsV))
  {
     #id=grep(sideeffectsV[j],realV[i],fixed = TRUE)
     id=grepl(sideeffectsV[j],realV[i],fixed = TRUE)
      if (id)
      {
        tmp<-c(i,sideeffectsV[j])
        SideEffectList=rbind(SideEffectList,tmp)
      }
  }
}
SideEffectList
DrugList = NULL
for (i in 1:length(realV))
{
  
  for (j in 1:length(genericdrugsV))
  {
    #id=grep(sideeffectsV[j],realV[i],fixed = TRUE)
    id=grepl(genericdrugsV[j],realV[i],fixed = TRUE)
    if (id)
    {
      tmp<-c(i,genericdrugsV[j])
      DrugList=rbind(DrugList,tmp)
    }
  }
}
DrugList
realV
#SideEffectList<-SideEffectList[!is.na(SideEffectList)]
#SideEffectList
write.csv(SideEffectList,file="SideEffectList.csv")
write.csv(DrugList,file="DrugList.csv")
mydata = read.csv("SideEffectList.csv")  # read csv file
mydata2 = read.csv("DrugList.csv")  # read csv file
mydata2

CleanData <- subset(mydata, select = -c(X))
typeof(CleanData)
CleanData2 <- subset(mydata2, select = -c(X))
CleanData2
names(CleanData)[names(CleanData) == 'V1'] <- 'Question#'
names(CleanData)[names(CleanData) == 'V2'] <- 'SideEffect'

names(CleanData2)[names(CleanData2) == 'V1'] <- 'Question#'
names(CleanData2)[names(CleanData2) == 'V2'] <- 'Drug'
CleanData2
typeof(CleanData2)

#trying to find freq of drugs
SideEffect_df <- data_frame(matrix(unlist(CleanData), nrow=132, byrow=T),stringsAsFactors=FALSE)
tidy_CleanData <- CleanData %>%
unnest_tokens(word, 'SideEffect')

df <-ldply(CleanData, data.frame)
df
#Method 2
#SideEffectList = NULL
#for (i in 1:length(realV))
#{
 # Question <- strsplit(realV[i], " ")[[1]]
 # for (j in 1:length(sideeffectsV))
 # {
 #   if (sideeffectsV[j] %in% Question)
  #    SideEffectList[i] <- sideeffectsV[j]
  #}
#}
#SideEffectList
#realV[1]
#isc = 0
#for (j in 1:length(sideeffectsV))
#{
 # if ('tic' %in% sideeffectsV[j])
 #   isc <- j
#}
#isc
#SideEffectList
#realV[1]
realV[7]

DrugEffects <- Corpus(VectorSource(SideEffectList))
#DrugEffects <- tm_map(DrugEffects, removeWords, stopwords("SMART"))
ndtm <- DocumentTermMatrix(DrugEffects)

nfreq <- colSums(as.matrix(ndtm))
neword <- order(nfreq,decreasing = TRUE)
neword

nwf=data.frame(term=names(nfreq),occurrences=nfreq)

np <- ggplot(subset(nwf, nfreq>100), aes(term, occurrences))
np <- np + geom_bar(stat="identity")
np <- np + theme(axis.text.x=element_text(angle=45, hjust=1))
np
#wordcloud(DrugEffects,max.words = 100, random.order= FALSE,colors = brewer.pal(6,"Dark2"))

