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

#The actual data
#realV <- readLines("C:/Users/Luisman/Desktop/NewTestR/NewTest.txt")  #reads in text as string, if blank lines, will pull in as list
realV <- readLines("NewTest.txt")
realV <- (str_replace_all(realV, "[[:punct:]]", ""))   #removes special characters

for (i in 1:length(realV))
{
  realV[i] <- sapply(realV[i],tolower)
  realV[i] <- sapply(realV[i],removeNumbers)
}


#sideeffectsV <- readLines("C:/Users/Luisman/Desktop/BigDataSideEffects.txt")                                 
#similar to above, modified to resemble read.table()
sideeffectsV <- readLines("BigDataSideEffects.txt")
sideeffectsV <- (str_replace_all(sideeffectsV, "[[:punct:]]", ""))
sideeffectsV <- unique(sideeffectsV)



for (i in 1:length(sideeffectsV))
{
  sideeffectsV[i] <- sapply(sideeffectsV[i],tolower)
  #sideeffectsV[i] <- gsub("\\s","++",sideeffectsV[i])
}
id=which(sideeffectsV=="tic")
sideeffectsV=sideeffectsV[-id]

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
write.csv(SideEffectList,file="SideEffectList.csv")

#Method 2
SideEffectList = NULL
for (i in 1:length(realV))
{
  Question <- strsplit(realV[i], " ")[[1]]
  for (j in 1:length(sideeffectsV))
  {
    if (sideeffectsV[j] %in% Question)
      SideEffectList[i] <- sideeffectsV[j]
  }
}
SideEffectList
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
#sideeffectsV <- paste(sideeffectsV,sep = ' ',collapse = ' ')                # <---- Change made here
#sideeffectsV <- as.list(strsplit(sideeffectsV," "))  
#sideeffectsV <- as.data.frame(sideeffectsV,header = FALSE)
#colnames(sideeffectsV) <- "SideEffects"
#removenewline <- "SideEffects"
#sideeffectsV <- as.data.frame(sideeffectsV[ sideeffectsV$SideEffects != removenewline,],header= FALSE)
#colnames(sideeffectsV) <- "SideEffects"

#hits <- unique(sideeffectsV$SideEffects)                         # all of the side effects
#findus <- as.data.frame(realV[realV$WORDS %in% hits,])          # matching hits in realV
#withoutus <-as.data.frame(realV[!(realV$WORDS %in% hits),])     # all without hits
#colnames(findus) <- "WORDS"
#colnames(withoutus) <- "WORDS" */


#words_matched_in_real <- findus$WORDS                                        #all words matched
#junked_words <- withoutus$WORDS                                              #all junked words
#DrugEffects <- Corpus(VectorSource(words_matched_in_real))
#DrugEffects <- tm_map(DrugEffects, removeWords, stopwords("SMART"))
#ndtm <- DocumentTermMatrix(DrugEffects)

#nfreq <- colSums(as.matrix(ndtm))
#neword <- order(nfreq,decreasing = TRUE)


#nwf=data.frame(term=names(nfreq),occurrences=nfreq)

#np <- ggplot(subset(nwf, nfreq>100), aes(term, occurrences))
#np <- np + geom_bar(stat="identity")
#np <- np + theme(axis.text.x=element_text(angle=45, hjust=1))
#np
#wordcloud(DrugEffects,max.words = 100, random.order= FALSE,colors = brewer.pal(6,"Dark2"))

