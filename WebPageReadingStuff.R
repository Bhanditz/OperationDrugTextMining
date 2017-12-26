library(xml2)
library(rvest)

URL <- "https://answers.search.yahoo.com/search;_ylt=AwrC0wxqPQ9aE2YAmApPmolQ;_ylc=X1MDMTM1MTE5NTIxNQRfcgMyBGZyA3VoM19hbnN3ZXJzX3ZlcnRfZ3MEZ3ByaWQDaWhjNkhNTDVTME8xQXB1QTVDaG94QQRuX3JzbHQDMARuX3N1Z2cDMQRvcmlnaW4DYW5zd2Vycy5zZWFyY2gueWFob28uY29tBHBvcwMwBHBxc3RyAwRwcXN0cmwDBHFzdHJsAzI3BHF1ZXJ5A3BlbmljaWxsaW4lMjBzaWRlJTIwZWZmZWN0cwR0X3N0bXADMTUxMDk0ODIxNQ--?p=penicillin+side+effects&fr2=sb-top-answers.search&fr=uh3_answers_vert_gs&type=2button"

page <- read_html(URL)

head(html_attr(html_nodes(pg,"a"),"href"))

var <- "no"
var <- "no-question"
URL <- "https://answers.search.yahoo.com/search;_ylt=AwrXnCeldjFarAoAkhxPmolQ;_ylc=X1MDMTM1MTE5NTIxNQRfcgMyBGZyA3VoM19hbnN3ZXJzX3ZlcnRfZ3MEZ3ByaWQDBG5fcnNsdAMwBG5fc3VnZwMwBG9yaWdpbgNhbnN3ZXJzLnNlYXJjaC55YWhvby5jb20EcG9zAzAEcHFzdHIDBHBxc3RybAMEcXN0cmwDMjUEcXVlcnkDd2FyZmFyaW4lMjBzaWRlJTIwZWZmZWN0cwR0X3N0bXADMTUxMzE5MjM5NQ--?p=warfarin+side+effects&fr2=sb-top-answers.search&fr=uh3_answers_vert_gs&type=2button"


#Get main url
page <- read_html("https://answers.search.yahoo.com/search;_ylt=AwrC0wxqPQ9aE2YAmApPmolQ;_ylc=X1MDMTM1MTE5NTIxNQRfcgMyBGZyA3VoM19hbnN3ZXJzX3ZlcnRfZ3MEZ3ByaWQDaWhjNkhNTDVTME8xQXB1QTVDaG94QQRuX3JzbHQDMARuX3N1Z2cDMQRvcmlnaW4DYW5zd2Vycy5zZWFyY2gueWFob28uY29tBHBvcwMwBHBxc3RyAwRwcXN0cmwDBHFzdHJsAzI3BHF1ZXJ5A3BlbmljaWxsaW4lMjBzaWRlJTIwZWZmZWN0cwR0X3N0bXADMTUxMDk0ODIxNQ--?p=penicillin+side+effects&fr2=sb-top-answers.search&fr=uh3_answers_vert_gs&type=2button")
urlList <- page %>% html_nodes(".fz-m") %>% html_attr('href')
urlList <- urlList[-length(urlList)]

#Read each url in list
Q3 <- read_html(urlList[3])
title3<- Q1 %>% html_nodes("title") %>% html_text("title")
question3<-Q1 %>% html_node(".ya-q-text") %>% html_text("text")

#Get next page
page2 <- page %>% html_nodes("a.next") %>% html_attr('href')
url2 <- read_html(page2)
urlList2 <- url2 %>% html_nodes(".fz-m") %>% html_attr('href')
urlList2 <- urlList2[-length(urlList2)]

#Read from page 2 url
Q2 <- read_html(urlList2[1])
title2<- Q2 %>% html_nodes("title") %>% html_text("title")
question2<-Q2 %>% html_node(".ya-q-full-text") %>% html_text("text")






