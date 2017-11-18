library(xml2)
library(rvest)

URL <- "https://answers.search.yahoo.com/search;_ylt=AwrC0wxqPQ9aE2YAmApPmolQ;_ylc=X1MDMTM1MTE5NTIxNQRfcgMyBGZyA3VoM19hbnN3ZXJzX3ZlcnRfZ3MEZ3ByaWQDaWhjNkhNTDVTME8xQXB1QTVDaG94QQRuX3JzbHQDMARuX3N1Z2cDMQRvcmlnaW4DYW5zd2Vycy5zZWFyY2gueWFob28uY29tBHBvcwMwBHBxc3RyAwRwcXN0cmwDBHFzdHJsAzI3BHF1ZXJ5A3BlbmljaWxsaW4lMjBzaWRlJTIwZWZmZWN0cwR0X3N0bXADMTUxMDk0ODIxNQ--?p=penicillin+side+effects&fr2=sb-top-answers.search&fr=uh3_answers_vert_gs&type=2button"

pg <- read_html(URL)

head(html_attr(html_nodes(pg,"a"),"href"))







page <- read_html("https://answers.search.yahoo.com/search;_ylt=AwrC0wxqPQ9aE2YAmApPmolQ;_ylc=X1MDMTM1MTE5NTIxNQRfcgMyBGZyA3VoM19hbnN3ZXJzX3ZlcnRfZ3MEZ3ByaWQDaWhjNkhNTDVTME8xQXB1QTVDaG94QQRuX3JzbHQDMARuX3N1Z2cDMQRvcmlnaW4DYW5zd2Vycy5zZWFyY2gueWFob28uY29tBHBvcwMwBHBxc3RyAwRwcXN0cmwDBHFzdHJsAzI3BHF1ZXJ5A3BlbmljaWxsaW4lMjBzaWRlJTIwZWZmZWN0cwR0X3N0bXADMTUxMDk0ODIxNQ--?p=penicillin+side+effects&fr2=sb-top-answers.search&fr=uh3_answers_vert_gs&type=2button")
urlList <- page %>% html_nodes(".fz-m") %>% html_attr('href')

urlList[1]
page1 <- read_html(urlList[1])
page1
page1 %>% html_nodes("Fz-24 Fw-300 Mb-10") 
dl