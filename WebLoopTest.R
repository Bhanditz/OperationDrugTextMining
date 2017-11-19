library(xml2)
library(rvest)

pg_alive = TRUE
user_url <- readline(prompt="Please enter the Yahoo! Answers search url:\n")

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
