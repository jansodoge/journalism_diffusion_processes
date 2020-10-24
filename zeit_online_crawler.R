library(RCurl)
library(XML)
library(stringr)
library(RSelenium)
library(tidyverse)
remDr <- rsDriver(verbose = T,
                  remoteServerAddr = "localhost",
                  port = 4443L,
                  browser=c("firefox"))
rm <- remDr$client


#register
rm$navigate("https://meine.zeit.de/anmelden") 

webElem <- rm$findElement(using = 'xpath', '//input[@id = "login_email"]')
webElem$sendKeysToElement(list("your_username"))
webElem_2 <- rm$findElement(using = 'xpath', '//input[@id = "login_pass"]')
webElem_2$sendKeysToElement(list("your_password", key = "enter"))
# -> NOW  ONCE CLICK TO SKIP THE NEXT PANEL WITHIN THE SELENIUM CLIENT GUI





get_paper_article_links <- function(year, issue){
  url <- paste0("https://www.zeit.de/",year,"/",issue,"/index")
  rm$navigate(url) 
  page <- unlist(rm$getPageSource())
  tpage <- htmlParse(page)
  xpathSApply(tpage, "//a[@class='teaser-small__commentcount js-update-commentcount']", xmlAttrs)[2,]
}




#create a large list with articles to crawl for
articles_2012 <- c()

for(index in seq(10,52)){
try(articles_2012 <- append(articles_2012, as.vector(get_paper_article_links(2012, index))))
}



save(articles_2012, file = "articles_2012.RData")





#gather the timestamps of comments for the articles

get_comment_timestamps <- function(url){
  rm$navigate(url)
  Sys.sleep(3)
  page <- unlist(rm$getPageSource())
  tpage <- htmlParse(page)
  comment_pages <- xpathSApply(tpage, "//span[@class='comment-section__headline']/small", xmlValue)
  comment_pages <- as.numeric(str_extract_all(comment_pages, "[0-9]{1,2}")[[1]][2])
  print(url)
  author_raw <- NA
  try(author_raw <- xpathSApply(tpage, "//div[@class='byline']", xmlValue))
  keywords_raw <- NA
  try(keywords_raw <- xpathSApply(tpage, "//ul[@class='article-tags__list']", xmlValue))
  print(comment_pages)
  #create a list of the comment pages
  comment_urls <- c()
  for(comment_page in seq(1, comment_pages)){
  new_url_pattern <- paste0(str_split(url , "#" )[[1]][1],
                            "?page=",comment_page,"#comments")
  comment_urls <- append(comment_urls, new_url_pattern)
  }
  comment_database_article <- data.frame()
  #for each of these URLs get the comment timestamps and store them
  for(comment_url_page in comment_urls){
    rm$navigate(comment_url_page)
    Sys.sleep(3)
    print(comment_url_page)
    page <- unlist(rm$getPageSource())
    tpage <- htmlParse(page)
    #get the comment data
    timestamps <- as.vector(xpathSApply(tpage, "//a[@class='comment-meta__date']", xmlValue))
    user_id_raw <- c(seq(1, length(timestamps)))
    try(user_id_raw <- as.vector(xpathSApply(tpage, "//h4[@class='comment-meta__name']", xmlValue)))
    #print(timestamps)
    hour <- str_extract(timestamps, "[0-9]{1,2}:[0-9]{1,2}")
    day <- gsub("\\.","", str_extract(timestamps, "[0-9]{1,2}\\. "))
    print(day)
      month_comment <- c()
    for(timestamp in timestamps){
      #use string matching to get month
      months <- c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")
      for(month in months){
        if(grepl(month, timestamp)){
          article_month <- match(month, months)
          month_comment <- append(month_comment, article_month)
        }
      }
    }
    #print(length(month_comment))
    
    try(
    collected_data <- data.frame(article = url, hour = hour, day = day, month = month_comment, author = author_raw, keywords = keywords_raw,
                                 user_id = user_id_raw))
    comment_database_article <- dplyr::bind_rows(comment_database_article, collected_data)
  }
  
  
  return(comment_database_article)
}



articles_comment_2014 <- data.frame()
for(index in seq(1, length(articles_2014))){
  try(articles_comment_2014 <- dplyr::bind_rows(articles_comment_2014,
                                                get_comment_timestamps(articles_2014[index])))
  print(paste("index at", index))
  
}

save(articles_comment_2014, file = "nightly_data_2014.RData")

articles_comment_2015 <- data.frame()
for(index in seq(1, length(articles_2015))){
  try(articles_comment_2015 <- dplyr::bind_rows(articles_comment_2015,
                                                get_comment_timestamps(articles_2015[index])))
  print(paste("index at", index))
  
}
save(articles_comment_2015, file = "nightly_data_2015.RData")




#close selenium
rm$close()
rm(remDr)
rm(rm)
gc()







###
#script to get data of publication
###

publication_dates <- data.frame()

for(index in seq(1, length(articles))){
rm$navigate(articles[index])
Sys.sleep(4)
page <- unlist(rm$getPageSource())
tpage <- htmlParse(page)
publication_date <- NA

try(publication_date <- as.character(xpathSApply(tpage, "//time[@class='metadata__date']", xmlAttrs)[1,1]))
print(publication_date)
publication_dates <- bind_rows(publication_dates, as.data.frame(t(c(articles[index], publication_date))))

}