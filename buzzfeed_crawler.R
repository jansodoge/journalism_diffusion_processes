library(RCurl)
library(XML)
library(stringr)
library(RSelenium)
library(tidyverse)






get_article_list <- function(url, accept_agreements){
  
  rm$navigate(url) 
  
  
  if(accept_agreements == TRUE){  
    Sys.sleep(3)
    rm$findElement(using = "xpath", 
                   '/html/body/div[1]/div/div/div/div[2]/div/button[3]')$clickElement()
  }
  
  
  for(index in seq(1,4)){
  try(rm$findElement(using = "xpath", 
                 '//button[@class="js-feed-load-more button"]')$clickElement())
  }  
  
  page <- unlist(rm$getPageSource())
  tpage <- htmlParse(page)
  return(xpathSApply(tpage, "//a[@class='js-card__link link-gray']", xmlAttrs)[2,])

}



get_comments <- function(url, accept_agreements){
#open up the URL
rm$navigate(url) 
#accept data privacy terms
  
   if(accept_agreements == TRUE){  
 rm$findElement(using = "xpath", 
                '/html/body/div[1]/div/div/div/div[2]/div/button[3]')$clickElement()
 }
Sys.sleep(2)
  page <- unlist(rm$getPageSource())
  tpage <- htmlParse(page)
  
  
  views <- NA
  views <- try(xpathSApply(tpage, "//div[@class='trendingTitle__KRorkcBZDd trendingViewCount__1nq1Blc-zw']", xmlValue))
  
  
  return(c(xpathSApply(tpage, "//span[@class='commentCount__2iANm']", xmlValue)[1], views))
  

  }



#####
# WORKFLOW
#####


while(TRUE){
  
  
#establish remote driver for RSelinium
remDr <- rsDriver(verbose = T,
                  remoteServerAddr = "localhost",
                  port = 4444L,
                  browser=c("firefox"))

rm <- remDr$client

# crreate movies_tv dataset
url_movies_tv <- get_article_list(url = "https://www.buzzfeed.com/tvandmovies", accept_agreements = TRUE)
movie_tv_views <- data.frame(url = url_movies_tv, comment_number = NA, time = NA, views = NA, time_date = NA)
for(index in seq(1, nrow(movie_tv_views)/2)){
  movie_tv_views$time[index] <- Sys.time()
  tmp <- get_comments(movie_tv_views$url[index], accept_agreements = FALSE)
  
  movie_tv_views$comment_number[index] <- tmp[1]
  movie_tv_views$views[index] <- tmp[2]
  movie_tv_views$time_date[index] <- timestamp()
}


save(movie_tv_views, file = paste0(Sys.time(), "movie_tv.RData"))


url_topics_shopping <- get_article_list(url = "https://www.buzzfeed.com/shopping", accept_agreements = FALSE)
shopping_views <- data.frame(url = url_topics_shopping, comment_number = NA, time = NA, views = NA, time_date = NA)
for(index in seq(1, nrow(shopping_views)/2)){
  shopping_views$time[index] <- Sys.time()
  tmp <- get_comments(shopping_views$url[index], accept_agreements = FALSE)
  shopping_views$comment_number[index] <- tmp[1]
  shopping_views$views[index] <- tmp[2]
  shopping_views$time_date[index] <- timestamp()
  
  
}

save(shopping_views, file = paste0(Sys.time(), "shopping.RData"))


url_topic_quiz <- get_article_list(url = "https://www.buzzfeed.com/quizzes", accept_agreements = FALSE)
quiz_views <- data.frame(url = url_topic_quiz, comment_number = NA, time = NA, views = NA, time_date = NA)
for(index in seq(1, nrow(quiz_views)/2)){
  quiz_views$time[index] <- Sys.time()
  tmp <- get_comments(quiz_views$url[index], accept_agreements = FALSE)
  quiz_views$comment_number[index] <- tmp[1]
  quiz_views$views[index] <- tmp[2]
  quiz_views$time_date[index] <- timestamp()
  
  
}
save(quiz_views, file = paste0(Sys.time(), "quiz.RData"))
rm$close()
rm(remDr)
rm(rm)
gc()

Sys.sleep(60*30)


}

