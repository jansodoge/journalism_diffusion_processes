library(diffusion)





files <- list.files("buzzfeed_data", full.names = TRUE)


movie_tv_live_scrape_results <- data.frame()
#get tv and movie
movie_files <- files[grepl("movie", files)]
for(file in movie_files){
  
  load(file)
  movie_tv_live_scrape_results <- dplyr::bind_rows(movie_tv_live_scrape_results, movie_tv_views)
  
  
}


save(movie_tv_live_scrape_results, file = "movie_tv_scrape_results.RData")


#get shopping

shopping_live_scrape_results <- data.frame()
#get tv and movie
movie_files <- files[grepl("shopping", files)]
for(file in movie_files){
  
  load(file)
  shopping_live_scrape_results <- dplyr::bind_rows(shopping_live_scrape_results, shopping_views)
  
  
}
save(shopping_live_scrape_results, file = "shopping_scrape_results.RData")


#get quiz
quiz_live_scrape_results <- data.frame()
#get tv and movie
movie_files <- files[grepl("quiz", files)]
for(file in movie_files){
  
  load(file)
  quiz_live_scrape_results <- dplyr::bind_rows(quiz_live_scrape_results, movie_tv_views)
  
  
}
save(quiz_live_scrape_results, file = "quiz_scrape_results.RData")







#clean movie and tv dataset 

load("movie_tv_scrape_results.RData")
cleaned_tv_live_scrape_results <- movie_tv_live_scrape_results %>% 
  filter(!is.na(comment_number)) %>% 
  mutate(comment_number = str_extract(comment_number, "[0-9]{1,3}")) %>% 
  filter(!is.na(comment_number)) %>% 
  mutate(comment_number = as.numeric(comment_number))



#most prominent articles we crawled
top_crawled_articles <- cleaned_tv_live_scrape_results %>% 
  count(url) %>% 
  arrange(-n)


range_of_article_views <- cleaned_tv_live_scrape_results %>%
  group_by(url) %>% 
  summarise(max_views = max(comment_number),
            min_views = min(comment_number),
            mean_views = mean(comment_number),
            diff_views = max_views - min_views) %>% 
  filter(diff_views > 20)


  
relevant_articles <- cleaned_tv_live_scrape_results %>% 
  inner_join(range_of_article_views, by = "url") 






#to check specific articles
relevant_articles %>% 
  
  ggplot(aes(x = time, y = comment_number, fill = url))+
  geom_line(aes(fill = url))+
  theme(legend.text = element_blank())


#standardize t_0 for all articles

relevant_articles_t_standardized <- relevant_articles %>% 
  group_by(url) %>% 
  summarise(min_t_0 = min(time),
            max_t = max(time))



relevant_articles %>% 
  inner_join(relevant_articles_t_standardized, by = "url") %>% 
  mutate(time_standard = time  - min_t_0) %>% 
  ggplot(aes(x = time_standard, y = comment_number, fill = url))+
  geom_line(aes(fill = url))+
  theme(legend.text = element_blank())

#standardize comment capacity (speaking in Bass Model) for all articles
relevant_articles_t_comments_standardized <- relevant_articles %>% 
  group_by(url) %>% 
  summarise(min_comments = min(comment_number),
            max_comments = max(comment_number))


relevant_articles %>% 
  inner_join(relevant_articles_t_standardized, by = "url") %>% 
  mutate(time_standard = time  - min_t_0) %>% 
  inner_join(relevant_articles_t_comments_standardized, by = "url") %>% 
  mutate(comment_normalized = (comment_number - min_comments)/(max_comments - min_comments)) %>% 



  ggplot(aes(x = time_standard, y = comment_normalized, fill = url))+
  geom_line(aes(fill = url))+
  theme(legend.text = element_blank())
  





#further filter criteria
# --> comment number lower than x at t_0
relevant_articles_t_comments_standardized <- relevant_articles %>% 
  group_by(url) %>% 
  summarise(min_comments = min(comment_number),
            max_comments = max(comment_number))


diffusion_model_dataset <- relevant_articles %>% 
  inner_join(relevant_articles_t_standardized, by = "url") %>% 
  mutate(time_standard = time  - min_t_0) %>% 
  inner_join(relevant_articles_t_comments_standardized, by = "url") %>% 
  mutate(comment_normalized = (comment_number - min_comments)/(max_comments - min_comments)) %>% 
  filter(min_comments < 30) %>% #less than 30 comments in the beginning
  filter(max_t - min_t_0 > 100000) 
  
diffusion_model_dataset_tv_movie <- diffusion_model_dataset
  
  
  ggplot(data = diffusion_model_dataset, aes(x = time_standard/60/60, y = comment_number, fill = url))+
  geom_line(aes(fill = url))+
  theme(legend.text = element_blank())
  
  
# Fit a diffusion curve
  
coefficients_per_article <- data.frame()
  
for(url_article in unique(as.vector(diffusion_model_dataset$url))){  
print(url_article)
    
curve_data <- as.vector(dplyr::filter(diffusion_model_dataset, url == url_article)$comment_number)
curve_data_t <-  as.vector(dplyr::filter(diffusion_model_dataset, url == url_article)$time_standard)
print(length(curve_data))




#plot(diffusion(curve_data, cumulative = FALSE, type = "bass"))
#plot(curve_data)
#interpolation on a linear level
interpolation <- approx(x = curve_data_t, y = curve_data, xout = seq(0,curve_data_t[length(curve_data_t)], 10000))
#plot(interpolation$x, interpolation$y)
diffusion_parameters <- diffusion(interpolation$y, cumulative = FALSE, type = "bass")
coefficient_innovation <- diffusion_parameters$w[1]
coefficient_imitation <- diffusion_parameters$w[2]

tmp <- c(url_article, coefficient_innovation, coefficient_imitation)
coefficients_per_article <- dplyr::bind_rows(coefficients_per_article, as.data.frame(t(tmp)))


}

coefficients_per_article <- coefficients_per_article %>% 
  mutate(p = as.numeric(p),
         q = as.numeric(q))
  

ggplot(data = coefficients_per_article, aes(x = p, y = q))+
  geom_point()+
  xlim(c(0,0.3))+
  ylim(c(0,0.3))+
  geom_abline(slope = 1)
  
coefficients_per_article_tv_movie <- coefficients_per_article  %>% 
  mutate(type = "tv_movie")




######################################
# Analyzing quiz data
######################################


#get quiz
quiz_live_scrape_results <- data.frame()
#get tv and movie
movie_files <- files[grepl("quiz", files)]
for(file in movie_files){
  
  load(file)
  quiz_live_scrape_results <- dplyr::bind_rows(quiz_live_scrape_results, quiz_views)
  
  
}



#clean movie and tv dataset 



cleaned_tv_live_scrape_results <- quiz_live_scrape_results %>% 
  filter(!is.na(comment_number)) %>% 
  mutate(comment_number = str_extract(comment_number, "[0-9]{1,3}")) %>% 
  filter(!is.na(comment_number)) %>% 
  mutate(comment_number = as.numeric(comment_number))



#most prominent articles we crawled
top_crawled_articles <- cleaned_tv_live_scrape_results %>% 
  count(url) %>% 
  arrange(-n)


range_of_article_views <- cleaned_tv_live_scrape_results %>%
  group_by(url) %>% 
  summarise(max_views = max(comment_number),
            min_views = min(comment_number),
            mean_views = mean(comment_number),
            diff_views = max_views - min_views) %>% 
  filter(diff_views > 20)



relevant_articles <- cleaned_tv_live_scrape_results %>% 
  inner_join(range_of_article_views, by = "url") 






#to check specific articles
relevant_articles %>% 
  
  ggplot(aes(x = time, y = comment_number, fill = url))+
  geom_line()+
  theme(legend.text = element_blank())


#standardize t_0 for all articles

relevant_articles_t_standardized <- relevant_articles %>% 
  group_by(url) %>% 
  summarise(min_t_0 = min(time),
            max_t = max(time))



relevant_articles %>% 
  inner_join(relevant_articles_t_standardized, by = "url") %>% 
  mutate(time_standard = time  - min_t_0) %>% 
  ggplot(aes(x = time_standard, y = comment_number, fill = url))+
  geom_line(aes(fill = url))+
  theme(legend.text = element_blank())

#standardize comment capacity (speaking in Bass Model) for all articles
relevant_articles_t_comments_standardized <- relevant_articles %>% 
  group_by(url) %>% 
  summarise(min_comments = min(comment_number),
            max_comments = max(comment_number))


relevant_articles %>% 
  inner_join(relevant_articles_t_standardized, by = "url") %>% 
  mutate(time_standard = time  - min_t_0) %>% 
  inner_join(relevant_articles_t_comments_standardized, by = "url") %>% 
  mutate(comment_normalized = (comment_number - min_comments)/(max_comments - min_comments)) %>% 
  
  
  
  ggplot(aes(x = time_standard, y = comment_normalized, fill = url))+
  geom_line(aes(fill = url))+
  theme(legend.text = element_blank())






#further filter criteria
# --> comment number lower than x at t_0
relevant_articles_t_comments_standardized <- relevant_articles %>% 
  group_by(url) %>% 
  summarise(min_comments = min(comment_number),
            max_comments = max(comment_number))


diffusion_model_dataset <- relevant_articles %>% 
  inner_join(relevant_articles_t_standardized, by = "url") %>% 
  mutate(time_standard = time  - min_t_0) %>% 
  inner_join(relevant_articles_t_comments_standardized, by = "url") %>% 
  mutate(comment_normalized = (comment_number - min_comments)/(max_comments - min_comments)) %>% 
  filter(min_comments < 30) %>% #less than 30 comments in the beginning
  filter(max_t - min_t_0 > 100000) 

diffusion_model_dataset_quiz <- diffusion_model_dataset

articles_shopping <- unique(as.vector(diffusion_model_dataset$url))



ggplot(data = dplyr::filter(diffusion_model_dataset, url == articles_shopping[1]), 
       aes(x = time_standard, y = comment_number, fill = url))+
  geom_line(aes(fill = url))+
  theme(legend.text = element_blank())


ggplot(data = diffusion_model_dataset, 
       aes(x = time_standard/60/60, y = comment_number, fill = url))+
  geom_line()+
  theme(legend.text = element_blank())


# Fit a diffusion curve

coefficients_per_article <- data.frame()

for(url_article in unique(as.vector(diffusion_model_dataset$url))){  
  print(url_article)
  
  curve_data <- as.vector(dplyr::filter(diffusion_model_dataset, url == url_article)$comment_number)
  curve_data_t <-  as.vector(dplyr::filter(diffusion_model_dataset, url == url_article)$time_standard)
  print(length(curve_data))
  
  
  
  
  #plot(diffusion(curve_data, cumulative = FALSE, type = "bass"))
  #plot(curve_data)
  #interpolation on a linear level
  interpolation <- approx(x = curve_data_t, y = curve_data, xout = seq(0,curve_data_t[length(curve_data_t)], 10000))
  plot(interpolation$x, interpolation$y)
  diffusion_parameters <- diffusion(interpolation$y, cumulative = FALSE, type = "bass")
  coefficient_innovation <- diffusion_parameters$w[1]
  coefficient_imitation <- diffusion_parameters$w[2]
  
  tmp <- c(url_article, coefficient_innovation, coefficient_imitation)
  coefficients_per_article <- dplyr::bind_rows(coefficients_per_article, as.data.frame(t(tmp)))
  
  
}

coefficients_per_article <- coefficients_per_article %>% 
  mutate(p = as.numeric(p),
         q = as.numeric(q))


coefficients_per_article_quiz <- coefficients_per_article %>% 
  mutate(type = "quiz")



ggplot(data = coefficients_per_article, aes(x = p, y = q))+
  geom_point()+
  xlim(c(0,.5))+
  ylim(c(0,.5))+
  geom_abline(slope = 1)



######################################
# Merging both datasets of quiz and tv/movie
######################################

#coefficients
buzzfeed_result_dataset <- dplyr::bind_rows(coefficients_per_article_quiz, coefficients_per_article_tv_movie)

ggplot(buzzfeed_result_dataset, aes(x = p, y = q, color = type))+
  geom_point()+
  xlim(c(0,.5))+
  ylim(c(0,.5))+
  geom_abline(slope = 1)





#diffusion curves
buzzfeed_article_db_merged <- dplyr::bind_rows(diffusion_model_dataset_quiz, diffusion_model_dataset_tv_movie) %>% 
  mutate(trending = ifelse(!is.null(views), TRUE, FALSE))

p <- ggplot(data = buzzfeed_article_db_merged, 
       aes(x = time_standard/60/60, y = comment_normalized, fill = url))+
  geom_line(alpha=0.5)+
  geom_point(alpha=0)+
  #geom_smooth(data = buzzfeed_article_db_merged,  aes(x = time_standard/60/60, y = comment_normalized), inherit.aes = FALSE)+
  theme(legend.position = "none")+
  xlab("Time [h]")+
  ylab("Standardized Number of Comments [0,1]")+
  xlim(c(0,60))
ggMarginal(p, type="histogram",  margins = 'x',)


# What is the average number of comments finally received?
mean((buzzfeed_article_db_merged %>% 
  group_by(url) %>% 
  summarise(max_views = mean(min_comments)))$max_views)





######################################
# Create a dataset for cross-media comparison
######################################




cross_media_comparison_data_buzzfeed <- buzzfeed_result_dataset %>% 
  mutate(url = V1) %>% 
  inner_join(buzzfeed_article_db_merged) %>% 
  distinct(url, .keep_all = TRUE) %>% 
  select(url, p, q, type, max_comments, min_t_0, max_t) 


save(cross_media_comparison_data_buzzfeed, file = "final_buzzfeed_dataset.RData")