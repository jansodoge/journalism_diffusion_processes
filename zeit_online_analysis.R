################################################################################
#What we did in the data analysis
# 1. Used the comments dataset to plot diffusion curves where we realized
#    some of the articles had comments marked with a timestamp before the publication
#    we figured out this being due to edits in the article not covered in the 
#    publication timestamo --> we drop all articles where this is the case
#    as these are likely biased
################################################################################




load("updated_zeit_comment_dataset.RData")
load("publication_dates_zeit.RData")
library(tidyverse)
library(stringr)
library(diffusion)




publication_dates_edited <- publication_dates %>%
  mutate(year = as.numeric(str_extract(V2, "[0-9]{4}")),
         month = as.numeric(gsub("-","", str_extract(V2, "-[0-9]{2}-"))),
         day =  as.numeric(gsub("T","", str_extract(V2, "[0-9]{2}T"))),
         hour = as.numeric(gsub("T","", str_extract(V2, "T[0-9]{2}"))),
         min = as.numeric(gsub(":","", str_extract(V2, ":[0-9]{2}:"))),

         article = V1) %>%
  mutate(date_iso_publication = as.numeric(ISOdate(year = year,
                                       month = month,
                                       day = day,
                                       hour  = hour,
                                       min = min,
                                       sec = 0))) %>%
  select(article, date_iso_publication)






analysis_dataset_zeit <- updated_zeit_comment_dataset  %>% 
  mutate(hour_digit = as.numeric(str_extract(hour, "[0-9]{1,2}")),
         min_raw = as.numeric(gsub(pattern = ":", replacement = "", x = str_extract(hour, ":[0-9]{1,2}"))),
         hour_min = hour_digit + min_raw) %>% 

  mutate(date_iso = as.numeric(ISOdate(year = year,
                                       month = as.numeric(month), 
                                       day = as.numeric(day), 
                                       hour = as.numeric(hour_digit),
                                       min = min_raw,
                                       sec = 0)),
         comment_count = 1) %>% 
  select(article, date_iso, comment_count, year) %>% 
  inner_join(publication_dates_edited, by = "article") 



print(paste("Dataset size before reducing comments that happend before the publication date", nrow(analysis_dataset_zeit)))


# Clean all comments before date of publication
analysis_dataset_zeit_cleaned <- analysis_dataset_zeit %>% 
  filter(date_iso > date_iso_publication) 
print(paste("Dataset size after reducing comments that happend before the publication date", nrow(analysis_dataset_zeit)))



# Clean all articles that had comments which deviate the publication date as t_0
analysis_dataset_zeit_messed <- analysis_dataset_zeit %>% 
  filter(date_iso < date_iso_publication) %>% 
  select(article)

analysis_dataset_zeit_cleaned <- analysis_dataset_zeit_cleaned %>% 
  anti_join(analysis_dataset_zeit_messed)

print(paste("Dataset size after reducing articles with comments that happend before the publication date", 
            nrow(analysis_dataset_zeit_cleaned)))


# now decide for one dataset version to continue with
analysis_dataset_zeit <- analysis_dataset_zeit_cleaned 






articles <- unique(as.vector(analysis_dataset_zeit$article))



cumulative_curve <- analysis_dataset_zeit %>%
  group_by(article) %>%
  arrange(date_iso) %>% 
  mutate(cum_comments = cumsum(comment_count))






################################################################################
#What we did in the data analysis
# 2. standardize the timeframes where the data of publication becomes t_0
# 3. use a timeframe of 60 hours for data analysis (i.e. 3 days) that enables comparison to Buzzfeed, too.
# 4. only pick articles with at least an increase of 0 --> 30 comments within these 3 days 
################################################################################

relevant_articles_t_standardized <- cumulative_curve %>% 
  group_by(article) %>% 
  summarise(min_t_0 = min(date_iso),
            max_t = max(date_iso))

cumulative_curve_standardized <- cumulative_curve %>% 
  inner_join(relevant_articles_t_standardized, by = "article") %>% 
  mutate(time_standard = (date_iso  - min_t_0))


articles_max_comments <- cumulative_curve_standardized %>% 
  filter(time_standard/60/60 < 60) %>% 
  group_by(article) %>% 
  summarise(max_comments = max(cum_comments)) %>% 
  mutate(eligible_article = ifelse(max_comments > 30, TRUE, FALSE))



articles_init_comment_density <- cumulative_curve_standardized %>% 
  filter(time_standard/60/60 < 10) %>% 
  summarise(init_max_comments = max(cum_comments)) %>% 
  select(article, init_max_comments)
  
            

cumulative_curve_standardized_cleaned <- cumulative_curve_standardized %>% 
  inner_join(articles_max_comments, by = "article") %>% 
  filter(eligible_article == TRUE) %>% 
  inner_join(articles_init_comment_density)
  

library(ggExtra)

p <- cumulative_curve_standardized_cleaned %>% 
  filter((time_standard/60/60) < 60) %>% 
  
  mutate(cum_comments_standardized  = (cum_comments - 0)/(max_comments-0)) %>% 
  ggplot(aes(x = time_standard/60/60, y = cum_comments_standardized, fill = article))+
  geom_line(alpha=0.2)+
  geom_point(alpha=0)+
  theme(legend.text = element_blank(),
        legend.position = "none")+
  
  xlab("Time [h]")+
  ylab("Standardized Number of Comments [0,1]")+
  xlim(c(0,60))

ggMarginal(p, type="histogram",  margins = 'x')



  



################################################################################
#What we did in the data analysis
# 5. we need to cover the issue of time lags which seem highly biased to us 
# in the context of the errors we covered in step 1. 
# thus for the beginning we drpp articles with larger time lags
################################################################################



################################################################################
# Let's fit diffusion curves
articles_modelling_data <- cumulative_curve_standardized_cleaned %>% 
  mutate(time_standard  = time_standard/60/60) %>% 
  filter(time_standard < 48) 


articles_to_be_removed <- articles_modelling_data %>% 
  mutate(previous_article_timestamp = abs(lag(time_standard)-time_standard)) %>% 
  filter(previous_article_timestamp > 4 & cum_comments < 10) %>% 
  select(article)

articles_modelling_data <- articles_modelling_data %>% 
  anti_join(articles_to_be_removed, by = "article")
 

articles_modelling <- unique(as.vector(articles_modelling_data$article))

articles_model_parameters <- data.frame()

for(model_article in articles_modelling){
  selected_article <- articles_modelling_data %>% 
    filter(article == model_article)
  curve_data_t <- as.vector(selected_article$time_standard)
  curve_data <- as.vector(selected_article$cum_comments)
  try(interpolation <- approx(x = curve_data_t, y = curve_data, xout = seq(0,curve_data_t[length(curve_data_t)], 1)))
  try(diffusion_parameters <- diffusion(interpolation$y, cumulative = TRUE, type = "bass"))
  coefficient_innovation <- diffusion_parameters$w[1]
  coefficient_imitation <- diffusion_parameters$w[2]
  articles_model_parameters <- dplyr::bind_rows(articles_model_parameters,
                                          data.frame(innovatin = coefficient_innovation,
                                                    imitation = coefficient_imitation,
                                                    ratio_bass = coefficient_imitation/coefficient_innovation,
                                                    article = selected_article$article[1]))

}




################################################################################
#create a dataset for analysis
################################################################################


article_meta_information <- cumulative_curve_standardized_cleaned %>% 
  distinct(article, .keep_all = TRUE)
articles_model_parameters_beta <- articles_model_parameters %>% 
  left_join(article_meta_information)






################################################################################
#Visual and descriptive statistics investigation of the dataset
# a) Looking at the first boxplot, we observe the significant impact of the outliers 
#    as within the bass ratio --> higher importance of imitation than innovation
#    these are increasingly taking place for later years
# b) zooming in to the center points of the boxplots in the second Fig.
#    we see this trend, too, but that the actual median values differ not too much
# c) This trend is more detailed depicted in Fig. 3 where the median value 
#    of 2011 and 2012 are actually higher than those for 2018/2019,
#    it is the mean values that, given they are more vulnerable to outliers
#    shift significantly and explain the coefficients we later observe in the the
#    regression model

# --> there comes a trade-off of these outliers resulting of curves that do not 
#     diffuse in the beginning but then start doing so later
#     we actually filtered out some of these super extreme cases earlier using the
#     time-difference (lag) approach, but at some point these cases will take place
#     where diffusion takes place just after a while (i.e. endogenous diffusion we are looking for)
#     and it shows to be a relevant result that these diffusions rather and primarily take place in 
#     later years

################################################################################
ggplot(data = articles_model_parameters_beta, aes(x = as.factor(year), y= ratio_bass, fill = as.factor(year)))+
  geom_boxplot()+
  theme(legend.position = "none")+
  ylab("Ratio Endogenous to Exogenous Diffusion")+
  xlab("Year")


ggplot(data = articles_model_parameters_beta, aes(x = as.factor(year), y= ratio_bass, fill = as.factor(year)))+
  geom_boxplot()+
  ylim(c(0, 20))+
  theme(legend.position = "none")+
  ylab("Ratio Endogenous to Exogenous Diffusion")+
  xlab("Year")



articles_model_parameters_beta %>% 
  group_by(year) %>% 
  summarise(median_per_year = median(ratio_bass),
            mean_per_year  = mean(ratio_bass),
            standard_deviation = sd(ratio_bass, na.rm = TRUE)) %>% 
  pivot_longer(!year, names_to = "type", values_to = "value") %>% 
  ggplot(aes(x = year, y = value, color = type))+
  geom_line()+
  geom_point()






################################################################################
# Linear regression model analysis
################################################################################
library(stargazer)



# A first model using only the years shows a significant impact of the year
# in predicting an increased ratio --> the endogenous diffusion becomes more important
# nevertheless these findings need to be interpreted in the context of the outliers
# visualized previously that heavily impact the regression design here
# also notice the very low R-square

stargazer(lm(articles_model_parameters_beta$ratio_bass 
           ~ articles_model_parameters_beta$year),
          
          lm(articles_model_parameters_beta$ratio_bass 
             ~ articles_model_parameters_beta$year + 
               articles_model_parameters_beta$max_comments),
          lm(articles_model_parameters_beta$ratio_bass 
             ~ articles_model_parameters_beta$year + 
               articles_model_parameters_beta$max_comments + 
               articles_model_parameters_beta$init_max_comments)
          
          
          )




#categorizing early and late years
articles_model_parameters_beta_discrete <- articles_model_parameters_beta


articles_model_parameters_beta_discrete[articles_model_parameters_beta_discrete==2011]<- "early"
articles_model_parameters_beta_discrete[articles_model_parameters_beta_discrete==2012]<- "early"

articles_model_parameters_beta_discrete[articles_model_parameters_beta_discrete==2014]<- "mid"
articles_model_parameters_beta_discrete[articles_model_parameters_beta_discrete==2015]<- "mid"


articles_model_parameters_beta_discrete[articles_model_parameters_beta_discrete==2017]<- "late"
articles_model_parameters_beta_discrete[articles_model_parameters_beta_discrete==2018]<- "late"

stargazer(lm(articles_model_parameters_beta_discrete$ratio_bass 
           ~ articles_model_parameters_beta_discrete$year),

lm(articles_model_parameters_beta_discrete$ratio_bass 
           ~ articles_model_parameters_beta_discrete$year + 
             articles_model_parameters_beta_discrete$max_comments),







lm(articles_model_parameters_beta_discrete_daytimecontrol$ratio_bass 
           ~ articles_model_parameters_beta_discrete_daytimecontrol$year + 
             articles_model_parameters_beta_discrete_daytimecontrol$max_comments + 
             articles_model_parameters_beta_discrete_daytimecontrol$daytime))












# A second model checks the impact of the number of comments the article
# overall collected (max_comments)


summary(lm(articles_model_parameters_beta$ratio_bass 
           ~ articles_model_parameters_beta$year + 
             articles_model_parameters_beta$max_comments))



# Third and fourth model that introduce a control to check on late diffusers using the init_max_comments

summary(lm(articles_model_parameters_beta$ratio_bass 
           ~ articles_model_parameters_beta$year + 
             articles_model_parameters_beta$max_comments + 
             articles_model_parameters_beta$init_max_comments))




summary(lm(articles_model_parameters_beta$ratio_bass 
           ~ articles_model_parameters_beta$year + 
            
             articles_model_parameters_beta$init_max_comments))








################################################################################
# Buzzfeed
load("final_buzzfeed_dataset.RData")

################################################################################




cross_media_comparison_data_buzzfeed <- cross_media_comparison_data_buzzfeed %>% 
  mutate(innovation = p,
         imitation = q,
         media = "buzzfeed",
         article = url) %>% 
  mutate(ratio_bass = innovation / imitation,
        ) %>% 
  select(article, innovation, imitation, ratio_bass,max_comments, media)




cross_media_comparison_data_zeit <- articles_model_parameters_beta %>% 
  mutate(innovation = innovatin,
         media = "zeit") %>% 
  select(article, innovation, imitation, ratio_bass,max_comments, media)


cross_media_comparison <- dplyr::bind_rows(cross_media_comparison_data_zeit, 
                                           cross_media_comparison_data_buzzfeed)


ggplot(data = cross_media_comparison, aes(y = imitation, x  = as.factor(media), fill = as.factor(media)))+
  geom_boxplot(alpha = .8)+
  ylim(c(0,1))+
  xlab("Media Outlet")+
  ylab("Bass Model Coefficient for Imitation/Endogenous Diffusion")+
  guides(fill=guide_legend(title="Media Outlet"))




###t-test for first hypothesis
with(cross_media_comparison, shapiro.test(imitation[media == "buzzfeed"]))
with(cross_media_comparison, shapiro.test(imitation[media == "zeit"]))# p = 0.1

var.test(imitation ~ media, data = cross_media_comparison)

t.test(imitation ~ media, data = cross_media_comparison, var.equal = TRUE)





ggplot(data = cross_media_comparison, aes(y = imitation, x  = as.factor(media), fill = as.factor(media)))+
  geom_boxplot()



cross_media_comparison %>% 
  group_by(media) %>% 
  summarise(median_imitation = median(imitation),
            mean_imitation = mean(imitation))


ggplot(data = cross_media_comparison, aes(y = ratio_bass, x  = as.factor(media), fill = as.factor(media)))+
  geom_boxplot()

  
         

################################################################################
#Robustness Check

sample_model <- articles_model_parameters_beta_discrete_daytimecontrol


#we use 70%
model_robustness <- data.frame()
for(index in seq(1,100)){

sample <- sample_model[sample(nrow(sample_model), 
                                                         nrow(sample_model)*0.8), ]


model_results <- as.data.frame(summary(lm(sample_model$ratio_bass 
                                          ~ sample_model$year + 
                                            sample_model$max_comments + 
                                            sample_model$daytime))$coefficients)
model_results$parameter <- rownames(as.data.frame(summary(lm(sample_model$ratio_bass 
                                                             ~ sample_model$year + 
                                                               sample_model$max_comments + 
                                                               sample_model$daytime))$coefficients))

model_robustness <- dplyr::bind_rows(model_robustness, model_results)



}


model_robustness %>% 
  mutate(significant = ifelse(`Pr(>|t|)` < 0.1, TRUE, FALSE)) %>% 
  group_by(significant, parameter) %>% 
  summarise(n = n())





################################################################################
# Adding a model to control for publication time


articles_daytime <- publication_dates %>%
  mutate(year = as.numeric(str_extract(V2, "[0-9]{4}")),
         month = as.numeric(gsub("-","", str_extract(V2, "-[0-9]{2}-"))),
         day =  as.numeric(gsub("T","", str_extract(V2, "[0-9]{2}T"))),
         hour = as.numeric(gsub("T","", str_extract(V2, "T[0-9]{2}"))),
         min = as.numeric(gsub(":","", str_extract(V2, ":[0-9]{2}:"))),
         
         article = V1) %>% 
  mutate(daytime = ifelse(hour < 8 | hour > 22, FALSE, TRUE)) %>% 
  select(article, daytime)



articles_model_parameters_beta_discrete_daytimecontrol <- articles_model_parameters_beta_discrete %>% 
  left_join(articles_daytime)
  




stargazer(lm(articles_model_parameters_beta_discrete_daytimecontrol$ratio_bass 
             ~ articles_model_parameters_beta_discrete_daytimecontrol$year),
          
          lm(articles_model_parameters_beta_discrete_daytimecontrol$ratio_bass 
             ~ articles_model_parameters_beta_discrete_daytimecontrol$year + 
               articles_model_parameters_beta_discrete_daytimecontrol$max_comments),
          
          
          
          
          
          
          
          lm(articles_model_parameters_beta_discrete_daytimecontrol$ratio_bass 
             ~ articles_model_parameters_beta_discrete_daytimecontrol$year + 
               articles_model_parameters_beta_discrete_daytimecontrol$max_comments + 
               articles_model_parameters_beta_discrete_daytimecontrol$daytime))





#we use 70%
model_robustness <- data.frame()
for(index in seq(1,10000)){
  
  sample <- articles_model_parameters_beta_discrete_daytimecontrol[sample(nrow(articles_model_parameters_beta_discrete_daytimecontrol), 
                                nrow(articles_model_parameters_beta_discrete_daytimecontrol)*0.8), ]
  
  
  model_results <- as.data.frame(summary(lm(sample$ratio_bass 
                                            ~ sample$year + 
                                              sample$max_comments + 
                                              sample$daytime))$coefficients)
  model_results$parameter <- rownames(as.data.frame(summary(lm(sample$ratio_bass 
                                                               ~ sample$year + 
                                                                 sample$max_comments + 
                                                                 sample$daytime))$coefficients))
  
  model_robustness <- dplyr::bind_rows(model_robustness, model_results)
  
  
  
}


model_robustness %>% 
  mutate(significant = ifelse(`Pr(>|t|)` < 0.1, TRUE, FALSE)) %>% 
  group_by(significant, parameter) %>% 
  summarise(n = n())







# Doing the hypothesis one test w/o nighttime outliers


cross_media_comparison_data_zeit <- articles_model_parameters_beta_discrete_daytimecontrol %>% 
  filter(daytime == TRUE) %>% 
  mutate(innovation = innovatin,
         media = "zeit") %>% 
  select(article, innovation, imitation, ratio_bass,max_comments, media)


cross_media_comparison <- dplyr::bind_rows(cross_media_comparison_data_zeit, 
                                           cross_media_comparison_data_buzzfeed)




with(cross_media_comparison, shapiro.test(imitation[media == "buzzfeed"]))
with(cross_media_comparison, shapiro.test(imitation[media == "zeit"]))# p = 0.1

var.test(imitation ~ media, data = cross_media_comparison)

t.test(imitation ~ media, data = cross_media_comparison, var.equal = TRUE)
