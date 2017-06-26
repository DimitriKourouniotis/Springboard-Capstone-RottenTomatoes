### read rotten tomatoes table : review, Title , review date posted
### webscrape >12,000 URLS
### needs to be done over several iterations 
### 1. some web pages were broken or being updated with reviews and the webscraping exited. I was never able to make an error free run 
### 2. CSV size limit (I did not know about saving as RDS file)
### 3. The whole data collection for 12,000 URLs took about 11 hours and creates 600,000 records

install.packages("rvest")
library(rvest)

install.packages("readr")
library(readr)

critics_list_dataset <- read_csv("critics_urls_pages.csv", header=TRUE, stringAsFactors= FALSE)
View(critics_urls_pages)

## Testing small reading dataset
critics_urls_pages <-c()
critics_urls_pages<- read_csv("critics_urls_ds_corrected_english.csv")
View(critics_urls_pages)

#initialize vectors
list_of_review <-c()
movie_title_1 <-c()
review_score_1 <-c()
posted_date_1 <-c()
publication_1 <-c()
review_text_summary_1 <-c()
url_1 <-c()
critic_name_1 <-c()


for (j in 1:nrow(critics_urls_pages)) {
  ##for t-meter score
  
  movie_review <- read_html(as.character(critics_urls_pages[j,2]))
  print(critics_urls_pages[j,2])

  tMeterScore <- movie_review %>%
    html_nodes(".tMeterScore") %>%
    html_text()
  
  movie_title <- movie_review %>%
    html_nodes(".movie-link") %>%
    html_text()

  movie_title_1 <-c(movie_title_1, movie_title)
  tMeterScore_1 <-c(tMeterScore_1, tMeterScore)
}


### 12659 urls of critics reviews
for (j in 1:nrow(critics_urls_pages)) {
##for (j in 1:250) {
    
    movie_review <- read_html(as.character(critics_urls_pages[j,2]))
print(critics_urls_pages[j,2])
    data_from_page <- movie_review %>%
      html_nodes(".small , .movie-link , #criticsReviewsChart_main td:nth-child(1)") %>%
      html_text()
 
    posted_date <- movie_review %>%
      html_nodes(".small") %>%
      html_text()
    
    movie_title <- movie_review %>%
      html_nodes(".movie-link") %>%
      html_text()
    
    review_score <- movie_review %>%
      html_nodes("#criticsReviewsChart_main td:nth-child(1)") %>%
      html_text()
    
    publication <- movie_review %>%
      html_nodes("#criticsReviewsChart_main .articleLink") %>%
      html_text()
   ## print(pubication)
    
    review_text_summary <- movie_review %>%
      html_nodes(".center+ td") %>%
      html_text()
    
    critic_name_1 <- c(critic_name_1,rep(as.character(critics_urls_pages[j,3]),length(movie_title)))    
    url_1 <- c(url_1,rep(as.character(critics_urls_pages[j,2]),length(movie_title)))
    
    movie_title_1 <-c(movie_title_1, movie_title)
    review_score_1 <-c(review_score_1, review_score)
    posted_date_1 <-c(posted_date_1, posted_date)
    publication_1 <-c(publication_1, publication)
    review_text_summary_1 <-c(review_text_summary_1, review_text_summary)
    
    ##print(movie_title_1)
#### - - - "contains publication", "read more and "posted date"
    ##movie_title <-c()
}


list_of_review8 <- data.frame(
            movie_title_1, 
            critic_name_1,
            review_score_1, 
            posted_date_1, 
            publication_1, 
            review_text_summary_1,
            url_1)
View(list_of_review8)
write.csv(list_of_review8, file = "list_of_review8.csv")
str(movie_title_1)
str(critic_name_1)
str(review_score_1)
str(posted_date_1)
str(publication_1)
str(review_text_summary_1)
str(url_1)

###save as
##write.csv(list_of_review4, file = "list_of_review4.csv")

publication_2 <- c(publication_1, NA)
View(url_1)
str(publication_2)


#### CLEAN UP
unique_reviews_2 <- unique_reviews_1[!is.na(unique_reviews_1$clean_score1),]
