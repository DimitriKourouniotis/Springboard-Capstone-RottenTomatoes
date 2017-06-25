### read rotten tomatoes table : review, Title , review date posted

### .small , .movie-link , #criticsReviewsChart_main td:nth-child(1)
install.packages("knitr")
install.packages("rvest")
library(rvest)

install.packages("readr")
library(readr)

critics_list_dataset <- read_csv("critics_urls_pages.csv", header=TRUE, stringAsFactors= FALSE)
View(critics_urls_pages)


#initialize vectors
movie_title_1 <-c()
review_score_1 <-c()
posted_date_1 <-c()
publication_1 <-c()
review_text_summary_1 <-c()
critic_name_1 <-c()
critic_reviews_totals_1 <-c()
nrow(critics_urls_pages)

View(critics_urls_pages)
for (j in 9829:nrow(critics_urls_pages)) {
  critic_name_1 <- c(critic_name_1,as.character(critics_urls_pages[j,2]))
  critic_reviews_totals_1 <- c(critic_reviews_totals_1, as.numeric(as.character(critics_urls_pages[j,3])))
}

column2 <- data.frame(
  critic_name_1, 
  critic_reviews_totals_1)
View(column2)


View(critic_name_1)
str(critic_name_1)
str(critic_reviews_totals_1)

### 12659 urls of critics reviews
for (j in 9829:nrow(critics_urls_pages)) {

    ##print(critics_list_dataset[j,1])
    movie_review <- read_html(as.character(critics_urls_pages[j,1]))



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
    
    critic_name <- as.character(critics_urls_pages[j,2])
    
    critic_reviews_totals_1 <- c(critic_reviews_totals_1, as.numeric(as.character(critics_urls_pages[j,3])))
    
    movie_title_1 <-c(movie_title_1, movie_title)
    review_score_1 <-c(review_score_1, review_score)
    posted_date_1 <-c(posted_date_1, posted_date)
    publication_1 <-c(publication_1, publication)
    review_text_summary_1 <-c(review_text_summary_1, review_text_summary)
#### - - - "contains publication", "read more and "posted date"
}

list_of_review2 <-c()
list_of_review2 <- data.frame(
            movie_title_1, 
            review_score_1, 
            posted_date_1, 
            publication_1, 
            review_text_summary_1,
            critic_name_1, 
            critic_reviews_totals_1)

##review
movie_title_1
review_score_1 
posted_date_1 
publication_1 
review_text_summary_1
critic_name_1 
critic_reviews_totals_1


View(list_of_review2)
str(critic_name_1)

###save as
write.csv(list_of_review2, file = "list_of_review2.csv")



large_review <- read_csv("large_review.csv")
View(large_review)

