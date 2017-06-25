# Dimitri Kourouniotis March 2017
# Rotten Tomatoes has a critics section
# This R script will create 26 URLs (one for each letter of the alphabet) 
# and scrape through to collect the names of each reviewer.
# The code will then be cleaned up to convert each name to lowercase and remove 
# additional characters

 
#####-------------------------------------------------------------
# create vector of web pages for each letter of the alphabet
# save as critic_master_list_names
#####
library(rvest)
library(stringr)

critics_url <- "https://www.rottentomatoes.com/critics/authors?letter="
url_vectors <- paste(critics_url,paste(letters),sep="")

# list of critic authors webpages by last name catalog
# EXAMPLE : critic_page <- read_html("https://www.rottentomatoes.com/critics/authors?letter=a")
# initialize master list of critics
master_list <- c()
for (i in url_vectors) {
  # print (i)  
  critic_page <- read_html(i)
  critic_list <- critic_page %>%
    html_nodes(".critic-names > a") %>%
    html_text()
  #convert to lower case and remove initials' periods
  critic_list <- tolower(critic_list) 
  critic_list <- str_replace_all(critic_list, pattern= "\\.", repl="")
  critic_list <- str_replace_all(critic_list, pattern= "\\'", repl="")
  critic_list <- str_replace_all(critic_list, pattern= "\"", repl="")
  critic_list <- str_replace_all(critic_list, pattern=" ", repl="-")
  #     url of alphabetized critics pages
  #     appending to master list
  master_list <- c(master_list, critic_list)
}
# save as CSV file
write.csv(master_list, file = "critics_master_list_names.csv")


# vector of URLs of each critic/author on Rotten Tomatoes
critics_review_page <- paste("https://www.rottentomatoes.com/critic/", paste(master_list) , "/", sep="")


write.csv(critics_review_page, file = "critic_authors_urls.csv")
write.csv(page_count_vector, file = "page_counts.csv")
#create data set of movie titles and reviews
#detect number of pages of reviews



critics_ds <- data.frame(critics_review_page)

#### corrected for english characters in Excel



#####
# read and collate number of pages of reviews from each critic (ex: showing 1 to 50 of 300) 
#####------------------------------------------ 

page_count <- c()
page_count_vector <- c()
i <- 0
for ( j in critics_review_page){
  i <- i+1  
  page_count = tryCatch({
    page_count_url <- read_html(j )
    page_count <- page_count_url %>%
      html_nodes(".medium+ .pull-right .disabled:nth-child(3) a") %>%
      html_text()
    critics_ds[i,2] <- page_count 

  }, warning = function(wcondition) {
    paste("warning on", j, sep = " ")
    critics_ds[i,2] <- "warning"
  }, error = function(errorcondition) {
    paste("error on", j, sep = " ")
    critics_ds[i,2] <- "error"
  }#, finally={
  #critics_ds[i,2] <- page_count 
  #}
  )
}

write.csv(critics_ds, file = "critics_ds.csv")

#---collect reviews list

#####
# load critics_ds.csv into data frame
# with readr
install.packages("readr")
library(readr)

critics_list_dataset <- read_csv("critics_ds_corrected_for_enlish.csv")
View(critics_list_dataset)

#####---- create list
critics_page_lists <- c()
no_of_reviews <- c(critics_list_dataset$pages_of_reviews)
print(no_of_reviews)



#####----- nested loop to create extended vector of each critic urls = 12582

#####

#####----- initialize critic review url list
critic_review_urls <-c()
critic_name <- c()
total_reviews <-c()

for (j in 1: dim(critics_list_dataset)[1]) {
  for (k in 1:critics_list_dataset[j,4]) {
    
    #print(paste(critics_list_dataset[j,1],"?page=",k, sep=""))
    critic_review_urls <- c( critic_review_urls, paste(critics_list_dataset[j,1],"?page=",k, sep=""))
    critic_name <- c(critic_name,critics_list_dataset[j,2] )
    total_reviews <- c(total_reviews,critics_list_dataset[j,3])
  }
}

#### ---- dataset of critic urls, critic names and total reviews
critics_urls_ds <- data.frame(critic_review_urls, critic_name, total_reviews)
write.csv(critics_urls_ds , file = "critics_urls_ds.csv")
#####


#### ---- write critic urls list to csv
write.csv(critic_review_urls , file = "critic_review_urls.csv")









#### footnote: 
####----- convert accented characters
#### "\xf1 -> "n" https://www.rottentomatoes.com/critic/michael-ordo\xf1a/?page=1
#### "\xed -> "i" https://www.rottentomatoes.com/critic/marcos-gand\xeda/?page=1
#### "\xe1 -> "a" https://www.rottentomatoes.com/critic/mariana-fern\xe1ndez/?page=1
#### "\xf3 -> "o" https://www.rottentomatoes.com/critic/carlos-mara\xf1\xf3n/?page=1
#### "\xe9 -> "e" https://www.rottentomatoes.com/critic/virginie-s\xe9lavy/?page=1
#### "\xfa -> "u" https://www.rottentomatoes.com/critic/jes\xfas-chavarria/?page=1
#### "\xe7 -> "c" https://www.rottentomatoes.com/critic/jc-ma\xe7ek-iii/?page=2
