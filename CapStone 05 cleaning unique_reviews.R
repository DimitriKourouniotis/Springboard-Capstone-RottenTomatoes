# cleaning up the dataset of Movie Critics reviews "unique_reviews" & 
# cleaning up Movie table of clean_tMeterScore_ds

unique_reviews <- readRDS("unique_reviews.RDS")
view(unique_reviews)

## remove '-' from publication_2 variable
test_publication_2 <- unique_reviews$publication_2
pub <- sapply(strsplit(test_publication_2, split='&dash; ',fixed=TRUE), function(x) (x[2]))
unique_reviews$publication_2 <- pub

## remove 'Read More| Posted ' &  'Posted ' string from posted_date_1 variable
pdate <- unique_reviews$posted_date_1
pdate2 <- sapply(strsplit(pdate, split='Posted ',fixed=TRUE), function(x) (x[2]))
unique_reviews$posted_date_1 <- pdate2

## clean up text summary by removing all links etc after the &dash
unique_reviews$review_text_summary_1 <- sapply(strsplit(unique_reviews$review_text_summary_1, 
                                                        split='&dash; ',fixed=TRUE), function(x) (x[1]))


#cleanup and split dataset: "clean_tMeterScore_ds"
workingset <- clean_tMeterScore_ds$movie_title_and_year_1

# remove and separate '1900s and 2000s within () but retain translated titles also appearing within ()
#collect 1900's movies
year <- sapply(strsplit(workingset, split=' (1',fixed=TRUE), function(x) (x[2]))
year_1900s <- ifelse(!is.na(year),paste("1",year,sep=""),NA)
year_1900s <- sapply(strsplit(year_1900s, split=')',fixed=TRUE), function(x) (x[1]))  

#collect 2000's movies
year <- sapply(strsplit(workingset, split=' (2',fixed=TRUE), function(x) (x[2]))
year_2000s <- ifelse(!is.na(year),paste("2",year,sep=""),NA)
year_2000s <- sapply(strsplit(year_2000s, split=')',fixed=TRUE), function(x) (x[1]))  

#combine the movie years 
clean_tMeterScore_ds$movie_year <- pmin(year_1900s, year_2000s, na.rm=TRUE)

#strip off years to leave behind movie title including subtitles/renames in "()" by first 
# removing movies dated 1900's and then movies dated 2000's
movie_title_2000 <- sapply(strsplit(workingset, split=' (2',fixed=TRUE), function(x) (x[1]))
clean_tMeterScore_ds$movie_title_allyears <- sapply(strsplit(movie_title_2000, 
                                                             split=' (1',fixed=TRUE), function(x) (x[1]))
