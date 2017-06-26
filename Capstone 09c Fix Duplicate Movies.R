##
## some movies are in here twice.. some movies have the same title
## and some movies are in the IMDB CSV file more than once.
## 


# correct main database to have movie_year as integer
tMeter_IMDB_Oscars$movie_year <- as.integer(tMeter_IMDB_Oscars$movie_year)
View(tMeter_IMDB_Oscars)

# collect the movie basic unique information to use as Lookup Keys
movie.title.year <- unique(subset(tMeter_IMDB_Oscars, select = c("movie_title","movie_year","tMeter_num")))
View(movie.title.year)

# group by movie title to isolate duplicates
movie.title.year.duplicates <- movie.title.year %>%
  group_by(movie_title) %>%
  summarize (Total = n()) %>%
  arrange(desc(Total))

movie.title.year.duplicates <- ddply(movie.title.year, .(movie_title), summarise, 
                                     count = length (movie_title))


movie.title.year.duplicates <- movie.title.year.duplicates %>%
  filter (count > 1)
View(movie.title.year.duplicates)


tMeter_IMDB_Oscars <- tMeter_IMDB_Oscars %>%
  left_join(movie.title.year.duplicates, by = "movie_title")
View(tMeter_IMDB_Oscars)

# there were 100 duplicate entries in the IMDB cvs file!
IMDB_96plus_genres.unique <- unique(IMDB_96plus_genres)
tMeter_IMDB_Oscars.duplicates <- tMeter_IMDB_Oscars %>%
  filter(count > 1)
View(tMeter_IMDB_Oscars.duplicates)
View(IMDB_96plus_genres.unique)


# movie_title, title_year - create lookup to find applicable and removable duplicates.

IMDB_96plus_genres.unique.lookup <- IMDB_96plus_genres.unique[c("movie_title","title_year")]
View(IMDB_96plus_genres.unique.lookup)
IMDB_96plus_genres.unique.lookup$IMDB_Only <- 1


# Would the real movie release date please stand up
# this join associates the IMDB only title_year field with the duplicates to identify the 
# movie_title that will be kept
movie.title.year.duplicates <- movie.title.year.duplicates %>%
  left_join(IMDB_96plus_genres.unique.lookup, by = "movie_title")
View(movie.title.year.duplicates)



#################################
#inspect elements of duplicate movies
# enter the movie title one at a time:
suspectmovie = "Labor Day"

######
# select the IMDB correct year of movie
movie_correct <- movie.title.year.duplicates %>%
  filter(movie_title == suspectmovie)
# create a ds of all movie years including wrong ones
movie_wrong <- movie.title.year %>%
  filter(movie_title == movie_correct$movie_title)
# isolate the incorrect movie year
movie_delete <- movie_wrong %>%
  filter(movie_year != movie_correct$title_year)

# View(movie_delete)
# Eden - 2015
# repair dataset - removing wrong pictures(not in IMDB usually and wrong year)
tMeter_IMDB_Oscars <- tMeter_IMDB_Oscars %>%
  filter(movie_title_and_year_1 != paste(suspectmovie, " (", movie_delete$movie_year, ")", sep = ""))

# Or Manually remove when there is a need, such as incorrect year in a dataset.
tMeter_IMDB_Oscars <- tMeter_IMDB_Oscars %>%
  filter(movie_title_and_year_1 != paste(suspectmovie, "(2009)"))



#confirm deletion of dupliate
lookat_dup_movies <-tMeter_IMDB_Oscars %>%
  filter(movie_title == suspectmovie)
View(lookat_dup_movies)
######

saveRDS(tMeter_IMDB_Oscars,"tMeter_IMDB_Oscars.RDS")

## remove duplicates from Training dataset
library(dplyr)
RT.train <- RT.train %>% 
  filter(movie_title_and_year_1 != paste("Dark Water", " (", 2002, ")", sep = "")) #remove "Dark Water (2002)"

RT.train <- RT.train %>% 
  filter(movie_title_and_year_1 != paste("The Divide", " (", 2016, ")", sep = "")) #remove "The Divide (2016)"

RT.train <- RT.train %>% 
  filter(movie_title_and_year_1 != paste("What Women Want", " (", 2011, ")", sep = "")) #remove "What Women Want (2011)"


RT.train$removeTrust <- paste(RT.train$movie_title,RT.train$tMeter_num)
RT.train <- RT.train %>% 
  filter(removeTrust != "Trust 83") #remove "Trust" not directed by David Schwimmer
RT.train$removeTrust <- NULL
