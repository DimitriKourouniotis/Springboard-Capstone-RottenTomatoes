## Deconstructing Oscars to get no 2015 or 2016 data

# create lookup of oscars data to match title to year

# split into 2 groups
# movie_year is Rotten Tomatoes data
# title_year is IMDB database (with matching data from Genres)
# which is a more accurate dataset?
#
# separate the differences


release_years_different <- unique(tMeter_IMDB_Oscars [,c("movie_title","movie_year","title_year")])



release_years_different <- release_years_different %>%
  filter(movie_year != title_year) 

View(release_years_different)  
#482 movies have different release dates in IMDB dataset from RT webscrapped dataset

# create train and test datasets
RT.train <- tMeter_IMDB_Oscars %>%
  filter(title_year <= 2013)

RT.test <- tMeter_IMDB_Oscars %>%
  filter(title_year >= 2014)
View (RT.test)
#The Goal is to remove Oscar cumulative oscars information
# from Train data set for Oscars that appear in Test data

detach("package:plyr", unload=TRUE)

RT.train.titles <- RT.train %>%
  group_by(movie_title) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
View(RT.train.titles)

#2921 Movies in Train dataset

RT.test.titles <- RT.test %>%
  group_by(movie_title) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
View(RT.test.titles)

#397 Movies in Test dataset

# isolate movies and cast that have nominations in test to remove them from train 
RT.test.director <- RT.test %>%
  filter(!is.na(nom_director)) %>%
  group_by(director_name, movie_title) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
View(RT.test.director)






RT.test.actor_2 <- RT.test %>%
  filter(!is.na(nom_actor_2)) %>%
  group_by(actor_2_name, movie_title) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
View(RT.test.actor_2)

RT.test.actor_3 <- RT.test %>%
  filter(!is.na(nom_actor_3)) %>%
  group_by(actor_3_name, movie_title) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
View(RT.test.actor_3)

# Find Directors in : 
#      RT.test.director (Directors with Oscar Nominations)
# and  RT.train.director

extraOscars_Directors <- RT.test.director %>%
  semi_join(RT.train.director, by = "director_name")
extraOscars_Directors

# get number of extra accolades/oscar nominations (films) in test dataset
extraOscars_count <- count (extraOscars_Directors)
extraOscars_count$n

RT.test.titles
View(RT.test.titles)

RT.train <- RT.train %>% 
  mutate(noms_director_career = ifelse(director_name == extraOscars_count$director_name,
                                       noms_director_career - extraOscars_count$n,
                                       noms_director_career)) %>%
  mutate(cast_director_osc = ifelse(director_name == extraOscars_count$director_name,
                                    cast_director_osc - extraOscars_count$n,
                                    cast_director_osc))
View(RT.train)
saveRDS(RT.train, "RT_train.RDS")
saveRDS(RT.test, "RT_test.RDS")

# test for Actor 1
extraOscars_actors <- RT.test.actor_1 %>%
  semi_join(RT.train.actor_1, by = "actor_1_name")
extraOscars_actors

# get number of extra accolades/oscar nominations (films) in test dataset
extraOscars_count <- count (extraOscars_Directors)
extraOscars_count$n

## ACTORS in TEST role of Actor_1
RT.test.actor_1 <- RT.test %>%
  filter(!is.na(nom_actor_1)) %>%
  group_by(actor_1_name, movie_title) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
View(RT.test.actor_1)

RT.train.actor_1 <- RT.train %>%
  filter(!is.na(nom_actor_1)) %>%
  group_by(actor_1_name, movie_title) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
View(RT.train.actor_1)


# Find actors in : 
#      RT.test.actor_1 (Directors with Oscar Nominations)
# and  RT.train.actor_1

extraOscars_actor_1 <- RT.test.actor_1 %>%
  semi_join(RT.train.actor_1, by = "actor_1_name")
extraOscars_actor_1

# get number of extra accolades/oscar nominations (films) in test dataset
extraOscars_count <- count (extraOscars_actor_1)
extraOscars_count$n

# Loop through persons of interest

extraOscars_actor_1 <- as.data.frame(extraOscars_actor_1)
extraOscars_actor_1$actor_1_name <- as.character(extraOscars_actor_1$actor_1_name)

##add extras:


extraOscarsactors2 <- c("Bradley Cooper","J.K. Simmons","Benedict Cumberbatch","Sylvester Stallone","Eddie Redmayne")


sampleTrain <- RT.train

for (i in 1:5) {
#  actor_name <- extraOscars_actor_1[i,1]
actor_name <- extraOscarsactors2[i]
RT.train <- RT.train %>%
 #sampleTrain <- sampleTrain %>%
  mutate(noms_actor_1_career = ifelse(actor_1_name == actor_name,
                                       noms_actor_1_career - 1,
                                       noms_actor_1_career)) %>%
  mutate(noms_actor_2_career = ifelse(actor_2_name == actor_name,
                                      noms_actor_2_career - 1,
                                      noms_actor_2_career)) %>%
  mutate(noms_actor_3_career = ifelse(actor_3_name == actor_name,
                                      noms_actor_3_career - 1,
                                      noms_actor_3_career)) %>%
    mutate(cast_director_osc = ifelse(actor_1_name == actor_name,
                                    cast_director_osc - 1,
                                    cast_director_osc))
}


View(RT.train)

## ACTORS in TEST role of Actor_2
RT.test.actor_2 <- RT.test %>%
  filter(!is.na(nom_actor_1)) %>%
  group_by(actor_2_name, movie_title) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
View(RT.test.actor_2)

RT.train.actor_2 <- RT.train %>%
  filter(!is.na(nom_actor_2)) %>%
  group_by(actor_2_name, movie_title) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
View(RT.train.actor_2)

# Find actors in : 
#      RT.test.actor_2 (ACtors with Oscar Nominations)
# and  RT.train.actor_2

extraOscars_actor_2 <- RT.test.actor_1 %>%
  semi_join(RT.train.actor_1, by = "actor_1_name")
extraOscars_actor_2

# get number of extra accolades/oscar nominations (films) in test dataset
extraOscars_count <- count (extraOscars_actor_1)
extraOscars_count$n

# Loop through persons of interest

RT.test.actor_2 <- as.data.frame(RT.test.actor_2)
RT.test.actor_2$actor_2_name <- as.character(RT.test.actor_2$actor_2_name)

for (i in 1:10) {
  #  actor_name <- extraOscars_actor_1[i,1]
  actor_name <- RT.test.actor_2[i,1]
 RT.train <- RT.train %>%
#sampleTrain <- sampleTrain %>%
    mutate(noms_actor_1_career = ifelse(actor_1_name == actor_name,
                                        noms_actor_1_career - 1,
                                        noms_actor_1_career)) %>%
    mutate(noms_actor_2_career = ifelse(actor_2_name == actor_name,
                                        noms_actor_2_career - 1,
                                        noms_actor_2_career)) %>%
    mutate(noms_actor_3_career = ifelse(actor_3_name == actor_name,
                                        noms_actor_3_career - 1,
                                        noms_actor_3_career)) %>%
    mutate(cast_director_osc = ifelse(actor_1_name == actor_name,
                                      cast_director_osc - 1,
                                      cast_director_osc)) %>%
    mutate(cast_director_osc = ifelse(actor_2_name == actor_name,
                                      cast_director_osc - 1,
                                      cast_director_osc)) %>%
    mutate(cast_director_osc = ifelse(actor_3_name == actor_name,
                                      cast_director_osc - 1,
                                      cast_director_osc))
}

View(sampleTrain)