# Genres Analysis and grouping

# Count Genres/movie
tMeter_IMDB_Oscars.genres <- tMeter_IMDB_Oscars %>%
  mutate(genres.count = rowSums(tMeter_IMDB_Oscars[21:41]))

View(tMeter_IMDB_Oscars.genres)


tMeter_IMDB_Oscars.genres.sort <- tMeter_IMDB_Oscars.genres %>%
  group_by(genres.count) %>%
  arrange(desc(genres.count))

View(tMeter_IMDB_Oscars.genres.sort)

tMeter_IMDB_Oscars.genres.unique <- unique(tMeter_IMDB_Oscars.genres) 
View(tMeter_IMDB_Oscars.genres.unique)

simple_ds <- unique(simple_ds)
simple_ds.genres <- simple_ds %>%
  mutate(genres.count = rowSums(simple_ds [5:25]))
View(simple_ds.genres)
