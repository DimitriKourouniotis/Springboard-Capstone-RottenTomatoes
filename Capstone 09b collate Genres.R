# Collate commonality 

saveRDS(RT.train,"RT_train.RDS")
saveRDS(RT.train.actor_1, "RT_train_actor_1.RDS")
saveRDS(RT.train.actor_2, "RT_train_actor_2.RDS")
saveRDS(RT.train.director, "RT_train_director.RDS")
saveRDS(RT.train.titles, "RT_train_titles.RDS")
saveRDS(simple_ds_train, "simple_ds_train.RDS")

saveRDS(RT.test,"RT_test.RDS")
saveRDS(RT.test.actor_1,"RT_test_actor_1.RDS")
saveRDS(RT.test.actor_2,"RT_test_actor_2.RDS")
saveRDS(RT.test.director,"RT_test_director.RDS")
saveRDS(RT.test.titles,"RT_test_titles.RDS")

RT.train <- readRDS("RT_train.RDS")

simple_ds_train <- RT.train [,c(4,11,16,19,21:41,42:52,54)]
simple_ds_test <- unique(RT.test [,c(4,11,16,19,21:41,42:52,54)])
#write.csv(simple_ds_train,"simple_ds_train.csv")

# Genres Analysis and grouping

# Count Genres within each movie
RT.train.genres <- RT.train %>%
  mutate(genres.count = rowSums(RT.train[21:41]))
View(RT.train.genres)

# what are most common genre combinations?
RT.train.genre.frequency <- unique(subset(RT.train, select = c(movie_title, genres)))
head(RT.train.genre.frequency)
library(plyr)
RT.train.genre.frequency.count <- count(RT.train.genre.frequency,"genres")
genres.wordcloud <- arrange(RT.train.genre.frequency.count,desc(freq))
install.packages("wordcloud")
library(wordcloud)
require(RColorBrewer)
#datain <- read.csv("csv/searchterms.csv", colClasses=c("character", "numeric"))
pal2 <- brewer.pal(8,"Spectral")
png("wordcloud.png", width=1200, height=1200)
wordcloud(genres.wordcloud$genres, 
          genres.wordcloud$freq, 
          scale=c(8,.3),
          min.freq=3, max.words=70, 
          random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

# Create word cloud for Actor Nomination 2 or more Oscars
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud.png", width=1200, height=1200)
wordcloud(accolades_actor$Actor, 
          accolades_actor$noms_actor, 
          scale=c(8,.3),
          min.freq=2, max.words=500, 
          random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

# Create word cloud for Actor Nomination 2 or more Oscars
wordcloud.director <- unique(subset(accolades_director, select = c(director_name, noms_director)))
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud.png", width=1200, height=1200)
wordcloud(wordcloud.director$director_name, 
          wordcloud.director$noms_director, 
          scale=c(8,.3),
          min.freq=1, max.words=500, 
          random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

# sort by number of Genres
RT.train.genres.sort <- RT.train.genres %>%
  group_by(genres.count) %>%
  arrange(desc(genres.count))
View(RT.train.genres.sort)

#RT.train.genres.unique <- unique(RT.train.genres) 
#View(RT.train.genres.unique)

simple_ds_train <- unique(simple_ds_train)
simple_ds.genres <- simple_ds_train %>%
  mutate(genres.count = rowSums(simple_ds_train [6:26]))
View(simple_ds.genres)

movie.genre <- simple_ds_train[,c(1, 5:25)]
title.movie.genres <-  simple_ds_train[,c(1, 5:25)]
movie.genre <- 1 * movie.genre[2:22]
genres <- movie.genre[c(2:22)]
genres.matrix <- cor(movie.genre)
install.packages("corrplot")
library(corrplot)
library(RColorBrewer)
corrplot(genres.matrix, 
         method = "circle", 
         type = "lower", 
         order = "hclust", 
         col=brewer.pal(n=8, name="PuOr"),
         tl.col="black",
         tl.srt=45,
         bg="lightblue")


#create histogram of Frequencies
genres.count <- colSums(title.movie.genres[2:22])
genres.count <- as.data.frame(genres.count) #convert to dataframe
colnames(genres.count) <- c("occurance") #rename column
genre.count <- data.frame(genre=rownames(genres.count), genres.count) #remove rownames
rownames(genre.count) <- NULL

ggplot(genres.count, aes(x=genre, y=occurance)) +
         geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  ggtitle("Distribution of Genres Across Film Titles")

install.packages(("igraph"))
library(igraph)
get.adjacency(graph.edgelist(as.matrix(df), directed=FALSE))


library(tidyr)
title.movie.genres.gather <- gather(title.movie.genres, 
                                    genre, 
                                    status, 
                                    genre_Action:genre_Western)


title.movie.genres.gather  <- title.movie.genres.gather %>% 
  select(movie_title,genre,status) %>%
  filter(status == TRUE)

title.genre <- title.movie.genres.gather %>%
  select(movie_title,genre)

title.movie.genres.gather <- title.movie.genres.gather[3] *1

title.movie.genres[,2:22]<- 1 * title.movie.genres[,2:22] #convert TRUE/FALSE  to 1/0
title.movie.genres <- unique(title.movie.genres)


titlerownames <- title.movie.genres$movie_title # add row names to make this a matrix
row.names(title.movie.genres) <- titlerownames
title.movie.genres <- title.movie.genres %>%
  select(-movie_title)


#average linkng clusters # LOL - NOPE
title.movie.genres.scaled <- scale (title.movie.genres)
d <- dist(title.movie.genres.scaled)
fit.average <- hclust(d,method = "average")
plot(fit.average, hang = -1, cex= 0.8, main= "Average Linkage Clustering") # LOL
# - lets transpose this so rownames = genres

library(NbClust)
devAskNewPage(ask=TRUE)
nc<- NbClust(title.movie.genres.scaled, # how many clusters?
             distance = "euclidean",
             min.nc=5, max.nc = 16, method = "average") #min 2 max 16 = 12 clusters
table(nc$Best.n[1,])

#transpose matrix 

genre.title <- t(title.movie.genres)
gt <- dist(genre.title) #unscaled genre.title
fit.average.gt <- hclust(gt,method = "average")
plot(fit.average.gt, hang = -1, cex= 0.8, main= "Average Linkage Clustering Movie Genres") 

#genres & titles clusters
gtc<- NbClust(t(gt),
             distance = "euclidean",
             min.nc=2, max.nc = 16, method = "average", index="duda")

gtcluster_average <- NbClust(gt,
              distance = "euclidean",
              min.nc=2, max.nc = 16, method = "average", index="duda")

# from springboard class clustering with Netflix
distances.genre <- dist(title.movie.genres, method = "euclidean")
clusterMovies <- hclust(distances.genre, method = "ward.D")
plot(clusterMovies,cex = 0.1, xlab = NULL, sub = "", main= "Clustering Dendrogram \nof Movie Titles")
clustergroups11 <- cutree(clusterMovies,k=11)

cluster11.df <- data.frame(
  action <- tapply(title.movie.genres$genre_Action, clustergroups11, mean),
  adventure <- tapply(title.movie.genres$genre_Adventure, clustergroups11, mean),
  animation <- tapply(title.movie.genres$genre_Animation, clustergroups11, mean),
  biography <- tapply(title.movie.genres$genre_Biography, clustergroups11, mean),
  comedy <- tapply(title.movie.genres$genre_Comedy, clustergroups11, mean),
  crime <- tapply(title.movie.genres$genre_Crime, clustergroups11, mean),
  drama <- tapply(title.movie.genres$genre_Drama, clustergroups11, mean),
  documentary <- tapply(title.movie.genres$genre_Documentary, clustergroups11, mean),
  Family <- tapply(title.movie.genres$genre_Family, clustergroups11, mean),
  Fantasy <- tapply(title.movie.genres$genre_Fantasy, clustergroups11, mean),
  History <- tapply(title.movie.genres$genre_History, clustergroups11, mean),
  Horror<- tapply(title.movie.genres$genre_Horror, clustergroups11, mean),
  Romance<- tapply(title.movie.genres$genre_Romance, clustergroups11, mean),
  SciFi <- tapply(title.movie.genres$genre_Sci_Fi, clustergroups11, mean),
  Sport <- tapply(title.movie.genres$genre_Sport, clustergroups11, mean),
  Short <- tapply(title.movie.genres$genre_Short, clustergroups11, mean),
  Thriller <- tapply(title.movie.genres$genre_Thriller, clustergroups11, mean),
  Musical <- tapply(title.movie.genres$genre_Musical, clustergroups11, mean),
  Mystery <- tapply(title.movie.genres$genre_Mystery, clustergroups11, mean),
  War <- tapply(title.movie.genres$genre_War, clustergroups11, mean),
  Western <- tapply(title.movie.genres$genre_Western, clustergroups11, mean))

cluster11.df
colnames(cluster11.df) <- c("action",
                         "adventure",
                         "animation",
                         "biography",
                         "comedy",
                         "crime",
                         "drama",
                         "documentary",
                         "family",
                         "fantasy",
                         "history",
                         "horror",
                         "romance",
                         "scifi",
                         "sport",
                         "short",
                         "thirller",
                         "musical",
                         "mystery",
                         "war",
                         "western")

write.csv(cluster11.df,"cluster11_df_2.csv")

RT.train_clustered <- cbind(simple_ds_train, clustergroups11)
View(RT.train_clustered)
saveRDS(RT.train_clustered,"RT_train_clustered.RDS")

#model_clustered <- lgm()
