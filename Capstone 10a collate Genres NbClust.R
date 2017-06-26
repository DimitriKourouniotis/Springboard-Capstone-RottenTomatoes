# do cluster analysis by Genre
# and confusion table
# predict and ROC curve
head(movies)

movies_for_clust_analysis <- movies[,c(1,5:25)]

library(NbClust)
library(dplyr)

ind <- which(with( movies_for_clust_analysis, movie_title == "Ben-Hur" )) #remove duplicate Ben-Hur entry
movies_for_clust_analysis <- movies_for_clust_analysis[ -3211, ]

rownames(movies_for_clust_analysis) <- movies_for_clust_analysis[,1]
movies_for_clust_analysis$movie_title <-NULL
movies_for_clust_analysis <- movies_for_clust_analysis * 1
#movies$title <- rownames(movies)

#movies$title <- rownames(movies_for_clust_analysis)


distances = dist(movies_for_clust_analysis[1:21], method = "euclidean")
clusterMovies = hclust(distances, method = "ward.D")
plot(clusterMovies,cex = 0.1, xlab = NULL, sub = "", main= "Clustering Dendrogram \nof Movie Titles")



#subset(movies, title == "Erin Brokovich")
#tapply(movies$genre_Action, clusterGroups,mean)

clusterGroupsList <- cutree(clusterMovies,k=11)

cluster11.df <- data.frame(
  action <- tapply(movies_for_clust_analysis$genre_Action, clusterGroupsList, mean),
  adventure <- tapply(movies_for_clust_analysis$genre_Adventure, clusterGroupsList, mean),
  animation <- tapply(movies_for_clust_analysis$genre_Animation, clusterGroupsList, mean),
  biography <- tapply(movies_for_clust_analysis$genre_Biography, clusterGroupsList, mean),
  comedy <- tapply(movies_for_clust_analysis$genre_Comedy, clusterGroupsList, mean),
  crime <- tapply(movies_for_clust_analysis$genre_Crime, clusterGroupsList, mean),
  drama <- tapply(movies_for_clust_analysis$genre_Drama, clusterGroupsList, mean),
  documentary <- tapply(movies_for_clust_analysis$genre_Documentary, clusterGroupsList, mean),
  Family <- tapply(movies_for_clust_analysis$genre_Family, clusterGroupsList, mean),
  Fantasy <- tapply(movies_for_clust_analysis$genre_Fantasy, clusterGroupsList, mean),
  History <- tapply(movies_for_clust_analysis$genre_History, clusterGroupsList, mean),
  Horror<- tapply(movies_for_clust_analysis$genre_Horror, clusterGroupsList, mean),
  Romance<- tapply(movies_for_clust_analysis$genre_Romance, clusterGroupsList, mean),
  SciFi <- tapply(movies_for_clust_analysis$genre_Sci_Fi, clusterGroupsList, mean),
  Sport <- tapply(movies_for_clust_analysis$genre_Sport, clusterGroupsList, mean),
  Short <- tapply(movies_for_clust_analysis$genre_Short, clusterGroupsList, mean),
  Thriller <- tapply(movies_for_clust_analysis$genre_Thriller, clusterGroupsList, mean),
  Musical <- tapply(movies_for_clust_analysis$genre_Musical, clusterGroupsList, mean),
  Mystery <- tapply(movies_for_clust_analysis$genre_Mystery, clusterGroupsList, mean),
  War <- tapply(movies_for_clust_analysis$genre_War, clusterGroupsList, mean),
  Western <- tapply(movies_for_clust_analysis$genre_Western, clusterGroupsList, mean))

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

write.csv(cluster11.df,"cluster11_df_3.csv")

##
ind <- which(with( movies, movie_title == "Ben-Hur" )) #remove duplicate Ben-Hur entry
movies <- movies[ -3211, ]

movies_with_clustergroups <- cbind(movies, clusterGroupsList)
View(movies_with_clustergroups)

movies_with_clustergroups$clusterGroupsList <- as.factor(movies_with_clustergroups$clusterGroupsList)


saveRDS(movies_with_clustergroups,"movies_with_clustergroups.RDS")

movies_with_clustergroups_train <- subset(movies_with_clustergroups, train == 1)
movies_with_clustergroups_test <- subset(movies_with_clustergroups, train == 0)

regcluster <- glm(fresh ~ clusterGroupsList - 1, data = movies_with_clustergroups_train, family = binomial)
summary(regcluster)
movies_pred <- predict(regcluster, type = "response")
table(movies_with_clustergroups_train$fresh, movies_pred >= 0.5)
#     FALSE TRUE
# 0   1381  245
# 1   966  329

movies_pred_test <- predict(regcluster, newdata = movies_with_clustergroups_test, type = "response")
table(movies_with_clustergroups_test$fresh, movies_pred_test >= 0.5)
#     FALSE TRUE
# 0   194   33
# 1   131   39


library(pROC)
#prob <- predict(model5log, simple_test, type = "response")
movies_with_clustergroups_test$prob=movies_pred_test
g <- roc(fresh ~ prob, data = movies_with_clustergroups_test)
plot(g, main = "Logistic Regression for Fresh \n with 11 Clusters")    
auc(g)
