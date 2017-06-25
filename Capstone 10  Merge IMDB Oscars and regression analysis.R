###########
## Merge Directors and Actors from Oscars to dataset

ggplot(testmerge, aes(x=accolades_tot, y= reviewer_score)) +
  geom_point(shape=1) +
  geom_jitter() +
  geom_smooth(method=lm)

#just accolades (faulty - double counting)
ggplot(testmerge.accoladesonly, aes(x=accolades_tot, y= reviewer_score)) +
  geom_point(shape=1, alpha = 0.1) +
  geom_jitter() +
  geom_smooth(method=lm)


#merging data
acctest <- subset(Noms_Aca_ds, Actor !="", select = c(Actor,Title))
acctest <- merge(x = acctest,y = accolades_actor, by = "Actor", all.x = TRUE )

testmerge <- merge(x = tMeter_IMDB_ds, y = accolades_director, by = "director_name", all.x = TRUE)

tMeter_IMDB_Oscars$Oscars <- rowSums(tMeter_IMDB_Oscars[,c("noms_director_career",
                                                               "noms_actor_1_career",
                                                               "noms_actor_2_career",
                                                               "noms_actor_3_career",
                                                               "noms_movie_total")],
                                                                na.rm = TRUE) -
                                         rowSums(tMeter_IMDB_Oscars[,c("nom_director", 
                                                                    "nom_actor_1",
                                                                    "nom_actor_2",
                                                                    "nom_actor_3")], 
                                                                     na.rm =TRUE)

tMeter_IMDB_Oscars$fresh <- ifelse (tMeter_IMDB_Oscars$tMeter_num > 59, 1, 0)
  
simple_ds <- tMeter_IMDB_Oscars[,c(4,11,16,17, 19, 21:41,51:52)]
grouped_genres <- tMeter_IMDB_Oscars[,c(4,5,11,15, 16,17, 19,51:52)]


model1 = lm(tMeter_num ~ Oscars, data = simple_ds)
summary(model1)
model1$residuals
SSE = sum(model1$residuals^2)
SSE
model2 = lm(tMeter_num ~ Oscars + budget, data = simple_ds)
summary(model2)
model3 = lm(tMeter_num ~ Oscars + budget + duration + 
              genre_Comedy +
              genre_Crime +
              genre_Drama +
              genre_Documentary +
              genre_Family +
              genre_Fantasy +
              genre_History +
              genre_Horror +
              genre_Romance +
              genre_Sci_Fi +
              genre_Sport +
              genre_Short +
              genre_Thriller +
              genre_Musical +
              genre_Mystery +
              genre_War +
              genre_Western, data = simple_ds)
summary(model3)
model4 = lm(tMeter_num ~ Oscars + duration + 
              genre_Comedy +
              genre_Drama +
              genre_Documentary +
              genre_Family +
              genre_Romance +
              genre_Thriller +
              genre_Musical +
              genre_Western, data = simple_ds)
summary(model4)

tMeter_IMDB_Oscars$score_dif <- tMeter_IMDB_Oscars$reviewer_score - tMeter_IMDB_Oscars$tMeter_num
View(tMeter_IMDB_Oscars)

critic_differences <- ddply(tMeter_IMDB_Oscars, .(critic_name), summarise,
      mean_dif = mean (score_dif), 
      max_dif = max (score_dif),
      min_dif = min (score_dif),
      mean = mean (reviewer_score),
      reviews = length (reviewer_score),
      sd = sd (reviewer_score),
      max = max(reviewer_score),
      min = min(reviewer_score)) 

ggplot(critic_differences, aes(x=reviews, y= mean_dif)) +
  geom_point(shape=1, alpha = 0.1) +
  geom_jitter() +
  geom_smooth() +
  ggtitle("Critic Reviews Qty against mean difference from tMeter Score")


critic_differences$score_range <- critic_differences$max - critic_differences$min
ggplot(differences, aes(x=reviews, y= score_range)) +
  geom_point(shape=1, alpha = 0.1) +
  geom_jitter() +
  geom_smooth() +
  ggtitle("Critic Reviews Qty against score range")



ddply(tMeter_IMDB_Oscars, .(critic_name , genre_Comedy,
                              genre_Crime ,
                              genre_Drama ,
                              genre_Documentary ,
                              genre_Family ,
                              genre_Fantasy ,
                              genre_History ,
                              genre_Horror ,
                              genre_Romance ,
                              genre_Sci_Fi ,
                              genre_Sport ,
                              genre_Short ,
                              genre_Thriller ,
                              genre_Musical ,
                              genre_Mystery ,
                              genre_War ,
                              genre_Western), summarise,
      mean = mean (reviewer_score),
      count = length (reviewer_score),
      sd = sd (reviewer_score),
      max = max(reviewer_score),
      min = min(reviewer_score)) 

tMeter_IMDB_Oscars$score_dif <- tMeter_IMDB_Oscars$reviewer_score - tMeter_IMDB_Oscars$tMeter_num
View(tMeter_IMDB_Oscars)

tMeter_vs_Oscars <- subset(tMeter_IMDB_Oscars, Oscars > 0, select = c(tMeter_num, Oscars, fresh))
tMeter_vs_Oscars <- unique(tMeter_vs_Oscars)

ggplot(tMeter_vs_Oscars, aes(x=Oscars, y= tMeter_num, col=Oscars)) +
  geom_point(alpha = 0.1) +
  geom_jitter() +
  geom_smooth(method = lm) +
    ggtitle("Oscars Nominations for Dir/Actor/Title vs tMeter Score")

ggplot(tMeter_vs_Oscars, aes(x=Oscars, y= fresh, col=Oscars)) +
  geom_point(alpha = 0.1) +
  geom_jitter() +
  ggtitle("Oscars Nominations for Dir/Actor/Title vs Fresh/Rotten")


model5 = glm(fresh ~ Oscars + budget + duration +
              genre_Action +
              genre_Adventure +
              genre_Animation +
              genre_Biography +
              genre_Comedy +
              genre_Crime +
              genre_Drama +
              genre_Documentary +
              genre_Family +
              genre_Fantasy +
              genre_History +
              genre_Horror +
              genre_Romance +
              genre_Sci_Fi +
              genre_Sport +
              genre_Short +
              genre_Thriller +
              genre_Musical +
              genre_Mystery +
              genre_War +
              genre_Western, data = simple_ds , family = binomial)

summary(model5)
predictfresh = predict(model5, type="response")
summary(predictfresh)

head (differences, 5)
head (tMeter_vs_Oscars)
head (simple_ds)

#
# using genre cluster as a variable
#


model6 = lm(tMeter_num ~ genres, data = tMeter_IMDB_Oscars)
summary(model6)
movies_2015 <- tMeter_IMDB_Oscars[,c(4,10)]

#
# genre distribution
#

genre_distribution <- unique(tMeter_IMDB_Oscars[,c(1, 20:41, 43, 44, 51, 52)])
head (genre_distribution)

 genre_distribution2 <- unique(tMeter_IMDB_Oscars[,21:41])
 genre_count <- sapply(genre_distribution2,sum)
 genre_count

 genre_dist_osc_fre <-  unique(tMeter_IMDB_Oscars[,c(21:41, 51:52)])
 head(genre_dist_osc_fre)

#test conversion:
 genre_dist_osc_fre$genre_Musical [genre_dist_osc_fre$genre_Musical== 0] <- NA
 genre_dist_osc_fre$genre_Musical
 genre_dist_osc_fre$genre_Musical*genre_dist_osc_fre$fresh

#convert TRUE to 1 then convert 0 to NA
#
genre_dist_osc_fre <- genre_dist_osc_fre[,c(1:21)]*1
genre_dist_osc_fre[,1:21][genre_dist_osc_fre[,1:21] == 0] <- NA
genre_fresh <-  unique(tMeter_IMDB_Oscars[,c(21:41, 51:52)])

library(dplyr)
results.temp <- tMeter_IMDB_Oscars%>%
  group_by(movie_title)%>% 
  summarise(Mean=mean(reviewer_score), Max=max(reviewer_score), Min=min(reviewer_score), Median=median(reviewer_score), Std=sd(reviewer_score))

library(ggplot2)  
ggplot(tMeter_IMDB_Oscars, aes( x= score_dif, y = tMeter_num, col = fresh)) +
  geom_jitter()

ggplot(tMeter_IMDB_Oscars, aes( x= score_dif, y = tMeter_num, col = fresh)) +
  +   geom_jitter()
#
#finally - 7 or more oscars in team or film mean very high Fresh percentage
#
library(ggplot2)
ggplot(tMeter_vs_Oscars, aes( x= Oscars, y = tMeter_num, col = fresh)) +  geom_jitter()

movie_reviews_ranges$Fresh_Rotten <- ifelse(movie_reviews_ranges$fresh == 1, "Fresh","Rotten")

ggplot(movie_reviews_ranges, aes( x= title_nominations,  fill = Fresh_Rotten)) +
  ggtitle("Fresh & Rotten by Title Oscar Nominations") +
  geom_bar (position = position_dodge())


#
#cast and director oscars
tMeter_IMDB_Oscars$cast_director_osc <- rowSums(tMeter_IMDB_Oscars [, c("noms_director_career",
                                                                        "noms_actor_1_career",
                                                                        "noms_actor_2_career",
                                                                        "noms_actor_3_career")], na.rm = TRUE)
tMeter_actor_director_career_Oscars <- unique(tMeter_IMDB_Oscars[,c(4,11,52,54)])
tMeter_actor_director_career_Oscars$Fresh_Rotten <- ifelse(tMeter_actor_director_career_Oscars$fresh == 1, "Fresh","Rotten")

#
# crew with **3** or more oscars combined in the careers of Director or top 3 billed actors
# do not have good enough data on producers, writers or art directors and cinematograhy
#
ggplot(tMeter_actor_director_career_Oscars, aes( x= cast_director_osc,  fill = Fresh_Rotten)) + 
  geom_bar (position = position_dodge()) + 
  ggtitle("Fresh & Rotten by Director & Actor Oscar Nominations")


#
# calculate median score from reviewers for each movie and quartiles
#
library(plyr)
movie_reviews_ranges <- ddply(tMeter_IMDB_Oscars, .(movie_title), summarise,
                            mean_rev = mean (reviewer_score), 
                            median_rev = median (reviewer_score),
                            max_rev = max (reviewer_score),
                            min_rev = min (reviewer_score),
                            reviews = length (reviewer_score),
                            sd = sd (reviewer_score),
                            IQR = IQR(reviewer_score),
                            tMeter_num = mean(tMeter_num),
                            Oscars = mean (Oscars),
                            cast_career_osc = mean(cast_director_osc),
                            fresh = mean(fresh))
View(movie_reviews_ranges)

ggplot(critic_differences, aes(x=reviews, y= mean_dif)) +
  geom_point(shape=1, alpha = 0.1) +
  geom_jitter() +
  geom_smooth() +
  ggtitle("Critic Reviews Qty against mean difference from tMeter Score")

#
#  create column of nominations that title received
#
movie_reviews_ranges <- movie_reviews_ranges %>% mutate(title_nominations = Oscars - cast_career_osc)

#
# plot title nominations against tMeter RT Score
#
ggplot(movie_reviews_ranges, aes(x=title_nominations, y=tMeter_num, col =fresh)) + 
  geom_jitter() + 
  ggtitle("RT Score vs Movie Title Oscar Nominations 96-16")

#
# career wise cast does appear in bad films until they reach 7 oscar nominations
#

ggplot(movie_reviews_ranges, aes(x=cast_career_osc, y=tMeter_num, col =fresh)) + 
  geom_jitter() + 
  ggtitle("RT Score vs Casts' Career Oscar Nominations 96-16")

#
# Do films with more reviewers get higher RT scores?
# it looks like films with over 150 reviews get higher scores.. 
#

ggplot(movie_reviews_ranges, aes(x=reviews, y=tMeter_num, col =fresh)) + 
  geom_jitter() + 
  ggtitle("RT Score vs RT no. Reviews 96-16")

#
# Do films with narrower IQR fair well?
# A. Interesting cresent curve
#

ggplot(movie_reviews_ranges, aes(x=IQR, y=tMeter_num, col =fresh)) + 
  geom_jitter() + 
  ggtitle("RT Score vs Reviewers' IQR 96-16")

#
# Do films with narrower SD fair well?
# A. meh
#

ggplot(movie_reviews_ranges, aes(x=sd, y=tMeter_num, col =fresh)) + 
  geom_jitter() + 
  ggtitle("RT Score vs Reviewer's SD 96-16")

#
# Do films get nominated for more oscars per IQR?
# A. Films are nominated for oscars in 10-30 IQR 
#

ggplot(movie_reviews_ranges, aes(x=IQR, y=title_nominations, col =fresh)) + 
  geom_jitter() + 
  ggtitle("Oscar Nominations vs Reviewer's IQR 96-16")


#
# is median Reviewer Score better than RT Score for Oscar movies ?
# 
#

ggplot(movie_reviews_ranges, aes(y=title_nominations, x=median_rev, col =fresh)) + 
  geom_jitter() + 
  ggtitle("Nominations vs Reviewer's Median Score 96-16")

ggplot(movie_reviews_ranges, aes(y=title_nominations, x=tMeter_num, col =fresh)) + 
  geom_jitter() + 
  ggtitle("Nominations vs RT Score 96-16")

fresh_only <- tMeter_IMDB_Oscars %>% 
  filter(fresh == 1)

rotten_only <- tMeter_IMDB_Oscars %>% 
  filter(fresh == 0)

model7 = glm(fresh ~ cast_career_osc + sd + IQR + reviews,
               data = movie_reviews_ranges, family = binomial)
summary(model7)

model8 = lm(title_nominations ~ cast_career_osc + IQR + reviews,
          data = movie_reviews_ranges)
summary(model8)

model9 = lm(title_nominations ~ cast_career_osc + IQR + reviews +
              fresh + tMeter_num + mean_rev + median_rev,
            data = movie_reviews_ranges)
summary(model9)

model10 = lm(title_nominations ~ cast_career_osc + reviews +
              tMeter_num + mean_rev,
            data = movie_reviews_ranges)
summary(model10)
