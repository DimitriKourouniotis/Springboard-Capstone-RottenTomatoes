#########################
# clean up newdata 
#########################
#critic_score_and_image_1 errors
#
#      A  minus     1
#      A m inus     2
#      A min us     1
#       A minus   219
#            A+   297
#            A=     2
#        Aminus     1
#     ..etc..
# > newdata[25289,]
# critic_name_1 critic_score_and_image_1 tMeter_and_image_1 movie_title_and_year_1 movie_title_allyears movie_year
# 60987 harvey-s-karten                       B(subscript "i"??)       72% American Sniper (2015)      American Sniper       2015
#
#
#  df$b[df$b == "B"] <- "C"
#
#
#########################

clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "A  minus"] <- "A-"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "A m inus"] <- "A-"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "A min us"] <- "A-"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "A minus"] <- "A-"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "Aminus"] <- "A-"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "A="] <- "A+"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "B minus"] <- "B-"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "B plus"] <- "B+"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "B="] <- "B+"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "BIt's still fun, but you feel more like you're wat"] <- "B"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "Bomb"] <- "1/4"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "C plus"] <- "C+"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "Bplus"] <- "B+"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "B--"] <- "B-"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "c-"] <- "C-"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "C  minus"] <- "C-"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "C +"] <- "C+"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "C minus"] <- "C-"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "Cplus"] <- "C+"
clean_tMeterScore_ds$critic_score_and_image_1 [clean_tMeterScore_ds$critic_score_and_image_1 == "D plus"] <- "D+"

newdata[25289,"critic_score_and_image_1"] <- "B"


clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="10 Empty"] <- 2008
clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="108"] <- 2013
clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="11 Minut"] <- 2016
clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="12 Monkeys"] <- 1995
clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="12:08 East of Bucharest"] <- 2007
clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="13 Ghosts"] <- 2001
clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="1732 Høtten"] <- 2000
clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="18 Years Later"] <- 2010
clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="14 no yoru"] <- 2016
clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="20th Century Boys 1: Beginning of the End"] <- 2008
clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="20th Century Boys 3: Redemption"] <- 2009
clean_tMeterScore_ds$movie_year[clean_tMeterScore_ds$movie_year=="20th Century Boys: Chapter Two - The Last Hope"] <- 2009
clean_tMeterScore_ds[60987,"critic_score_and_image_1"] <- "B"

save(clean_tMeterScore_ds,file="clean_tMeterScore_ds.RDS")


## Clean up spurios "***" scores and No Score Yet entry

## migrate to tMeter.1996plus

tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "*****"] <- "5/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "****1/2"] <- "4.5/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "****"] <- "4/5"

tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "***1/2"] <- "3.5/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "***"] <- "3/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "**1/2"] <- "2.5/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "**"] <- "2/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "* 1/2"] <- "1.5/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "*"] <- "1/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "A--"] <- "A-"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "C--"] <- "C-"

tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "-1/4"] <- "1/4"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "-1/10"] <- "1/10"

tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "1.5/.5"] <- "1.5/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "3/.5"] <- "3/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "9/19"] <- "9/10"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "9.1/19"] <- "9.1/10"

tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "4 stars out of 5"] <- "4/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "4 out of 5 stars"] <- "4/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "3.5 out of 5 stars"] <- "3.5/5"

tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "2 stars out of 5"] <- "2/5"

tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "4/5 stars"] <- "4/5"


tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "1/5 stars"] <- "1/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "4/5 stars"] <- "4/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "2/4 stars"] <- "2/4"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == " stars out of "] <- "/"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == " out of "]  <- "/"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "2 stars out of 5"] <- "2/5"
tMeter.1996plus$critic_score_and_image_1 [tMeter.1996plus$critic_score_and_image_1 == "9 out of 10"] <- "9/10"


tMeter.1996plus$critic_score_and_image_1 <- sub( " out of ","/", tMeter.1996plus$critic_score_and_image_1)
tMeter.1996plus$critic_score_and_image_1 <- sub( " 1/2",".5", tMeter.1996plus$critic_score_and_image_1)

tMeter.1996plus$critic_score_and_image_1 <- sub( "stars/5","/5", tMeter.1996plus$critic_score_and_image_1)
tMeter.1996plus$critic_score_and_image_1 <- sub( "/5 stars","/5", tMeter.1996plus$critic_score_and_image_1)
tMeter.1996plus$critic_score_and_image_1 <- sub( "/4 stars","/4", tMeter.1996plus$critic_score_and_image_1)

saveRDS(tMeter.1996plus, file="tMeter1996plus.RDS")

## create subset of non-missing denominators and non missing review scores



tMetertest <- tMeter.1996plus[ which (!is.na(tMeter.sample$critic_score_and_image_1)),]
View(tMetertest)

## Missing denominaters


tMeter.sample$critic_score_and_image_1[which( tMeter.sample$critic_name_1 == "gerald-peary" & 
                                     is.na(tMeter.sample$score_denominator) &
                                     !is.na(tMeter.sample$score_numerator & 
                                     !is.na(tMeter.sample$critic_score_and_image_1) &
                                     is.numeric(tMeter.sample$critic_score_and_image_1))) ] <- paste(tMeter.sample$critic_score_and_image_1, "/4", sep="")

# YES
tMeter96complete <- subset(tMeter.1996plus, critic_score_and_image_1 != "")
tMeter96complete$critic_score_and_image_1 <- gsub("'",'',tMeter96complete$critic_score_and_image_1)
View(tMeter96complete)

tMetertest <- tMeter96complete

# remove `900 records where the reviewer did not give a denominator (TEMPORARY)
tMeter96complete <- tMeter96complete [-which(is.na(tMeter96complete$score_denominator) & !is.na(tMeter96complete$score_numerator) ),]
saveRDS(tMeter96complete,file="tMeter96complete.RDS")

tMetertestMissing <- subset(tMeter96complete, is.na(tMeter96complete$score_denominator) & !is.na(tMeter96complete$score_numerator))

