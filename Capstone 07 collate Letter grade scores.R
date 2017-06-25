########################################
# EDA for Letter grades of Movie reviews
########################################

########################################
# count(letter grades)
########################################

library(dplyr)
library(stringr)

critic_letter_grade <- c()
critic_letter_grade <- (newdata[ ,c(1:3,5,6)])

critic_letter_grade$letter_grade <- str_detect(critic_letter_grade$critic_score_and_image_1, "[ABCDEF]") 

summary(critic_score_letter_grade)

critic_letter_grade$letter_grade <- ifelse(critic_letter_grade$letter_grade, 
                                           critic_letter_grade$critic_score_and_image_1,
                                           NA)
count(critic_letter_grade, letter_grade)

critic_letter_grade <- complete.cases(critic_letter_grade)
only_letter_grades <- critic_letter_grade[!is.na(critic_letter_grade["letter_grade"]),]

########################################
# factor letter grades for sorting 
# 15 levels
########################################
only_letter_grades$ordered.letter_grade <- factor(only_letter_grades$letter_grade,
                                   levels=c("F-","F","F+","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"),
                                    ordered=TRUE)
only_letter_grades$tMeter_numeric <- as.integer(sapply(strsplit(only_letter_grades$tMeter, split='%',fixed=TRUE), function(x) (x[1])))




library(ggplot2)

####################################
# letter grades histogram
####################################

ggplot(only_letter_grades, aes(x=ordered.letter_grade)) + 
  geom_histogram( stat= "count",
                  col = "black",
                  fill = "blue") +
  scale_y_continuous( limits = c(0,12000)) +
  ggtitle(paste("Letter grade review distribution for",count(only_letter_grades),"movies(2006+)"))

####################################
# tomato meter score for same movies 
# with letter grade
####################################

ggplot(only_letter_grades, aes(x=tMeter_numeric)) + 
  geom_histogram(aes (y = ..count..), 
                 col = "black",
                 fill = "red",
                 binwidth = 7) +
  scale_x_continuous( breaks = seq(0,100,10))+
  scale_y_continuous( limits = c(0,12000)) +
  ggtitle(paste("TomatoMeter % Distribution for",count(only_letter_grades),"movies(2006+) with letter grades"))


ggplot(only_letter_grades, aes(x=tMeter_numeric)) 
+ geom_density(alpha=.3)

ggplot(only_letter_grades, aes(x=tMeter_numeric)) + geom_histogram(bins=15, stat= "count") 



ggplot(only_letter_grades, aes(x= letter_grade)) +
    geom_histogram() +
    geom_density(alpha=.3) +
    ggtitle("Letter Grade Distn for yr 2006+ movies")


####################################
# Histogram plot Letter Grade distn
# with tMeter Score
####################################

ggplot(only_letter_grades, aes(x=tMeter_numeric, fill=ordered.letter_grade))+
  + geom_histogram(binwidth=1,alpha=.5, position= "identity")


####################################
# Density plot Letter Grade distn
# with tMeter Score
####################################
ggplot(only_letter_grades, aes(x=tMeter_numeric, fill=ordered.letter_grade))+ geom_density(alpha=.2)


####################################
# Density/Histogram plot Letter Grade distn
# with tMeter Score & mean & median
####################################


library(plyr)
concise_tMeter_grades <- ddply(only_letter_grades, "ordered.letter_grade", 
                               summarise, tMeter_numeric.mean=mean(tMeter_numeric), 
                               tMeter_num.median=median(tMeter_numeric)) 
concise_tMeter_grades
# Density plots showing mean
ggplot(only_letter_grades, 
           aes(x=tMeter_numeric,
              colour=ordered.letter_grade)) +
  geom_density() +
  geom_vline(data=concise_tMeter_grades, 
             aes(xintercept=tMeter_numeric.mean,
             colour=ordered.letter_grade),
             linetype="dashed", size=1) +
  geom_vline(data=concise_tMeter_grades, 
             aes(xintercept=tMeter_num.median),
             linetype="dashed", size=1, colour="blue")+
  facet_grid(ordered.letter_grade ~.) +
  ggtitle(paste("TomatoMeter % Distn w/mean&median: of movies(2006+) faceted by Letter Grades"))


# Histogram plots showing means, 
ggplot(only_letter_grades, aes(x=tMeter_numeric)) + 
  geom_histogram(binwidth=2, colour="black", fill="pink") + 
  facet_grid(ordered.letter_grade ~ .) +
  geom_vline(data=concise_tMeter_grades, aes(xintercept=tMeter_numeric.mean),
             linetype="dashed", size=1, colour="red")+
  geom_vline(data=concise_tMeter_grades, aes(xintercept=tMeter_num.median),
             linetype="dashed", size=1, colour="blue")+
  ggtitle(paste("TomatoMeter % Distn w/mean&median: of movies(2006+) faceted by Letter Grades"))

library(stringr)

LetterA <- subset(only_letter_grades, str_detect(ordered.letter_grade,"[A]"))
LetterB <- subset(only_letter_grades, str_detect(ordered.letter_grade,"[B]"))
LetterC <- subset(only_letter_grades, str_detect(ordered.letter_grade,"[C]"))
LetterD <- subset(only_letter_grades, str_detect(ordered.letter_grade,"[D]"))
LetterF <- subset(only_letter_grades, str_detect(ordered.letter_grade,"[F]"))

letterA_group <- ddply(LetterA, "ordered.letter_grade", 
                       summarise, tMeter_num.mean=mean(tMeter_numeric), 
                       tMeter_num.median=median(tMeter_numeric)) 
letterB_group <- ddply(LetterB, "ordered.letter_grade", 
                       summarise, tMeter_num.mean=mean(tMeter_numeric), 
                       tMeter_num.median=median(tMeter_numeric)) 
letterC_group <- ddply(LetterC, "ordered.letter_grade", 
                       summarise, tMeter_num.mean=mean(tMeter_numeric), 
                       tMeter_num.median=median(tMeter_numeric)) 
letterD_group <- ddply(LetterD, "ordered.letter_grade", 
                       summarise, tMeter_num.mean=mean(tMeter_numeric), 
                       tMeter_num.median=median(tMeter_numeric)) 
letterF_group <- ddply(LetterF, "ordered.letter_grade", 
                       summarise, tMeter_num.mean=mean(tMeter_numeric), 
                       tMeter_num.median=median(tMeter_numeric)) 
#------------ Letter Grade A
ggplot(LetterA, aes(x=tMeter_numeric)) + 
  geom_histogram(binwidth=2, colour="black", fill="red", alpha = .2) + 
  facet_grid(ordered.letter_grade ~ .) +
  geom_vline(data=letterA_group, aes(xintercept=tMeter_num.mean),
             linetype="dashed", size=1, colour="red")+
  geom_vline(data=letterA_group, aes(xintercept=tMeter_num.median),
             linetype="dashed", size=1, colour="blue")+
  ggtitle(paste("Letter Grade A: tMeter Score Distn w/mean&median"))

#------------ Letter Grade B
ggplot(LetterB, aes(x=tMeter_numeric)) + 
  geom_histogram(binwidth=2, colour="black", fill="blue", alpha = .2) + 
  facet_grid(ordered.letter_grade ~ .) +
  geom_vline(data=letterB_group, aes(xintercept=tMeter_num.mean),
             linetype="dashed", size=1, colour="red")+
  geom_vline(data=letterB_group, aes(xintercept=tMeter_num.median),
             linetype="dashed", size=1, colour="blue")+
  ggtitle(paste("Letter Grade B: tMeter Score Distn w/mean&median"))

#------------ Letter Grade C
ggplot(LetterC, aes(x=tMeter_numeric)) + 
  geom_histogram(binwidth=2, colour="black", fill="green", alpha = .2) + 
  facet_grid(ordered.letter_grade ~ .) +
  geom_vline(data=letterC_group, aes(xintercept=tMeter_num.mean),
             linetype="dashed", size=1, colour="red")+
  geom_vline(data=letterC_group, aes(xintercept=tMeter_num.median),
             linetype="dashed", size=1, colour="blue")+
  ggtitle(paste("Letter Grade C: tMeter Score Distn w/mean&median"))

#------------ Letter Grade D
ggplot(LetterD, aes(x=tMeter_numeric)) + 
  geom_histogram(binwidth=10, colour="black", fill="yellow", alpha = .2) + 
  facet_grid(ordered.letter_grade ~ .) +
  geom_vline(data=letterD_group, aes(xintercept=tMeter_num.mean),
             linetype="dashed", size=1, colour="red")+
  geom_vline(data=letterD_group, aes(xintercept=tMeter_num.median),
             linetype="dashed", size=1, colour="blue")+
  ggtitle(paste("Letter Grade D: tMeter Score Distn w/mean&median"))

#------------ Letter Grade F
ggplot(LetterF, aes(x=tMeter_numeric)) + 
  geom_histogram(binwidth=2, colour="black", fill="orange", alpha = .2) + 
  facet_grid(ordered.letter_grade ~ .) +
  geom_vline(data=letterF_group, aes(xintercept=tMeter_num.mean),
             linetype="dashed", size=1, colour="red")+
  geom_vline(data=letterF_group, aes(xintercept=tMeter_num.median),
             linetype="dashed", size=1, colour="blue")+
  ggtitle(paste("Letter Grade F: tMeter Score Distn w/mean&median"))


##############################################
# plotting and calculating linear and non-linear regression
##############################################


ggplot(only_letter_grades, aes(y=tMeter_numeric, x=ordered.letter_grade, col = ordered.letter_grade)) + 
  geom_jitter(alpha = 0.3)+
  geom_smooth(method = lm)

#################################################
# converting ordered letter grade to numeric 1-15
#################################################
only_letter_grades$numeric_letter <- as.numeric(only_letter_grades$ordered.letter_grade)

ggplot(only_letter_grades, aes(y=tMeter_numeric, x=numeric_letter, col = numeric_letter)) + 
  geom_jitter(alpha = 0.3)+
  geom_smooth(method = lm) +
  ggtitle("Linear Regression of tMeter Score vs Numeric Letter Grade A+ thru F-")

ggplot(only_letter_grades, aes(y=tMeter_numeric, x=numeric_letter, col = numeric_letter)) + 
  geom_jitter(alpha = 0.3)+
  geom_smooth()+
  ggtitle("Non-Linear Regression 'loess' of tMeter Score vs Numeric Letter Grade A+ thru F-")

tMeter_reg = lm(tMeter_numeric ~ numeric_letter, data = only_letter_grades)
grade_reg = lm(numeric_letter ~ tMeter_numeric, data = only_letter_grades)
summary(grade_reg)
SSE = sum(grade_reg$residuals^2)
SSE
RMSE = sqrt(SSE/nrow(only_letter_grades))
RMSE
