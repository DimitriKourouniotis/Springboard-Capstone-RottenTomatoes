## generate raw number for tMeter score


# split % from tMeter score to make numeric score
Movies_After_2006$tMeter_numeric <- as.integer(sapply(strsplit(Movies_After_2006$tMeter_and_image_1, 
                                                               split='%',
                                                               fixed=TRUE), function(x) (x[1])))
str(Movies_After_2006)

# create A to F conversion from medians of tMeter scores
# using only letter grades dataset
# get median tMeter score for each letter into lookup convert dataset
AtoF_convert <- aggregate(only_letter_grades$tMeter_numeric, 
                         list(only_letter_grades$ordered.letter_grade),
                         median)

# rename columns of Lookup dataset
colnames(AtoF_convert) <- c("letter_grade", 
                            "letter_grade.median")
AtoF_convert

# merge back median results lookup to dataset of letter grade
only_letter_grades.copy <- only_letter_grades
only_letter_grades.copy2 <- merge( AtoF_convert, 
                                   only_letter_grades,
                                   by= "letter_grade")


#rename colums back for merging
colnames(AtoF_convert) <- c("critic_score_and_image_1", 
                            "letter_grade.median")
Movies_After_2006_copy <- Movies_After_2006

#merge scores to letter Movies dataset
Movies_After_2006_copy <- merge(Movies_After_2006, 
                                AtoF_convert, 
                                by= "critic_score_and_image_1",
                                all=TRUE)


saveRDS(AtoF_convert, "AtoF_convert.RDS")
saveRDS(Movies_After_2006, "Movies_After_2006.RDS")
saveRDS(Movies_After_2006_copy, "Movies_After_2006_copy.RDS")
saveRDS(only_letter_grades, "only_letter_grades.RDS") 
saveRDS(only_letter_grades.copy, "only_letter_grades_copy.RDS")  
saveRDS(only_letter_grades.copy2, "only_letter_grades_copy2.RDS") 
