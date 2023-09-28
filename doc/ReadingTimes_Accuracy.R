library(tidyverse)
library(lme4)
library(coda)

#read data
accuracy <- read.csv('Cleaned_211_All_accuracy.csv')
data <- read.csv('Cleaned_211_Correct.csv')

#transform data
data$art_z <- as.numeric(scale(data$ART_score_value))
data$re_z <- as.numeric(scale(data$RE_Score))

accuracy$art_z <- as.numeric(scale(accuracy$ART_score_value))
accuracy$re_z <- as.numeric(scale(accuracy$RE_Score))

#read data structure
glimpse(accuracy)
glimpse(data)

#code the comparison contrasts by assigning dummy coding
#for "data" dataset
data$easy_hard <- as.numeric(with(data, ifelse(SentenceType == "Active" | SentenceType == "Passive", "-1", "1")))
data$easy <- as.numeric(with(data, ifelse(SentenceType == "Active", "-1", 
                                       ifelse(SentenceType == "Passive", "1", "0"))))
data$hard <- as.numeric(with(data, ifelse(SentenceType == "SRC", "-1", 
                                        ifelse(SentenceType == "ORC", "1", "0"))))

data$linear_trend <- as.numeric(with(data, ifelse(SentenceType == "Active", "-3", 
                                              ifelse(SentenceType == "Passive", "-1", 
                                                     ifelse(SentenceType == "SRC", "1", "3")))))

# for "accuracy" dataset
accuracy$easy_hard <- as.numeric(with(accuracy, ifelse(SentenceType == "Active" | SentenceType == "Passive", "-1", "1")))
accuracy$easy <- as.numeric(with(accuracy, ifelse(SentenceType == "Acive", "-1", 
                                        ifelse(SentenceType == "Passive", "1", "0"))))
accuracy$hard <- as.numeric(with(accuracy, ifelse(SentenceType == "SRC", "-1", 
                                        ifelse(SentenceType == "ORC", "1", "0"))))

accuracy$linear_trend <- as.numeric(with(accuracy, ifelse(SentenceType == "Active", "-3", 
                                               ifelse(SentenceType == "Passive", "-1", 
                                                      ifelse(SentenceType == "SRC", "1", "3")))))




#code exploratory treatment contrast with Active sentences set as a baseline

data$condition <- as.factor(with(data, ifelse(SentenceType == "Active", "1", 
                                            ifelse(SentenceType == "Passive", "2", 
                                                   ifelse(SentenceType == "SRC", "3", "4")))))

accuracy$condition <- as.factor(with(accuracy, ifelse(SentenceType == "Active", "1", 
                                            ifelse(SentenceType == "Passive", "2", 
                                                   ifelse(SentenceType == "SRC", "3", "4")))))


data$SentenceType <- as.factor(data$SentenceType)
accuracy$SentenceType <- as.factor(accuracy$SentenceType)



#Response time raw and log transformed vs ART and RE in separate models

art_orthogonal <- lmer(ReadingTime_ms ~ easy_hard * art_z + easy * art_z + hard * art_z + 
                         (1 | ItemType) + (1 | ParticipantCode), data = data)
summary(art_orthogonal)
art_orthogonal_log <- lmer(Reading_Time_Log ~ SES + easy_hard * art_z + easy * art_z + 
                            hard * art_z + (1 | ItemType) + (1 | ParticipantCode), data = data)
summary(art_orthogonal_log)


re_orthogonal <- lmer(ReadingTime_ms ~ easy_hard * re_z + easy * re_z + hard * re_z + 
                       (1 | ItemType) + (1 | ParticipantCode), data = data)
summary(re_orthogonal)
re_orthogonal_log <- lmer(Reading_Time_Log ~ easy_hard * re_z + easy * re_z + hard * re_z + 
                            (1 | ItemType) + (1 | ParticipantCode), data = data)
summary(re_orthogonal_log)


#Response time raw and log transformed, both ART and RE in one model
art_re_orthogonal_three_way <- lmer(ReadingTime_ms ~ easy_hard * art_z + easy * art_z + hard * art_z + easy_hard * re_z + 
                                     easy * re_z + hard * re_z + (1 | ItemType) + (1 | ParticipantCode), data = data)
summary(art_re_orthogonal_three_way)


art_re_orthogonal_three_way_log <- lmer(Reading_Time_Log ~ easy_hard * art_z + easy * art_z + hard * art_z + easy_hard * re_z +
                                          easy * re_z + hard * re_z + (1 | ItemType) + (1 | ParticipantCode), data = data)
summary(art_re_orthogonal_three_way_log)



#exploratory  analyses - treatment contrast
art_treatment_raw <- lmer(ReadingTime_ms ~ condition * art_z * SES + (1 | ItemType) + (1 | ParticipantCode), data = data)
summary(art_treatment_raw)
art_treatment_log <- lmer(Reading_Time_Log ~ condition * art_z * SES + (1 | ItemType) + (1 | ParticipantCode), data = data)
summary(art_treatment_log)

re_treatment_raw <- lmer(ReadingTime_ms ~ condition * re_z * SES + (1 | ItemType) + (1 | ParticipantCode), data = data)
summary(re_treatment_raw)
re_treatment_log <- lmer(Reading_Time_Log ~ condition * re_z * SES + (1 | ItemType) + (1 | ParticipantCode), data =data)
summary(re_treatment_log)

art_re_treatment_three <- lmer(ReadingTime_ms ~ condition * art_z + condition * re_z + (1 | ItemType) + (1 | ParticipantCode), data = data)
summary(art_re_three)
confint(art_re_three)
art_re_treatment_three_log <- lmer(Reading_Time_Log ~ condition * art_z + condition * re_z + (1 | ItemType) + (1 | ParticipantCode), data = data)
summary(art_re_three_log )
confint(art_re_three_log )

anova(art_treatment_raw, art_re_three)


#exploratory  analyses - accuracy contrast

art_orthogonal <- glmer(Accuracy ~ easy_hard * art_z + easy * art_z + hard * art_z + (1 | ItemType) + (1 | ParticipantCode), data = accuracy, family = binomial)
summary(art_orthogonal)
confint(art_orthogonal)

re_orthogonal <- glmer(Accuracy ~ easy_hard * re_z + easy * re_z + hard * re_z + (1 | ItemType) + (1 | ParticipantCode), data = accuracy, family = binomial)
summary(re_orthogonal)
confint(re_orthogonal)

art_treatment_accuracy <- glmer(Accuracy ~ condition * art_z * SES + (1 | ItemType) + (1 | ParticipantCode), data = accuracy, family = binomial)
summary(art_treatment_accuracy)

re_treatment_accuracy <- glmer(Accuracy ~ condition * re_z * SES + (1 | ItemType) + (1 | ParticipantCode), data = accuracy, family = binomial)
summary(re_treatment_accuracy)


art_re_orthogonal_three <- glmer(Accuracy ~ easy_hard * art_z + easy * art_z + hard * art_z + easy_hard * re_z + easy * re_z + hard * re_z + (1 | ItemType), data = accuracy, family = binomial)
summary(art_re_orthogonal_three)
confint(art_re_orthogonal_three)

art_re_treatment_three <- glmer(Accuracy ~ condition * art_z + condition * re_z + (1 | ItemType), data = accuracy, family = binomial)
summary(art_re_treatment_three)
confint(art_re_treatment_three)

anova(art_treatment_accuracy, art_re_treatment_three)

art_re_orthogonal_three_lm <- lm(Accuracy ~ easy_hard * art_z * re_z + easy * art_z * re_z + hard * art_z * re_z, data = accuracy)
summary(art_re_orthogonal_three_lm)
confint(art_re_orthogonal_three_lm)

art_re_treatment_three_lm <- lm(Accuracy ~ condition * art_z + condition * re_z, data = accuracy)
summary(art_re_treatment_three_lm)

re_treatment_lm <- lm(Accuracy ~ condition * re_z, data = accuracy)
summary(re_treatment_lm)



