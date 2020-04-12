# The association between parenting, attention and working memory in early childhood

# Packages ----------------------------------------------------------------

install.packages("psych")
install.packages("PerformanceAnalytics")
install.packages("Hmisc")
library("Hmisc")
library(tidyverse)
library(naniar)
library(psych)
library(PerformanceAnalytics)

# Data files --------------------------------------------------------------

background <- read_csv("E:/Dissertation/Study 2/W_Background_Questionnaire.csv")
trackit <- read.csv("E:/Dissertation/Study 2/W Track-it Task data.csv")
wordspan <- read_csv("E:/Dissertation/Study 2/W Verbal WM.csv")
forwardtap <- read.csv("E:/Dissertation/Study 2/W Corsi Forward data.csv")
backwardtap <- read.csv("E:/Dissertation/Study 2/W Corsi Backward data.csv")
parenting <- read.csv("E:/Dissertation/Study 2/W Parenting.csv")
CBQ <- read.csv("E:/Dissertation/Study 2/W CBQ.csv")
spaceship <- read.csv("W SWMtask.csv")
study2 <- read_csv("study2.csv")
spaceship_mdntgt <- read.csv("E:/Dissertation/Study 2/Data for HLM/spaceship_mdntgt.csv")
Updated_SWMtask <- read_csv("Updated_SWMtask.csv")


# Background questionnaire ------------------------------------------------

background <- background %>% 
  filter(Subject <= 32066, Subject != 32026, Subject != 32027, Subject != 32032, Subject != 32039, Subject != 32040, Subject != 32048, Subject != 32059)

background$SEX <- ifelse(background$SEX == "M", 0, 1) 

background <- background %>% 
  select(Subject, age = 'Age in years', sex = SEX, ratio)

dplyr::count(background, MEDU)

dplyr::count(background, FEDU)

dplyr::count(background, SEX)

dplyr::count(background, ranum)

dplyr::count(background, Ethnicity)


# Track-it ----------------------------------------------------------------

trackit <- as_tibble(trackit)

trackit <- select(trackit, -(Birthdate:Gender), -X)
trackit <- select(trackit, Subject = ID, everything())

trackit <- trackit %>% 
  filter(Subject <= 32066, Subject != 32026, Subject != 32027, Subject != 32032, Subject != 32039, Subject != 32040, Subject != 32048, Subject != 32059)

trackit <- filter(trackit, Memory.of.location != ".")

trackit$Memory.of.location <- as.numeric(trackit$Memory.of.location == "TRUE")

trackit <- trackit %>% 
  filter(Trial != 2 ) %>% 
  group_by(Subject) %>% 
  mutate(percentcorrect = sum(Memory.of.location)/10) 

trackit <- trackit %>% 
  group_by(Subject) %>% 
  summarise(trackit_mean = mean (sum(percentcorrect)/10, na.rm = TRUE)) %>% 
  filter(trackit_mean != 0)

trackit %>% 
  ungroup(trackit) %>% 
  summarise(trackit_mean = mean(percentcorrect, na.rm = TRUE),
            trackit_sd = sd(percentcorrect, na.rm = TRUE)) # Mean & sd for trackit

trackit_df <- filter(trackit, Trial == 2)# filter out test trials


# Forward Corsi-block tapping task ----------------------------------------

forwardtap <- forwardtap %>% 
  filter(Subject <= 32066, 
         Subject != 32026, 
         Subject != 32027, 
         Subject != 32032, 
         Subject != 32039, 
         Subject != 32040, 
         Subject != 32048, 
         Subject != 32059
)

forwardtap %>% 
  summarise(forwardtap_mean = mean(ftapsum, na.rm = TRUE), 
            forwardtap_sd = sd(ftapsum, na.rm = TRUE)
) #Screening data - outliers

forwardtap <- forwardtap %>% 
  filter(ftapsum != 0 & ftapsum != "NA")


# Backward Corsi-block tapping task ---------------------------------------

backwardtap <- backwardtap %>% 
  select(Subject = ID, bncorrect = ncorrect, bspan = span, btapsum = total.score)

backwardtap <- backwardtap %>% 
  filter(Subject <= 32066, 
         Subject != 32026, 
         Subject != 32027, 
         Subject != 32032, 
         Subject != 32039, 
         Subject != 32040, 
         Subject != 32048, 
         Subject != 32059
)

backwardtap <- backwardtap %>% 
  filter (btapsum != 0 & btapsum != "NA")


# Verbal working memory ---------------------------------------------------

wordspan <- wordspan %>% 
  select(Subject, wordspan = Score)

wordspan <- wordspan %>% 
  filter(Subject <= 32066, 
         Subject != 32026, 
         Subject != 32027, 
         Subject != 32032, 
         Subject != 32039, 
         Subject != 32040, 
         Subject != 32048, 
         Subject != 32059
)

wordspan <- wordspan %>% 
  filter(wordspan != 0 & wordspan != "NA")

wordspan <- mutate(wordspan, wordspan = wordspan + 1)

dplyr::count(wordspan, wordspan)


# Parenting behaviors and dimension questionnaire -------------------------

parenting <- as_tibble(parenting)

parenting <- parenting %>% 
  filter(Subject <= 32066, 
         Subject != 32026, 
         Subject != 32027, 
         Subject != 32032, 
         Subject != 32039, 
         Subject != 32040, 
         Subject != 32048, 
         Subject != 32059, 
         Subject !=32051
)

parenting <- select (parenting, -(Respondence:Sex))

warmth <- select(parenting, Subject, Q1, Q4, Q16, Q20, Q26, Q32)
punitive <- select(parenting, Subject, Q7, Q14, Q18, Q21, Q29)
autonomy <- select(parenting, Subject, Q2, Q8, Q19, Q25, Q30)
permissive <- select(parenting, Subject, Q5, Q9, Q15, Q24, Q27, Q31)
democratic <- select(parenting, Subject, Q10, Q12, Q17, Q23, Q33)
anxious <- select(parenting, Subject, Q3, Q6, Q11, Q13, Q22, Q28)

warmth <- warmth %>% 
  gather(`Q1`,`Q4`, `Q16`, `Q20`, `Q26`, `Q32`, key = "items", value = "warmthscore") %>% 
  group_by(Subject) %>% 
  summarise(warmthtotal = mean(warmthscore, na.rm = TRUE))

punitive <- punitive %>% 
  gather(`Q7`,`Q14`, `Q18`, `Q21`, `Q29`, key = "items", value = "punitivescore") %>%
  group_by(Subject) %>% 
  summarise(punitivetotal = mean(punitivescore, na.rm = TRUE))

autonomy <- autonomy %>% 
  gather(`Q2`,`Q8`, `Q19`, `Q25`, `Q30`, key = "items", value = "autonomyscore") %>% 
  group_by(Subject) %>% 
  summarise(autonomytotal = mean(autonomyscore, na.rm = TRUE))

permissive <- permissive %>% 
  gather(`Q5`,`Q9`, `Q15`, `Q24`, `Q27`, `Q31`, key = "items", value = "permissivescore") %>% 
  group_by(Subject) %>% 
  summarise(permissivetotal = mean(permissivescore, na.rm = TRUE))

democratic <- democratic %>% 
  gather(`Q10`,`Q12`, `Q17`, `Q23`, `Q33`, key = "items", value = "demoscore") %>% 
  group_by(Subject) %>% 
  summarise(demototal = mean(demoscore, na.rm = TRUE))

anxious <- anxious %>% 
  gather(`Q3`,`Q6`, `Q11`, `Q13`, `Q22`,`Q28`, key = "items", value = "anxiousscore") %>% 
  group_by(Subject) %>% 
  summarise(anxioustotal = mean(anxiousscore, na.rm = TRUE))

parenting <- left_join(warmth, punitive, by = "Subject")
parenting <- left_join(parenting, autonomy, by = "Subject")
parenting <- left_join(parenting, permissive, by = "Subject")
parenting <- left_join(parenting, democratic, by = "Subject")
parenting <- left_join(parenting, anxious, by = "Subject")

parenting <- parenting %>% 
  select (Subject, warmth = warmthtotal, punitive = punitivetotal, autonomy = autonomytotal,permissive = permissivetotal, democratic = demototal, anxious = anxioustotal)

describe(parenting)

chart.Correlation(parenting[2:7],use = "pairwise.complete.obs", method = "spearman", exact = FALSE)


# Children's behavior questionnaire ---------------------------------------

CBQ <- as_tibble(CBQ)

CBQ <- CBQ %>% 
  filter(Subject <= 32066, 
         Subject != 32026, 
         Subject != 32027, 
         Subject != 32032, 
         Subject != 32039, 
         Subject != 32040, 
         Subject != 32048, 
         Subject != 32059, 
         Subject !=32017
)

CBQ <- CBQ %>% 
  mutate(Q38R = 8 - Q38, Q47R = 8 - Q47, Q171R = 8- Q171, Q195R = 8 - Q195, Q6R = 8 - Q6, Q95R = 8 - Q95, Q184R = 8 - Q184, Q71R = 8 - Q71, Q79R = 8 - Q79, Q90R = 8 - Q90, Q137R = 8 - Q137, Q169R = 8 - Q169, Q183R = 8 - Q183)

focusing <- select(CBQ, Subject, Q16, Q38R, Q47R, Q125, Q144, Q160,Q171R,Q186,Q195R)

shifting <- select(CBQ, Subject, Q6R, Q29, Q95R, Q180, Q184R)

impulsivity <- select(CBQ, Subject, Q13, Q26, Q46, Q59, Q71R,Q79R, Q90R,
                      Q104, Q114, Q137R, Q155, Q169R, Q183R)

focusing <- focusing %>% 
  gather(`Q16`, `Q38R`, `Q47R`, `Q125`, `Q144`, `Q160`, `Q171R`, `Q186`, `Q195R`, key = "items", value = "focusing") %>% 
  group_by(Subject) %>% 
  summarise( focusingtotal= mean(focusing, na.rm = TRUE))

shifting <- shifting %>% 
  gather(`Q6R`, `Q29`, `Q95R`, `Q180`, `Q184R`, key = "items", value = "shifting") %>% 
  group_by(Subject) %>% 
  summarise(shiftingtotal = mean(shifting, na.rm = TRUE))

impulsivity <- impulsivity %>% 
  gather(`Q13`, `Q26`, `Q46`, `Q59`, `Q71R`, `Q79R`, `Q90R`,
         `Q104`, `Q114`, `Q137R`, `Q155`, `Q169R`, `Q183R`, key = "items", value = "impulsivity") %>% 
  group_by(Subject) %>% 
  summarise(impulsivitytotal = mean(impulsivity, na.rm = TRUE))

CBQ <- left_join(focusing, shifting, by = "Subject")
CBQ <- left_join(CBQ, impulsivity, by = "Subject")

CBQ <- CBQ %>% 
  select (Subject, cbqfocusing = focusingtotal, cbqshifting = shiftingtotal, cbqimpulsivity = impulsivitytotal)


# Merge data --------------------------------------------------------------
#export datafiles
write.csv(background, file = "background.csv", row.names = FALSE)
write.csv(parenting, file = "parenting.csv", row.names = FALSE)
write.csv(CBQ, file = "CBQ.csv", row.names = FALSE)
write.csv(trackit, file = "trackit.csv", row.names = FALSE)
write.csv(wordspan, file = "wordspan.csv", row.names = FALSE)
write.csv(forwardtap, file = "ftap.csv", row.names = FALSE)
write.csv(backwardtap, file = "btap.csv", row.names = FALSE)
write.csv(spaceship_median, file = "spaceship_median.csv", row.names = FALSE)
write.csv(spaceship_mdntgt, file = "spaceship_mdntgt.csv", row.names = FALSE)
write.csv(location, file = "location.csv", row.names = FALSE)

#merge data & export data

study2 <- left_join(background, parenting, by = "Subject")
study2 <- left_join(study2, CBQ, by = "Subject")
study2 <- left_join(study2, trackit, by = "Subject")
study2 <- left_join(study2, wordspan, by = "Subject")
study2 <- left_join(study2, forwardtap, by = "Subject")
study2 <- left_join(study2, backwardtap, by = "Subject")
study2 <- left_join(study2, spaceship_median, by = "Subject")

write.csv(study2, file = "study2.csv", row.names = FALSE)


# Spaceship task ----------------------------------------------------------

swm <- spaceship %>% 
  select(ExperimentName, Subject, Session, Delay, Trial,TargDeg, ABS_mm, errdir, errdis)

swm <- swm %>% 
  filter(Subject <= 32066, 
         Subject != 32026, 
         Subject != 32027, 
         Subject != 32032, 
         Subject != 32039, 
         Subject != 32040, 
         Subject != 32048, 
         Subject != 32059
)

swm5000 <- swm %>% 
  filter(Delay == 5000) %>%    
  filter(ABS_mm <= 203.7711)


swm5000 <- swm5000 %>% 
  mutate(errdir_new = case_when(TargDeg <0  ~ errdir*(-1), 
                                TargDeg > 0 ~ errdir))

location <- swm5000 %>% 
  select(ExperimentName, Subject, Session, Trial, TargDeg, ABS_mm, errdir = errdir_new)

location <- location %>%
  mutate(condition = stringr::str_sub(ExperimentName, 21, 21))

location <- location %>% 
  select(Subject, Session, Trial, TargDeg, ABS_mm, errdir, condition)

location$condition <- ifelse(location$condition == "L", 0, 1)

ggplot(data = swm5000) +
  geom_histogram(mapping = aes(x = errdir), binwidth = 0.5)

swm_descriptive <- swm %>% 
  group_by(TargDeg) %>% 
  summarise(errdir_mean = mean(errdir, na.rm = TRUE), 
            errdir_sd = sd(errdir, na.rm = TRUE), 
            errdir_min = min(errdir, na.rm = TRUE), 
            errdir_max = max(errdir, na.rm = TRUE), 
)

swm %>% 
  filter(Delay == 5000) %>% 
  summarise(ABS_mean = mean(ABS_mm, na.rm = TRUE), 
            ABS_sd = sd(ABS_mm, na.rm = TRUE)) 

swm5000 %>% 
  summarise(ABS_mean = mean(ABS_mm, na.rm = TRUE), 
            ABS_sd = sd(ABS_mm, na.rm = TRUE), 
            ABS_min = min(ABS_mm, na.rm = TRUE), 
            ABS_max = max(ABS_mm, na.rm = TRUE))


location_errabs <- location %>% 
  group_by(Subject) %>% 
  summarise(ABS_mean = mean(ABS_mm, na.rm = TRUE), 
            #ABS_sd = sd(ABS_mm, na.rm = TRUE), 
            #ABS_median = median(ABS_mm, na.rm = TRUE), 
            errdir_mean = mean(errdir, na.rm = TRUE), 
            #errdir_sd = sd(errdir, na.rm = TRUE),
            #errdir_median = median(errdir, na.rm = TRUE)
)#within individual variablity

study2 <- left_join(study2, location_errabs, by = "Subject")

res2 <- rcorr(as.matrix(study2))
res2
res2$r
res2$P
study2_r <- res2$r
study2_P <- res2$P
write.csv(study2_r, file = "study2_r.csv")
write.csv(study2_P, file = "study2_P.csv")

spaceship_median <- location %>% 
  group_by(Subject, condition) %>% 
  summarise(ABS_mean = mean(ABS_mm, na.rm = TRUE), 
            ABS_median = median(ABS_mm, na.rm = TRUE), 
            errdir_mean = mean(errdir, na.rm = TRUE), 
            errdir_median = median(errdir, na.rm = TRUE), 
            errdis_mean = mean(errdis, na.rm = TRUE), 
            errdis_median = median(errdis, na.rm = TRUE)
  )

spaceship_mdntgt <- location %>% 
  group_by(Subject, TargDeg, condition) %>% 
  summarise(ABS_mean = mean(ABS_mm, na.rm = TRUE), 
            ABS_median = median(ABS_mm, na.rm = TRUE), 
            errdir_mean = mean(errdir, na.rm = TRUE), 
            errdir_median = median(errdir, na.rm = TRUE), 
            errdis_mean = mean(errdis, na.rm = TRUE), 
            errdis_median = median(errdis, na.rm = TRUE)
  )

### spaceship data descriptive statistics
location_age <- location %>% 
  mutate(Target = case_when(TargDeg <0  ~ TargDeg*(-1),
                            TargDeg > 0 ~ TargDeg)
  ) %>% 
  select(Subject, Target, ABS_mm, errdir) %>% 
  group_by(Subject, Target) %>% 
  summarise(errdir = mean(errdir, na.rm = TRUE),
            ABS = mean(ABS_mm, na.rm = TRUE)
  ) 

location_study2 <- left_join(location_age, study2, by = "Subject") 

describe(location_study2, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE,omit=FALSE) 

location_study2$Target <- as.factor(location_study2$Target)


location_study2_age <- location_study2 %>% 
  mutate(agegroup = case_when( age < 3.78  ~ 1,
                               age < 4.11 & age >= 3.78 ~ 2,
                               age < 4.44 & age >= 4.11 ~ 3,
                               age >= 4.44 ~ 4)
  )

location_study2_age$agegroup <- as.factor(location_study2_age$agegroup)

ggplot(data = location_study2_age) + 
  geom_smooth(mapping = aes(x = democratic, y = errdir, color = Target))+
  facet_wrap(~ agegroup, nrow = 2)


# ---plot for errdir & ABS by target location ------------------------------

spaceship_mdntgt <- location %>% 
  group_by(Subject, TargDeg) %>% 
  summarise(ABS_mean = mean(ABS_mm, na.rm = TRUE), 
            errdir_mean = mean(errdir, na.rm = TRUE)
  )

spaceship_mdntgt <- spaceship_mdntgt %>% 
  mutate(Target = case_when(TargDeg <0  ~ TargDeg*(-1), 
                            TargDeg > 0 ~ TargDeg)
  )

spaceship_mdntgt$Target <- as.factor(spaceship_mdntgt$Target)


ggplot(data = spaceship_mdntgt, mapping = aes(x = Target, y = errdir_mean)) + 
  geom_boxplot() + # for errdir_mean
  theme_bw() + theme(panel.border = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


ggplot(data = spaceship_mdntgt, mapping = aes(x = Target, y = ABS_mean)) +
  geom_boxplot() + ## for ABS_mean
  theme_bw() + theme(panel.border = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



df <- spaceship_tgt %>% 
  group_by(TargDeg) %>% 
  summarise(ABS = mean(ABS_mean, na.rm = TRUE), errdir = mean(errdir_mean, na.rm = TRUE), errdis = mean(errdis_mean, na.rm = TRUE))

barplot(errdir ~ TargDeg, data = df)
barplot(ABS ~ TargDeg, data = df)

spaceship_mdntgt_wide <- spaceship_mdntgt %>% 
  mutate(Target = case_when(TargDeg <0  ~ TargDeg*(-1), 
                            TargDeg > 0 ~ TargDeg))

spaceship_mdntgt_wide %>% 
  pivot_wider(names_from = Target, values_from = ABS_median)


# -- ABS by TargDeg --------------------------------------------

df3 <- spaceship_mdntgt_wide %>% 
  pivot_wider(names_from = Target, values_from = ABS_mean)

df3 <- df3 %>% 
  select(Subject, `60`, `40`, `20`,`10`, `5`)

df3 <- df3 %>% 
  group_by(Subject) %>% 
  summarise(ABS_m_60 = mean(`60`, na.rm = TRUE),ABS_m_40 = mean(`40`, na.rm = TRUE),ABS_m_20 = mean(`20`, na.rm = TRUE),ABS_m_10 = mean(`10`, na.rm = TRUE),ABS_m_5 = mean(`5`, na.rm = TRUE))

ABS_mean <- df3


# -- errdir by TargDeg ----------------------------------------------------

df4 <- spaceship_mdntgt_wide %>% 
  pivot_wider(names_from = Target, values_from = errdir_mean)

df4 <- df4 %>% 
  select(Subject, `60`, `40`, `20`,`10`, `5`)

df4 <- df4 %>% 
  group_by(Subject) %>% 
  summarise(errdir_m_60 = mean(`60`, na.rm = TRUE),errdir_m_40 = mean(`40`, na.rm = TRUE),errdir_m_20 = mean(`20`, na.rm = TRUE),errdir_m_10 = mean(`10`, na.rm = TRUE),errdir_m_5 = mean(`5`, na.rm = TRUE))

errdir_mean <- df4


# --- correlation betwen variables ----------------------------------------

swmtarget <- left_join(ABS_mean, errdir_mean, by = "Subject")

write.csv(swmtarget, file = "swmtarget.csv", row.names = FALSE)

Study2 <- left_join(Study2, swmtarget, by = "Subject")

chart.Correlation(Study2[2:7],use = "pairwise.complete.obs", method = "spearman", exact = FALSE)


# Multiple Linear Regression Models ---------------------------------------

# linear regression
# wordspan
study2 <- read.csv("E:/Dissertation/Study 2/Data for HLM ERRDIR no transformation/study2.csv")

wordspan1 <- lm(wordspan~ age + ratio, data = study2)
summary(wordspan1)

wordspan2 <- lm(wordspan~ age + warmth + autonomy + democratic +  cbqfocusing + cbqshifting + trackit_mean, data = study2)
summary(wordspan2)

# ftapsum
ftapsum1 <- lm(ftapsum ~ age + ratio, data = study2)
summary(ftapsum1)

ftapsum2 <- lm(ftapsum ~ age + warmth + autonomy + democratic +  cbqfocusing + cbqshifting + trackit_mean, data = study2)
summary(ftapsum2)

# btapsum
btapsum1 <- lm(btapsum ~ age + ratio, data = study2)
summary(btapsum1)

btapsum2 <- lm(btapsum ~ age + warmth + autonomy + democratic + cbqfocusing + cbqshifting + trackit_mean, data = study2)
summary(btapsum2)


# Dummy code: TargDeg -----------------------------------------------------

location <- location %>% 
  mutate(target = case_when(TargDeg <0  ~ TargDeg*(-1), 
                            TargDeg > 0 ~ TargDeg))


location$target60 <- ifelse(location$target == "60", 1, 0)
location$target40 <- ifelse(location$target == "40", 1, 0)
location$target10 <- ifelse(location$target == "10", 1, 0)
location$target5 <- ifelse(location$target == "5", 1, 0)


