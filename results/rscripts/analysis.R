library(ggplot2)
library(tidyverse)
library(lme4)
library(lmerTest)


# set  results/  to be the working directory
setwd("../")

filename1 <- "helloworld-1"
df1 <- read.csv(paste("../data/trimmed_", filename1, ".csv", sep=""))

filename2 <- "helloworld-2"
df2 <- read.csv(paste("../data/trimmed_", filename2, ".csv", sep=""))

filename3 <- "helloworld-3"
df3 <- read.csv(paste("../data/trimmed_", filename3, ".csv", sep=""))

filename4 <- "helloworld-4"
df4 <- read.csv(paste("../data/trimmed_", filename4, ".csv", sep=""))

df_all4 <- bind_rows(df1, df2, df3, df4)

#================= plots ==============================
theme_set(theme_bw() )

plotFullPanels <- function(df){
  proplook_by_nth <- df %>% group_by(NthAfterAdj, AdjectiveType, Condition) %>%
    summarise(Target = (prop.table(table(LookedType))["Target"]), 
              Competitor = (prop.table(table(LookedType))["Competitor"]), 
              Contrast = (prop.table(table(LookedType))["Contrast"]), 
              Distractor = (prop.table(table(LookedType))["Distractor"])) %>% 
    gather(LookedType, PropLook, -NthAfterAdj, -AdjectiveType, -Condition) %>% arrange(NthAfterAdj) %>% 
    mutate(TimeAfterAdj = 1000/60 * (NthAfterAdj -1))
  
  ggplot(proplook_by_nth %>% filter(LookedType %in% c("Target", "Competitor")), aes(x=TimeAfterAdj, y=PropLook, color=LookedType)) + 
    geom_line() +
    facet_grid( Condition ~ AdjectiveType) +
    ylim(0,1) + geom_vline(xintercept = 650, linetype="dotted") +
    xlab("time after adjective onset (ms)") + 
    ylab("proportion of looks")
}



plotFullPanels(df1)
plotFullPanels(df2)
plotFullPanels(df3)
plotFullPanels(df4)
plotFullPanels(df_all4)




# statistical analyses
df <- df_all4

proptarget <- df %>% filter(NthAfterAdj >= 13 & NthAfterAdj <= 57) %>%
  mutate(window = (NthAfterAdj - 13) %/% 9) %>%
  mutate(LookingTarget = (LookedType == "Target"))

m.norandom0 <- glm(LookingTarget ~ AdjectiveType * Condition, 
                 data=proptarget %>% filter(window == 0), family="binomial")
summary(m.norandom0)

m.random0 <- glmer(LookingTarget ~ AdjectiveType * Condition + (1|Subject) + (1|SceneID) , 
                  data=proptarget %>% filter(window == 0), family="binomial")
summary(m.random0)

m.norandom1 <- glm(LookingTarget ~ AdjectiveType * Condition, 
                  data=proptarget %>% filter(window == 1), family="binomial")
summary(m.norandom1)

m.random1 <- glmer(LookingTarget ~ AdjectiveType * Condition + (1|Subject) + (1|SceneID) , 
                  data=proptarget %>% filter(window == 1), family="binomial")
summary(m.random1)

m.norandom2 <- glm(LookingTarget ~ AdjectiveType * Condition, 
                  data=proptarget %>% filter(window == 2), family="binomial")
summary(m.norandom2)

m.random2 <- glmer(LookingTarget ~ AdjectiveType * Condition + (1|Subject) + (1|SceneID) , 
                  data=proptarget %>% filter(window == 2), family="binomial")
summary(m.random2)


m.norandom3 <- glm(LookingTarget ~ AdjectiveType * Condition, 
                  data=proptarget %>% filter(window == 3), family="binomial")
summary(m.norandom3)

m.random3 <- glmer(LookingTarget ~ AdjectiveType * Condition+ (1|Subject) + (1|SceneID) , 
                  data=proptarget %>% filter(window == 3), family="binomial")
summary(m.random3)

m.norandom4 <- glm(LookingTarget ~ AdjectiveType * Condition, 
                  data=proptarget %>% filter(window == 4), family="binomial")
summary(m.norandom3)

m.random4 <- glmer(LookingTarget ~ AdjectiveType * Condition + (1|Subject) + (1|SceneID) , 
                  data=proptarget %>% filter(window == 4), family="binomial")
summary(m.random4)


# inspect click data

# participant 1's click data are unreliable due to a bug in the experiment which was later corrected

df2.click <- unique(df2[, c("Subject", "Session", "TrialId", "SoundFile", "Condition",   
                            "AdjectiveType", "SceneID", "ClickedType", "RT",
                            "Target", "Competitor", "Contrast",  "Distractor", 
                            "AOI1", "AOI2",   "AOI3",   "AOI4", "ClickedObj")])

df3.click <- unique(df3[, c("Subject", "Session", "TrialId", "SoundFile", "Condition",   
                            "AdjectiveType", "SceneID", "ClickedType", "RT",
                            "Target", "Competitor", "Contrast",  "Distractor", 
                            "AOI1", "AOI2",   "AOI3",   "AOI4", "ClickedObj")])

df4.click <- unique(df4[, c("Subject", "Session", "TrialId", "SoundFile", "Condition",   
                            "AdjectiveType", "SceneID", "ClickedType", "RT",
                            "Target", "Competitor", "Contrast",  "Distractor", 
                            "AOI1", "AOI2",   "AOI3",   "AOI4", "ClickedObj")])

