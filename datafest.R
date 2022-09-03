### Setup
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(ggfortify)
library(Hmisc)

setwd("/Users/shinsukeadachi/Desktop/Datafest/data files/")
df <- read_csv("logs.csv",guess_max=2106600)

player1 <- read_csv("player-6427031.csv", guess_max = 7332)
player2 <- read_csv("player-6486029.csv")
survey <- read_csv("S5_scores_cleaned.csv")

df  <- subset(df, select =c(player_id,event_id,event_description,event_time, event_time_dbl ) )
newdf <- df %>% 
  mutate (timedif = event_time_dbl - lag(event_time_dbl))

player1 <- subset(player1, select =c(player_id,event_id,event_description,event_time, event_time_dbl ) ) %>%
  mutate (timedif = event_time_dbl - lag(event_time_dbl))
player2 <- subset(player2, select =c(player_id,event_id,event_description,event_time, event_time_dbl ) ) %>%
  mutate (timedif = event_time_dbl - lag(event_time_dbl))

### enter the data for minigame_id based on event_id
#df$minigame_id <- if((df$event_id>=400)&(df$event_id<=420), ')


### Summary
summary(df)  
# get a random small sample of size 1000
set.seed(111)
subset1<-df[sample(1:2106597, 1000, replace = F), ]
summary(subset1)


subset1_total_time <- subset1 %>%
  group_by(player_id) %>%
  summarise(total_time = max(event_time_dbl)/3600) %>%
  as.data.frame()

summary(survey)
boxplot(survey$S5_mean)
survey[survey$S5_mean==1.3,]

lowest_s5 <- df[df$player_id==6486018,]
lowest_s5 %>% 
  summarize(playtime = max(event_time_dbl))

player1[player1$event_id==500,] %>%
  group_by(event_id) %>%
  count() %>%
  as.data.frame() 

player2[player2$event_id==500,] %>%
  group_by(event_id) %>%
  count() %>%
  as.data.frame() 

event500 <- df[df$event_id == 500,] %>%
  group_by(player_id) %>%
  count() %>%
  as.data.frame()

hist(event500$n)

knowledge <- df %>% 
  filter(event_id == 419) %>%
  count(player_id) 

min(knowledge$n)
max(knowledge$n)
hist(knowledge$n)
boxplot(knowledge$n)
mean(knowledge$n)


player1_knowledge <- player1 %>%
  filter((event_id >= 400) & (event_id <=420)) %>%
  count(event_id) 

player2_knowledge <- player2 %>%
  filter((event_id >= 400) & (event_id <=420)) %>%
  count(event_id) 


barplot(player1_knowledge$n)
barplot(player2_knowledge$n)

hist.data.frame(player1_knowledge)

### For each player, subset the log for knowledge 
player1 %>%
  filter((event_id >= 400) & (event_id <=420)) %>%
  count(event_id) %>%
  view()

player2 %>%
  filter((event_id >= 400) & (event_id <=420)) %>%
  count(event_id) %>%
  view()


player1 %>%
  filter(event_id == 420) %>%
  count()
player2 %>%
  filter(event_id == 420) %>%
  count()

df %>%
  filter(event_id == 420) %>%
  count(player_id) %>%
  ggplot(aes(n)) + geom_bar()


player1_sub <- player1[,c("row_id", "event_id", "event_time_dbl")] %>%
  filter((event_id >= 400) & (event_id <=420))
player2_sub <- player2[,c("row_id", "event_id", "event_time_dbl")] %>%
  filter((event_id >= 400) & (event_id <=420))

player1 %>%
  filter(event_id == c(402,403,404)) %>%
  nrow()

player2 %>%
  filter(event_id == c(402,403,404)) %>%
  nrow()

clicks <- df %>%
  filter(event_id == c(402,403,404)) %>%
  count(player_id) 
mean(clicks$n)  
boxplot(clicks$n)

### How much time each player spends on minigames
minigame_time <- newdf %>%
  filter((event_id >= 400 & event_id <=420) |
         (event_id >= 500 & event_id <=515) |
         (event_id >= 800 & event_id <=818) |
         (event_id >= 900) & (event_id <=91)) %>%
  group_by(player_id) %>%
  summarise(total_time = sum(timedif)/3600) 
  
knowledge_time <- newdf %>%
  filter((event_id >= 400 & event_id <=420)) %>%
  group_by(player_id) %>%
  summarise(total_time = sum(timedif)/3600) 

refuse_time <- newdf %>%
  filter((event_id >= 500 & event_id <=515)) %>%
  group_by(player_id) %>%
  summarise(total_time = sum(timedif)/3600) 

priority_time <- newdf %>%
  filter((event_id >= 800 & event_id <=818)) %>%
  group_by(player_id) %>%
  summarise(total_time = sum(timedif)/3600) 

people_time <- newdf %>%
  filter((event_id >= 900 & event_id <=912)) %>%
  group_by(player_id) %>%
  summarise(total_time = sum(timedif)/3600) 

minigame_time <- merge(x=knowledge_time,y=refuse_time,by="player_id",all=TRUE)
minigame_time <- merge(x=minigame_time, y=priority_time,by="player_id",all=TRUE)
minigame_time <- merge(x=minigame_time, y=people_time,by="player_id",all=TRUE)
colnames(minigame_time) <- c('player_id','knowledge_time', 'refuse_time','priority_time','people_time')

minigame_time[is.na(minigame_time)] = 0

minigame_time$total_time <- minigame_time$knowledge_time + 
  minigame_time$refuse_time + minigame_time$priority_time + minigame_time$people_time

minigame_time %>%
  ggplot(aes(total_time)) + geom_histogram(bins=10)

p1 <- minigame_time %>%
  ggplot(aes(knowledge_time)) + geom_histogram(bins=10)
p2 <- minigame_time %>%
  ggplot(aes(refuse_time)) + geom_histogram(bins=10)
p3 <- minigame_time %>%
  ggplot(aes(priority_time)) + geom_histogram(bins=10)
p4 <- minigame_time %>%
  ggplot(aes(people_time)) + geom_histogram(bins=10)

grid.arrange(p1, p2,p3,p4, nrow=2)





### Survey data 
survey_df <- survey %>%
  group_by(player_id) %>%
  summarise(max = max(S5_mean), min = min(S5_mean), mean = mean(S5_mean))

### Join survey data and minigame time data
time_survey <- merge(minigame_time_2, survey_df, by = 'player_id', all = FALSE)

### Run regression max/min survey score on total time 
lm_max <- lm(max ~ total_time, data=time_survey)
summary(lm_max)
time_survey %>%
  ggplot(aes(x=total_time, y=max)) + geom_smooth(method = 'lm')

lm_min <- lm(min ~ total_time, data=time_survey)
summary(lm_min)
time_survey %>%
  ggplot(aes(x=total_time, y=min)) + geom_smooth(method = 'lm')

lm_avg <- lm(mean ~ total_time, data=time_survey)
summary(lm_avg)
time_survey %>%
  ggplot(aes(x=total_time, y=mean)) + geom_smooth(method = 'lm')


### Join Latifa's data. Cleaning
minigame_time_merged <- log_merge
minigame_time_merged$total_fail <- minigame_time_merged$n3starsPeople + minigame_time_merged$nFailRefuse

minigame_time_merged2 <- na.omit(minigame_time_merged)

### K-means Clustering
kmean_model <- stats::kmeans(minigame_time_merged2[9:10], 2)
autoplot(kmean_model, minigame_time_merged2[9:10], frame = TRUE)

kmean_model <- stats::kmeans(minigame_time_merged2[9:10], 3)
autoplot(kmean_model, minigame_time_merged2[9:10], frame = TRUE)



#stats::kmeans(minigame_time, centers = 5, nstart = 10)
#pairs(minigame_time[, -5], col = minigame_time[, 5], pch = 19)
#minigame_pca <- prcomp(minigame_time[, -5])
#biplot(minigame_pca)

library(cluster)
x <- minigame_time_merged2[c(2,9:10)]
withinss <- 0

for (i in 1:10){
  model=kmeans(x,i)
  autoplot(model, x, frame = TRUE)
  withinss[i] <- model$tot.withinss 
}

plot(1:10, withinss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares",
     main = "Scree Plot")

model <- list()
model[[1]]=kmeans(x,2)
clusplot(x,model[[1]]$cluster)
clusplot(x,model[[1]]$cluster,color=T,shade=T)
autoplot(model[[1]], x, frame = TRUE)

model[[2]]=kmeans(x,3)
clusplot(x,model[[2]]$cluster)
clusplot(x,model[[2]]$cluster,color=T,shade=T)
autoplot(model[[2]], x, frame = TRUE)

x2 <- minigame_time_merged2 %>%
  mutate(clust2cl = as.factor(model[[1]]$cluster), clust3cl = as.factor(model[[2]]$cluster))

### EDA of Clustering

## k=2
x2 %>%
  group_by(clust2cl) %>%
  summarise(n = n(), 
            total_time = mean(total_time),
            total_fail = mean(total_fail),
            knowledge = mean(knowledge_time),
            refuse = mean(refuse_time),
            priority = mean(priority_time),
            people = mean(people_time)) 

boxplot(x2$knowledge_time ~  as.factor(x2$clust2cl), main = 'knowledge')
boxplot(x2$refuse_time ~  as.factor(x2$clust2cl), main = 'refuse')
boxplot(x2$priority_time ~  as.factor(x2$clust2cl), main = 'priority')
boxplot(x2$people_time ~  as.factor(x2$clust2cl), main = 'people')
boxplot(x2$total_time ~  as.factor(x2$clust2cl), main = 'total time')
boxplot(x2$total_fail ~  as.factor(x2$clust2cl), main = 'total failures')

# k=3

x2 %>%
  group_by(clust3cl) %>%
  summarise(n = n(), 
            total_time = mean(total_time),
            total_fail = mean(total_fail),
            knowledge = mean(knowledge_time),
            refuse = mean(refuse_time),
            priority = mean(priority_time),
            people = mean(people_time)) 

boxplot(x2$knowledge_time ~  as.factor(x2$clust3cl), main = 'knowledge')
boxplot(x2$refuse_time ~  as.factor(x2$clust3cl), main = 'refuse')
boxplot(x2$priority_time ~  as.factor(x2$clust3cl), main = 'priority')
boxplot(x2$people_time ~  as.factor(x2$clust3cl), main = 'people')
boxplot(x2$total_time ~  as.factor(x2$clust3cl), main = 'total time')
boxplot(x2$total_fail ~  as.factor(x2$clust3cl), main = 'total failures')



minigame_time_2 <- minigame_time %>%
  mutate(clust2cl = model[[1]]$cluster, clust3cl = model[[2]]$cluster)


