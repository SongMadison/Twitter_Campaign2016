
rm(list =ls())

library(tidyverse) #include ggplot2, dyplyr, readr, tydyr
library(dygraphs) # dynamic graph
library(xts)
theme_update(plot.title = element_text(hjust = 0.5))


retweets0 <- read.csv("../data/trump_hadoop1/trumpRT.csv", sep = '\t', colClasses = c("character"))
ids <- readLines("../data/friends_info/edgelist_Feb27/ids_13M_cleaned.txt")
retweets0$rt.user.id_str

#missing
sum(retweets0$rt.id_str == '\\N') # 855 don't have a retweets

retweets <- subset(retweets0, rt.id_str != '\\N')


idx = match(retweets$user.id_str, ids)
sum(!is.na(idx))/nrow(retweets)



## histograms of follower_count


followers <- read.csv("../data/friends_info/edgelist_Feb27/all_followers_idsn.csv", colClasses = c("character"))
i1 = 3130834
i2 = 9902771
followers$followers_count <- as.numeric(followers$followers_count)
followers = followers %>% mutate(log_followers_count = log10(1+followers_count))
p1 <- followers[1:i1,] %>% ggplot(aes(followers_count)) + geom_histogram(bins =1000) # not good
p1 <- followers[1:i1,]  %>% ggplot(aes(log_followers_count)) + geom_histogram(bins = 100)
p2 <- followers[(i1+1):i2,]  %>% ggplot(aes(log_followers_count)) + geom_histogram(bins = 100)
p3 <- followers[(i2+1):nrow(followers),]  %>% ggplot(aes(log_followers_count)) + geom_histogram(bins = 100)


samp <- read.csv("../data/friends_info/edgelist_Feb27/all_samp_info.csv", colClasses = c("character"))

#samp <-read.csv("/data/SongWang/Twitter_Campaign2016/data/friends_info/edgelist_Feb27/all_samp_info.csv", colClasses = c("character"))
samp$followers_count <- as.numeric(samp$followers_count)
samp = samp %>% mutate(log_followers_count = log10(1+followers_count))

ii1 = 60955
ii2 = 60955+191812
pp1 <- samp[1:ii1,]  %>% ggplot(aes(log_followers_count)) + geom_histogram(bins = 100)
pp2 <- samp[(ii1+1):ii2,]  %>% ggplot(aes(log_followers_count)) + geom_histogram(bins = 100)
pp3 <- samp[(ii2+1):nrow(samp),]  %>% ggplot(aes(log_followers_count)) + geom_histogram(bins = 100)



pdf(file = "histograms_followers_count.pdf",width = 8, height = 6)
p3 + ggtitle("population followers_count for stage 1")
pp3 + ggtitle("sample followers_count for stage 1")

p2 + ggtitle("population followers_count for stage 2")
pp2 + ggtitle("sample followers_count for stage 2")

p1 + ggtitle("population followers_count for stage 3")
pp1 + ggtitle("sample followers_count for stage 3")
dev.off()



