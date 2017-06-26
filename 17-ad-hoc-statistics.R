
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





library(tidyverse) #include ggplot2, dyplyr, readr, tydyr
library(dygraphs) # dynamic graph
library(xts)

#represented deplorables
followers_info <- read.csv("../results_following/followers_with_cluster_info.csv")
cluster_info <- read.csv("../combined_data/cluster_summary.csv")
levels(cluster_info$clust_cat)[1] <- "men's interest"

desired_order<- c('Trump supporters','conservatives','liberals', "men's interest",
                  'news&regional', 'non-political','other countries','international')
cluster_info$clust_cat <- match(cluster_info$clust_cat, desired_order)
cluster_info$clust_cat <- factor(as.factor(cluster_info$clust_cat), 1:8, desired_order)

deplorable <- cluster_info[,c(1:6,11:20)]
vars = names(deplorable)
vars

pdf("deplorables.pdf")
deplorable %>% ggplot( aes(clust_cat, jitter(Cernovich.222952, amount = 0.005) ) ) + 
    geom_point(shape = "x", aes(color = period))
deplorable  %>% ggplot( aes(clust_cat, jitter(DrDavidDuke.34240, amount = 0.005) ) ) + 
    geom_point(shape = "x", aes(color = period))    
deplorable  %>% ggplot( aes(clust_cat, jitter(RichardBSpencer.52903, amount = 0.005) ) ) + 
    geom_point(shape = "x", aes(color = period))  
deplorable  %>% ggplot( aes(clust_cat, jitter(PrisonPlanet.546402, amount = 0.005) ) ) + 
    geom_point(shape = "x", aes(color = period))  
deplorable  %>% ggplot( aes(clust_cat, jitter(RealAlexJones.593708, amount = 0.005) ) ) + 
    geom_point(shape = "x", aes(color = period)) 
deplorable  %>% ggplot( aes(clust_cat, jitter(BreitbartNews.670268, amount = 0.005) ) ) + 
    geom_point(shape = "x", aes(color = period)) 
deplorable  %>% ggplot( aes(clust_cat, jitter(ramzpaul.32979, amount = 0.005) ) ) + 
    geom_point(shape = "x", aes(color = period)) 
deplorable  %>% ggplot( aes(clust_cat, jitter(Suthen_boy.54734, amount = 0.005) ) ) + 
    geom_point(shape = "x", aes(color = period)) 
deplorable  %>% ggplot( aes(clust_cat, jitter(LadyAodh.23573, amount = 0.005) ) ) + 
    geom_point(shape = "x", aes(color = period)) 
dev.off()


# entering top 10  twice
ids = NULL
for (i in 11:20){
    idx <- which(cluster_info[,i] > sort(cluster_info[,i])[140])
    ids <- c(ids, idx)
}
counts = table(ids)
counts = counts[counts >= 9]

cluster_info$period[as.integer(names(counts))]
cluster_info$clust_cat[as.integer(names(counts))]


deplorable_ids = as.integer(names(counts))




selected_tweets1 <- read.csv("../results_following/result1/most_freq_tweets.csv", 
                            colClasses = c("character"))
selected_tweets1$cluster <- as.integer(selected_tweets1$clusterid)+100
selected_tweets2 <- read.csv("../results_following/result2/most_freq_tweets.csv", 
                            colClasses = c("character"))
selected_tweets2$cluster <- as.integer(selected_tweets2$clusterid)+50
selected_tweets3 <- read.csv("../results_following/result3/most_freq_tweets.csv", 
                            colClasses = c("character"))
selected_tweets3$cluster <- as.integer(selected_tweets3$clusterid)

selected_tweets <- rbind(selected_tweets3, selected_tweets2, selected_tweets1)
deplorables_tweets <- selected_tweets[selected_tweets$cluster %in% deplorable_ids, ] 
cc <- table(deplorables_tweets$Text)
cc[cc>2]


library(ggplot2)
library(ggradar)
suppressPackageStartupMessages( library(dplyr) )
library(scales)

mtcars_radar <- mtcars %>%
    rownames_to_column( var = "group" ) %>%
    mutate_each( funs(rescale), -group) %>%
    tail(4) 

ggradar(mtcars_radar, font.radar="Times",axis.label.size=4, 
        group.point.size=2, group.line.width=0.7,grid.label.size=4,grid.line.width=0.4,legend.text.size=8)





rm(list = ls())
library(Matrix)
library(xlsx)
library(igraph)

followers_info <- read.csv("../results_following/followers_with_cluster_info.csv", colClasses = c("character"))
##   remove missing cluster == NA
followers <- subset(followers_info, !is.na(cluster) )
dim(followers)#322461 rows  
#377725 = 324964 following >1,  failed: 52510, zeros: 251
#further due to followers very few people, < 10; 


#correct variable types

#factor variable.
followers_info$cluster <- as.factor(as.numeric(followers_info$cluster))

#data time
followers_info$created_at <- date <- as.POSIXct(followers_info$created_at, format = "%a %b %d %H:%M:%S %z %Y")
followers_info$first_date_timeline <- as.Date(followers_info$first_date_timeline)
followers_info$last_date_timeline <- as.Date(followers_info$last_date_timeline)

#non-numerical
non_numerical_cols <- c("period","cluster", "clust_label","clust_cat","id_str", "screen_name", "name", 
                        "description","created_at", "lang",
                        "location", "time_zone", "verified", "first_date_timeline","last_date_timeline")
non_numerical_ids <- match(non_numerical_cols, names(followers_info))

for( i in 1:ncol(followers_info)){
    if (!(i %in% non_numerical_ids)){
        followers_info[,i] <- as.numeric(followers_info[,i]) 
    }
}
str(followers_info)#377725


#missing values:
followers_info$retweet_trump_count[is.na(followers_info$retweet_trump_count)] <- 0

#data time
followers_info$created_at <- as.POSIXct(followers_info$created_at, format = "%a %b %d %H:%M:%S %z %Y")
followers_info$first_date_timeline <- as.Date(followers_info$first_date_timeline)
followers_info$last_date_timeline <- as.Date(followers_info$last_date_timeline)

#factor 
followers_info$cluster <- as.factor(followers_info$cluster)





#11k, or 3% followers within some clusters, don't have tweets, 
cat ( sum(is.na(followers$count_mean_timeline))/nrow(followers), "of followers have no tweets")
#0.03417778




f1 <- read.csv("../results_following/followers_with_cluster_info.csv", colClasses = c("character"))
f2 <- read.csv("../results_following/followers_with_cluster_info2.csv", colClasses = c("character"))


sns <- f1$screen_name[c(n1+n2+(1:n3),n1+(1:n2),(1:n1))]
library(tidyverse)
pagerank1 <- data.frame(screen_name = sns) %>% 
    left_join(f1[,c("screen_name", "pg1", "pg1_personalized")], by = "screen_name")
f1$pg1 <- pagerank1$pg1
f1$pg1_personalized <- pagerank1$pg1_personalized
f1 <- f1[,-27]    
write.csv(f1, file = "../results_following/followers_with_cluster_info.csv", row.names = F)





