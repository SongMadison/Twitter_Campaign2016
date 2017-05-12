

library(xlsx)
segments_with_label <- read.xlsx('../results_following/Trump_followers_three_stages.xlsx', 
                                 colClasses = c("character"),
                                 sheetIndex = 8)
segements_labels <- segments_with_label$cluster_label
followers <- NULL
followers_info <- read.csv("../results_following/result3/followers_info_upated.csv", 
                           colClasses = c("character"))
followers_info$cluster <- as.integer(followers_info$cluster)
followers <- rbind(followers, followers_info)
followers_info <- read.csv("../results_following/result2/followers_info_upated.csv", 
                           colClasses = c("character"))
followers_info$cluster <- as.integer(followers_info$cluster)+50
followers <- rbind(followers, followers_info)
followers_info <- read.csv("../results_following/result1/followers_info_upated.csv", 
                           colClasses = c("character"))
followers_info$cluster <- as.integer(followers_info$cluster)+100
followers <- rbind(followers, followers_info)
followers$cluster_label <- segements_labels[followers$cluster]

colnames(followers)
followers_final <- followers[,c(1:14,15,16,70,17:69)]


#60955, 191811, 124959 
write.csv(followers_final, file ="../results_following/followers_with_cluster_info.csv", row.names = F)





#####---################
followers_final <- read.csv("../results_following/followers_with_cluster_info.csv", 
                            colClasses = c("character"))
colnames(followers_final)
for(i in 21: 70) {
  followers_final[,i] <- as.integer(followers_final[,i])
}
dim(followers_final)

#cluster is NA
idx_cluster_NA <- which(is.na(followers_final$cluster))
length(idx_cluster_NA)
followers_info <- followers_final[-idx_cluster_NA,]
dim(followers_info)
followers_info$time_order <- as.numeric(followers_info$time_order)
followers_info$retweet_trump_count   <- as.numeric(followers_info$retweet_trump_count)
followers_info$cluster <- as.factor(followers_info$cluster)
followers_info$cluster <- factor(followers_info$cluster,levels = 1:150, labels = 1:150)
dim(followers_info)

f1 <- followers[,1:20]
colnames(f1)
f2 <- followers[,21:70]
users_info <- read.csv("../results_following/user_information.csv", colClasses = c("character"))
colnames(users_info)

##
a<- match( 1:150, followers_info$cluster)
clusters <- followers[a,]
cluster_info <- clusters[,c("cluster", "cluster_label")]
cluster_info$cluster_size <- table(followers_info$cluster)[1:150]


# time order in each cluster
lm.timeorder <- lm(time_order ~ cluster -1, data = followers_info)
lm.timeorder$coefficients
plot(1:150, lm.timeorder$coefficients/1e6)
cluster_info$'time_order-million' <- lm.timeorder$coefficients/1e6



## some other cluster features -- id, clustersize, trump_Ratio, top40(friends screen-names)
k = 50; top =40
sns <- character(50)
for (i in 1:k) {
  sns[i] <- paste0(keyfriends$screen_name[1:top+(i-1)*top],collapse = ',')
}
cluster_info$'keyfriends_sns' <- sns


## add retweet count - over time each month
lm.trump <- lm(retweet_trump ~ cluster -1, data = followers_info)
lm.trump$coefficients
plot(1:50, lm.trump$coefficients)
cluster_info$'retweet-count' <- lm.trump$coefficients

## add retweet retweet ratio
cluster_x_retweet <- table(followers_info$cluster, !is.na(followers_info$retweet_trump))
cluster_info$'retweet-per-person' <- round( cluster_x_retweet[,"TRUE"]/rowSums(cluster_x_retweet),digits = 3)
plot(cluster_info$'retweet-per-person')

# time of the retweets
# partition of tweets based on the following clusters
retweet_cluster <- followers_info$cluster[match(rownames(A), followers_info$id_str)]
table(retweet_cluster, exclude = NULL)
A1 <- A[!is.na(retweet_cluster),]; #remove followers belong to 'NA'
retweet_cluster <- as.numeric(retweet_cluster)[!is.na(retweet_cluster)]
length(retweet_cluster)
dim(A1)
Z <- membershipM(retweet_cluster)

#time_id - year-month
year.months <- gsub("^(\\d{4}-\\d{2}).*","\\1", tweets$created_at)
label.year.month <- unique(year.months)
# year.month.ids <- match(year.months, label.year.month)
# year.months[is.na(year.month.ids)] <- "2015-00"; table(year.months)
# label.year.month <- c("2015-00",sort(label.year.month))
time_cluster <- match(year.months,label.year.month)
Y <- membershipM(time_cluster)

#cluster x year.month
clus_x_year.month <- t(Z) %*% A1 %*% Y[,ncol(Y):1]
clus_x_year.month <- as.matrix(clus_x_year.month )
colnames(clus_x_year.month) <- paste0('Y-',label.year.month[ncol(Y):1])
cluster_info <- data.frame(cluster_info, data.frame(clus_x_year.month))

write.csv(cluster_info, file = paste0(ResultPath, "cluster_summary.csv"), row.names = F)


#cluster x tweets  - > most frequent tweets
freqZ <- t(Z) %*% A1
ids <- apply(freqZ, 1, function(x) order(-x)[1:10])
ids <- as.vector(ids)
freq_tweets <- data.frame(clusterid = rep(1:50, each =10),clust_size = rep(table(retweet_cluster), each =10), tweets[ids,])
write.csv(freq_tweets, file =paste0(ResultPath,"most_freq_tweets.csv"), row.names = F)




segment_name_file <- read.xlsx("../results_following/Trump_followers_three_stages.xlsx", 
                               sheetIndex = 8)
category_names <- segment_name_file$cluster_label
tmp <- unique(category_names)
category_names[category_names %in% tmp [c(2,3,5,6,7,10,11,15,16,17,20,22,25,26,28,29,30,31)]] <-"other countries" 
unique(category_names)

category_names <- c("Trump supporters",   "conservatives", "anti-feminist conservatives", 
                    "liberals", "NRA members", "far left", "non-political","religious",
                    "journalists","Miami", "California tech scene","news junkies" ,
                    "other countries","mixed")                                                       

cluster_info <- read.csv("../results_following/result3/cluster_summary.csv", colClasses = c("character"))
cluster_info$cluster_name <- segment_name_file$Pre
cluster_info$category_name <- category_names[1:50]
cluster_info <- cluster_info[,c(1,ncol(cluster_info)-1,ncol(cluster_info), 2:(ncol(cluster_info)-2))]
write.csv(cluster_info, file ="../results_following/result3/cluster_summary.csv", row.names = F)




dim(users_info)

rm(list = ls())
source("function.R")
## followers_info, cluster_followers
library(xlsx)
followers_info <- read.csv("../results_following/result1/followers_info.csv", 
                           colClasses =  c("character"), stringsAsFactors = F)
followers_info$cluster <- as.integer(followers_info$cluster)


cluster_info <- read.csv("../results_following/result1/cluster_summary.csv", colClasses = c("character"))
#retweet counts of each topics 
cluster_info$cluster_id <- as.integer(cluster_info$cluster_id)

#tweets info



## incorporate with topics and segments -------------
#topics over time
library(xlsx)
tweets_with_cluster <- read.csv("../results_topics/unigram/clustered_tweets_text_k28.csv", colClasses = c("character"))
topics_info <- read.csv("../results_topics/unigram/cluster_names_k28.csv", colClasses = c("character"))
names(topics_info) <- c("cluster_id", "cluster_label","note")
tweets_with_cluster$cluster_id <- as.integer(tweets_with_cluster$cluster_id)
tweets_with_cluster$created_at <- as.POSIXct(tweets_with_cluster$created_at)

#order the tweets, by the order in tweets  in graph k = 1,2, 3
tweets_with_cluster <- tweets_with_cluster[match(tweets$id_str, tweets_with_cluster$id_str),]
ntopics <- max(tweets_with_cluster$cluster_id,na.rm = T); 
tweets_with_cluster$cluster_id[!is.na(tweets_with_cluster$thank)] <- ntopics+1
tweets_with_cluster$cluster_id[!is.na(tweets_with_cluster$join)] <- ntopics+2
tweets_with_cluster$cluster_id[!is.na(tweets_with_cluster$retweet_endorse)] <- ntopics+3
tweets_with_cluster$cluster_id[!is.na(tweets_with_cluster$via_at)] <- ntopics+4
stopifnot(sum(is.na(tweets_with_cluster$cluster_id))==0)
ntopics <- max(tweets_with_cluster$cluster_id)

topics_info <- rbind(topics_info, data.frame(cluster_id = (ntopics-4+1):ntopics, 
                                             cluster_label = c("thank_you", "join_us","retweet_endorse", "via_at"),
                                             note=c(NA, NA, NA, NA)))
topics_info$cluster_size =table(tweets_with_cluster$cluster_id)


## topics change over time 2015-06-16 - 2016-11-08
dates <- seq(from = as.Date('2015-06-01'), to =as.Date('2016-11-09'), by = 'month')
dates <- c(dates, as.Date('2016-11-09'))
topics_x_time <- matrix(0, nrow = ntopics, ncol = length(dates)-1)   # number of tweets in each clusters

for( i in 1:ntopics){
  tw1 <- tweets_with_cluster[tweets_with_cluster$cluster_id == i, ]
  topics_x_time[i,] <- count_by_interval(tw1$created_at, dates)
}

colnames(topics_x_time) <- gsub('(.*)-01.*', '\\1', x= dates[-19])
topics_info <- data.frame(topics_info,topics_x_time)



write.csv(topics_info, file = "../results_topics/unigram/cluster_names_k28_updated.csv", row.names = F)




#re-order the segments_names
category_names <- unique(cluster_info$category_name)
category_full<- c("Trump supporters",   "conservatives", "anti-feminist conservatives", 
                  "liberals", "NRA members", "far left", "non-political","religious",
                  "journalists","Miami", "California tech scene","news junkies" ,
                  "other countries","mixed") 
category_names <- category_names[order(match(category_names, category_full))] #expected order
id1_to_id2 <- order(match(cluster_info$category_name, category_names))  # idx to trump etc

# cluster re-ordered,
cluster_info <- cluster_info[id1_to_id2,]; 
cluster_info$cluster_id <-  match(cluster_info$cluster_id, id1_to_id2)
followers_info$cluster <- match(followers_info$cluster, id1_to_id2)


segments_names <- cluster_info$cluster_name
categoryZ <- matrix(0, length(segments_names), length(category_names))    # 50 cluster x 10 category
for(i in 1: length(segments_names)){
  categoryZ[i, which(category_names ==cluster_info$category_name[i])] <- 1
}
colnames(categoryZ) <- category_names

##ordering the segments_names according the category name
Z <- membershipM(followers_info$cluster)
followingZ <- Z[match(rownames(A), followers_info$id_str),]; dim(followingZ)
colnames(followingZ) <- segments_names

topics_names <- topics_info$cluster_label
tmp <- as.matrix(t(followingZ)%*%A%*% membershipM(tweets_with_cluster$cluster_id))
colnames(tmp) <- topics_names
#cluster_info <- data.frame(cluster_info,tmp)

followers_info$retweet_trump <- rep(0, nrow(followers_info))
followers_info$retweet_trump[match(rownames(A),followers_info$id_str)] <- rowSums(A)

for (i in 1:ntopics) { 
  followers_info[paste0(i,'-',topics_names[i])] <- 
  rep(0,nrow(followers_info))
}
followers_info[match(rownames(A),followers_info$id_str),paste0(1:ntopics,'-',topics_names)] <-
  A %*% membershipM(tweets_with_cluster$cluster_id)  #53046

##category level membership matrix
Z_followers_category <- followingZ %*% categoryZ ; 
tmp <- data.frame(clustername = unlist(lapply(segments_names, function(x) substr(x, 1,30))),
                  size_retweet = colSums(followingZ), size_in_76k = colSums(Z), ratio_of_engage =        round(colSums(followingZ)/colSums(Z),2))
rownames(tmp) <- NULL; tmp



### retweet volumne, 50 clusters x 14 time periods of tweets over time
timeY <- membershipM(findInterval(tweets$created_at, dates))
colnames(timeY) <- gsub('(.*) .*', '\\1', x= dates[-1])
blockM <-t(followingZ  %*% Diagonal(dim(followingZ)[2], colSums(followingZ)^(-1))) %*%A %*% timeY
balloon.plot(blockM, ylabel = paste0(substr(rownames(blockM),1,30), ' (', colSums(followingZ), ')'),
             xlabel = paste0(colnames(blockM),' (',colSums(timeY), ')'), text = F) + 
  ggtitle( paste("Trump followers engagement with his tweets over time\n", nrow(el),"retweets between", sum(followingZ), "followers x", sum(timeY), "tweets: \n among 50 following vs  14 time periods") )
colnames(blockM) <- NULL; blockM



### retweet volumne, 10 categories x 14 time periods of tweets over time
blockM <-t(Z_followers_category  %*% Diagonal(dim(Z_followers_category)[2], colSums(Z_followers_category)^(-1))) %*%A %*% timeY
balloon.plot(blockM, ylabel = paste0(substr(rownames(blockM),1,30), ' (', colSums(Z_followers_category), ')'),
             xlabel = paste0(colnames(blockM),' (',colSums(timeY) ,')'), text = F) + ggtitle( paste("Trump followers engagement with his tweets over time\n", nrow(el),"retweets between", sum(followingZ), "followers x", sum(timeY), "tweets: \n among 50 following vs  14 time periods") )
colnames(blockM) <- NULL; blockM
# I did a bigram on the text, which has the advantages to taking into account the order and create more words for our use. also I double the importance of the hashtags.



write.csv(topics_info, file ="../results_retweeting/result3/topics_summary.csv", row.names = F)
write.csv(cluster_info, file ="../results_following/result3/cluster_summary.csv", row.names = F)
write.csv(followers_info, file ="../results_following/result3/followers_info.csv", row.names = F)