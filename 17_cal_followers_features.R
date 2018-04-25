
# combine results from 
        # retweeting network, 
        # following network
        # topic modeling about Trump tweets
        # tweets from their timeline  -- added in combined manner in cal_cluster_features.R
# graph 1,2,3


rm(list =ls())
library(Matrix)
library(xlsx)
source("function.R")

## identify which graph, 1, 2, 3
ResultPath <- "../results_following/result3/"
followers_info <- read.csv("../data/friends_info/edgelist_Feb27/samp3_info.csv", 
                           colClasses = c("character"))
load("../results_following/result3/analysis.RData")  
#L, svd_L, km_row, result50,
Af = 1*(L>0)
# segments_with_label <- read.xlsx('../results_following/Trump_followers_three_stages.xlsx', 
#                                  colClasses = c("character"),
#                                  sheetIndex = 8)
segments_with_label  <- xlsx::read.xlsx("../combined_data/Trump_followers_labels 6_18.xlsx", 
                           sheetIndex = 1)
segments_with_label  <- segments_with_label[,2:5]

segements_labels <- segments_with_label$cluster_label[1:50]

# retweeting result
load("../data/friends_info/edgelist_Feb27/RData/retweet_A3.RData")  
#A, users, tweets #note, the graph will be covered
users_cluster_retweeting <- read.csv("../results_retweeting/result3/user_cluster.csv", 
                                     colClasses = c("character"))
#topics
tweets_with_cluster <- read.csv("../results_topics/unigram/clustered_tweets_text_k28.csv", 
                                colClasses = c("character"))  #full tweets
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
table(tweets_with_cluster$cluster_id, exclude = NULL) # no NA
nrow(tweets_with_cluster); nrow(tweets)

idx_following <- which(is.na(match(followers_info$screen_name, rownames(L)))) 
#some followers removed from following analysis -- <10 friends
idx_retweeting <- which(is.na(match(followers_info$id_str, rownames(A))))
tmp1 <- which(is.na(match( rownames(L),followers_info$screen_name)))
tmp2 <- which(is.na(match( rownames(A),followers_info$id_str)))
tmp1
tmp2
if (length(tmp2) >0) {
  users_cluster_retweeting <- users_cluster_retweeting[-tmp2,]
  A <- A[-tmp2,]
}





#------------------------ update followers_info.csv ---------------------
followers_info <- read.csv("../data/friends_info/edgelist_Feb27/samp3_info.csv", colClasses = c("character"))
headers <- c("id_str","screen_name", "name","description","created_at",
             "lang","location","time_zone", "verified",
             "favourites_count","followers_count", "friends_count","listed_count", "statuses_count")
followers_info <- followers_info[,headers]

#add time order to follow Trump
ids <- readLines("../data/friends_info/edgelist_Feb27/ids_13M_cleaned.txt")
N_ids = length(ids) #12998006 # after remove  some duplicates
idx <- match(followers_info$id_str, ids)
stopifnot(sum(is.na(idx)) == 0)
followers_info$time_order <- N_ids - idx

#add cluster_id 
#load("../results/result3/analysis.RData")  #L, svd_L, km_row, result50,
followers_info$cluster<-rep(NA, nrow(followers_info))
followers_info$cluster[-idx_following] <- km_row$cluster
table(followers_info$cluster,exclude = F)


#add friend_count in the following graph
followers_info$friends_count_samp <- rep(NA, nrow(followers_info))
followers_info$friends_count_samp[-idx_following] <- rowSums(L>0)



 

#features calculated based retweeting graph analysis
#add cluster in the retweeting pattern.
followers_info$retweet_cluster <- rep(NA, nrow(followers_info))
followers_info$retweet_cluster[-idx_retweeting]  <- users_cluster_retweeting$cluster

#add the number of trump's tweets, retweeted by this follower
followers_info$retweet_trump_count <- rep(NA, nrow(followers_info)) 
followers_info$retweet_trump_count[-idx_retweeting] <- rowSums(A)


#tweets - time distribution for the Trump's tweets retweeted by this person
dates <- seq(from = as.Date('2015-06-01'), to =as.Date('2016-11-09'), by = 'month')
dates <- c(dates, as.Date('2016-11-09'))
ids <- id_by_interval(tweets_with_cluster$created_at, as.POSIXct(dates))
Z_tweets_time <- membershipM(ids)
Z_followers_time_part <- A%*%Z_tweets_time
dim(Z_followers_time_part)
Z_followers_time <- matrix(0, nrow(followers_info), ncol = length(dates)-1)
Z_followers_time[-idx_retweeting,] <- as.matrix(Z_followers_time_part)
sum(Z_followers_time) == sum(Z_followers_time_part)

colnames(Z_followers_time) <- gsub('(.*)-01.*', 'X\\1', x= dates[-19])
followers_info <- data.frame(followers_info, Z_followers_time)



#followers x topics
Z_tweets_topics <- membershipM(tweets_with_cluster$cluster_id)
Z_followers_topics_part <- A%*%Z_tweets_topics
dim(Z_followers_topics_part)
Z_followers_topics <- matrix(0, nrow(followers_info), ncol = ncol(Z_tweets_topics))
Z_followers_topics[-idx_retweeting,] <- as.matrix(Z_followers_topics_part)
sum(Z_followers_topics) == sum(Z_followers_topics_part)
colnames(Z_followers_topics) <- matrix(topics_info$cluster_label)
followers_info <- data.frame(followers_info, Z_followers_topics)



#write.csv(followers_info, file = paste0(ResultPath,"followers_info_upated.csv"), row.names = F)










