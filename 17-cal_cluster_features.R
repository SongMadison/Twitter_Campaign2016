
rm(list =ls())

library(Matrix)
library(xlsx)
source("function.R")
#graph 3
ResultPath <- "../results_following/result3/"
followers_info <- read.csv("../data/friends_info/edgelist_Feb27/samp3_info.csv", 
                           colClasses = c("character"))
load("../results_following/result3/analysis.RData")  
#L, svd_L, km_row, result50,
Af = 1*(L>0)
segments_with_label <- read.xlsx('../results_following/Trump_followers_three_stages.xlsx', 
                                 colClasses = c("character"),
                                 sheetIndex = 8)
segements_labels <- segments_with_label$cluster_label[1:50]



# retweeting
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
#followers_info <- read.csv("../data/friends_info/edgelist_Feb27/samp3_info.csv", colClasses = c("character"))
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


#add friends in the following graph
followers_info$friends_count_samp <- rep(NA, nrow(followers_info))
followers_info$friends_count_samp[-idx_following] <- rowSums(L>0)


#features calculated based retweeting graph

#add the number of trump's tweets, retweeted by this follower
followers_info$retweet_cluster <- rep(NA, nrow(followers_info))
followers_info$retweet_cluster[-idx_retweeting]  <- users_cluster_retweeting$cluster

followers_info$retweet_trump_count <- rep(NA, nrow(followers_info)) 
followers_info$retweet_trump_count[-idx_retweeting] <- rowSums(A)



#tweets - time distribution
dates <- seq(from = as.Date('2015-06-01'), to =as.Date('2016-11-09'), by = 'month')
dates <- c(dates, as.Date('2016-11-09'))
ids <- id_by_interval(tweets_with_cluster$created_at, dates)
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


write.csv(followers_info, file =paste0(ResultPath,"followers_info_upated.csv"), row.names = F)







if(FALSE){
  # ------------------------- update cluster_summary.csv -------------------
  keyfriends <- read.csv(paste0(ResultPath,"keyfriends_k50_40.csv"))
  #str(keyfriends)
  cluster_features <- keyfriends[seq(1,2000,by = 40),c(1:3)]; rownames(cluster_features) <- 1:50 # id, size, trump_ratio
  #1-cluster_id, 
  #2-cluster_size
  #3-trump_ratio; score of avarage trump following rate 
  #-- weighted by column and rows, over the grand mean
  
  #variable types
  followers_info$time_order <- as.numeric(followers_info$time_order)
  followers_info$retweet_trump <- as.numeric(followers_info$retweet_trump)
  followers_info$cluster <- as.factor(followers_info$cluster)
  followers_info$cluster <- factor(followers_info$cluster,levels = 1:50, labels = c(paste0('0',1:9),10:50))
  
  
  # time order in each cluster
  lm.timeorder <- lm(time_order ~ cluster -1, data = followers_info)
  lm.timeorder$coefficients
  plot(1:50, lm.timeorder$coefficients/1e6)
  cluster_features$'time_order-million' <- lm.timeorder$coefficients/1e6
  
  
  ## some other cluster features -- id, clustersize, trump_Ratio, top40(friends screen-names)
  k = 50; top =40
  sns <- character(50)
  for (i in 1:k) {
    sns[i] <- paste0(keyfriends$screen_name[1:top+(i-1)*top],collapse = ',')
  }
  cluster_features$'keyfriends_sns' <- sns
  
  
  ## add retweet count - over time each month
  lm.trump <- lm(retweet_trump ~ cluster -1, data = followers_info)
  lm.trump$coefficients
  plot(1:50, lm.trump$coefficients)
  cluster_features$'retweet-count' <- lm.trump$coefficients
  
  ## add retweet retweet ratio
  cluster_x_retweet <- table(followers_info$cluster, !is.na(followers_info$retweet_trump))
  cluster_features$'retweet-per-person' <- round( cluster_x_retweet[,"TRUE"]/rowSums(cluster_x_retweet),digits = 3)
  plot(cluster_features$'retweet-per-person')
  
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
  cluster_features <- data.frame(cluster_features, data.frame(clus_x_year.month))
  
  write.csv(cluster_features, file = paste0(ResultPath, "cluster_summary.csv"), row.names = F)
  
  
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
  
  
  
}


