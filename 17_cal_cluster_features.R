
rm(list =ls())

library(Matrix)
library(xlsx)
library(igraph)

library(tidyverse)
library(dygraphs)
library(xts)


#-----------------------------update  followers_info.csv ---------------
# combined the 3 followers_info together
# add some new features.


f1<- read.csv("../results_following/result1/followers_info_upated.csv", 
                           colClasses = c("character"))
f1$cluster <- as.numeric(f1$cluster)+100 #after primary
f2<- read.csv("../results_following/result2/followers_info_upated.csv", 
              colClasses = c("character"))
f2$cluster <- as.numeric(f2$cluster)+50 # before primary
f3<- read.csv("../results_following/result3/followers_info_upated.csv", 
              colClasses = c("character"))
f3$cluster <- as.numeric(f3$cluster) #before announcement
followers_info <- rbind(f3,f2,f1)
n1 = nrow(f1); n2 = nrow(f2); n3 = nrow(f3)
followers_info$cluster <- as.factor(followers_info$cluster)
rm(f1,f2,f3)


#1-14 profile
profile <- followers_info[, 1:14]
#16, cluster, id
#15,17,19, time_order, friends_count_samp, retweet_trump_count, retweet_cluster  
calculated_features <- followers_info[,c(15,17,19, 18)]
#trump_topic_features 20-37, 38-69
trump_topic_features <- followers_info[,c(20:69)]

#page rank features
pg1 <- read.csv("../results_following/pagerank1.csv", colClasses = c("character"))
pg1$pagerank <- as.numeric(pg1$pagerank)   
tmp <- left_join(data.frame(sn = followers_info$screen_name), pg1, by = c("sn" = "id"))
calculated_features$pg1 <- tmp$pagerank
calculated_features$pg1_personalized <- tmp$pg_personalized
pg2 <- read.csv("../results_following/pagerank2.csv", colClasses = c("character"))
pg2$pagerank <- as.numeric(pg2$pagerank)
tmp <- left_join(data.frame(sn = followers_info$screen_name), pg2, by = c("sn" = "id"))
calculated_features$pg2 <- tmp$pagerank
calculated_features$pg2_personalized <- tmp$pg_personalized
cat("two pageranks is done.")



#clsuter_features  
segments_with_label <- read.xlsx('../results_following/Trump_followers_three_stages.xlsx',
                                 colClasses = c("character"),
                                 sheetIndex = 8)
category_names <- read.csv("../results_following/cluster_cat.csv", colClasses = c("character"))
cluster_features <- data.frame(
  period = rep(c("before announcement", "before primary", "before election"), 
                                          times = c(n3,n2, n1)), 
  cluster = followers_info$cluster
  )
#add cluster_label, and category names #some duplicated labels give warnings
cluster_features$clust_label = factor(cluster_features$cluster, levels = 1:150,
                     labels = segments_with_label$cluster_label)
cluster_features$clust_label <- as.character(cluster_features$clust_label)
cluster_features$clust_cat <- cluster_features$clust_label
for( i in 1:nrow(category_names)){
  cluster_features$clust_cat[which(cluster_features$clust_label == category_names$cluster_label[i])] <-
    category_names$cluster_cat1[i]
}                                                    
cat("cluster_features is done.")


##tweeting frequency features -- first_date etc.
tweet_freq <- data.table::fread("../data/friends_info/edgelist_Feb27/daily_counts.csv", 
                       colClasses = c("character"))
tweet_freq$count <- as.numeric(tweet_freq$count)
length(unique(tweet_freq$user_id_str));sum(tweet_freq$count)/nrow(tweet_freq) 

#summarized at user - level
tweet_by_user = tweet_freq %>% 
  group_by(user_id_str) %>% summarise(count_mean_timeline = mean(count))
tweet_by_user = left_join(tweet_by_user, 
                          tweet_freq %>% group_by(user_id_str) %>% 
                            summarise(count_total_timeline = sum(count)), by = "user_id_str")
tweet_by_user = left_join(tweet_by_user, 
                          tweet_freq %>% group_by(user_id_str) %>% 
                            summarise(first_date_timeline = min(created_date)), by = "user_id_str")
tweet_by_user = left_join(tweet_by_user, 
                          tweet_freq %>% group_by(user_id_str) %>% 
                            summarise(last_date_timeline = max(created_date)), by = "user_id_str")
# some irregularity among user_ids matching                
idx1 <- match(followers_info$id_str, tweet_by_user$user_id_str)
sum(is.na(idx1)) / nrow(followers_info)  #12.8% don't have tweets
idx2 <- match(tweet_by_user$user_id_str, followers_info$id_str)
sum(is.na(idx2))    #523, don't know swhy?, not even in the ids_13M
missing_ids <- tweet_by_user$user_id_str[is.na(idx2)]
tweet_by_user<- left_join(data.frame(id_str = followers_info$id_str),  tweet_by_user,  by = c("id_str" = "user_id_str"))
cat("tweeting frequency is done.")


df <- data.frame(cluster_features, profile, calculated_features, tweet_by_user, trump_topic_features)
# 4+ 14 + 3 +5+ 50 
#period, "cluster", "clust_label" , "clust_cat", cluster_label #profile, 
# [20] "time_order"                                 "friends_count_samp"                        
# [21] "retweet_trump_count"                        "id_str.1"                                  
# [23] "count_mean_timeline"                        "count_total_timeline"                      
# [25] "first_date_timeline"                        "last_date_timeline" 
rm(cluster_features,  profile, calculated_features, tweet_by_user, trump_topic_features)


#check
names(df)
rm(followers_info)
# interaction with trump , #interaction with topics
#write.csv(df, file ="../results_following/followers_with_cluster_info.csv", row.names = F)


followers_info <- read.csv("../results_following/followers_with_cluster_info.csv", colClasses = c("character"))
load("../data/friends_info/edgelist_Feb27/RData/retweet_A_comments.RData")
retweets = retws; rm(retws)
ret1 <- subset(retweets, quoted_status_user_id_str == "25073877")
ret2 <- subset(retweets, retweets$retweet_status_user_id_str == "25073877")
retweets = rbind(ret1,ret2) ; rm(ret1, ret2)
df = data.frame(id_str = retweets$user_id_str, RT_only= !is.na(retweets$retweet_status_id_str), RT_comments= !is.na(retweets$quoted_status_id_str))
counts = df %>% group_by(id_str,RT_only, RT_comments) %>% count()
counts1 = subset(counts, RT_only == TRUE)
counts2 = subset(counts, RT_comments == TRUE)
result = data.frame(id_str= unique(df$id_str)) %>% left_join(counts1, by = "id_str") %>% left_join(counts2, by ="id_str")
names(result) #"id_str"        "RT_only.x"     "RT_comments.x" "n.x"           "RT_only.y"     "RT_comments.y" "n.y"
result <- result[,c("id_str","n.x","n.y")]
names(result) <-c("id_str" , "RT_only", "RT_comments"); 
result[is.na(result[,2]),2] <-0; result[is.na(result[,3]),3] <-0 
sum(result[,2]); sum(result[,3])

followers_info = followers_info %>% left_join(result, by ="id_str")
followers_info$RT_only[is.na(followers_info$RT_only)] = 0
followers_info$RT_comments[is.na(followers_info$RT_comments)] = 0
followers_info$created_at <- date <- as.POSIXct(followers_info$created_at, format = "%a %b %d %H:%M:%S %z %Y")
#write.csv(followers_info, file ="../results_following/followers_with_cluster_info.csv", row.names = F)











# ------------------------- update cluster_summary.csv -------------------
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




#labels, id=1-150, cluster_size
segments_with_label <- xlsx::read.xlsx('../results_following/Trump_followers_three_stages.xlsx', 
                                 colClasses = c("character"),
                                 sheetIndex = 8)
category_names <- read.csv("../results_following/cluster_cat.csv")
cluster_features = segments_with_label[,1:4] 
cluster_features$clust_cat <- cluster_features$cluster_label
for( i in 1:nrow(category_names)){
  cluster_features$clust_cat[which(cluster_features$cluster_label == category_names$cluster_label[i])] <-
    category_names$cluster_cat1[i]
  cat(i, "\n")
} 
cluster_features$clust_size = table(followers$cluster)

keyfriends1 <- read.csv("../results_following/result1/keyfriends_k50_40.csv", colClasses = c("character"))
keyfriends2 <- read.csv("../results_following/result2/keyfriends_k50_40.csv", colClasses = c("character"))
keyfriends3 <- read.csv("../results_following/result3/keyfriends_k50_40.csv", colClasses = c("character"))
keyfriends <- rbind(keyfriends3,keyfriends2,keyfriends1)
rm(keyfriends1,keyfriends2, keyfriends3)
#trump_ratio; score of avarage trump following rate in the weighted L (calculated by weighted A)
cluster_features$trump_ratio = keyfriends$trump_ratio[seq(1,6000,40)]

# time order in each cluster
lm.timeorder <- lm(time_order ~ cluster -1, data = followers_info)
cluster_features$'time_order-million' <- round(lm.timeorder$coefficients/1e6,3)


# some other cluster features -- id, clustersize, trump_Ratio, top40(friends screen-names)
k = 150; top =40
sns <- character(150)
for (i in 1:k) {
  sns[i] <- paste0(keyfriends$screen_name[1:top+(i-1)*top],collapse = ',')
}
cluster_features$'keyfriends_sns' <- sns

# some representative rows in each cluster
sf1 <- read.csv("../results_following/result1/selected_followers.csv")
sf2 <- read.csv("../results_following/result2/selected_followers.csv")
sf3 <- read.csv("../results_following/result3/selected_followers.csv")
sf <- rbind(sf3,sf2,sf1)
k = 150; top =40
sns <- character(150)
for(i in 1:k){
  sns[i]  <- paste0(sf$screen_name[1:top+(i-1)*top], collapse = ',')
}
cluster_features$'typical_followers_sns' <- sns

deplorable1 <- read.csv("../results_following/result1/deplorable_features.csv")
deplorable2 <- read.csv("../results_following/result2/deplorable_features.csv")
deplorable3 <- read.csv("../results_following/result3/deplorable_features.csv")
sizes <- gsub(".*\\.(.*)", '\\1', colnames(deplorable2)); sizes[1] <-""
nn <- gsub("(.*)\\..*", '\\1', colnames(deplorable2))
colnames(deplorable1) <- nn; colnames(deplorable2) <- nn; colnames(deplorable3) <- nn;
deplorable_features <- rbind(deplorable3, deplorable2, deplorable1); rm(deplorable1, deplorable2, deplorable3)
colnames(deplorable_features) <- paste0(nn,'-', sizes)
cluster_features <- data.frame(cluster_features, deplorable_features[,2:ncol(deplorable_features)])


# add retweet_only_ratio, retweet_comments
lm.RT_only <- lm(followers$RT_only ~ cluster-1, data = followers)
cluster_features$RT_only_count = lm.RT_only$coefficients

lm.RT_only_ratio <- lm(1*(followers$RT_only>0) ~ cluster-1, data = followers)
cluster_features$RT_only_ratio = lm.RT_only_ratio$coefficients

lm.RT_comments <- lm(followers$RT_comments ~ cluster-1, data = followers)
cluster_features$RT_comments_count = lm.RT_comments$coefficients

lm.RT_comments_ratio <- lm(1*(followers$RT_comments>0) ~ cluster-1, data = followers)
cluster_features$RT_comments_ratio = lm.RT_comments_ratio$coefficients


# add retweet_only_ratio, retweet_comments
lm.pg1 <- lm(followers$pg1 ~ cluster-1, data = followers)
cluster_features$pg1 = lm.pg1$coefficients

lm.pg1_personalized <- lm(followers$pg1_personalized ~ cluster-1, data = followers)
cluster_features$pg1_personalized = lm.pg1_personalized$coefficients

lm.pg2 <- lm(followers$pg2 ~ cluster-1, data = followers)
cluster_features$pg2 = lm.pg2$coefficients

lm.pg2_personalized <- lm(followers$pg2_personalized ~ cluster-1, data = followers)
cluster_features$pg2_personalized = lm.pg2_personalized$coefficients

rm(lm.pg1, lm.pg2, lm.pg1_personalized, lm.pg2_personalized)


lm.trump <- lm(retweet_trump_count ~ cluster -1, data = followers)
cluster_features$retweet_trump_count <- round(lm.trump$coefficients,3)

# add retweet count - over time each month, missing filled with 0
lm.trump_ratio <- lm(1*(retweet_trump_count>0) ~ cluster -1, data = followers)
cluster_features$retweet_trump_ratio <- round(lm.trump_ratio$coefficients,3)



# add retweet or not ratio
cluster_x_retweet <- table(followers$cluster, followers$retweet_trump_count>0)
cluster_features$retweet_or_not <- round( cluster_x_retweet[,"TRUE"]/rowSums(cluster_x_retweet),digits = 3)




# add egonetwork features
# -- mean degree, density
all_samp <- read.csv("../data/friends_info/edgelist_Feb27/all_samp_info.csv", colClasses = c("character"))
dim(all_samp)
idx1 <- match(followers$screen_name, all_samp$screen_name);  stopifnot(sum(is.na(idx1)) == 0)
idx <- match(followers$id_str, all_samp$id_str)
stopifnot(sum(is.na(idx))==0)
which(idx != idx1)

#adjlistall <- readLines("../data/friends_info/edgelist_Feb27/originalData/adjlist_all.txt")
load("../data/friends_info/edgelist_Feb27/originalData/adjlist_all.RData") #377809
library(parallel)
sns <- unlist(  mclapply(adjlist_all, function(x) gsub("(.*?),.*", replace = "\\1", x), mc.cores =5) )

idx <- match(followers$screen_name, sns)
stopifnot(sum(is.na(idx)) == 0)

network_features = NULL
for(i in 1:150){
  sub_ids = followers$id_str[which(followers$cluster == i)]  #note: NA will return NA
  idx <- match(followers$screen_name[which(followers$cluster==i)], sns); 
  adjlist_str <- adjlist_all[idx]
  adj_list = lapply(adjlist_str, function(x) strsplit(x, split = ','))
  adj_list1 = lapply(adj_list, function(x) {
    xx = unlist(x); idx = match(sub_ids, xx);
    xx[idx[!is.na(idx)]] #keep friends in sub_ids only. return chr(0) if follows no people; 
  })
  adj_list_ids <- lapply(adj_list1, function(x){
    xx = unlist(x)
    match(xx, sub_ids)
  })
  
  G <- graph_from_adj_list(adj_list_ids)  #max(unlist(adj_list_ids))
  network_features <- rbind(network_features, c(
      mean(degree(G, mode = 'in')),
      sd(degree(G, mode = 'in')),
      mean(degree(G, mode = "out")),
      sd(degree(G, mode = 'out')),
      ecount(G)/vcount(G)/(vcount(G)-1)
      ))
  cat("i=", i,'\t')
}
colnames(network_features) <- c("deg_in_mean","deg_in_sd","deg_out_mean", "deg_out_sd", "edge_density")



# the mean frequencies of tweeting trump [each month, each topics]
freqs <- matrix(0,nrow = 150, ncol = 50)
csize <-c(0,cumsum(cluster_features$clust_size))
for(i in 1:150){
  freqs[i,] <- round(colMeans(followers[which(followers$cluster == i),30:79]),3)
}
colnames(freqs) <- names(followers)[30:79]


#information from followers's timeline





#network 
cluster_features <- data.frame(cluster_features, network_features)
#frequency
cluster_features <- data.frame(cluster_features, freqs)
#write.csv(cluster_features, file ="../results_following/cluster_summary.csv", row.names = F)





#cluster_features <- read.csv("../results_following/cluster_summary.csv", colClasses = c("character"))





#back up--------------------------------------------------------------------

#features from retweeting A
#i) cluster
retweet_cluster <- followers$retweet_cluster
retweet_cluster <- as.numeric(retweet_cluster)[!is.na(retweet_cluster)]
length(retweet_cluster)
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


