
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
followers_info$created_at <- as.POSIXct(followers_info$created_at, 
                                        format = "%a %b %d %H:%M:%S %z %Y")
rm(f1,f2,f3)



#16, cluster, id

#1-14 profile
profile <- followers_info[, 1:14]


#15,17,19,18, time_order, friends_count_samp, retweet_trump_count, retweet_cluster  
calculated_features <- followers_info[,c(15,17,19, 18)]
#trump_topic_features 20-37, 38-69

trump_topic_features <- followers_info[,c(20:69)]

#add 6 new page rank features
#network 1, pg1, pg1_personalized, pg1_core3, pg1_p_core3
#network 1, pg2, pg2_personalized, pg2_core3, pg2_p_core3
pg1 <- read.csv("../results_following/pagerank1.csv", colClasses = c("character"))
pg1$pagerank <- as.numeric(pg1$pagerank)   
tmp <- left_join(data.frame(sn = followers_info$screen_name), pg1, by = c("sn" = "screen_name"))
calculated_features$pg1 <- tmp$pagerank
calculated_features$pg1_personalized <- tmp$pg_personalized
calculated_features$pg1_core3 <- tmp$pg_core3
calculated_features$pg1_p_core3 <- tmp$pg_p_core3

pg2 <- read.csv("../results_following/pagerank2.csv", colClasses = c("character"))
pg2$pagerank <- as.numeric(pg2$pagerank)
tmp <- left_join(data.frame(sn = followers_info$screen_name), pg2, by = c("sn" = "id"))
calculated_features$pg2 <- tmp$pagerank
calculated_features$pg2_personalized <- tmp$pg_personalized
# calculated_features$pg2_core3 <- tmp$pg_core3
# calculated_features$pg2_p_core3 <- tmp$pg_p_core3

cat("two pageranks is done.")



#clsuter_features: 5(period, cluster id, cluster_label, cluster_cat, cluster_note
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
cat("cluster_features is done. \n")




##tweeting frequency features 4(first_date, last_date_timeline, count_mean_timeline, count_total_timeline) 
tweet_freq <- data.table::fread("../data/friends_info/edgelist_Feb27/daily_counts.csv", 
                                colClasses = c("character"))
tweet_freq$count <- as.numeric(tweet_freq$count)
length(unique(tweet_freq$user_id_str));sum(tweet_freq$count)/nrow(tweet_freq) 

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
sum(is.na(idx2))    #523, don't know swh?, not even in the ids_13M
missing_ids <- tweet_by_user$user_id_str[is.na(idx2)]

tweet_by_user<- left_join(data.frame(id_str = followers_info$id_str),  
                          tweet_by_user,  by = c("id_str" = "user_id_str"))
tweet_by_user <- tweet_by_user[,-match("id_str", names(tweet_by_user))]
cat("tweeting frequency is done. \n")




followers_info <- data.frame(cluster_features, profile, calculated_features, tweet_by_user, trump_topic_features)
# 4+ 14 + 10 +4+ 50 
#period, "cluster", "clust_label" , "clust_cat", cluster_label #profile, 
# [20] "time_order"                                 "friends_count_samp"                        
# [21] "retweet_trump_count"                        "id_str.1"                                  
# [23] "count_mean_timeline"                        "count_total_timeline"                      
# [25] "first_date_timeline"                        "last_date_timeline" 
#rm(cluster_features,  profile, calculated_features, tweet_by_user, trump_topic_features)





#followers_info <- read.csv("../results_following/followers_with_cluster_info.csv", colClasses = c("character"))
load("../data/friends_info/edgelist_Feb27/RData/retweet_A_comments.RData")
retweets = retws; rm(retws)
ret1 <- subset(retweets, quoted_status_user_id_str == "25073877")
ret2 <- subset(retweets, retweets$retweet_status_user_id_str == "25073877")
retweets = rbind(ret1,ret2) ; rm(ret1, ret2)
tmp = data.frame(id_str = retweets$user_id_str, RT_only= !is.na(retweets$retweet_status_id_str), RT_comments= !is.na(retweets$quoted_status_id_str))
counts = tmp %>% group_by(id_str,RT_only, RT_comments) %>% count()
counts1 = subset(counts, RT_only == TRUE)
counts2 = subset(counts, RT_comments == TRUE)
result = data.frame(id_str= unique(followers_info$id_str)) %>% left_join(counts1, by = "id_str") %>% left_join(counts2, by ="id_str")
names(result) #"id_str"        "RT_only.x"     "RT_comments.x" "n.x"           "RT_only.y"     "RT_comments.y" "n.y"
result <- result[,c("id_str","n.x","n.y")]
names(result) <-c("id_str" , "RT_only", "RT_comments"); 
result[is.na(result[,2]),2] <-0; result[is.na(result[,3]),3] <-0 
sum(result[,2]); sum(result[,3])

followers_info = followers_info %>% left_join(result, by ="id_str")
followers_info$RT_only[is.na(followers_info$RT_only)] = 0
followers_info$RT_comments[is.na(followers_info$RT_comments)] = 0



## deplorables
deplorables_str <- c("Mike Cernovich: @Cernovich"
                     , "Richard Spencer: @RichardBSpencer"
                     ,"Daily Stormer: @rudhum"
                     ,"David Duke: @DrDavidDuke"
                     ,"Paul Joseph Watson: @PrisonPlanet"
                     ,"Alex Jones: @RealAlexJones"
                     ,"Breitbart: @BreitbartNews"
                     ,"Paul Ray Ramsey: @ramzpaul"
                     ,"Gen. Robert E Lee: @Suthen_boy"
                     ,"Ann Kelly: @LadyAodh")
deplorable_sns<- gsub(".*@(.*)", '\\1', deplorables_str)
# library(smappR)
# deplorable_df <- getUsersBatch(oauth_folder = 'credentials/credential_mixed01/', 
#                                 screen_names = deplorable_sns,
#                                 output = "../results_following/10-deplorable.json")
# write.csv(deplorable_df, file = "../results_following/deplorable.csv", row.names = F)
deplorables_df <- read.csv("../results_following/deplorable_accounts_info.csv", colClasses = c("character"))
load("../data/friends_info/edgelist_Feb27/RData/full_graph.RData") #rr, cc, i_set, j_set
# (idx <- match(deplorables_df$id_str, c(rr,cc)))
# 355105 357563     NA 667601 361039 361042 361128 357564 359574 357565
deplorables_features <- matrix(0, nrow(followers_info), 10)
for (i in 1:10){
  id_str = deplorables_df$id_str[i]
  if(id_str %in% cc){  # the 3rd is not in cc.
    sn <- rr[i_set[which(j_set == match(id_str, c(rr,cc)))]] ##
    deplorables_features[match(sn, followers_info$screen_name), i] <- 1
  }
}
deplorables_features <- as.data.frame(deplorables_features)
names(deplorables_features) <- paste0(deplorable_sns,"_",deplorables_df$followers_count)


followers_info <- data.frame(followers_info, deplorables_features)
write.csv(followers_info, file ="../results_following/followers_with_cluster_info.csv", row.names = F)



