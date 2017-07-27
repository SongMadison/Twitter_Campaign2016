

# ------------------------- update cluster_summary.csv -------------------
rm(list = ls())
library(Matrix)
library(xlsx)
library(igraph)

followers_info <- read.csv("../results_following/followers_with_cluster_info.csv", colClasses = c("character"))
#correct variable types
#factor variable.
followers_info$cluster <- as.factor(as.numeric(followers_info$cluster))
#data time
followers_info$created_at <-  as.POSIXct(followers_info$created_at)
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
followers_info$cluster <- as.factor(as.integer(followers_info$cluster))



##   remove missing cluster == NA
followers <- subset(followers_info, !is.na(cluster) )
dim(followers)#322461 rows  
#377725 = 324964 following >1,  failed: 52510, zeros: 251
#further due to followers very few people, < 10; 
table(followers$cluster)

#11k, or 3% followers within some clusters, don't have tweets, 
cat ( sum(is.na(followers$count_mean_timeline))/nrow(followers), "of followers have no tweets")
#0.03417778






###########-------------------------------------------------------------------------------

#labels, id=1-150, cluster_size
segments_with_label <- xlsx::read.xlsx('../results_following/Trump_followers_three_stages.xlsx', 
                                 colClasses = c("character"),
                                 sheetIndex = 8)
category_names <- read.csv("../results_following/cluster_cat.csv")
cluster_features = segments_with_label[,1:4] #"period", "cluster_label", "label_note", cluster_id        
cluster_features$cluster_id <- as.integer(cluster_features$cluster_id)
cluster_features$cluster_id[51:100] <- cluster_features$cluster_id[51:100]+50
cluster_features$cluster_id[101:150] <- cluster_features$cluster_id[101:150]+100
cluster_features$cluster_id <- as.factor(cluster_features$cluster_id)
cluster_features$clust_cat <- cluster_features$cluster_label #cobinced label
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
  freqs[i,] <- round(colMeans(followers[which(followers$cluster == i),32:81]),3)
}
colnames(freqs) <- names(followers)[32:81]

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


