
rm(list =ls())

library(Matrix)
library(xlsx)
library(igraph)

#graph 3
ResultPath <- "../results_following/result3/"
f1<- read.csv("../results_following/result1/followers_info_upated.csv", 
                           colClasses = c("character"))
f1$cluster <- as.numeric(f1$cluster)+100 #after primary
f2<- read.csv("../results_following/result2/followers_info_upated.csv", 
              colClasses = c("character"))
f2$cluster <- as.numeric(f2$cluster)+50 # before primary
f3<- read.csv("../results_following/result3/followers_info_upated.csv", 
              colClasses = c("character"))
f3$cluster <- as.numeric(f3$cluster) #before announcement
followers_info <- data.frame( period=rep(c("before announcement", "before primary", "before election"), 
times = c(nrow(f3),nrow(f2), nrow(f1))), rbind(f3,f2,f1))
rm(f1,f2,f3)


followers_info$cluster <- as.factor(followers_info$cluster)
segments_with_label <- read.xlsx('../results_following/Trump_followers_three_stages.xlsx',
                                 colClasses = c("character"),
                                 sheetIndex = 8)


cluster_labels <- segments_with_label$cluster_label ## EXTRA INFO!!! labels,
followers_info <- data.frame(
  clust_label = factor(followers_info$cluster, levels = 1:150, labels = cluster_labels),
  followers_info)
names(followers_info) #sorting

#write.csv(followers_info[,c(2,1,18,3:17,19:71)], file ="../results_following/followers_with_cluster_info.csv", row.names = F)






# ------------------------- update cluster_summary.csv -------------------

#variable types
followers_info$time_order <- as.numeric(followers_info$time_order)
followers_info$retweet_trump_count <- as.numeric(followers_info$retweet_trump_count)
followers_info$retweet_trump_count[is.na(followers_info$retweet_trump_count)] <- 0
for( i in 22:71){
  followers_info[,i] <- as.numeric(followers_info[,i]) 
}
dim(followers_info)#377725

followers_info <- subset(followers_info, !is.na(cluster) )
dim(followers_info)#322461 rows   
#377725 = 324964 following >1,  failed: 52510, zeros: 251
#further due to followers very few people, < 10; 


followers <- followers_info

#labels, id=1-150, cluster_size
segments_with_label <- read.xlsx('../results_following/Trump_followers_three_stages.xlsx', 
                                 colClasses = c("character"),
                                 sheetIndex = 8)
cluster_features = segments_with_label[,1:4] 
cluster_features$clust_size = table(followers_info$cluster)

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


# add retweet count - over time each month, missing filled with 0
lm.trump <- lm(retweet_trump_count ~ cluster -1, data = followers_info)
cluster_features$retweet_trump_count <- round(lm.trump$coefficients,3)
##Note: ERROR!!! # different from results given to Yini before. 
cluster_features$retweet_trump_count- segments_with_label$retweet.count 

# add retweet or not ratio
cluster_x_retweet <- table(followers_info$cluster, followers_info$retweet_trump>0)
cluster_features$retweet_or_not <- round( cluster_x_retweet[,"TRUE"]/rowSums(cluster_x_retweet),digits = 3)





# add egonetwork features
# -- mean degree, density
all_samp <- read.csv("../data/friends_info/edgelist_Feb27/all_samp_info.csv", colClasses = c("character"))
dim(all_samp)
idx1 <- match(followers$screen_name, all_samp$screen_name); 
stopifnot(sum(is.na(idx1)) == 0)
idx <- match(followers$id_str, all_samp$id_str)
stopifnot(sum(is.na(idx))==0); which(idx != idx1)

followers = followers_info
density <- numeric(150)
#adjlistall <- readLines("../data/friends_info/edgelist_Feb27/originalData/adjlist_all.txt")
load("../data/friends_info/edgelist_Feb27/originalData/adjlist_all.RData") #377809
sns <- gsub("(.*?),.*", replace = "\\1", x = adjlist_all)


idx <- match(followers$screen_name, sns)
stopifnot(sum(is.na(idx)) == 0)

network_features = NULL
for(i in 1:5){
  sub_ids = followers$id_str[followers$cluster == i]
  idx <- match(followers$screen_name[followers$cluster==i], sns); 
  adjlist_str <- adjlist_all[idx]
  adj_list = lapply(adjlist_str, function(x) strsplit(x, split = ','))
  adj_list1 = lapply(adj_list, function(x) {
    xx = unlist(x); idx = match(sub_ids, xx);
    xx[idx[!is.na(idx)]] #return chr(0) if follows no people
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

cluster_features <- data.frame(cluster_features, network_features)


# the mean frequencies of tweeting trump [each month, each topics]
# colnums 22 :71
freqs <- matrix(0,nrow = 150, ncol = 50)
csize <-c(0,cumsum(cluster_features$clust_size))
for(i in 1:150){
  freqs[i,] <- round(colMeans(followers_info[(csize[i]+1):csize[i+1],22:71]),3)
}
colnames(freqs) <- names(followers_info)[22:71]
cluster_features <- data.frame(cluster_features, freqs)



#re-ordering
category_names <- segments_with_label$cluster_label
tmp <- unique(category_names)
category_names[category_names %in% tmp [c(2,3,5,6,7,10,11,15,16,17,20,22,25,26,28,29,30,31)]] <- 
  "other countries" 
unique(category_names)

category_names <- c("Trump supporters",   "conservatives", "anti-feminist conservatives", 
                    "liberals", "NRA members", "far left", "non-political","religious",
                    "journalists","Miami", "California tech scene","news junkies" ,
                    "other countries","mixed")                                                       




write.csv(cluster_features, file ="../results_following/cluster_summary.csv", row.names = F)


#back up--------------------------------------------------------------------

#features from retweeting A
#i) cluster
retweet_cluster <- followers_info$retweet_cluster
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


