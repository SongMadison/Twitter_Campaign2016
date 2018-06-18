source("18_0_read_followers_info.R")

library(tidyverse) #include ggplot2, dyplyr, readr, tydyr
followers_info %>% mutate(log_daily_count = log10(1+count_mean_timeline)) %>% ggplot(aes(x = log_daily_count)) +
    geom_histogram(stat = "bin",  binwidth = 0.05) +labs(x = "average daily tweets (in log10)")




# cluster
cluster_info <- read.csv("../combined_data/Trump_followers_labels_6_18.csv", colClasses = c("character"))
non_numerical <- c("period","cluster_label","cluster_type", "label_note","cluster_id","keyfriends_sns")
non_numerical_ids <- match(non_numerical, names(cluster_info))
cluster_info$cluster_id <- as.factor(cluster_info$cluster_id)
for( i in 1:ncol(cluster_info)){
    if (!(i%in% non_numerical_ids)){
        cluster_info[,i] <- as.numeric(cluster_info[,i])
    }
}
rm(non_numerical,non_numerical_ids)
str(cluster_info)



#read in the graph
#calculate the within/across cluster connections
load("../data/friends_info/edgelist_Feb27/RData/adj_followers_ego.RData")
ls()
#[1] "adj_list_ids"   "cluster_info"   "followers_info" "i"             [5] "sns"
i_set = rep(1:length(adj_list_ids), times = sapply(adj_list_ids,length))
j_set = unlist(adj_list_ids)
nNodes = length(adj_list_ids)
library(Matrix)
A <- sparseMatrix(i=i_set, j= j_set,dims = c(nNodes,nNodes))
sum(A); length(i_set); length(j_set) # all equal to 20766474
z <- match(followers_info$cluster_type, c("alt right","far right conservatives","Trump supporters","mainstream conservatives",
                                          "mainstream politics", "men's interest","liberals",
                                          "apolitical","other countries"))
sum(is.na(z)) #55264
z[is.na(z)] =10
Z <- matrix(0, nNodes, 10)
for (i in 1:10){ Z[z==i, i] = 1 }
colnames(Z) <-c("alt right","far right conservatives","Trump supporters","mainstream conservatives",
                "mainstream politics", "men's interest","liberals",
                "apolitical","other countries","unknown")

#pre announcement
Z1 <- matrix(0, nNodes, 10)
Z1[followers_info$period == "pre announcement",] = Z[followers_info$period == "pre announcement",]
Z2 <- matrix(0, nNodes, 10)
Z2[followers_info$period == "primary election",] = Z[followers_info$period == "primary election",]
Z3 <- matrix(0, nNodes, 10)
Z3[followers_info$period == "general election",] = Z[followers_info$period == "general election",]

S <- t(Z) %*% A %*% Z
S1 <- t(Z1) %*% A %*% Z1
S2 <- t(Z2) %*% A %*% Z2
S3 <- t(Z3) %*% A %*% Z3
cluster_type = c("alt right","far right conservatives","Trump supporters","mainstream conservatives",
                 "mainstream politics", "men's interest","liberals",
                 "apolitical","other countries", "unknown")
save(S,S1,S2,S3, cluster_type, file ="../results_following/following_within_across_total.RData" )
write.csv(as.matrix(S1), file = "../results_following/following_within_across_pre-announcement.csv")
write.csv(as.matrix(S2), file = "../results_following/following_within_across_primary.csv")
write.csv(as.matrix(S3), file = "../results_following/following_within_across_general.csv")
write.csv(as.matrix(S), file = "../results_following/following_within_across_overall.csv")






#retweeting