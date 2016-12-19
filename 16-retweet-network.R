### engagement among Trump's followers and Trump's twitters

## folders with 1200 recent twitters of ech followers

rm(list =ls())
source("Head_file.R")

# source("16-1.R")
# source("16-2.R")
# source("16-3.R")


graph_component <- function(edgelist){
  user_ids <- unique(edgelist$user_id_str); tweets_ids <- unique(edgelist$status_id_str)
  graph <- graph_from_edgelist(as.matrix(edgelist), directed = T)
  clustg <- clusters(graph)
  giant_id <- which.max(clustg$csize)
  giant <- induced_subgraph(graph, vids = which(clustg$membership==giant_id)) 
  nodes <- names( V(giant) ); 
  idx_users <- match(user_ids, nodes); idx_users <- idx_users[which(!is.na(idx_users))];
  idx_tweets <- match(tweets_ids, nodes); idx_tweets <- idx_tweets[which(!is.na(idx_tweets))];
  A <- get.adjacency(giant) ; dim(A);sum(A)/nrow(A)
  A <- A[idx_users,idx_tweets]; 
  return(A)
}


representative_rows <- function(X, clust, top = 10){
  nr <- dim(X)[1]; k <- dim(X)[2]
  norm1 <- sqrt(1e-6 + rowSums(X*X))
  X1 <- Diagonal(nr,norm1^(-1))%*%X
  Z <- membershipM(clust)
  NZ <- Z %*% Diagonal(k, 1/colSums(Z))
  centers <- t(NZ) %*% X1
  goodrows <- matrix(NA, top, k)
  for( i in 1:k ){
    top1 <- min(top, sum(clust == i))
    values <- X[clust == i, ]%*%as.vector(centers[i,])
    ord <- order(-values)[1:top1]
    goodrows[1:top1,i] <- which(clust == i)[ord]
  }
  return(goodrows)
}
## retweet network from 76k followers and trump's tweets

edgelist <- read.csv("../data/trump_tweets/edgeList_fromFollowers.csv", stringsAsFactors = F)
edgelist$user_id_str <- as.character(edgelist$user_id_str);
edgelist$status_id_str <- as.character(edgelist$status_id_str)
tweets <- read.csv("../data/trump_tweets/trump_tweets_fromFollowers.csv", stringsAsFactors = F)   #2425 tweets
users <- read.csv("../data/followers_Network/followers_info_status.csv", stringsAsFactors = F)

tmp <- match(edgelist$status_id_str, tweets$id_str)
edgelist <- edgelist[!is.na(tmp),]

tmp <- match(edgelist$user_id_str, users$id_str)
edgelist <- edgelist[!is.na(tmp),]# 109 rows removed

tmp <- match(unique(edgelist$user_id_str), users$id_str); users <- users[tmp[!is.na(tmp)],]; dim(users)   #3569   14
tmp <- match(unique(edgelist$status_id_str), tweets$id_str); tweets<- tweets[tmp[!is.na(tmp)],];dim(tweets)    #2168   17
save(edgelist, users, tweets, file ="../data/trump_tweets/retweet_fromFollowers.RData")


edgelist <- read.csv("../data/trump_tweets/edgelist_fromHadoop.csv", stringsAsFactors = F)
edgelist$user_id_str <- as.character(edgelist$user_id_str);
edgelist$status_id_str <- as.character(edgelist$status_id_str); 
dim(edgelist); length(unique(edgelist$user_id_str)); length(unique(edgelist$status_id_str))

tweets <- read.csv("../data/trump_tweets/trump_tweets_fromHadoop.csv", stringsAsFactors = F) 
tweets$id_str <- as.character(tweets$id_str)  ;dim(tweets)

users <- read.csv("../data/trump_tweets/users_fromHadoop.csv", stringsAsFactors = F)
users$id_str <- as.character(users$id_str) ; dim(users)
tmp <- match(edgelist$status_id_str, tweets$id_str); sum(is.na(tmp))
edgelist <- edgelist[!is.na(tmp),]; dim(edgelist); length(unique(edgelist$status_id_str))

tmp <- match(edgelist$user_id_str, users$id_str); sum(is.na(tmp))
edgelist <- edgelist[!is.na(tmp),];dim(edgelist); length(unique(edgelist$status_id_str))

tmp <- match(unique(edgelist$user_id_str), users$id_str); users <- users[tmp[!is.na(tmp)],]; dim(users)   #3569   14
tmp <- match(unique(edgelist$status_id_str), tweets$id_str); tweets<- tweets[tmp[!is.na(tmp)],];dim(tweets)    
save(edgelist, users, tweets, file ="../data/trump_tweets/retweet_fromHadoop.RData")





load("../data/trump_tweets/retweet_fromFollowers.RData")
output_clustered_tweets <- "../data/trump_tweets/clustered_tweets_Followers.csv"
output_good_tweets <-file ="../data/trump_tweets/good_tweets_fromFollowers.csv"


load("../data/trump_tweets/retweet_fromHadoop.RData")
output_clustered_tweets <- "../data/trump_tweets/clustered_tweets_Hadoop.csv"
output_good_tweets <- "../data/trump_tweets/good_tweets_Hadoop.csv"

A <- graph_component(edgelist)  #bipartiete graph
dim(A)
user_ids <- rownames(A); tweets_ids <- colnames(A) 
k =10
labs <- DisimA(A, k)
Z <- membershipM(labs$row); Y <- membershipM(labs$col)
B <- t(Z) %*% A %*% Y
rownames(B) <- colSums(Z); colnames(B) <- colSums(Y)
B


i =1
data <- tweets[which(tweets$id_str %in% tweets_ids[labs$col == i] ), ]
data$source <- gsub( ".* rel=\"nofollow\">(.*)</a>", "\\1", data$source)
data<- data[,c('id_str',"created_at",'text','source')]; dim(data)
write.table( cbind(cluster_id = rep(i, nrow(data)), data), sep = "," ,file = output_clustered_tweets , row.names = F )
for ( i in 2:k){
  data <- tweets[which(tweets$id_str %in% tweets_ids[labs$col == i] ), ]
  write.table( cbind(cluster_id = rep(i, nrow(data)), data),  sep = ",", file = output_clustered_tweets, row.names = F , append = T)
  #write.table( cbind(cluster_id = rep(i, nrow(data)), data), col.names = F, file = clustered_tweets, row.names = T , append = T)
}

X <- labs$V  
clust <- labs$col
good_tweets_id <- representative_rows(X, clust)
tmp <- match(edgelist$status_id_str[as.vector(good_tweets_id)], tweets$id_str)
good_tweets <- matrix(tweets$text[tmp],ncol = k)
colnames(good_tweets) <- paste0("cluster",1:k)

write.csv(good_tweets, file =output_good_tweets)





# edgelist <- read.csv("../data/trump_tweets/edgelist_fromHadoop.csv", stringsAsFactors = F)
# edgelist$user_id_str <- as.character(edgelist$user_id_str);
# edgelist$status_id_str <- as.character(edgelist$status_id_str)
# tweets <- read.csv("../data/trump_tweets/tweets_fromHadoop.csv",stringsAsFactors = F) #2425 tweets
# nrow(tweets)  #13560/14129 details are avaiable
# tweets$id_str <- as.character(tweets$id_str)
# users <- read.csv("../data/followers_Network/users_fromHadoop.csv", stringsAsFactors = F)
# users$id_str <- as.characters(users$id_str)
# save(edgelist, users, tweets, file ="../data/trump_tweets/retweet_fromHadoop.RData")
# 


A <- graph_component(edgelist)  #bipartiete graph
user_ids <- rownames(A); tweets_ids <- colnames(A) 
labs <- DisimA(A, k=18)
Z <- membershipM(labs$row); Y <- membershipM(labs$col)
B <- t(Z) %*% A %*% Y
rownames(B) <- colSums(Z); colnames(B) <- colSums(Y)
B

clustered_tweets <- "../data/trump_tweets/clustered_tweets_Hadoop.csv"
k =18
i =1
data <- tweets[which(tweets$id_str %in% tweets_ids[labs$col == i] ), ]
write.table( cbind(cluster_id = rep(i, nrow(data)), data), sep = "," ,file = clustered_tweets, row.names = F )
for ( i in 2:k){
  data <- tweets[which(tweets$id_str %in% tweets_ids[labs$col == i] ), ]
  write.table( cbind(cluster_id = rep(i, nrow(data)), data),  sep = ",", file = clustered_tweets, row.names = F , append = T)
  #write.table( cbind(cluster_id = rep(i, nrow(data)), data), col.names = F, file = clustered_tweets, row.names = T , append = T)
}






















































#retweeted_status = unlist(dat$retweeted_status)
data.df <- rbind(data.df, dat1)
data1 <-  read.csv("bip1.csv", stringsAsFactors =F)
data1 <- data1[,c("X0","X1")]
data4 <- read.csv("bip4.csv", stringsAsFactors = F)
data1 <- data4[,c('X0',"X1")]

followers <- unique(data1$X0)
tweets <- unique(data1$X1)
length(followers)
length(tweets)
nrow(data1)



library(Matrix)
library(igraph)
i_set = match(data1$X0, unique(data1$X0))
j_set = match(data1$X1, unique(data1$X1))

# g <- graph_from_edgelist (as.matrix(data1), directed = T)
# vcount(g)
# ecount(g)
#A <- get.adjacency(g)

A <- sparseMatrix(i = i_set,j = j_set)
dim(A)
row_deg = rowSums(A)
col_deg = colSums(A)
summary(row_deg)
summary(col_deg)


fs <- follower2[idx[!is.na(idx)]]
commonOrnot <- !is.na(match(follower2, fs))
t.test(row_deg~commonOrnot)

