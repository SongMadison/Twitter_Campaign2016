rm(list = ls())

library(igraph)
library(Matrix)
library(irlba)
library(ggplot2)
library(data.table)
source("function.R")


####################random sample ###
# on the random 100K
if( !file.exists("../data/followers_Network/data1.RData")){
  # followers Info
  ID_SNs <- fread("../data/followers_Network/id_counts.csv", 
                  colClasses = c("integer", "integer", "character","character","character") )
  
  # friends info
  friends_ID_SN <- fread('../data/followers_Network/friends_ID_SN_random.csv',
                         colClasses = c("integer", "integer", "character","character"))
  setkey(friends_ID_SN, id_str)
  
  #edgelist of from the 100K followers
  edgelist = fread("../data/followers_Network/edgelist-non-random100K.csv",
                   colClasses = c( "character","character") )
  followersIDs <- edgelist$followers_id_str
  idx1 <- match(followersIDs, ID_SNs$id_str) 
  print(paste0("--- number of followersIDs, not available in Trump's all followers: -- ",
               sum(is.na(idx1))))
  edgelist$followers_id_str[which(!is.na(idx1))] <- 
             ID_SNs$screen_name[idx1[!is.na(idx1)]]
  friendsIDs <- edgelist$friends_id_str
  idx2 <- match(friendsIDs, friends_ID_SN$id_str) 
  print(paste0("--- number of followersIDs, not available in whole friends set: -- ",
               sum(is.na(idx2)) ,"/", sum(length(idx2))))
  edgelist$friends_id_str[which(!is.na(idx2))]<- 
    friends_ID_SN$screen_name[idx2[!is.na(idx2)]]
  
  #remove rows that don't have any info
  edgelist <- edgelist[-which(is.na(idx1)+is.na(idx2)>0)]
  
  #rows and columns are considered to be different, even with same screen_name
  edgelist$friends_id_str <- paste0("W_",edgelist$friends_id_str)
  edgelist$followers_id_str <- paste0("V_",edgelist$followers_id_str) 
  
  res1 <- createGraph( as.matrix(edgelist ))
  A <- res1$adj
  id.trump = which.max(colSums(A))
  A1 <- A[ ,-id.trump]  # remove trump
  
  deg.in <- colSums(A1); deg.out <- rowSums(A1)
  tau.in = sqrt(mean(deg.in))
  tau.out = sqrt(mean(deg.out))
  L1 <- Diagonal(n = length(deg.out), (deg.out+tau.out)^(-1/2))%*%A1 %*% Diagonal(length(deg.in), (deg.in+tau.in)^(-1/2)) 
  svd_L1 <- irlba(L1, 50)
  
  save(A1, svd_L1, file =  "../data/followers_Network/data1.RData")
}else{
  load( "../data/followers_Network/data1.RData")
}

# clustering individually.
plot(svd_L1$d)
k =12
set.seed(100)
U1 <- svd_L1$u[,1:k]
rowN <- unlist(apply(U1, MARGIN = 1, function(x) sqrt(sum(x*x)+1e-8)))
U1 <- Diagonal(length(rowN), rowN^(-1/2))%*%U1
km1_row = kmeans(U1 , k, nstart = 100, iter.max =50) 

V1 <- svd_L1$v[,1:k]
colN <- unlist(apply(V1, MARGIN = 1, function(x) sqrt(sum(x*x)+1e-8)))
V1 <- Diagonal(length(colN), colN^(-1/2))%*%V1
km1_col = kmeans(V1 , k, nstart = 100, iter.max =50) 

#bloomplot:
km_obj <- km1_col
p1 <- balloonplot(A1, km1_row$cluster, km1_col$cluster)


deg.res <- high.deg.cluster(A1, km1_row$cluster, km1_col$cluster )
high.deg.friends_screenN <- deg.res$high.deg.rows
write.csv(high.deg.friends_screenN, 
          file = "../data/followers_Network/km1UV_high_deg_friends_by_cluster_random.csv")


high.deg.followers_screenN <- deg.res$high.deg.cols
write.csv(high.deg.followers_screenN, 
          file = "../data/followers_Network/km1UV_high_deg_followers_by_cluster_random.csv")


unU1 <- svd_L1$u[,1:k]  #unnomarlized U2
rownames(unU1) <- rownames(A1)
high.innerprod.followers <- high.innerProd(unU1, U1, km1_row$cluster)
write.csv(high.deg.followers_screenN, 
          file = "../data/followers_Network/km1UV_inner_prod_followers_by_cluster_random.csv")

unV1 <- svd_L1$u[,1:k]  #unnomarlized U2
rownames(unV1) <- rownames(A1)
high.innerprod.followers <- high.innerProd(unV1, V1, km1_col$cluster)
write.csv(high.deg.followers_screenN, 
          file = "../data/followers_Network/km1UV_inner_prod_friends_by_cluster_random.csv")



k =12
X1<-rbind(U1, V1)
km1_X  <- kmeans(X1, centers = k, nstart = 100, iter.max = 50)
label_1 <- km1_X$cluster[1:nrow(U1)]
label_2 <- km1_X$cluster[(nrow(U1)+1):nrow(X1)]


p1_2 <- balloonplot(A1, label_1, label_2)
 

result <- high.deg.cluster(A1, label_1, label_2)
high.deg.followers_screenN <- result$high.deg.rows
write.csv(high.deg.friends_screenN, 
          file = "../data/followers_Network/km1X_high_deg_followers_by_cluster_random.csv")
high.deg.friends_screenN <- result$high.deg.cols
write.csv(high.deg.followers_screenN, 
          file = "../data/followers_Network/km1X_high_deg_friends_by_cluster_random.csv")

#most aligned and highest degree nodes in each clusters
inner.prod.followers_screenN <- high.innerProd(unU1, U1, label_1)
write.csv(high.deg.followers_screenN, 
          file = "../data/followers_Network/km1X_inner_prod_followers_by_cluster_random.csv")

inner.prod.friends_screenN <- high.innerProd(unV1, V1, label_2)
write.csv(high.deg.friends_screenN, 
          file = "../data/followers_Network/km1X_inner_prod_friends_by_cluster_random.csv")



###
pdf("../data/followers_Network/balloonplots_random.pdf", onefile = T)
p1
p1_2
dev.off()








