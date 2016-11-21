

rm(list = ls())

library(igraph)
library(Matrix)
library(irlba)
library(ggplot2)
library(data.table)
source("function.R")



######
# followers info
ID_SNs <- fread("../data/followers_Network/id_counts.csv", 
                colClasses = c("integer", "integer", "character","character","character") )
setkey(ID_SNs, screen_name)
# V1 followers_count                    id_str protected     screen_name
# 1:  428142               6           80621196     False                
# 2: 2113225               7           76264693     False                
# 3: 2810437               3           72081729     False                
# 4: 5081296               0           82450642     False                
# 5: 3459414             108 728395348515934208     False  0000000000000w
# 6: 3727564              42 745317325914705924     False 000000000000hpo
# 
tmp <- unique(ID_SNs$screen_name)
tmp <- tmp[!is.na(tmp)]
ID_SNs <- ID_SNs[tmp, mult ="first"] 
# 5138802

# index, followers_count, id_str, protected, screen_name
# friends info
friends_ID_SN <- fread('../data/followers_Network/friends_ID_SN_nonrandom.csv',
                       colClasses = c("integer", "integer", "character","character"))

setkey(friends_ID_SN, screen_name)
#index, followers_count    id_str     screen_name


  



######
edgelist = fread("../data/followers_Network/edgelist-non-random100K.csv",
                 colClasses = c( "character","character") )
followers = unique(edgelist$followers_id_str)
friends = unique(edgelist$friends_id_str)

id1 <- match(followers, ID_SNs$id_str)
print(paste("#friends info unavaiable: ", sum(is.na(id1))))
name1 <- ID_SNs$screen_name[id1[!is.na(id1)]]
id2 <- match(friends, friends_ID_SN$id_str)
print(paste("#followers info unavaiable: ", sum(is.na(id2))))
name2 <- friends_ID_SN$screen_name[id2[!is.na(id2)]] #those info are available

name2 <- name2[which(friends_ID_SN[name2]$followers_count>=1000)]
i_set = match(edgelist$followers_id_str, ID_SNs[name1]$id_str)
j_set = match(edgelist$friends_id_str, friends_ID_SN[name2]$id_str)
idx <- unique(which(is.na(i_set)+is.na(j_set)>0))

i_set <- i_set[-idx]; j_set <- j_set[-idx]
name1 <- name1[1:max(i_set)]; name2 <- name2[1: max(j_set)]
A <- sparseMatrix(i = i_set , j = j_set, dimnames = list(name1, name2))
dim(A)  #75938 365684

# library(smappR)
# friends_ID_SN <- getUsersBatch(screen_name = name2, 
#             output = "../data/followers_Network/friends_non-random2.json",
#                     oauth_folder = "./credentials/credential_mixed2/")
# #
count2 <- friends_ID_SN[name2,followers_count]
count1 <- ID_SNs[name1,friends_count] 

save(A, count2, file = "../data/followers_Network/data.RData")






load("../data/followers_Network/data.RData")
library(igraph)
library(Matrix)
library(irlba)
library(ggplot2)
library(data.table)
source("function.R")

name1 <- rownames(A)
name2 <- colnames(A)
deg_row = rowSums(A); deg_col = colSums(A)

summary(count2)
summary(deg_col)

#
hist(log(1+deg_row), breaks = 100)
hist(log(1+deg_col), breaks = 100)
#hist(log(1+count1), breaks = 100)
hist(log(1+count2), breaks = 100)

#id.trump = which.max(deg_col)
#A <- A[ ,-id.trump]

n = dim(A)[1]
m = dim(A)[2]


## using population col deg + inner product
L3 <- A %*% Diagonal(m, (deg_col/ count2) * log( (n+1)/(deg_col+1) )  ) # min(count2)>=1000, can be ommitted
norm1 <- rowSums(L3 *L3)                               #apply cannot work, saying too large
norm1 <- norm1 + 0.1*mean(norm1) 
L3 =  Diagonal(n, norm1^(-0.5)) %*% L3
svd_L3 <- irlba::irlba(L3, nv = 25)


L4 <- A %*% Diagonal(m, sqrt(deg_col/ count2) * log( (n+1)/(deg_col+1) )  ) # min(count2)>=1000, can be ommitted
norm1 <- rowSums(L4 *L4)                               #apply cannot work, saying too large
norm1 <- norm1 + 0.1*mean(norm1) 
L4 =  Diagonal(n, norm1^(-0.5)) %*% L4
svd_L4 <- irlba(L4, nv = 50)


L <- L3
svd_L <- svd_L3
plot(svd_L$d)




#normalization of rows
k = 7 # or 7
U <- svd_L$u[,1:k] %*%Diagonal(k, svd_L$d[1:k]^(1/2))      # first eigenvectors are more important??
km_row1 = kmeans(U, k, nstart = 500, iter.max =50)

rowN <- unlist(apply(U, MARGIN = 1, function(x) sqrt(sum(x*x)+1e-8)))
U1 <- Diagonal(length(rowN), rowN^(-1))%*%U
km_row2 = kmeans(U1, k, nstart = 500, iter.max =50)



km_row = km_row2
# representative words
top = 20
keyfriends <- matrix("", top, k)
scores <- matrix(0, top, k)
for(i in 1:k){
  c_i1 <- colMeans( L[which(km_row$cluster==i), ] )
  c_i2 <- colMeans( L[which(km_row$cluster!=i), ] )
  #variance stablization transformation
  c_i <- sqrt(c_i1) - sqrt(c_i2)
  names_tmp <- colnames(A)[order(-c_i)]
  c_i <- c_i[order(-c_i)]
  idx <- which(!is.na(names_tmp))[1:top]  
  
  keyfriends[,i]<- names_tmp[idx] 
  scores[,i] <- round(c_i[idx],3)
  print(c_i['realDonaldTrump'])
}



library(smappR)
keyfriends_info <- getUsersBatch( 
  screen_names = as.vector(keyfriends),                                
  include_entities = T, 
  oauth_folder = "./credentials/credential_mixed2/")


clustering <- cbind(as.vector(keyfriends),rep(1:k, each =20),
                  rep(km_row$size, each =20),  as.vector(scores))
clustering <- data.table(clustering)
names(clustering) <- c("screen_names","clusters","Sizes","scores")
setkey(clustering, screen_names)
keyfriends_info <- data.table(keyfriends_info)
setkey(keyfriends_info, screen_name)
result <- cbind(clustering,keyfriends_info[clustering$screen_name])
write.csv(result, file ="../data/followers_Network/result_LL3_k7_n.csv")


#  2-dimension visualization:
set.seed(12)
samp1 <- sample(1:nrow(U), 2000)
pdf("../data/followers_Network/LL3_k7_n.pdf")
plot(U1[samp1,2],U1[samp1,3], pch =km_row$cluster[samp1],  col = as.factor(km_row$cluster[samp1]))
legend("topright", legend = paste0(1:k,"-",km_row$size), pch=1:k, col =as.factor(1:k) )
dev.off()



#####################################################################################################
k = 10 
U <- svd_L$u[,1:k] %*%Diagonal(k, svd_L$d[1:k]^(1/2))
km_row1 = kmeans(U, k, nstart = 500, iter.max =50)

rowN <- unlist(apply(U, MARGIN = 1, function(x) sqrt(sum(x*x)+1e-8)))
U1 <- Diagonal(length(rowN), rowN^(-1))%*%U
km_row2 = kmeans(U1, k, nstart = 500, iter.max =50)

km_row = km_row2


# representative words:
top = 20
keyfriends <- matrix("", top, k)
scores <- matrix(0, top, k)
for(i in 1:k){
  c_i1 <- colMeans( L[which(km_row$cluster==i), ] )
  c_i2 <- colMeans( L[which(km_row$cluster!=i), ] )
  #variance stablization transformation
  c_i <- sqrt(c_i1) - sqrt(c_i2)
  names_tmp <- name2[order(-c_i)]
  c_i <- c_i[order(-c_i)]
  idx <- which(!is.na(names_tmp))[1:top]  
  
  keyfriends[,i]<- names_tmp[idx] 
  scores[,i] <- round(c_i[idx],3)
  print(c_i['realDonaldTrump'])
}


library(smappR)
keyfriends_info <- getUsersBatch( 
  screen_names = as.vector(keyfriends),                                
  include_entities = T, 
  oauth_folder = "./credentials/credential_mixed2/")


clustering <- cbind(as.vector(keyfriends),rep(1:k, each =20),
                  rep(km_row$size, each =20),  as.vector(scores))
clustering <- data.table(clustering)
names(clustering) <- c("screen_names","clusters","Sizes","scores")
setkey(clustering, screen_names)
keyfriends_info <- data.table(keyfriends_info)
setkey(keyfriends_info, screen_name)
result <- cbind(clustering,keyfriends_info[clustering$screen_name])
write.csv(result, file ="../data/followers_Network/result_LL3_k10_n.csv")

#   2-dimension visualization:
set.seed(12)
samp1 <- sample(1:nrow(U), 2000)
pdf("../data/followers_Network/LL3_k10_n.pdf")
plot(U1[samp1,2],U1[samp1,3], pch =km_row$cluster[samp1],  col = as.factor(km_row$cluster[samp1]))
legend("topright", legend = paste0(1:k,"-",km_row$size), pch=1:k, col =as.factor(1:k) )
dev.off()










########################################################################################
k = 20 
U <- svd_L$u[,1:k] %*%Diagonal(k, svd_L$d[1:k]^(1/2))
km_row1 = kmeans(U, k, nstart = 500, iter.max =50)

rowN <- unlist(apply(U, MARGIN = 1, function(x) sqrt(sum(x*x)+1e-8)))
U1 <- Diagonal(length(rowN), rowN^(-1))%*%U
km_row2 = kmeans(U1, k, nstart = 500, iter.max =50)

km_row = km_row2


# representative words:
top = 20
keyfriends <- matrix("", top, k)
scores <- matrix(0, top, k)
for(i in 1:k){
  c_i1 <- colMeans( L[which(km_row$cluster==i), ] )
  c_i2 <- colMeans( L[which(km_row$cluster!=i), ] )
  #variance stablization transformation
  c_i <- sqrt(c_i1) - sqrt(c_i2)
  names_tmp <- name2[order(-c_i)]
  c_i <- c_i[order(-c_i)]
  idx <- which(!is.na(names_tmp))[1:top]  
  
  keyfriends[,i]<- names_tmp[idx] 
  scores[,i] <- round(c_i[idx],3)
  print(c_i['realDonaldTrump'])
}

library(smappR)
keyfriends_info <- getUsersBatch( 
  screen_names = as.vector(keyfriends),                                
  include_entities = T, 
  oauth_folder = "./credentials/credential_mixed2/")


clustering <- cbind(as.vector(keyfriends),rep(1:k, each =20),
                  rep(km_row$size, each =20),  as.vector(scores))
clustering <- data.table(clustering)
names(clustering) <- c("screen_names","clusters","Sizes","scores")
setkey(clustering, screen_names)
keyfriends_info <- data.table(keyfriends_info)
setkey(keyfriends_info, screen_name)
result <- cbind(clustering,keyfriends_info[clustering$screen_name])
write.csv(result, file ="../data/followers_Network/result_LL3_k20_n.csv")


 
#  2-dimension visualization:
set.seed(12)
samp1 <- sample(1:nrow(U), 2000)
pdf("../data/followers_Network/LL3_k20_n.pdf")
plot(U1[samp1,2],U1[samp1,3], pch =km_row$cluster[samp1],  col = as.factor(km_row$cluster[samp1]))
legend("topright", legend = paste0(1:k,"-",km_row$size), pch=1:k, col =as.factor(1:k) )
dev.off()

########################################################################################
k = 50 
U <- svd_L$u[,1:k] %*%Diagonal(k, svd_L$d[1:k]^(1/2))
km_row1 = kmeans(U, k, nstart = 500, iter.max =50)

rowN <- unlist(apply(U, MARGIN = 1, function(x) sqrt(sum(x*x)+1e-8)))
U1 <- Diagonal(length(rowN), rowN^(-1))%*%U
km_row2 = kmeans(U1, k, nstart = 500, iter.max =50)

km_row = km_row2


# representative words:
top = 20
keyfriends <- matrix("", top, k)
scores <- matrix(0, top, k)
for(i in 1:k){
  c_i1 <- colMeans( L[which(km_row$cluster==i), ] )
  c_i2 <- colMeans( L[which(km_row$cluster!=i), ] )
  #variance stablization transformation
  c_i <- sqrt(c_i1) - sqrt(c_i2)
  names_tmp <- name2[order(-c_i)]
  c_i <- c_i[order(-c_i)]
  idx <- which(!is.na(names_tmp))[1:top]  
  
  keyfriends[,i]<- names_tmp[idx] 
  scores[,i] <- round(c_i[idx],3)
  print(c_i['DrDavidDuke'])
}

library(smappR)
keyfriends_info <- getUsersBatch( 
  screen_names = as.vector(keyfriends),                                
  include_entities = T, 
  oauth_folder = "./credentials/credential_mixed2/")


clustering <- cbind(as.vector(keyfriends),rep(1:k, each =20),
                  rep(km_row$size, each =20),  as.vector(scores))
clustering <- data.table(clustering)
names(clustering) <- c("screen_names","clusters","Sizes","scores")
setkey(clustering, screen_names)
keyfriends_info <- data.table(keyfriends_info)
setkey(keyfriends_info, screen_name)
result <- cbind(clustering,keyfriends_info[clustering$screen_name])
write.csv(result, file ="../data/followers_Network/result_LL3_k20_n.csv")


 
#  2-dimension visualization:
set.seed(12)
pdf("../data/followers_Network/LL3_k20_n.pdf")
samp1 <- sample(1:nrow(U), 2000)
plot(U1[samp1,2],U1[samp1,3], pch =km_row$cluster[samp1],  col = as.factor(km_row$cluster[samp1]))
legend("topright", legend = paste0(1:k,"-",km_row$size), pch=1:k, col =as.factor(1:k) )
dev.off()




## bloomplot 
Z <- matrix(0, n, k )
for ( i in 1:k){
   Z [which(km_row$cluster == i), i ] <- 1/km_row$size[i]
}








