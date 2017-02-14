

rm(list = ls())

library(igraph)
library(Matrix)
library(irlba)
library(ggplot2)
library(data.table)
source("function.R")



######
# followers info
ID_SNs <- fread("../data/followers_Network//id_counts.csv", 
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

# index, followers_count, id_str, protected, screen_name # 367407 friends
friends_ID_SN <- fread('../data/followers_Network/friends_ID_SN_nonrandom.csv',
                       colClasses = c("integer", "integer", "character","character"))
setkey(friends_ID_SN, screen_name)  
dim(friends_ID_SN)

followers_info <- read.csv("../data/followers_info/jsons/sn_descriptions.csv", 
                           colClasses = c("character"), stringsAsFactors = F)



######
edgelist = fread("../data/followers_Network/edgelist-non-random100K.csv",
                 colClasses = c( "character","character") )  #10205589
followers = unique(edgelist$followers_id_str)
friends = unique(edgelist$friends_id_str)
length(followers); length(friends)

id1 <- match(followers, ID_SNs$id_str)
print(paste("#friends info unavaiable: ", sum(is.na(id1))))
name1 <- ID_SNs$screen_name[id1[!is.na(id1)]]

id2 <- match(friends, friends_ID_SN$id_str)
print(paste("#followers info unavaiable: ", sum(is.na(id2))))
name2 <- friends_ID_SN$screen_name[id2[!is.na(id2)]]; length(name2)

name2 <- name2[which(friends_ID_SN[name2]$followers_count>=1000)] ##remove some fiends 

i_set = match(edgelist$followers_id_str, ID_SNs[name1]$id_str)
j_set = match(edgelist$friends_id_str, friends_ID_SN[name2]$id_str)
idx <- unique(which(is.na(i_set)+is.na(j_set)>0))

i_set <- i_set[-idx]; j_set <- j_set[-idx];  length(i_set); length(j_set) #10096079
name1 <- name1[1:max(i_set)]; name2 <- name2[1: max(j_set)]
A <- sparseMatrix(i = i_set , j = j_set, dimnames = list(name1, name2))
dim(A)  #75938 365684




# library(smappR)
# friends_ID_SN <- getUsersBatch(screen_name = name2, 
#             output = "../data/followers_Network/friends_non-random2.json",
#                     oauth_folder = "./credentials/credential_mixed2/")
# #
# do i have more features ? # they are in folder ../data/followers_Network/friends_non-random2.json 
#364459 lines in json file
# idx, user_id_str, screen_name, follower_count
# friends_ID_SN <- fread('../data/followers_Network/friends_ID_SN_nonrandom.csv',
#                        colClasses = c("integer", "integer", "character","character")) #367407
# setkey(friends_ID_SN, screen_name)
# count2 <- friends_ID_SN[name2,followers_count]
friends_info <- fread('../data/followers_Network/friends_nonrandom_full_info.csv'
                         , colClasses = c("character")) ## 44 columns
setkey(friends_info, screen_name)
friends_info$followers_count <- as.integer(friends_info$followers_count)
count2 <- friends_info[name2,followers_count]
save(A, count2, file = "../data/followers_Network/data.RData")



rm(list =ls())
load("../data/followers_Network/data.RData")
library(igraph)
library(Matrix)
library(irlba)
library(ggplot2)
library(data.table)
source("function.R")


name1 <- rownames(A)
name2 <- colnames(A);names(count2) <- name2
deg_row = rowSums(A); deg_col = colSums(A)

summary(count2)
summary(deg_col)

#
par(mfrow =c(2,2))
hist(log(1+deg_row), breaks = 100, main ="distribution of log (sample row degree)")
hist(log(1+deg_col), breaks = 100 , main ="distribution of log (sample col degree)")
#hist(log(1+count1), breaks = 100, main ="distribution of log (population row degree)")
hist(log(1+count2), breaks = 100, main ="distribution of log (population row degree)")
par(mfrow =c(1,1))

#some frequent followers (population v.s sample)
count2[which(count2 > quantile(count2, 0.9999))]
deg_col[which(deg_col > quantile(deg_col, 0.9999))]
ratio <- deg_col/count2
ratio[which(ratio > quantile(ratio, 0.9999))]*(122/0.76)  #76k out 12.2
#id.trump = which.max(deg_col)
#A <- A[ ,-id.trump]

n = dim(A)[1]
m = dim(A)[2]


## using population col deg + inner product
A1 <- A %*% Diagonal(m, (deg_col/ count2) * log( (n+1)/(deg_col+1) )  ) # min(count2)>=1000, can be ommitted
norm1 <- rowSums(A1 *A1);     norm1 <- sqrt(norm1 + 1e-4)       #apply cannot work, saying too large
L =  Diagonal(n, norm1^(-0.5)) %*% A1
svd_L <- irlba(L, nv = 50+2)   # 5 mins





plot(svd_L$d)

#normalization of rows
k = 50 # or 7
U <- svd_L$u[,1:k] %*%Diagonal(k, svd_L$d[1:k]^(1/2))      # first eigenvectors are more important??

#km_row1 = kmeans(U, k, nstart = 500, iter.max =50)

rowN <- rowSums(U*U); rowN <- sqrt(rowN +1e-6)
U1 <- Diagonal(length(rowN), rowN^(-1))%*%U
set.seed(123)
km_row2 = kmeans(U1, k, nstart = 100, iter.max =50)
# number the cluster, in the order of 2nd eigenvector
# km_row3 = kmeans(U1, centers = km_row2$centers[order(km_row2$centers[,2]),], iter.max = 30)  


km_row = km_row2
# representative words
top = 40
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


keyfriends_info <- friends_info[as.vector(keyfriends)] 
keyfriends_info <- keyfriends_info[,c('id_str', 'screen_name','created_at', 'description','lang','location','time_zone',
                           'name','followers_count','friends_count', 'favourites_count','statuses_count','listed_count','protected','verified',
                           'geo_enabled' ), 
                         names(keyfriends_info), with = FALSE]
clustering <- cbind(as.vector(keyfriends),rep(1:k, each = top),
                  rep(km_row$size, each = top),  as.vector(scores))
clustering <- data.frame(clustering, stringsAsFactors = F)
names(clustering) <- c("screen_name","clusters","Sizes","scores")

result <- cbind(clustering[,2:4],keyfriends_info)
result$description <- gsub("[\t\n\r]", " ", result$description)
write.csv(result, file ="./1209/following/k50/distinctive_friends.csv", row.names = F)
sn_cluster <- data.frame(cbind( screenNames = rownames(A), cluster = km_row$cluster))
ids <- ID_SNs$id_str[match(sn_cluster$screenNames,ID_SNs$screen_name)]
sn_cluster <- cbind(id_str = ids, sn_cluster)
write.csv(sn_cluster, file = "./1209/following/k50/id_sn_cluster.csv", row.names = F)



#one way - to download directly, there is time conflicts
# library(smappR)
# keyfriends_info <- getUsersBatch( 
#   screen_names = as.vector(keyfriends),                                
#   include_entities = T, 
#   oauth_folder = "./credentials/credential_mixed2/")
# 
# 
# clustering <- cbind(as.vector(keyfriends),rep(1:k, each =20),
#                     rep(km_row$size, each =20),  as.vector(scores))
# clustering <- data.table(clustering)
# names(clustering) <- c("screen_names","clusters","Sizes","scores")
# setkey(clustering, screen_names)
# keyfriends_info <- data.table(keyfriends_info)
# setkey(keyfriends_info, screen_name)
# result <- cbind(clustering,keyfriends_info[clustering$screen_name])
# write.csv(result, file ="../data/followers_Network/result_50.csv")
# 




## bloomplot  -- cluster level
Z <- matrix(0, n, k )
for ( i in 1:k){
  Z[which(km_row$cluster == i), i ] <- 1
}
NZ <- Z %*% Diagonal(k, colSums(Z)^(-1))
B1 <- t(NZ) %*% A %*%t(A1) %*% NZ; image(B1) ## weighted A
B2 <- t(NZ) %*% L %*%t(L) %*% NZ; image(B2)


pdf("./1209/following/k50/blockB.pdf", height = 7, width = 8)
ggplot(data = melt(as.matrix(sqrt(B1))), aes(x=X1, y =X2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(B1)))+xlab("row")+ylab("col") 
ggplot(data = melt(as.matrix(sqrt(B2))), aes(x=X1, y =X2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(B2)))+xlab("row")+ylab("col") 
dev.off()
colSums(Z);



#  2-dimension visualization:
set.seed(12)
samp1 <- sample(1:nrow(U), 2000)
pdf("../data/followers_Network/LL3_k7_n.pdf")
plot(U1[samp1,2],U1[samp1,3], pch =km_row$cluster[samp1],  col = as.factor(km_row$cluster[samp1]))
legend("topright", legend = paste0(1:k,"-",km_row$size), pch=1:k, col =as.factor(1:k) )
dev.off()







