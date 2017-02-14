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
nrow(ID_SNs)# 5138802

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
deg_row = rowSums(A); deg_col = colSums(A)


# library(smappR)
# friends_ID_SN <- getUsersBatch(screen_name = name2, 
#             output = "../data/followers_Network/friends_non-random2.json",
#                     oauth_folder = "./credentials/credential_mixed2/")
# #
friends_ID_SN <- fread('../data/followers_Network/friends_ID_SN_nonrandom.csv',
                       colClasses = c("integer", "integer", "character","character")) #367407
setkey(friends_ID_SN, screen_name)
count2 <- friends_ID_SN[name2,followers_count]


summary(count2)
summary(deg_col)

#
hist(log(1+deg_row), breaks = 100)
hist(log(1+deg_col), breaks = 100)
hist(log(1+count2), breaks = 100)

#id.trump = which.max(deg_col)
#A <- A[ ,-id.trump]

n = dim(A)[1]
m = dim(A)[2]
n;m

## using population col deg + inner product
L1 <- A %*% Diagonal(m, (count2+ mean(count2))^(-0.5))
norm1 <- rowSums(L1 *L1)#mean(rowSums(L4))
norm1 <- norm1 + 0.1*mean(norm1) 
L1 =  Diagonal(n, norm1^(-0.5)) %*% L1
L =L1
svd_L <- irlba(L, nv = 50)



## both population column and row degree, regularized spectral clustering
L2 = Diagonal(n, (count1+10)^(-0.5)) %*% A %*% Diagonal(m, count2^(-0.5))
L = L2
svd_L <- irlba(L, nv = 50)


# 
# 
# #log degree
# n = dim(A)[1]
# idf_col <- log((n+1)/(deg_col+1))
# m = dim(A)[2]
# idf_row = log((m+1)/(deg_row+1))
# L3 = Diagonal(n, idf_row) %*% A %*% Diagonal(m, idf_col)
# 
# 
# 
# ## scale column + innter proeduct
# L3 = A %*% Diagonal(m, idf_col)
# norm1 <- rowSums(L3 *L3)
# L3 =  Diagonal(n, norm1^(-0.5)) %*% L3
# 
# 
# ### scale columns with sqrt deg, and interproduct
# tau2 <- mean(deg_col)
# L4 = A %*% Diagonal(m, (tau2+deg_col)^(-1/2))
# norm1 <- rowSums(L4 *L4)+ 1                         #mean(rowSums(L4))
# L4 =  Diagonal(n, norm1^(-0.5)) %*% L4




plot(svd_L$d)
k = 10 # or 7


U <- svd_L$u[,1:k]%*%Diagonal(k, svd_L$d[1:k]^(1/2))
km_row1 = kmeans(U, k, nstart = 200, iter.max =50)

rowN <- unlist(apply(U, MARGIN = 1, function(x) sqrt(sum(x*x)+1e-8)))
U1 <- Diagonal(length(rowN), rowN^(-1))%*%U
km_row2 = kmeans(U1, k, nstart = 200, iter.max =50)

km_row = km_row1

V <- svd_L$v[,1:k]
colN <- unlist(apply(V, MARGIN = 1, function(x) sqrt(sum(x*x)+1e-8)))
V1 <- Diagonal(length(colN), colN^(-1))%*%V
km_col = kmeans(V1 , k, nstart = 100, iter.max =50)


### evaluation of clusters

#2- dimension visualization:
set.seed(12)
samp1 <- sample(1:nrow(U), 4000)
plot(U[samp1,1],U[samp1,2], col = as.factor(km_row$cluster[samp1]))


# kmeans centers:
top = 20
keyfriends <- matrix("", top, k)
scores <- matrix(0, top, k)
for(i in 1:k){
  c_i1 <- colMeans(L[which(km_row$cluster==i), ])
  c_i2 <- colMeans(L[which(km_row$cluster!=i), ])
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
write.csv(result, file ="../data/followers_Network/result_50.csv")




## look at individual egienvectors
eigenfollowers <- matrix("", 100, k)
scores <- matrix("", 100, k)
for (i in 1:k){
   vec = U1[,i]
   tmp1 <- name1[order(vec)[1:50]]   # smallest
   s1 <- vec[order(vec)[1:50]]
   tmp2 <- name1[order(-vec)[1:50]] # biggest
   s2 <- vec[order(-vec)[1:50]]
   eigenfollowers[,i] <- c(tmp1, tmp2)
   scores[,i] <- c(s1,s2)
}

library(smappR)
eigenfollowers_info <- getUsersBatch( 
  screen_names = as.vector(eigenfollowers),                                
  include_entities = T, 
  oauth_folder = "./credentials/credential_mixed2/")

clustering <- cbind(as.vector(eigenfollowers),rep(1:k, each =100),
                 as.vector(scores))
clustering <- data.table(clustering)
names(clustering) <- c("screen_names","eigenvec","scores")
setkey(clustering, screen_names)
eigenfollowers_info <- data.table(eigenfollowers_info)
setkey(eigenfollowers_info, screen_name)
result <- cbind(clustering,eigenfollowers_info[clustering$screen_name])
write.csv(result, file ="../data/followers_Network/result.csv")

#bloomplot:
p <- balloonplot(L, km_row$cluster, km_col$cluster)
p

#based on degree  
result <- high.deg.cluster(L, km_row$cluster, km_col$cluster)
high.deg.followers_screenN <-  result$high.deg.rows

high.deg.friends_screenN <- result$high.deg.cols

# write.csv(high.deg.followers_screenN, 
#           file = "../data/followers_Network/7_kmUV_high_deg_followers_by_cluster_nonrandom.csv")
# write.csv(high.deg.friends_screenN, 
#           file = "../data/followers_Network/7_km_UV_high_deg_friends_by_cluster_nonrandom.csv")


### inner product
rownames(U) <- rownames(A)
high.innerprod.followers <- high.innerProd(U, U1, km_row$cluster)
high.prod.followers_screenN = high.innerprod.followers

rownames(V) <- colnames(A)
high.innerprod.friends <- high.innerProd(V, V1, km_col$cluster)
inner.prod.friends_screenN <- high.innerprod.friends 

# write.csv(high.deg.followers_screenN, 
#           file = "../data/followers_Network/7_kmUV_inner_prod_followers_by_cluster_random.csv")
# write.csv(high.deg.friends_screenN, 
#           file = "../data/followers_Network/7_kmUV_inner_prod_friends_by_cluster_random.csv")



X <- rbind(U1, V1)
km_X <- kmeans(X, k, nstart = 100, iter.max = 50)


label_1 <- km_X$cluster[1:nrow(U)]
label_2 <- km_X$cluster[(nrow(U)+1):nrow(X)]


p <- balloonplot(A, label_1, label_2)


result <- high.deg.cluster(A, label_1, label_2)
high.deg.followers_screenN_X <- result$high.deg.rows
high.deg.friends_screenN_X <- result$high.deg.cols
# write.csv(high.deg.friends_screenN_X, 
#           file = "../data/followers_Network/7_kmX_high_deg_followers_by_cluster_nonrandom.csv")
# write.csv(high.deg.followers_screenN_X, 
#           file = "../data/followers_Network/7_kmX_high_deg_friends_by_cluster_nonrandom.csv")

#most aligned and highest degree nodes in each clusters
inner.prod.followers_screenN_X <- high.innerProd(unU, U, label_1)
inner.prod.friends_screenN_X <- high.innerProd(unV, V, label_2)

# write.csv(high.deg.followers_screenN_X, 
#           file = "../data/followers_Network/7_kmX_inner_prod_followers_by_cluster_nonrandom.csv")
# write.csv(high.deg.friends_screenN_X, 
#           file = "../data/followers_Network/7_kmX_inner_prod_friends_by_cluster_nonrandom.csv")



weight = (deg_col/ count2)
A1 <- A %*% Diagonal(m, weight)
l1 <- rowSums(A1*A1); l1 <- sqrt(l1+1e-6); A2 <- Diagonal(n, l1^(-1))
nnzeros <- colSums(A>0)
A3 <- A2 * A1 * log( (n+1)/(nnzeros+1) )   # min(count2)>=1000, can be ommitted


L4 <- A %*% Diagonal(m, sqrt(deg_col/ count2) * log( (n+1)/(deg_col+1) )  ) # min(count2)>=1000, can be ommitted
norm1 <- rowSums(L4 *L4)                               #apply cannot work, saying too large
norm1 <- norm1 + 0.1*mean(norm1) 
L4 =  Diagonal(n, norm1^(-0.5)) %*% L4
svd_L4 <- irlba(L4, nv = 50)






# when tehre is anohte rclustering, compare the mathching
confMatrix <- t(Z1) %*% Y
pdf(file = "./1209/following/k50/blockB.pdf", onefile = T, width = 8, height = 7)
library(ggplot2)
confMatrix1 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix;
ggplot(melt(as.matrix(confMatrix1)), aes(x=X1, y= X2, fill=value)) + 
  geom_tile()+ labs(title = "confMat1") + xlab("row") + ylab("col")
confMatrix2 <- confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1));
ggplot(melt(as.matrix(confMatrix2)),aes(x=X1, y= X2, fill=value)) + 
  geom_tile()+ labs(title = "confMat2") + xlab("row") + ylab("col")
confMatrix3 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1))
ggplot(melt(as.matrix(confMatrix3)),aes(x=X1, y= X2, fill=value)) + 
  geom_tile()+ labs(title = "confMat3") + xlab("row") + ylab("col") #+scale_fill_gradient(low="green", high="red")
dev.off()
NMI(confMatrix)

