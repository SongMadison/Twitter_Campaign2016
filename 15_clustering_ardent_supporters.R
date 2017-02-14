

rm(list =ls())  


library(igraph)
library(Matrix)
library(irlba)
library(ggplot2)
library(data.table)



load('data4.RData')
# first 20 clusters, then further partioning some clusters
# This files includes everything on the 75K x366k data analysis

#for example:
km_row$size
keyfriends
davidduke <- numeric(20)
for (i in 1:20){
	davidduke[i] <- sum(A[which(km_row$cluster ==i), 'DrDavidDuke'])
}




#move on to focus on the Trump strong supporters
#based on my oberservation, cluster 1,3, 7, could change or include some more
ids <- which((( km_row$cluster==1)+ (km_row$cluster ==3) + (km_row$cluster==8)) >= 1)
A1 <- A[ids,]

deg1_row = rowSums(A1)
deg1_col = colSums(A1)
n = dim(A1)[1]  # around 9554
m = dim(A1)[2]

L1 <- A1 %*% Diagonal(m, (deg1_col/ count2) * log( (n+1)/(deg1_col+1) )  ) # min(count2)>=1000, can be ommitted
norm1 <- rowSums(L1 *L1)                               #apply cannot work, saying too large
norm1 <- norm1 + 0.1*mean(norm1) 
L1 =  Diagonal(n, norm1^(-0.5)) %*% L1
svd_L1 <- irlba::irlba(L1, nv = 25)


L <- L1
svd_L <- svd_L1
plot(svd_L$d)



k=10

## input: need svd obj -- svd_L
## output: kmeans obj

U <- svd_L$u[,1:k] %*%Diagonal(k, svd_L$d[1:k]^(1/2))
km_row1 = kmeans(U, k, nstart = 500, iter.max =50)

rowN <- unlist(apply(U, MARGIN = 1, function(x) sqrt(sum(x*x)) ))
rowN <- rowN +0.01*mean(rowN)
U1 <- Diagonal(length(rowN), rowN^(-1))%*%U
km_row2 = kmeans(U1, k, nstart = 500, iter.max =50)

km_row = km_row2


## input L, km_row, colnames(A): name2
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


#download friends info
library(smappR)
keyfriends_info <- getUsersBatch( 
  screen_names = as.vector(keyfriends),                                
  include_entities = T, 
  oauth_folder = "./credentials/credential_mixed2/" )

clustering <- cbind(as.vector(keyfriends),rep(1:k, each =20),
                  rep(km_row$size, each =20),  as.vector(scores))
clustering <- data.table(clustering)
names(clustering) <- c("screen_names","clusters","Sizes","scores")
setkey(clustering, screen_names)
keyfriends_info <- data.table(keyfriends_info)
setkey(keyfriends_info, screen_name)
result <- cbind(clustering,keyfriends_info[clustering$screen_name])
#write.csv(result, file ="../data/followers_Network/result_trump_k10_n.csv")

# 2-dimensional visualization:
set.seed(12)
samp1 <- sample(1:nrow(U), 2000)
# pdf("../data/followers_Network/trump_k10_n.pdf")
plot(U1[samp1,2],U1[samp1,3], pch =km_row$cluster[samp1],  col = as.factor(km_row$cluster[samp1]))
legend("topright", legend = paste0(1:k,"-",km_row$size), pch=1:k, col =as.factor(1:k) )
# dev.off()






