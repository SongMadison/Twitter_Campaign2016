

###  retweet/reply network from 76k followers and trump's tweets, from 2015-01-01 - 2016-11-08(included)
# each followers has 1200 recent twitters of each followers, due to time limit, 
#   also we the the range of time interval

rm(list =ls())
source("Head_file.R")
# library(Matrix)
# library(ggplot2)
# library(irlba)
# library(igraph)
# library(tm)
source("function.R")
set.seed(123)
load("../data/trump_tweets/followers_tweets/edgelistAll.RData")

tweets$created_at <- as.POSIXct(tweets$created_at)
tweets$source <- gsub('.* rel="nofollow">(.*)</a>',replacement = '\\1', tweets$source)


u2t <- graph_component(el, connected = F)  #bipartiete graph, followers/users to tweets
A <- u2t
user_ids <- rownames(A); tweets_ids <- colnames(A)   # twitts_ids
nr <- dim(A)[1]; nc <- dim(A)[2]
Dr <- rowSums(A); Dc <- colSums(A)
taus = mean(A) ; tau1 <- mean(Dr); tau2 <-mean(Dc)
L <- Diagonal(nr, (Dr+tau1)^(-1/2)) %*% A %*% Diagonal(nc, (Dc +tau2)^(-1/2))
irlba_L <- irlba::irlba(L, nv = 52)
plot(irlba_L$d, main ="scree plot of L_A"); abline(v = 10, col= "red", lty=2)
k=10
bip.result = list(); 
bip.result$row <- rep(NA, nr); bip.result$col <- rep(NA, nc);
U <- irlba_L$u[,1:k] ; V <- irlba_L$v[,1:k] 
norm1 <- rowSums(U*U); norm1 <- sqrt(norm1+1e-6); U1 <- Diagonal(nr, norm1^(-1)) %*% U
norm2 <- rowSums(V*V); norm2 <- sqrt(norm2+1e-6); V1 <- Diagonal(nc, norm2^(-1)) %*% V
X1 <- rbind(U1, V1);  
km1 <- kmeans(X1, k , iter.max = 100, nstart = 50);
km1 <- kmeans(X1, centers = km1$centers[order(km1$centers[,2]),], iter.max = 50)
bip.result$row <- km1$cluster[1:nr]; bip.result$col <- km1$cluster[(nr+1):(nr+nc)]

# tweets_A <- tweets[match(colnames(A), tweets$id_str),]
# clustered_tweets_by_retweet <- NULL
# for( i in 1:k){
#   #i= 15
#   tweets_i <- tweets_A[which( bip.result$col == i),]
#   scores <- U[which(bip.result$col == i),1:k]  %*% matrix(km1$centers[i,])
#   clustered_tweets_by_retweet <- rbind( clustered_tweets_by_retweet, 
#                                      data.frame(cluster_id = rep(i, nrow(tweets_i)),
#                                                 tweets_i[order(-scores),])  )                         
# }
# clustered_tweets_by_retweet <- clustered_tweets_by_retweet[!is.na(clustered_tweets_by_retweet$text)]



### pure text 
text <- tweets$text # from the downloading file
tdm <- bag_of_word_tweets(text, bigram = F)
sprintf( "after initial cleaning: %s words remains in %s docuemnts",
         dim(tdm)[1], dim(tdm)[2], '\n')  
inspect(removeSparseTerms(tdm[, 1:20], 0.95))  
dim(tdm); terms <- tdm$dimnames$Terms 
# 3377 8685
At <- spMatrix(i = tdm$i, j = tdm$j, x = tdm$v, nrow = tdm$nrow, ncol  = tdm$ncol) # frequency count
At <- t(At); At <- as(At, "dgCMatrix"); #A@x <- rep(1, length(A@x))
colnames(At) <- terms; rownames(At) <- tweets$id_str
At <- At[, -grep("http", colnames(At))] ; dim(At)
idx1<- grep("^#", colnames(At));
idx2 <- grep(' ', colnames(At)[idx1]); if (length(idx2) > 0)idx1 <- idx1[-idx2]
At[,idx1] <- At[,idx1] *2 ## double the effects of hashtag
Dr <- rowSums(At) ; Dc <- colSums(At)
nr <- dim(At)[1]; nc <- dim(At)[2]; tau1 <- mean(Dr); tau2 = mean(Dc)
Lt <- Diagonal(nr, (Dr+tau1)^(-0.5)) %*% At %*% Diagonal(nc, (Dc+tau2)^(-0.5)) 
irlba_Lt <- irlba::irlba(Lt, nv = 52 )  # make sure converge
d <- irlba_Lt$d; U <- irlba_Lt$u; V <- irlba_Lt$v 
plot(d); abline(v =20, lty= 2, col ="red", main= "screen plot Lt");abline(v =132, lty= 2, col ="red");

k = 20
X2 = U[,1:k] #%*% Diagonal(k, d[1:k]^(1/2))
X2 <- Diagonal(nrow(X2), sqrt(rowSums(X2*X2)+1e-6)^(-1)) %*% X2
km2 <- kmeans(X2, k, nstart = 100, iter.max = 50); 
km2 <- kmeans(X2, centers = km2$centers[order(km2$centers[,1]),], iter.max = 50)

clustering <-  data.frame(id_str = tweets$id_str, cluster = km2$cluster)
#write.csv(clustering, file = "./0102/tweets_text/id_cluster.csv", row.names = F)

clustered_tweets_by_text <- NULL
for( i in 1:k){
  #i= 15
  tweets_i <- tweets[which( km2$cluster == i),]
  scores <- U[which(km2$cluster == i),1:k]  %*% matrix(km2$centers[i,])
  clustered_tweets_by_text <- rbind( clustered_tweets_by_text, 
                                     data.frame(cluster_id = rep(i, nrow(tweets_i)),
                                                tweets_i[order(-scores),])  )                         
}



#### part3 combine L and Lt
lambda = 0.5  # join together
dim(L); dim(Lt)
idx1 <- match(rownames(Lt), colnames(L)); sum(is.na(idx1)) #364
L_lambda <- cbind(lambda*t(L[,idx1]), Lt)

irlba_Llabda <- irlba::irlba(L_lambda, nv = 52)
d <- irlba_Llabda$d; U <- irlba_Llabda$u; V <- irlba_Llabda$v 
plot(d); abline(v =20, lty= 2, col ="red", main= "screen plot Lt");abline(v =20, lty= 2, col ="red");

k = 20
X3 = U[,1:k] #%*% Diagonal(k, d[1:k]^(1/2))
X3 <- Diagonal(nrow(X3), sqrt(rowSums(X3*X3)+1e-6)^(-1)) %*% X3
km3 <- kmeans(X3, k, nstart = 100, iter.max = 50); 
km3 <- kmeans(X3, centers = km3$centers[order(km3$centers[,1]),], iter.max = 50)

clustering <-  data.frame(id_str = tweets$id_str, cluster = km3$cluster)
write.csv(clustering, file = "./0102/combined/id_cluster.csv", row.names = F)

clustered_tweets_by_combined <- NULL
for( i in 1:k){
  #i= 15
  tweets_i <- tweets[which( km3$cluster == i),]
  scores <- U[which(km3$cluster == i),1:k]  %*% matrix(km3$centers[i,])
  clustered_tweets_by_combined  <- rbind( clustered_tweets_by_combined , 
                                     data.frame(cluster_id = rep(i, nrow(tweets_i)),
                                                tweets_i[order(-scores),])  )                         
}
write.csv(clustered_tweets_by_combined, file = "./0102/combined/clustered_tweets_retweet_text.csv", row.names = F)

tab <- table(clustered_tweets_by_combined$cluster_id,clustered_tweets_by_combined$source)
#tab <- Diagonal(k, rowSums(tab)^(-1)) %*% Matrix(tab)
write.csv(as.matrix(tab)[,c(5,8,4,10,1:3,9,6:7)], file = "./0102/combined/cluster_vs_source.csv")
save(list = ls(), file = "./0102/result_lambda.RData")






