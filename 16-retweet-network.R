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


representative_rows <- function(X, clust, top = 20){
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



load("../data/trump_tweets/retweet_fromFollowers.RData")
output_clustered_tweets <- "../data/trump_tweets/clustered_tweets_Followers.xls"  # tab seperated
output_good_tweets  ="../data/trump_tweets/good_tweets_Followers.xls"

# 
# load("../data/trump_tweets/retweet_fromHadoop.RData")
# output_clustered_tweets <- "../data/trump_tweets/clustered_tweets_Hadoop.xls"
# output_good_tweets <- "../data/trump_tweets/good_tweets_Hadoop.xls"


A <- graph_component(edgelist)  #bipartiete graph
dim(A)
user_ids <- rownames(A); tweets_ids <- colnames(A)   # twitts_ids
users_A <- users[match(user_ids,users$id_str), ]; tweets_A <- tweets[match(tweets_ids, tweets$id_str),]


# labs <- DisimA(A, k)
set.seed(123)
nr <- dim(A)[1]; nc <- dim(A)[2]
Dr <- rowSums(A); Dc <- colSums(A)
taus = c(mean(Dr), mean(Dc)); tau1 <- taus[1]; tau2 <- taus[2]
L <- Diagonal(nr, (Dr+tau1)^(-1/2)) %*% A %*% Diagonal(nc, (Dc +tau2)^(-1/2))
svd <- irlba::irlba(L, nv = 50)

k = 18
plot(svd$d, main ="scree plot"); abline(v = k, col= "red", lty=2)
U <- svd$u[,1:k];   V <- svd$v[,1:k];  


labs = list(); 
labs$row <- rep(NA, nr); labs$col <- rep(NA, nc);
labs$U <- matrix(0, nr, k); labs$V <- matrix(0, nc, k)
 
norm1 <- rowSums(U*U); norm1 <- sqrt(norm1+1e-6); U <- Diagonal(nr, norm1^(-1)) %*% U
norm2 <- rowSums(V*V); norm2 <- sqrt(norm2+1e-6); V <- Diagonal(nc, norm2^(-1)) %*% V
X <- rbind(U, V);  km <- kmeans(X, k , iter.max = 30, nstart = 30);
labs$row <- km$cluster[1:nr]; labs$col <- km$cluster[(nr+1):(nr+nc)]

# km1 <- kmeans(U, k , iter.max = 30, nstart = 30); km2 <- kmeans(V, k , iter.max = 30, nstart = 30)
# labs$row <- km1$cluster; labs$column <- km2$cluster

Z <- membershipM(labs$row); Y <- membershipM(labs$col)
B <- t(Z) %*% A %*% Y
rownames(B) <- colSums(Z); colnames(B) <- colSums(Y)
B; dim(A)



clustered_tweets <- NULL
for ( i in 1:k){
  data <- tweets_A[ labs$col == i, ]
  clustered_tweets <- rbind(clustered_tweets, cbind(cluster_id = rep(i, nrow(data)), data))
  #write.table( cbind(cluster_id = rep(i, nrow(data)), data),  col.names = Tsep = ",", file = output_clustered_tweets, row.names = F , append = T)
}
write.table(clustered_tweets, file = output_clustered_tweets, sep ="\t", quote = FALSE,  row.names = F)


good_tweets_id <- representative_rows(labs$V, labs$col, 20)
sizes <- table(labs$col)
good_tweets <- cbind(cluster_id = rep(1:k, each =20), clustSize = rep(sizes, each =20), tweets_A[good_tweets_id, ])
write.table(good_tweets, file =output_good_tweets, sep = '\t', row.names = F)



#save(list =ls(), file ="../data/trump_tweets/followers_tweets/result_ReTweetFollowers.RData")



load("result_ReTweetFollowers.RData")
## result from retwitter -- bipartite graph

text <- tweets_A$text # from the downloading file
#text <- paste( followers$'description', followers$'status.text')  #downloaded around Nov 15

#toy.data <- read.csv("toy.csv", stringsAsFactors = F)
#text <- toy.data[,'description']  # use specific set

text <- removeMostPunctuation(text, preserve_intra_word_dashes = T)
text <- removeMostNumbers(text)
vc <- VCorpus( VectorSource(text) ) # just change the vector of strings to corpus
ctrl <- list(#removePunctuation = list(preserve_intra_word_dashes = TRUE),
  stopwords = TRUE,
  removeNumbers = FALSE
  
  #, stemming = TRUE                    # remove prefix or postfix
  #, bounds = list(global= c(1,Inf))   # remove low-frequency/high frequency words
  #, wordLengths = c(4, 20) # remove short words like 'a' 
  #, weighting = function(x) weightSMART(x, spec = "nnn")
)
tdm <- TermDocumentMatrix(vc, control =  ctrl)  ## term - ducoment matrix
tfidf <- weightSMART(tdm, spec = "ltc")
terms <- tfidf$dimnames$Terms
print ( sprintf( "after initial cleaning: %s words remains in %s docuemnts",
                 dim(tdm)[1], dim(tdm)[2], '\n') )  
#At = spMatrix(i = tdm$i, j = tdm$j, x = tdm$v, nrow = tdm$nrow, ncol  = tdm$ncol) # frequency count
At = spMatrix(i = tfidf$i, j = tfidf$j, x = tfidf$v, nrow = tfidf$nrow, 
              ncol  = tfidf$ncol) # weighted frequency count
rownames(At)  = tdm$dimnames$Terms
tmp <- grep("http", rownames(At))
At <- At[-tmp,]
extraTerms <- c("just","amp", "get","now","will","going","can"); 
tmp <- match(extraTerms, rownames(At))
if(sum(!is.na(tmp))>0){ At <- At[-tmp[!is.na(tmp)],] }
dim(At)

At <- t(At)


#given the clusters found in following networks
Z <- membershipM( labs$col )
clust_size <- colSums(Z)
k <- length(clust_size)
words_by_cluster <- Diagonal(k, clust_size^(-1)) %*% t(Z) %*% At
mean_vec <- colMeans(At)
terms <- colnames(At)
library(wordcloud)
pdf("words_in_cluster.pdf", width = 8, height = 9)
sizes <- colSums(Z)
for ( i in 1:k){
    diff <- words_by_cluster[i,] - mean_vec
    wordcloud(words = terms, freq = diff, 
            max.words =50,
            random.order = F, 
            scale = c(3,0.4))
  title(paste0("cluster of size: ", sizes[i]))
}
dev.off()




## clusters of twieeters
n <- dim(At)[1]  
m <- dim(At)[2]
rowN <- rowSums(At*At); rowN <- sqrt(rowN+ mean(rowN))
L <- Diagonal(n, rowN^(-1) )%*% At;
#L = At
irlba_L <- irlba(L, nv = 52)
d <- irlba_L$d; plot(d); abline(v =17, lty= 2, col ="red")
U <- irlba_L$u; V <- irlba_L$v 

k = 15
X1 = U[,1:k] %*% Diagonal(k, d[1:k]^(1/2))
set.seed(100)  # don't change!!!!!
km1 <- kmeans(X1, k, nstart = 50, iter.max = 100)

Z <- membershipM(km1$cluster)
sizes <- colSums(Z)
centers<-  Diagonal(k, sizes^(-1)) %*% t(Z) %*% At
terms <- colnames(At)
mean_vec <- colMeans(At)
pdf(file = paste0("wordscloud_",k,".pdf"), onefile = T, width = 8, height = 9)
#words in that collection
for (i in 1:k){
    diff = sqrt(centers[i,]) - sqrt(mean_vec)
    pos_idx <- which(diff > 0)
    wordcloud(words = terms[pos_idx], freq = diff[pos_idx], 
              max.words =50,
              rot.per = 0, random.order = F, scale = c(3,0.4))
    title(paste0("cluster of size: ", sizes[i]))
}
dev.off()
select_words <- matrix("", 20, k)
for ( i in 1:k){
    diff = sqrt(centers[i,]) - sqrt(mean_vec)
    select_words[,i] = terms[order(-diff)[1:20]]
}
write.csv(select_words, file =paste0("distinctive_words_",k,".csv"))


clustered_tweets_by_text <- NULL
for( i in 1:k){
    #i= 1
    tweets_i <- tweets_A$text[which(km1$cluster == i)]
    scores <- At[which(km1$cluster == i),] %*% as.vector(centers[i,])
    clustered_tweets_by_text <- rbind( clustered_tweets_by_text, 
                               data.frame(cluster_id = rep(i, length(tweets_i)),
                                      tweets = tweets_i[order(-scores)])
                             )
}
write.csv(clustered_tweets_by_text, file =paste0("clustered_tweets_by_text_",k,".csv"))

## interaction among clusters of followers and cluster of twitters


library(xlsx)
following <- read.xlsx("Topics and Following clusters.xlsx", 
                       sheetName= 2, 
                       stringsAsFactors = F)

a <- c(which(!is.na(following$Category)), 51)
ZZ1 <- matrix(0, 50, length(a)-1)
for(i in 1: (length(a)-1)){
    tmp <- following$Cluster...1[a[i]:(a[i+1]-1)]
    ZZ1[tmp, i] <- 1
}
name2  <- following$Category[a[1:(length(a)-1)]]
Z1 <- (Z%*% ZZ1) 

## L =
D1 <- rowSums(At); summary(D1) # >= 20
D2 <- colSums(At); summary(D2) # >= 5
tau1 <- mean(D1); tau2 <- mean(D2)
L <- Diagonal(n, (D1+tau1)^(-1/2)) %*% At %*%Diagonal(m, (D2+tau2)^(-1/2))





l1 <- rowSums(U[,1:k]^2); l1 <- sqrt(l1+1e-6); U1 <- Diagonal(n, l1^(-1))%*%U[,1:k]
r1 <- rowSums(V[,1:k]^2); r1 <- sqrt(r1+1e-6); V1 <- Diagonal(m, r1^(-1))%*%V[,1:k]
#X1 <- rbind(U1, V1)
X1 = U
set.seed(100)  # don't change!!!!!
km1 <- kmeans(X1, k, nstart = 50, iter.max = 30)

Z <- matrix(0, n+m, k)
for (i in 1:k){
  Z[km1$cluster == i, i] <- 1
}
Y <- Z[(n+1):(n+m),]
Z <- Z[1:n, ]
B <- t(Z) %*% L %*% Y
DB1 <- rowSums(B); DB2 <- colSums(B);WB <- Diagonal(k, DB1^(-1))%*%B %*% Diagonal(k, DB2^(-1))
# library(fields)
# sum(WB)/sum(diag(WB)); image.plot(1:k, 1:k, as.matrix(sqrt(WB)))
# sum(B)/sum(diag(B)) ; image.plot(1:k, 1:k, as.matrix(sqrt(B)))
#pdf("./1209/L2/A1_blockB_bip.pdf", height = 7, width = 8)
ggplot(data = melt(as.matrix(sqrt(WB))), aes(x=Var1, y =Var2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(WB)))+xlab("row")+ylab("col") 
ggplot(data = melt(as.matrix(sqrt(B))), aes(x=Var1, y =Var2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(B)))+xlab("row")+ylab("col") 
dev.off()
colSums(Z);colSums(Y)

Z1 <- membershipM(labs$col)
confMatrix <- t(Z1) %*% Z
library(ggplot2)
confMatrix1 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix;
ggplot(melt(as.matrix(confMatrix1)), aes(x=as.factor(Var1), y= as.factor(Var2), fill=value)) + 
  geom_tile()+ labs(title = "row sum =1") + xlab("following cluster") + ylab("text cluster")
confMatrix2 <- confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1));
ggplot(melt(as.matrix(confMatrix2)),aes(x=as.factor(Var1), y= as.factor(Var2), fill=value)) + 
  geom_tile()+ labs(title = "colnum sum =1") + xlab("following cluster") + ylab("text cluster")
confMatrix3 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1))
ggplot(melt(as.matrix(confMatrix3)),aes(x=as.factor(Var1), y= as.factor(Var2), fill=value)) + 
  geom_tile()+ labs(title = "normalized by row and col") + xlab("following cluster") + ylab("text cluster") #+scale_fill_gradient(low="green", high="red")
NMI(confMatrix)



# input: Y
Z <- membershipM(km1$cluster)
sizes <- colSums(Z)
documents<-  Diagonal(k, sizes^(-1)) %*% t(Z) %*% At
mean_vec <- colMeans(At)
pdf(file = "words_similarity3.pdf", onefile = T, width = 8, height = 9)
#words in that collection
for (i in 1:k){
  diff = documents[i,] - mean_vec
  pos_idx <- which(diff > 0)
  diff <- diff[pos_idx]; 
  wordcloud(words = names(diff), freq = diff, 
             max.words =50,
            rot.per = 0, random.order = F, scale = c(3,0.4))
  title(paste0("cluster of size: ", sizes[i]))
}
dev.off()

#distinctive words
document_mean <- At %*% Y%*% Diagonal(k, sizes^(-1))
selected_words <- matrix("", 40, k)
row_mean <- rowMeans(At)
for (i in 1:k){
  # i =2
  diff <- sqrt(document_mean[,i]) - sqrt(row_mean) # vector
  selected_words[,i] <- rownames(document_mean)[order(-diff)[1:40]]
}
write.csv(selected_words, file = "./1209/L2/A1_distictive_words.csv")



##use assoicated words, words are disjoined
selected_words2 <- matrix("", 40, k)
weight <-  colSums(At)
terms <- rownames(At)
for (i in 1:k){
  # i =2
  idx <- which(Z[,i] ==1)
  tmp_w <- weight[ idx ]
  tmp_t <- terms[idx]
  selected_words2[,i] <- tmp_t[order(-tmp_w)[1:40]]
}
write.csv(selected_words, file = "./1209/L2/A1_distictive_words.csv")






















