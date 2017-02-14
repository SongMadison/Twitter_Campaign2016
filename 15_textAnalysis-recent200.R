



# part 1,  text cleanning, - using parallel code





#timeline text data
library(doParallel)
# Find out how many cores are available (if you don't already know)
detectCores()
## [1] 4
# Create cluster with desired number of cores
cl <- makeCluster(8)
# Register cluster
registerDoParallel(cl)
# Find out how many cores are being used
getDoParWorkers()




path = "../data/followers_Network/followers_timeline2_200/"
files <- list.files(path,full.names = T)

interval = 1000
nbreaks = ceiling(length(files)/interval)
results <- foreach (k = 1: nbreaks, .combine = rbind, .packages= c("jsonlite") 
  ) %dopar% {
  # k= 1
  idx_set = interval*(k-1)+(1:interval)
  if (k == nbreaks){
    idx_set = (interval*(k-1)+1):length(files)
  }
    
  alldata <- NULL
  user_id = NULL
  user_screenName = NULL
  id_str = NULL
  created_at = NULL
  text = NULL
  for (i in idx_set){
    #i=3
    data <- readLines(files[i])
    if (length(data) != 0){
      data.js <- myToJSON(data)
      data.df <- jsonlite::fromJSON(data.js)
      user_id <- c(user_id, unlist(data.df$user$id_str))
      #user_screenName = c(user_screenName, 
      #          rep(gsub(paste0(path,"/(.*).json"), replacement = '\\1',files[i]),
      #              length(data.df$id_str)) )
      #id_str <- c(id_str, data.df$id_str)
      #created_at = c(created_at, data.df$created_at)
      user_screenName = c(user_screenName, 
                   gsub(paste0(path,"/(.*).json"), replacement = '\\1',files[i]))
      text <- c(text, paste(data.df$text, collapse =" "))
    }
    #print(paste("i = ", i, "is done!\n"))
  }
#   alldata <- cbind(user_id ,  user_screenName,
#                    id_str,created_at, text)
  alldata <- cbind(user_screenName, text)
  return (alldata)
}
stopCluster(cl)



#results is not a data.frame yet
results <- data.frame(results)
res= list()
res$user_id <- unlist(results$user_id)
res$user_screenName <- unlist(results$user_screenName)
res$id_str <- unlist(results$id_str)
res$created_at <- unlist(results$created_at)
res$text <- unlist(results$text)
res <- as.data.frame(res, stringsAsFactors = F)
#write.csv(res, file ="../data/followers_Network/tweets_2.csv", row.names = F)


N = length(unique(res$user_screenName))
sns <- character(N)
text <- character(N)
sns[1] <- res$user_screenName[1]

start_i = 1; end_i = 1
i = 1; j = 1
while (j <= length(res$id_str)){
  if (res$user_screenName[j] == sns[i]){
    end_i = j
  }else{
    text[i] <- paste(res$text[start_i:end_i],collapse = " ")
    i = i+1
    sns[i] <- res$user_screenName[j]  #new screen Name[j]
    start_i = j
    end_i = j
    print(end_i)
  }
  j = j+1
  #if (i %%1000 == 0)message("i = " , i)
}
text[i] <- paste(res$text[start_i:end_i], collapse = " ")
data <- data.frame(list(sns =sns, text = text), stringsAsFactors = F)

## combinded two such sets, remove the duplicates
#write.csv(dat, file ="../data/followers_Network/followers_sns_text_combined.csv", row.names = F )




#########################################################################################
# text to explain whether the results  make senses or not
#########################################################################################

#break the up file
# ss = 2000
# nbreaks = ceiling(dim(dat)[1]/ss)
# for (i in 1:nbreaks){
#    #i =1
#    idx = ((i-1)*ss+1):min(ss*i,dim(dat)[1])
#    dat1 <- dat[idx,]
#    if (i <10) {
#      i_str = paste0('0',i)
#    }else{
#      i_str = i
#    }
#    filename <- paste0("../data/followers_Network/followers_timeline_combined/",i_str,".csv")
#    write.csv(dat1, file = filename, row.names = F)
# }                 

#dat <- read.csv("../data/followers_Network/followers_timeline_top200_1129/01.csv", stringsAsFactors = F)
dat <- read.csv("../data/followers_Network/followers_timeline_top200.csv", stringsAsFactors = F)

dat$text <- gsub("[\t\n\r]", " ", x = dat$text )
dat$text <- gsub("\\s+", " ", x = dat$text )
dat$text <- removeMostPunctuation(dat$text, preserve_intra_word_dashes = T)
dat$text <- removeMostNumbers(dat$text)

vc <- VCorpus( VectorSource(dat$text) ) # just change the vector of strings to corpus
ctrl <- list(#removePunctuation = list(preserve_intra_word_dashes = TRUE),
  sdistinctive_words = TRUE,
  removeNumbers = FALSE
  #, stemming = TRUE                    # remove prefix or postfix
  , bounds = list(global= c(20,Inf))   # remove low-frequency/high frequency words
  , wordLengths = c(4, 25) # remove short words like 'a' 
  #, weighting = function(x) weightSMART(x, spec = "ltc")
)


tdm <- TermDocumentMatrix(vc, control =  ctrl)  # take about 30 min

## remove some words and documents 
if (length( grep(pattern = "https|http", tdm$dimnames$Terms))>0){
  tdm <- tdm [-grep(pattern = "https|http", tdm$dimnames$Terms), ]
}
A1 <- spMatrix(i = tdm$i, j = tdm$j, x = tdm$v, nrow = tdm$nrow, ncol  = tdm$ncol) 
idx2 <- which(colSums(A1>0)<5 ); 
tdm <- tdm[,-idx2]
A1 <- A1[,-idx2]
names_documents <- dat$user_screenName[-idx2]

#tfidf <- weightTfIdf(tdm, normalize = TRUE) #some empty documents
tfidf <- weightSMART(tdm, spec= 'ltu',   control= list( pivot = 10, slope = 0.5)) #use when calculate pivoted normalization
  
# frequency vary too much, taking log, invert docoment, not normalized

print ( sprintf( "after cleaning: %s words remains in %s docuemnts",
                 dim(tdm)[1], dim(tdm)[2], '\n' ) )  



#A1 <- spMatrix(i = tdm$i, j = tdm$j, x = tdm$v, nrow = tdm$nrow, ncol  = tdm$ncol) 
A2 <- spMatrix(i = tfidf$i, j = tfidf$j, x = tfidf$v, nrow = tfidf$nrow, ncol  = tfidf$ncol) 

rownames(A1)  = tdm$dimnames$Terms; colnames(A1) = names_documents
rownames(A2)  = tfidf$dimnames$Terms ; colnames(A2) = names_documents
stopifnot(dim(A1)==dim(A2))
## save(A1,A2, file ="clean_text.RData")  # tdm, tfidf




load("clean_text.RData")
### A1 is terms-document, A2 is tf-idf of A1, normalized


deg1 <- rowSums(A1); deg2 <- colSums(A1)
d1 <- rowSums(A2); d2 <- colSums(A2)
high2 <- cbind(colnames(A2)[order(-d2)[1:50]],d2[order(-d2)[1:50]]) # high degree documents
hist(d2, breaks =100)

A <- A2
#A <- A1 ;
# A@x <- log(1+A1@x)




# based on the original clusters discovered by following relationship

## validate based on representative words
clustering <- read.csv(
   "./1209/following/k50/sn_cluster_simplified.csv", stringsAsFactors = F)
k = max(clustering$cluster)
Z1 <- matrix(0, nrow(clustering), k)
for(i in  1:k){
  Z1[which(clustering$cluster == i), i] <-1
}
x <- match(colnames(A), clustering$screenNames)
Z1 <- Z1[x[which(!is.na(x))], ]
csize <- colSums(Z1)
NZ1 <- Z1 %*% Diagonal(k,csize^(-1))  # normalized Z1
cluster_mean <- t(NZ1)%*%t(A)
grand_mean <- rowMeans(A)
distinctive_words<- matrix("", 40 ,k)
terms <- rownames(A)
for(i in 1:k){
  diff <- sqrt(cluster_mean[i,]) - sqrt(grand_mean)
  distinctive_words[,i] <- terms[order(-diff)[1:40]]
}
distinctive_words 
#write.csv(distinctive_words, file ="../report/2016-12-01/top40_tweets.csv", row.names = F)
#write.csv(distinctive_words, file ="./1209/L2/k50/keywords_50.csv", row.names = F)





### di-sim algorithm
n <- dim(A)[1]  
m <- dim(A)[2]

## L =
D1 <- rowSums(A); summary(D1) # >= 20
D2 <- colSums(A); summary(D2) # >= 5
tau1 <- mean(D1); tau2 <- mean(D2)
L <- Diagonal(n, (D1+tau1)^(-1/2)) %*% A %*%Diagonal(m, (D2+tau2)^(-1/2))

#L = A
irlba_L <- irlba(L, nv =52)
d <- irlba_L$d
U <- irlba_L$u %*% Diagonal(length(d), d^(1/2))
V <- irlba_L$v %*% Diagonal(length(d), d^(1/2))

k = 50

l1 <- rowSums(U[,1:k]^2); l1 <- sqrt(l1+1e-6); U1 <- Diagonal(n, l1^(-1))%*%U[,1:k]
r1 <- rowSums(V[,1:k]^2); r1 <- sqrt(r1+1e-6); V1 <- Diagonal(m, r1^(-1))%*%V[,1:k]
X1 <- rbind(U1, V1)
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


# evaluate the Block in-out
#pdf("./1209/L2/A1_blockB_bip.pdf", height = 7, width = 8)
ggplot(data = melt(as.matrix(sqrt(WB))), aes(x=Var1, y =Var2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(WB)))+xlab("row")+ylab("col") 
ggplot(data = melt(as.matrix(sqrt(B))), aes(x=Var1, y =Var2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(B)))+xlab("row")+ylab("col") 
dev.off()
#colSums(Z);colSums(Y)



## compare cluster of following -- Z1, text -- Y, on 45k followers
confMatrix <- t(Z1) %*% Y
NMI(confMatrix)
write.csv(confMatrix, file = "./1209/L2/k50/following_text_50x50.csv")

#pdf(file = "./1209/L2/k50/A1_confMat_50_50.pdf", onefile = T, width = 8, height = 7)
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
#dev.off()



## combining certain similar clusters into 10 categories, 11 categories each
library(xlsx)
text200 <- read.xlsx("Topics and Following clusters.xlsx", 
                       sheetName= 1, 
                       stringsAsFactors = F)

a <- c(which(!is.na(text200$Category)), 51)
Yc <- matrix(0, 50, length(a)-1)   # categorial  membership
for(i in 1: (length(a)-1)){
  tmp <- text200$topic...1[a[i]:(a[i+1]-1)]
  Yc[tmp, i] <- 1
}
name1  <- text200$Category[a[1:(length(a)-1)]]

following <- read.xlsx("Topics and Following clusters.xlsx", 
                       sheetName= 2, 
                       stringsAsFactors = F,startRow = 2)
following <- following[1:50,]
a <- c(which(!is.na(following$Category)), 51)
Zc <- matrix(0, 50, length(a)-1)
for(i in 1: (length(a)-1)){
  tmp <- following$Cluster...1[a[i]:(a[i+1]-1)]
  Zc[tmp, i] <- 1
}
name2  <- following$Category[a[1:(length(a)-1)]]
confMatrix_categorial <- t(Z1 %*% Zc) %*% (Y%*%Yc)
rownames(confMatrix_categorial) <- name2; colnames(confMatrix_categorial) <- name1
write.csv(confMatrix_categorial, file = "./1209/L2/k50/following_text_11x13.csv")

#pdf(file = "./1209/L2/k50/confMat_categorial_10x10.pdf", onefile = T, width = 8, height = 7)
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


d1 <- dim(confMatrix_categorial)[1]; d2 <- dim(confMatrix_categorial)[2]
AA <- matrix(0, d1+d2, d1+d2)
AA[1:d1,(1+d1):(d2+d1)] <-1; AA[(d1+1):(d1+d2),1:d1] <- 1
G <- graph_from_adjacency_matrix(AA)
size1 <- rowSums(confMatrix_categorial); 
size2 <- colSums(confMatrix_categorial)
set.seed(100)
l = layout.bipartite(G)
V(G)$type <- c(rep(TRUE,d1), rep(FALSE, d2))
V(G)$label = c(paste0("Fo",1:d1), paste0("Tw", 1:d2))
V(G)$shape = c(rep("circle",d1), rep("circle", d2))
V(G)$color = c(rep("dodgerblue",d1), rep("red",d2))
V(G)$size = c(log(size1),log(size2))*2
E(G)$weight =  c( as.vector(t(confMatrix_categorial)), as.vector(confMatrix_categorial))
conf1 <- Diagonal(d1, size1^(-1)) %*% confMatrix_categorial;
conf2 <- confMatrix_categorial %*% Diagonal(d2, size2^(-1))
E(G)$width <- c(as.vector(t(conf1))*10, as.vector(conf2)*0)
E(G)$color <- c(rep("dodgerblue", d1*d2), rep("red", d1*d2))
plot(G, layout = l[,2:1], edge.arrow.size = 0.3 )

E(G)$width <- c(as.vector(t(conf1))*0, as.vector(conf2)*10)
E(G)$color <- c(rep("dodgerblue", d1*d2), rep("red", d1*d2))
plot(G, layout = l[,2:1], edge.arrow.size = 0.3)

# input: Y
sizes <- colSums(Y)
documents<- A %*% Y
pdf(file = "./1209/L2/A1_frequent_words.pdf", onefile = T, width = 8, height = 9)
#words in that collection
for (i in 1:k){
wordcloud(words = rownames(documents), freq = documents[,i], 
          min.freq = documents[order(-documents[,i])[50],i], 
          rot.per = 0, random.order = F, scale = c(2,0.2))
title(paste0("cluster of size: ", sizes[i]))
}
dev.off()

#distinctive words
document_mean <- A %*% Y%*% Diagonal(k, sizes^(-1))
selected_words <- matrix("", 40, k)
row_mean <- rowMeans(A)
for (i in 1:k){
  # i =2
  diff <- sqrt(document_mean[,i]) - sqrt(row_mean) # vector
  selected_words[,i] <- rownames(document_mean)[order(-diff)[1:40]]
}
write.csv(selected_words, file = "./1209/L2/A1_distictive_words.csv")



##use assoicated words, words are disjoined
selected_words2 <- matrix("", 40, k)
weight <-  colSums(A)
terms <- rownames(A)
for (i in 1:k){
  # i =2
  idx <- which(Z[,i] ==1)
  tmp_w <- weight[ idx ]
  tmp_t <- terms[idx]
  selected_words2[,i] <- tmp_t[order(-tmp_w)[1:40]]
}
write.csv(selected_words, file = "./1209/L2/A1_distictive_words.csv")




#clustering based on similarity separately, clustering the columns and rows separately
k = 50

D1 <- rowSums(A*A); summary(D1) # >= 20
D2 <- colSums(A*A); summary(D2) # >= 5
n <- dim(A)[1]
m <- dim(A)[2]
tau1 <- mean(D1); tau2 <- mean(D2)
L1 <- Diagonal(n, (D1+tau1)^(-1/2)) %*% A  ## L^T L is the similarity among documents
L2 <- A %*%Diagonal(m, (D2+tau2)^(-1/2))  ## L^T L is the similarity among documents
irlba_L1 <- irlba(L1, nv =50)
U <- irlba_L$u

irlba_L2 <- irlba(L2, nv =50)
V <- irlba_L2$v


k =50
l1 <- rowSums(U[,1:k]^2); l1 <- sqrt(l1+1e-6); U1 <- Diagonal(n, l1^(-1))%*%U[,1:k]
r1 <- rowSums(V[,1:k]^2); r1 <- sqrt(r1+1e-6); V1 <- Diagonal(m, r1^(-1))%*%V[,1:k]
set.seed(100)
km1 <- kmeans(U, k, nstart = 100, iter.max = 30)
km2 <- kmeans(V, k, nstart = 100, iter.max = 30)

Z <- matrix(0, n, k)
for (i in 1:k){
  Z[km1$cluster == i, i] <- 1
}
Y <- matrix(0, m, k)
for (i in 1:k){
  Y[km2$cluster == i, i] <- 1
}
B <- t(Z) %*% A2 %*% Y
DB1 <- rowSums(B); DB2 <- colSums(B); WB <- Diagonal(k, DB1^(-1))%*% B %*% Diagonal(k, DB2^(-1))
# library(fields)
# sum(WB)/sum(diag(WB)); image.plot(1:k, 1:k, as.matrix(sqrt(WB)))
# sum(B)/sum(diag(B)) ; image.plot(1:k, 1:k, as.matrix(sqrt(B)))
pdf("./1209/L2_sep/blockB_bip.pdf", height = 7, width = 8)
ggplot(data = melt(as.matrix(sqrt(WB))), aes(x=Var1, y =Var2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(WB)))+xlab("row")+ylab("col") 
ggplot(data = melt(as.matrix(sqrt(B))), aes(x=Var1, y =Var2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(B)))+xlab("row")+ylab("col") 
dev.off()
colSums(Z);colSums(Y)


confMatrix <- t(Z1) %*% Y
pdf(file = "./1209/L2_sep/confMat.pdf", onefile = T, width = 8, height = 7)
library(ggplot2)
confMatrix1 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix;
ggplot(melt(as.matrix(confMatrix1)), aes(x=Var1, y= Var2, fill=value)) + 
  geom_tile()+ labs(title = "confMat1") + xlab("row") + ylab("col")
confMatrix2 <- confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1));
ggplot(melt(as.matrix(confMatrix2)),aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+ labs(title = "confMat2") + xlab("row") + ylab("col")
confMatrix3 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1))
ggplot(melt(as.matrix(confMatrix3)),aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+ labs(title = "confMat3") + xlab("row") + ylab("col") #+scale_fill_gradient(low="green", high="red")
dev.off()
NMI(confMatrix)



# input: Y
sizes <- colSums(Y)
documents<- A %*% Y
pdf(file = "./1209/L2_sep/frequent_words.pdf", onefile = T, width = 8, height = 9)
#words in that collection
for (i in 1:k){
  wordcloud(words = rownames(documents), freq = documents[,i], 
            min.freq = documents[order(-documents[,i])[50],i], 
            rot.per = 0, random.order = F, scale = c(2,0.2))
  title(paste0("cluster of size: ", sizes[i]))
}
dev.off()

#distinctive words
document_mean <- A %*% Y%*% Diagonal(k, sizes^(-1))
selected_words <- matrix("", 40, k)
row_mean <- rowMeans(A)
for (i in 1:k){
  # i =2
  diff <- document_mean[,i] - row_mean # vector
  selected_words[,i] <- rownames(document_mean)[order(-diff)[1:40]]
}
write.csv(selected_words, file = "./1209/L2_sep/distictive_words.csv")

save(list =ls(), file ="./1209/L2_sep/result_A1.RData")









