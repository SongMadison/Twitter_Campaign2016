
source('Head_file.R')
source("function.R")

output_folder = "./0212/"
# part 1,  text merging and cleaning, - using parallel code



merged_text_folder <- function(folder_name){
  
  merged_text <- function(file){
    data_i <- read.csv(file, colClasses = c("character"), stringsAsFactors = F)
    unique_user <- unique(data_i$user_id_str)
    merged = NULL
    for ( i in 1:length(unique_user)){
      #i =1
      texts <- data_i$text[data_i$user_id_str == unique_user[i]]
      texts <- gsub("[\t\n\r]", " ", x = texts )
      texts <- gsub("\\s+", " ", x = texts )                  
      merged <- rbind(merged, paste0(texts, collapse = ' '))
    }
    dat_i <- data.frame(user_id_str = unique_user, text = merged, stringsAsFactors = F)
    return (dat_i)
  }
  files <- list.files(folder_name,full.names = T)
  nfiles = length(files)
  
  # functions defined before register of cl
  n_cores = detectCores()
  cl <- makeCluster(floor(n_cores / 2))
  registerDoParallel(cl)
  Dat <- foreach (i = 1:nfiles,
           .combine = rbind ) %dopar% {
             #i =1
             file = files[i]
             return( merged_text(file) )
           }
  stopCluster(cl)  # exit first, otherwise, got warnings()
  return (Dat)
}

library(doParallel)    
dat1 <- merged_text_folder("../data/followers_Network/followers_timeline_0_45k_csv/")
dat2 <- merged_text_folder("../data/followers_Network/followers_timeline_45k_76k_csv/")
dat <- rbind(Dat1, Dat2)  #51898 users
write.csv(dat, file = "../data/followers_Network/followers_userid_mergedtext_1200.csv", row.names = F)







#########################################################################################
# part 2 text to explain whether the results  make senses or not
#########################################################################################


#dat <- read.csv("../data/followers_Network/followers_timeline_top200_1129/01.csv", stringsAsFactors = F)
#dat <- read.csv("../data/followers_Network/followers_timeline_top200.csv", stringsAsFactors = F)
dat <- read.csv("../data/followers_Network/followers_userid_mergedtext_1200.csv", 
                colClasses = c("character"),
                stringsAsFactors = F)  ## user_id_str, text

id_sn <- read.csv("../data/followers_Network/id_counts.csv", colClasses = c("character"), stringsAsFactors = F)
idx1 <- which(!is.na(match(dat$user_id_str , id_sn$id_str))) 
dat <- dat[idx1,]
dat$user_screenName <- id_sn$screen_name[match(dat$user_id_str , id_sn$id_str)[idx1]]  ## user_id_str, user_screenName, text

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
  , wordLengths = c(4, 30) # remove short words like 'a' 
  #, weighting = function(x) weightSMART(x, spec = "ltc")
)


system.time(
  tdm <- TermDocumentMatrix(vc, control =  ctrl) )  # take about 30 min

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
#146110 words remains in 44611 docuemnts


#A1 <- spMatrix(i = tdm$i, j = tdm$j, x = tdm$v, nrow = tdm$nrow, ncol  = tdm$ncol) 
A2 <- spMatrix(i = tfidf$i, j = tfidf$j, x = tfidf$v, nrow = tfidf$nrow, ncol  = tfidf$ncol) 

rownames(A1)  = tdm$dimnames$Terms; colnames(A1) = names_documents
rownames(A2)  = tfidf$dimnames$Terms ; colnames(A2) = names_documents
stopifnot(dim(A1)==dim(A2))
save(A1,A2, file ="clean_text_1200.RData")  # tdm, tfidf




load("clean_text_1200.RData")
### A1 is terms-document, A2 is tf-idf of A1, normalized
deg1 <- rowSums(A1); deg2 <- colSums(A1)
d1 <- rowSums(A2); d2 <- colSums(A2)
high2 <- cbind(colnames(A2)[order(-d2)[1:50]],d2[order(-d2)[1:50]]) # high degree documents
hist(d2, breaks =100)

library(ineq)
plot(Lc(d2), col ="red")
A <- A2
#A <- A1 ;
# A@x <- log(1+A1@x)




# based on the original clusters discovered by following relationship
## validate based on representative words
clustering <- read.csv("./1209/following/k50/id_sn_cluster.csv", colClasses =  c("character","character","integer"), stringsAsFactors = F)
k = max(clustering$cluster)
Z1 <- membershipM(clustering$cluster)
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
write.csv(distinctive_words, file ="./0212/keywords_50.csv", row.names = F)
#write.csv(distinctive_words, file ="0212/L2/k50/keywords_50.csv", row.names = F)





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
#pdf("./0212/L2/A1_blockB_bip.pdf", height = 7, width = 8)
ggplot(data = melt(as.matrix(sqrt(WB))), aes(x=X1, y =X2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(WB)))+xlab("row")+ylab("col") 
ggplot(data = melt(as.matrix(sqrt(B))), aes(x=X1, y =X2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(B)))+xlab("row")+ylab("col") 
dev.off()
#colSums(Z);colSums(Y)



## compare cluster of following -- Z1, text -- Y, on 45k followers
confMatrix <- t(Z1) %*% Y
NMI(confMatrix)
write.csv(confMatrix, file = "./0212/following_text_50x50.csv")

#pdf(file = "./0212/L2/k50/A1_confMat_50_50.pdf", onefile = T, width = 8, height = 7)
library(ggplot2)
confMatrix1 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix;
ggplot(melt(as.matrix(confMatrix1)), aes(x=as.factor(X1), y= as.factor(X2), fill=value)) + 
  geom_tile()+ labs(title = "row sum =1") + xlab("following cluster") + ylab("text cluster")
confMatrix2 <- confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1));
ggplot(melt(as.matrix(confMatrix2)),aes(x=as.factor(X1), y= as.factor(X2), fill=value)) + 
  geom_tile()+ labs(title = "colnum sum =1") + xlab("following cluster") + ylab("text cluster")
confMatrix3 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1))
ggplot(melt(as.matrix(confMatrix3)),aes(x=as.factor(X1), y= as.factor(X2), fill=value)) + 
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
write.csv(confMatrix_categorial, file = "./0212/L2/k50/following_text_11x13.csv")

#pdf(file = "./0212/L2/k50/confMat_categorial_10x10.pdf", onefile = T, width = 8, height = 7)
library(ggplot2)
confMatrix1 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix;
ggplot(melt(as.matrix(confMatrix1)), aes(x=as.factor(X1), y= as.factor(X2), fill=value)) + 
  geom_tile()+ labs(title = "row sum =1") + xlab("following cluster") + ylab("text cluster")
confMatrix2 <- confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1));
ggplot(melt(as.matrix(confMatrix2)),aes(x=as.factor(X1), y= as.factor(X2), fill=value)) + 
  geom_tile()+ labs(title = "colnum sum =1") + xlab("following cluster") + ylab("text cluster")
confMatrix3 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1))
ggplot(melt(as.matrix(confMatrix3)),aes(x=as.factor(X1), y= as.factor(X2), fill=value)) + 
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
pdf(file = "./0212/L2/A1_frequent_words.pdf", onefile = T, width = 8, height = 9)
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
write.csv(selected_words, file = "./0212/L2/A1_distictive_words.csv")



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
write.csv(selected_words, file = "./0212/L2/A1_distictive_words.csv")




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
pdf("./0212/L2_sep/blockB_bip.pdf", height = 7, width = 8)
ggplot(data = melt(as.matrix(sqrt(WB))), aes(x=X1, y =X2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(WB)))+xlab("row")+ylab("col") 
ggplot(data = melt(as.matrix(sqrt(B))), aes(x=X1, y =X2, fill = value))+
  geom_tile() + labs(title= expression(sqrt(B)))+xlab("row")+ylab("col") 
dev.off()
colSums(Z);colSums(Y)


confMatrix <- t(Z1) %*% Y
pdf(file = "./0212/L2_sep/confMat.pdf", onefile = T, width = 8, height = 7)
library(ggplot2)
confMatrix1 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix;
ggplot(melt(as.matrix(confMatrix1)), aes(x=X1, y= X2, fill=value)) + 
  geom_tile()+ labs(title = "confMat1") + xlab("row") + ylab("col")
confMatrix2 <- confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1));
ggplot(melt(as.matrix(confMatrix2)),aes(x=X1, y=X2, fill=value)) + 
  geom_tile()+ labs(title = "confMat2") + xlab("row") + ylab("col")
confMatrix3 <- Diagonal(dim(Z1)[2], rowSums(confMatrix)^(-1))%*% confMatrix %*% Diagonal(dim(Y)[2], colSums(confMatrix)^(-1))
ggplot(melt(as.matrix(confMatrix3)),aes(x=X1, y=X2, fill=value)) + 
  geom_tile()+ labs(title = "confMat3") + xlab("row") + ylab("col") #+scale_fill_gradient(low="green", high="red")
dev.off()
NMI(confMatrix)



# input: Y
sizes <- colSums(Y)
documents<- A %*% Y
pdf(file = "./0212/L2_sep/frequent_words.pdf", onefile = T, width = 8, height = 9)
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
write.csv(selected_words, file = "./0212/L2_sep/distictive_words.csv")

save(list =ls(), file ="./0212/L2_sep/result_A1.RData")









