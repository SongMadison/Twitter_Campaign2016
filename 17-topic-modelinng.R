
rm(list =ls())
ResultPath <- "../results_topics/unigram/"
#dir.create(ResultPath)

source("Head_file.R")
source("function.R")
tws <- read.csv("../data/friends_info/edgelist_Feb27/trumptweets_election.csv", colClasses = c("character"))
tws$created_at <- as.POSIXct(tws$created_at)
tws$text <- tolower(tws$text)
hist(as.Date(tws$created_at), breaks =50, freq = T)

text_source <- rep(NA, length(tws$text))
text <- tws$text
text <- gsub("http", " http", text)

text_source[grep(".*\\s(http.*)",text)] <- gsub(".*\\s(http.*)", "\\1 ", text[grep(".*\\s(http.*)",text)])
text_source[grep(".*\\s(.*\\.com.*)",text)] <- gsub(".*\\s(.*\\.com.*)", "\\1 ",
                                                         text[grep(".*\\s(.*\\.com.*)",text)])
#pic don't have space ahead
text_source[grep(".*(pic.twitter.com.*)",text)] <- gsub(".*(pic.twitter.com.*)", "\\1 ",
                                                        text[grep(".*(pic.twitter.com.*)",text)])
sum(is.na(text_source))


text = tws$text
text <- gsub("http", " http", text)
text[grep("(.*)\\shttp.*",text)] <- gsub("(.*)\\shttp.*$", "\\1 ", text[grep("(.*)\\s(http.*$)",text)])
text[grep("(.*)\\shttp.*",text)] <- gsub("(.*)\\shttp.*$", "\\1 ", text[grep("(.*)\\s(http.*$)",text)])
text[grep("(.*)\\shttp.*",text)] <- gsub("(.*)\\shttp.*$", "\\1 ", text[grep("(.*)\\s(http.*$)",text)])
text[grep("(.*)\\shttp.*",text)] <- gsub("(.*)\\shttp.*$", "\\1 ", text[grep("(.*)\\s(http.*$)",text)])

text[grep("(.*)\\s.*?\\.com.*$",text)] <- gsub("(.*)\\s.*?\\.com.*$", "\\1 ",
                                                           text[grep("(.*)\\s.*?\\.com.*$",text)])
text[grep("(.*)pic.twitter.com.*",text)] <- gsub("(.*)pic.twitter.com.*", "\\1 ",
                                                        text[grep("(.*)pic.twitter.com.*",text)])
text[grep("(.*)\\s.*?\\.com.*$",text)] <- gsub("(.*)\\s.*?\\.com.*$", "\\1 ",
                                                      text[grep("(.*)\\s.*?\\.com.*$",text)])
tws$text<- text
idx1 <- grep("^\"@",tws$text)
idx2 <- grep("^thank",tws$text)
idx3 <- grep("^join" ,tws$text)
idx4 <- grep("^via @" ,tws$text)
idxes <- unique(c(idx1,idx2,idx3,idx4))

tws$"retweet_endorse" <- rep(NA, nrow(tws)); tws$"retweet_endorse"[idx1] <- 1
tws$"thank" <- rep(NA, nrow(tws)); tws$"thank"[idx2] <- 1
tws$"join" <- rep(NA, nrow(tws)); tws$"join"[idx3] <- 1
tws$"via_at" <- rep(NA, nrow(tws)); tws$"via_at"[idx4] <- 1
tws$url<- text_source
tweets <- tws[-idxes,] #4879


### ------------------------------------- after extracting http, removing ^thank etc --------------
text <- tweets$text
text <- gsub("new hampshire", "new-hampshire", text)
text <- gsub("new york", "new-york", text)
text <- gsub("new jersey", "new-jersey", text)
text <- gsub("new mexico", "new-mexico", text)
text <- gsub("south carolina", "south-carolina", text)
text <- gsub("north carolina", "north-carolina", text)
text <- gsub("west virgina", "west-virgina", text)

text_full = text
length(text_full)
text <- unique(text_full)
length(text)





At <- bag_of_word_tweets(text, bigram = F) #8098 3529
dc <- colSums(At); dc <- sort(dc, decreasing = T) 
freq <- data.frame(names(dc), freq = as.numeric(dc))
write.csv(freq, file =paste0(ResultPath,"words_freq.csv"), row.names = F)
#some words selected with frequency >= 100
extra_words <- c("just", "people", "now", "big", "get", "can", "today", "tonight", 
                 "time", "like", "many", "last", "president", "one", "country", "don",
                 "much", "never", "back", "night", "really" )
At <- At[, -match(extra_words, colnames(At))]

nr_t <- nrow(At); nc_t <- ncol(At)
dr_t <- rowSums(At); dc_t <- colSums(At)
#tfidf
Lt <- At %*% Diagonal(nc_t,log((nr_t+1)/(dc_t+1))); 
Lt <- t(apply(Lt, 1, function(x) x/sqrt(sum(x*x)+0.01)))  
corr <- Lt %*%t(Lt); quantile(as.vector(corr), seq(0,1,length.out = 11))

system.time(svd_Lt <- irlba(corr, nv = 30+2))
pdf(paste0(ResultPath, "eigen_values.pdf"))
plot(svd_Lt$d);abline(v=20, col ="red")
dev.off()


k = 20
U <- svd_Lt$u[,1:k]
U1 <- t(apply(U, 1, function(x) x/ sqrt(sum(x*x+0.01))))
set.seed(1234)
km <- kmeans(U1, k, nstart = 100, iter.max =30)
km <- kmeans(U1, centers = km$centers[order(-km$centers[,1]),], nstart = 1, iter.max = 1)



Z <- membershipM(km$cluster)
centers <-  Diagonal(k,colSums(Z)^(-1)) %*% t(Z) %*% Lt # centers in the original space
terms <- colnames(Lt); mean_vec <- colMeans(Lt)
pdf(paste0(ResultPath, "wordcloud_",k,".pdf"))
for (i in 1:k){
  diff = sqrt(centers[i,]) - sqrt(mean_vec)
  pos_idx <- which(diff > 0)
  wordcloud(words = terms[pos_idx], freq = diff[pos_idx], 
            max.words =50,
            rot.per = 0, random.order = F, scale = c(2,0.3))
  title(paste0("cluster of size: ", km$size[i]))
}
dev.off() 

selected_words <- matrix("", 20, k)
for ( i in 1:k){
  diff = sqrt(centers[i,]) - sqrt(mean_vec)
  selected_words[,i] = terms[order(-diff)[1:20]]
  selected_words[1:10,]
}
write.csv(selected_words, 
          file =paste0(ResultPath, "selected_words_k",k,".csv"), row.names = F)

cluster_idx <- NULL
for( i in 1:k){
  #i= 15
  scores <- U[which(km$cluster == i),1:k]  %*% matrix(km$centers[i,])
  cluster_idx <- c(cluster_idx,which(km$cluster == i)[order(-scores)])
}

clustered_tweets <- data.frame(cluster_id = rep(1:k, km$size), text = text[cluster_idx])
write.csv(clustered_tweets, file =paste0(ResultPath, "clusted_tweets_by_text_unique_k", k,".csv"))
text_full_df <- data.frame(text = text_full)
clustered_tweets <-left_join(text_full_df, clustered_tweets, by ="text")
clustered_tweets$text = tweets$text
clustered_tweets <- data.frame(cluster_id = clustered_tweets$cluster_id, tweets)

clustered_tweets_by_text <- rbind(clustered_tweets,
                                  data.frame(cluster_id = rep(NA,length(idxes)), 
                                             tws[idxes,]))
write.csv(clustered_tweets_by_text, 
          file =paste0(ResultPath, "clustered_tweets_text_k",k,".csv"), row.names = F)

selected_tweets <- NULL
for( i in 1:k){
  tweets_i <- tweets[which(km$cluster == i),]
  scores <- U[which(km$cluster == i),1:k]  %*% as.vector(km$centers[i,])
  tweets_i <- tweets_i[order(-scores)[1:5],]
  selected_tweets<- rbind(selected_tweets, 
                          data.frame(cluster_id = rep(i, nrow(tweets_i)),tweets_i)  )  
}
rownames(selected_tweets) <- NULL
#selected_tweets$text
write.csv(selected_tweets, 
          file =paste0(ResultPath, "selected_tweets_text_k",k,".csv"), row.names = F)

#based on 1, keywords, selected tweets,
c(
"hillary_emails_rigged",
"dishonest_media",
"obama_isis_warming" ,
"economy_jobs",
"sanders_sold_rigged",
"caimpaign_join",
"cnn_morningjoe_fox",
"citing_polls_news",
"cruz_kasich",
"MAGA_slogan",
"convention_ryan",
"wonPrimary_unfair",
"terrorism_islam",
"campaign_thankyou",
"mitt_romney",
"border_immigration",
"failingnytimes",
"specialInterests",
"beingInterviewed",
"elizabethWarren")


#for bigram, cluster 3 is mixed -- to be split
km$size
cluster_to_be_split <- c(14,20)
U2 <- U1[km$cluster%in%cluster_to_be_split ,]
set.seed(123)
kk=10
km1 <- kmeans(U2,centers = kk, iter.max = 30, nstart = 100)
centers <- rbind(km$centers[-cluster_to_be_split,],km1$centers)
centers <- centers[order(-centers[,1]),]
clus <- apply(U1, MARGIN = 1, FUN = function(x) {
    d <- apply(centers, 1, FUN = function(r) sum((r-x)^2))
    which.min(d)   })          
km2 <- list()
km2$cluster <- clus
km2$centers = centers
km2$size <- as.numeric(table(km2$cluster))

k <- length(unique(km2$clus))
Z <- membershipM(km2$clus)
centers <-  Diagonal(k,colSums(Z)^(-1)) %*% t(Z) %*% Lt # centers in the original space
terms <- colnames(Lt); mean_vec <- colMeans(Lt)
pdf(paste0(ResultPath, "wordcloud_",k,".pdf"))
for (i in 1:k){
  diff = sqrt(centers[i,]) - sqrt(mean_vec)
  pos_idx <- which(diff > 0)
  wordcloud(words = terms[pos_idx], freq = diff[pos_idx], 
            max.words =50,
            rot.per = 0, random.order = F, scale = c(2,0.3))
  title(paste0("cluster of size: ", km$size[i]))
}
dev.off() 

selected_words <- matrix("", 20, k)
for ( i in 1:k){
  diff = sqrt(centers[i,]) - sqrt(mean_vec)
  selected_words[,i] = terms[order(-diff)[1:20]]
  selected_words[1:10,]
}
write.csv(selected_words, 
          file =paste0(ResultPath, "selected_words_k",k,".csv"), row.names = F)

cluster_idx <- NULL
for( i in 1:k){
  #i= 15
  scores <- U[which(km2$cluster == i),]  %*% matrix(km2$centers[i,], ncol= 1)
  cluster_idx <- c(cluster_idx,which(km2$cluster == i)[order(-scores)])
}

clustered_tweets <- data.frame(cluster_id = rep(1:k, km2$size), text = text[cluster_idx])
write.csv(clustered_tweets, file =paste0(ResultPath, "clusted_tweets_by_text_unique_k", k,".csv"))
text_full_df <- data.frame(text = text_full)
clustered_tweets <-left_join(text_full_df, clustered_tweets, by ="text")
clustered_tweets$text = tweets$text
clustered_tweets <- data.frame(cluster_id = clustered_tweets$cluster_id, tweets)

clustered_tweets_by_text <- rbind(clustered_tweets,
                                  data.frame(cluster_id = rep(NA,length(idxes)), 
                                             tws[idxes,]))
write.csv(clustered_tweets_by_text, 
          file =paste0(ResultPath, "clustered_tweets_text_k",k,".csv"), row.names = F)

selected_tweets <- NULL
for( i in 1:k){
  tweets_i <- tweets[which(km2$cluster == i),]
  scores <- U[which(km2$cluster == i),]  %*% as.vector(km2$centers[i,])
  tweets_i <- tweets_i[order(-scores)[1:5],]
  selected_tweets<- rbind(selected_tweets, 
                          data.frame(cluster_id = rep(i, nrow(tweets_i)),tweets_i)  )  
}
rownames(selected_tweets) <- NULL
#selected_tweets$text
write.csv(selected_tweets, 
          file =paste0(ResultPath, "selected_tweets_text_k",k,".csv"), row.names = F)

