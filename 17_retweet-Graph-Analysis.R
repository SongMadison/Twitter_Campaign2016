
#adapted from 16-retweet-network-allTweets.Rmd

rm(list =ls())
set.seed(100)
source("Head_file.R")
source("function.R")

options(digits=3)
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))  #theme(plot.title = element_text(hjust =0.5)) other needed for each plot

load("../data/friends_info/edgelist_Feb27/RData/retweet_A2.RData")
ResultPath <- "../results_retweeting/result2/"
if (!file.exists(ResultPath)) dir.create(ResultPath)




# ---------------------------------------- retweeet network ---- 
dim(A);   #8220 3948
stopifnot(sum(is.na(match(rownames(A), users$id_str))) ==0)
stopifnot(sum(is.na(match(colnames(A), tweets$id_str))) ==0)
dim(tweets); 
dim(users)
k = 10 
#tweets over time
#"2015-01-01 00:00:00", "2015-02-01 00:00:00", "2015-03-01 00:00:00",
# "2015-04-01 00:00:00","2015-05-01 00:00:00",
dates  <- c("2015-06-01 00:00:00","2015-07-01 00:00:00",
            "2015-08-01 00:00:00", "2015-09-01 00:00:00","2015-10-01 00:00:00",
            "2015-11-01 00:00:00", "2015-12-01 00:00:00","2016-01-01 00:00:00", 
            "2016-02-01 00:00:00", "2016-03-01 00:00:00",
            "2016-04-01 00:00:00","2016-05-01 00:00:00","2016-06-01 00:00:00","2016-07-01 00:00:00",
            "2016-08-01 00:00:00","2016-09-01 00:00:00","2016-10-01 00:00:00","2016-11-01 00:00:00",
            "2016-11-09 00:00:00")
dates <- as.POSIXct(dates)



counts <-count_by_interval(tweets$created_at, dates)
p1 <- ggplot(data = data.frame(dates = gsub('(.*) .*', '\\1', x= dates),counts = c(0,counts)),
             aes(x = dates, y = counts))+geom_bar(stat='identity')+
  ggtitle(("Trump's tweets frequency over time"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

nr <- dim(A)[1]; nc <- dim(A)[2]
Dr <- rowSums(A); Dc <- colSums(A)
dat <- data.frame(Dr = Dr); rownames(dat) <- NULL

# row degree
quantile(Dr, probs = seq(0,1,length.out = 11))
intervals <- c(1,2,4,10,50,max(Dr))
counts <- table(id_by_interval(Dr,  intervals)  )
tweets.by.user <- data.frame(order = factor(1:length(intervals[-1])), count = as.vector(counts)); 
tweets.by.user$order <-factor(tweets.by.user$order, levels = 1:length(counts), labels =   paste0('[',intervals[1:(length(intervals)-1)],',',intervals[2:length(intervals)], ')'))
p2 <- ggplot(tweets.by.user, aes(order, count, fill = order))+
  geom_bar(stat="identity") + labs(title = paste0("number of retweets by each followers ", nrow(A),"x", ncol(A)), x = '', ylab = "Number")
p21 <- tail_plot(log(Dr))


## column degree very skewed, take log
power <- table(Dc)
tmp <- data.frame(degree = log2(as.numeric(names(power))) , freq = as.vector(log2(power)) )
coefs <- lm(freq~degree, data =tmp)$coef
power_law.plot <- ggplot(data =tmp , aes(x=degree, y=freq))+geom_point()+
  geom_smooth(method = 'lm', colour = 'red', se= FALSE) + 
  labs(title = paste0("a=",round(coefs[1],2),", b=",round(coefs[2],2)), x = "log2 degree", y = 'log2 freq')


##-------------------- retweet bi-partition ---
tau1 <- sqrt(mean(Dr)); tau2 <- sqrt(mean(Dc))
L <- Diagonal(nr, (Dr+tau1)^(-1/2))%*%A%*%Diagonal(nc, (Dc+tau2)^(-1/2))
svd_L <- irlba(L, nv = 30+2)

plot(svd_L$d); abline(v = 20, col ="red")

k = 20
U <- rbind(svd_L$u[,1:k], svd_L$v[,1:k]) 
U1 <- t(apply(U, 1, function(x) x/sqrt(sum(x*x)+1e-2)))
set.seed(1234)
km <- kmeans(U1,k, nstart = 100, iter.max = 30)
bip.result <-list()
bip.result$row <- km$cluster[1:nr];
bip.result$col <- km$cluster[(1+nr):(nr+nc)]
table(bip.result$row); table(bip.result$col)
#membership output
users_cluster <- data.frame(users[,c('id_str',"screen_name")],cluster = bip.result$row)
tweets_cluster <- data.frame(tweets[,c("id_str","created_at","text")], cluster = bip.result$col)

write.csv(users_cluster, file =paste0(ResultPath, "user_cluster.csv"), row.names = F)
write.csv(users_cluster, file =paste0(ResultPath, "tweets_cluster.csv"), row.names = F)





#------- visualization the patterns------

Z <- membershipM(bip.result$row); Y <- membershipM(bip.result$col)
colSums(Z)
blockM <- t(Z) %*% A %*% Y
p3 <- balloon.plot(blockM, xlabel  = paste0(1:k,' (',colSums(Y),")"), ylabel = paste0(1:k,' (',colSums(Z), ')')) +  
  labs( title = paste(sum(blockM), "retweets between", 
                      sum(Z), "follower and", sum(Y), "tweets \n both clusters are estimated from retweeting network"), 
              y = paste0("clusters for followers"),
        x= paste("clusters for tweets"))
colnames(blockM) <- paste0(1:k,'-',colSums(Y)); rownames(blockM) <- paste0(1:k,'-',colSums(Z)) 
blockM
diag(blockM)/(rowSums(blockM)-diag(blockM))
diag(blockM)/(colSums(blockM)-diag(blockM))

#partition tweets
clustered_tweets <- NULL
for ( i in 1:k){
  data <- tweets[ bip.result$col == i, ]
  clustered_tweets <- rbind(clustered_tweets, data.frame(cluster_id = rep(i, nrow(data)), data))
}
rownames(clustered_tweets) <- 1:nrow(clustered_tweets)
clustered_tweets <- clustered_tweets[which(!is.na(clustered_tweets$created_at)), ]
write.csv(clustered_tweets, file = paste0(ResultPath,"clustered_tweets_retweeting.csv"),  row.names = F)

# select some tweets for each users groups
#1, close to center -- based on v vectors
res <- select_rows(svd_L$v, top = 10, labs = bip.result$col, method = "center", verbose = T)
selected_tweets <- data.frame(cluster = res$cluster, tweets[res$sample_id,])
names(selected_tweets)
rownames(selected_tweets) <- NULL
write.csv(selected_tweets, file = paste0(ResultPath,"selected_tweets_retweeting.csv"),  row.names = F)


#2, distinctive tweets
res <- select_columns(L, top = 10, labs = bip.result$row, verbose = T)
res
distinct_tweets <- data.frame(cluster = res$cluster, tweets[res$features_id,]) 
names(distinct_tweets)
rownames(distinct_tweets) <- NULL
write.csv(distinct_tweets, file = paste0(ResultPath,"distinct_tweets_retweeting.csv"),  row.names = F)


#twitter clusters distributed over time
counts_cluster <- matrix(0,  k ,length(dates))   #tweets in each clusters
for( i in 1:k){
  tmp <- clustered_tweets[clustered_tweets$cluster_id == i, ]
  counts_cluster[i,] <- c(0,count_by_interval(tmp$created_at, dates))
}
dat <- flattenMatrix(counts_cluster) ## by col
names(dat) <- c("cluster","time", "count");
dat$time <- as.factor(dat$time); 
dat$time <- factor(dat$time, 1:length(dates), labels = gsub('(.*) .*', '\\1', x= dates)) 
dat$cluster <- as.factor(dat$cluster)
p4 <- ggplot(data =dat, aes(x = time, y = count))+
  geom_line(aes(group = cluster, colour = cluster))+ 
  facet_wrap(~cluster, nrow =2)+
  ggtitle(paste0("Trump's tweets frequency over time(",nrow(tweets),")"))+
  theme(axis.text.x=element_blank()) +
  theme(legend.position="none")
p4

p5 <- balloon.plot(counts_cluster, xlabel = gsub('(.*) .*', '\\1', x= dates),
                   ylabel = )+ labs(title = "time distribution of the each group of tweets(retweeting)") 
counts_cluster

p5  ## ballplot 


#most frequently retweeted tweets
tmp <- cbind(retweet = colSums(A)[which(colSums(A) > 200)], 
             tweets[which(colSums(A) > 200),])
tmp


pdf(paste0(ResultPath, "plots.pdf"), width = 8, height =6)
p1
p2
p3
p4
p5
dev.off()



