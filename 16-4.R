


#### sentiment analysis

#create the samp_reweets.csv






retweets_reply <- read.csv("../data/trump_tweets/followers_tweets/alltweets-followers-Trump.csv", 
                            colClasses = c("character"), stringsAsFactors = F)
rt_ids <- grep("^RT ", retweets_reply$text)
retweets_sa <- retweets_reply[-rt_ids,]  # excluded purely retweet

filepath <- "../data/trump_tweets/followers_tweets/tweets_w_comments.csv"
#write.csv(retweets_sa, file =  filepath,  row.names = F)
counts <- table(retweets_sa$user_id_str); table(counts)
set.seed(100)
# random order
rnd <- sample(1:nrow(retweets_sa), nrow(retweets_sa),replace = F)
retweets_sa_rnd <- retweets_sa[rnd,]
user_id_unique <- unique(retweets_sa$user_id_str); 
idx <- match(user_id_unique, retweets_sa_rnd$user_id_str)
samp_tweets<- retweets_sa_rnd[idx,]; 

# retweets_sa_rnd <- retweets_sa_rnd[-idx,]
# idx2 <- match( names(which(counts>10)),retweets_sa_rnd$user_id_str); 
# samp_tweets <- rbind(samp_tweets, retweets_sa_rnd[idx2,])

reply.df <- read.csv( "../data/trump_tweets/followers_tweets/reply.csv", colClasses = c("character"), 
                      stringsAsFactors = F)
temp<- match(samp_tweets$in_reply_to_status_id_str[which(!is.na(samp_tweets$in_reply_to_status_id_str))], 
             reply.df$id_str)
cat(sum(is.na(temp)), "out of ",length(temp), "replies in sample are NOT downloaded!")
samp_tweets$in_reply_to_text <- rep(NA, nrow(samp_tweets))
samp_tweets$in_reply_to_text[which(!is.na(samp_tweets$in_reply_to_status_id_str))] <- reply.df$text[temp]
samp_tweets$in_reply_to_created_at <- rep(NA, nrow(samp_tweets))
samp_tweets$in_reply_to_created_at[which(!is.na(samp_tweets$in_reply_to_status_id_str))] <- 
  reply.df$created_at[temp]

following_cluster <- read.csv("./1209/following/k50/id_sn_cluster.csv", colClasses = c('character'), 
                              stringsAsFactors = F)
samp_tweets$screen_name <- following_cluster$screenNames[match(samp_tweets$user_id_str, following_cluster$id_str)]



#some tweets are missing, match those with twitts in hadoop database.
tweetsFromHadoop <- read.csv( "../data/trump_tweets/tweets_fromHadoop.csv",colClasses = c("character"),
                              stringsAsFactors = F)
dim(tweetsFromHadoop)  ## 11305 

idx1 <- which(is.na(samp_tweets$in_reply_to_text) * (!is.na(samp_tweets$in_reply_to_status_id_str)) >0 )
tw_ids <- samp_tweets$in_reply_to_status_id_str[idx1]; length(tw_ids)
cat( sum(!is.na( match(tw_ids, tweetsFromHadoop$id_str))), "out of ",length(tw_ids)," missing replied_twitters are availalbe in Hadoop")

samp_tweets$in_reply_to_created_at[idx1] <- tweetsFromHadoop$created_at[match(tw_ids, tweetsFromHadoop$id_str)]
samp_tweets$in_reply_to_text[idx1] <- tweetsFromHadoop$text[match(tw_ids, tweetsFromHadoop$id_str)]

#for retweeted, either id_str exists with text ; or nothing is avaialbe
idx2 <- which(is.na(samp_tweets$quoted_status_text) * (!is.na(samp_tweets$quoted_status_id_str)) >0 )
length(idx2)


samp_tweets$quoted_status_created_at <- smappR::formatTwDate(samp_tweets$quoted_status_created_at )
samp_tweets$retweet_status_created_at <- smappR::formatTwDate(samp_tweets$retweet_status_created_at)
#samp_tweets$in_reply_to_created_at <- smappR::formatTwDate(samp_tweets$in_reply_to_created_at)
names(samp_tweets)

samp_tweets <- samp_tweets[,c(26,16, 2,1,3,5,4, 17:23,6,25,24,7:15)]



### for retweet_status_id_str, should have been removed, but there is one thing 
#wired going on, about 20 twitts
# idx <- which(!is.na(samp_tweets$retweet_status_id_str))
# samp_tweets <- samp_tweets[-idx,]



write.csv(samp_tweets, file ="../data/trump_tweets/followers_tweets/samp_retweets.txt", row.names = F)







