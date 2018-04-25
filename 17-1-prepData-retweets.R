
#introduction:
# all the retweets, reply, quoted
# retweet_status_id_str, in_reply_to_status_id_str, quoted_status_id_str
# theses are tweets from trump followers and other tweets
# byproduct, created a set of trump tweets -- 

rm(list =ls())

RDataPath <- "../data/friends_info/edgelist_Feb27/RData/"
retweets <- read.csv("../data/friends_info/edgelist_Feb27/allReTweets_full.csv", colClasses = c("character"))
retweets$retweet_status_created_at <- as.POSIXct(retweets$retweet_status_created_at, format = "%a %b %d %H:%M:%S %z %Y")  
# local time, -6 timezone
retweets$quoted_status_created_at <- as.POSIXct(retweets$quoted_status_created_at, format = "%a %b %d %H:%M:%S %z %Y")
#trumptweets <- read.csv("../data/friends_info/edgelist_Feb27/trumptweets.csv",
#                       colClasses = c("character"))
#trumptweets$created_at <- as.POSIXct(trumptweets$created_at)

names(retweets)
'
[1] "created_at"                 "id_str"                    
[3] "text"                       "truncated"                 
[5] "source"                     "in_reply_to_status_id_str" 
[7] "in_reply_to_user_id_str"    "quoted_status_created_at"  
[9] "quoted_status_id_str"       "quoted_status_text"        
[11] "quoted_status_user_id_str"  "retweet_status_id_str"     
[13] "retweet_status_created_at"  "retweet_status_text"       
[15] "retweet_status_user_id_str" "user_id_str"               
[17] "place_name"                 "place_country"             
[19] "retweet_count"              "favorite_count"            
[21] "favorited"                  "retweeted"                 
[23] "lang"
'
library(dplyr)
df <- data.frame(reply = !is.na(retweets$in_reply_to_status_id_str), 
                 RT_comments = !is.na(retweets$quoted_status_id_str), 
                 RT = !is.na(retweets$retweet_status_id_str))

df %>% group_by(reply, RT_comments, RT) %>% count()
#   reply RT_comments    RT       n
# 1 FALSE       FALSE  TRUE 1615393
# 2 FALSE        TRUE FALSE  256031
# 3  TRUE       FALSE FALSE  352251
# 4  TRUE        TRUE FALSE    7145   #quoted other tweets, but reply to Trump, not know how it works
# total 2230820


#collection of all trump tweets
dat1 <- retweets[,c("retweet_status_id_str",     
                   "retweet_status_created_at", 
                    "retweet_status_text",       
                    "retweet_status_user_id_str")]
dat2 <- retweets[,c("quoted_status_id_str",  
                     "quoted_status_created_at",    "quoted_status_text",        
                     "quoted_status_user_id_str")]
colnames(dat1) <- c("id_str","created_at", "text", "user_id_str")
colnames(dat2) <- c("id_str","created_at", "text", "user_id_str")
trumptweets <- unique(rbind(dat1,dat2)) #21703
trumptweets <- subset(trumptweets, user_id_str == "25073877") #20178
sum(retweets$quoted_status_user_id_str=='25073877', na.rm = T) #260802
sum(!is.na(retweets$quoted_status_id_str)) #263176, quoted some others.
#write.csv(trumptweets, file ="../data/friends_info/edgelist_Feb27/trumptweets_followers.csv", row.names = F)


#retweet with comments -- quoted_status + retweets
samp<- read.csv("../data/friends_info/edgelist_Feb27/all_samp_info.csv", colClasses = c("character"))
idx1 <- match(retweets$user_id_str, samp$id_str)
retws0 <- retweets[!is.na(idx1),]

RT_only_idx  = which((!is.na(retws0$retweet_status_id_str))*(retws0$created_at <'2016-11-09 00:00:00')*
                       (retws0$retweet_status_created_at >'2015-06-16 00:00:00')==1)
retws1 <- retws0[RT_only_idx,]
edgelist1 <- data.frame(from_user = retws1$user_id_str, to_tweet = retws1$retweet_status_id_str)

RT_comments_idx <- which((!is.na(retws0$quoted_status_id_str))*(retws0$created_at <'2016-11-09 00:00:00')*
                           (retws0$quoted_status_created_at >'2015-06-16 00:00:00')==1)
retws2 <- retws0[RT_comments_idx,]
edgelist2 <- data.frame(from_user = retws2$user_id_str, to_tweet = retws2$quoted_status_id_str)

edgelist <- rbind(edgelist1, edgelist2) #847706
edgelist <- subset(edgelist, to_tweet %in% trumptweets$id_str ) #846841

user_ids <- unique(edgelist$from_user); 
user_ids <- samp$id_str[!is.na(match(samp$id_str,user_ids))] #order
i_set <- match(edgelist$from_user, user_ids)

tweets_ids <- unique(edgelist$to_tweet)
idx <- match(tweets_ids, trumptweets$id_str); stopifnot(sum(is.na(idx)) == 0) # no missing tweets
tweets_ids <-  trumptweets$id_str[!is.na(match(trumptweets$id_str, tweets_ids))] #re-order the tweets_ids
j_set <- match(edgelist$to_tweet, tweets_ids)
A <- sparseMatrix(i= i_set, j = j_set, x = rep(1,nrow(edgelist)) )
rownames(A) <- user_ids; colnames(A) <- tweets_ids
tweets <- trumptweets[match(tweets_ids,trumptweets$id_str),]
retws = rbind(retws1, retws2)
users <- samp[match(user_ids, samp$id_str),]
#save(A,users, tweets, retws,  file =paste0(RDataPath,"retweet_A_comments.RData"))
#retws, retweet happend time, retweet altitudes, some info on the edges



## retweet from RT only, from 2015-06-16 - 2016-11-08
samp<- read.csv("../data/friends_info/edgelist_Feb27/all_samp_info.csv", colClasses = c("character"))
idx1 <- match(retweets$user_id_str, samp$id_str); sum(is.na(idx1))
retws <- retweets[!is.na(idx1),]
retws <- retws[!is.na(retws$retweet_status_id_str),]
retws <- retws[retws$created_at <'2016-11-09 00:00:00',]
retws <- retws[retws$retweet_status_created_at >'2015-06-16 00:00:00',]

edgelist <- data.frame(from_user = retws$user_id_str, to_tweet = retws$retweet_status_id_str)
user_ids <- unique(edgelist$from_user); user_ids <- samp$id_str[!is.na(match(samp$id_str,user_ids))]
i_set <- match(edgelist$from_user, user_ids)

tweets_ids <- unique(edgelist$to_tweet)
idx <- match(tweets_ids, trumptweets$id_str); stopifnot(sum(is.na(idx)) == 0) # no missing tweets
tweets_ids <-  trumptweets$id_str[!is.na(match(trumptweets$id_str, tweets_ids))] #re-order the tweets_ids
j_set <- match(edgelist$to_tweet, tweets_ids)

A <- sparseMatrix(i= i_set, j = j_set, x = rep(1,nrow(edgelist)) )
rownames(A) <- user_ids; colnames(A) <- tweets_ids
tweets <- trumptweets[match(tweets_ids,trumptweets$id_str),]
users <- samp[match(user_ids, samp$id_str),]

#save(A,users, tweets, retws, file =paste0(RDataPath,"retweet_A123.RData"))
#retws, retweet happend time, retweet altitudes, some info on the edges




#retweeting network based on quoted tweets 2015-06-16 - 2016-11-08
samp<- read.csv("../data/friends_info/edgelist_Feb27/all_samp_info.csv", colClasses = c("character"))
idx1 <- match(retweets$user_id_str, samp$id_str)
retws0 <- retweets[!is.na(idx1),]

RT_comments_idx <- which((!is.na(retws0$quoted_status_id_str))*(retws0$created_at <'2016-11-09 00:00:00')*
                           (retws0$quoted_status_created_at >'2015-06-16 00:00:00')==1)
retws2 <- retws0[RT_comments_idx,]
edgelist2 <- data.frame(from_user = retws2$user_id_str, to_tweet = retws2$quoted_status_id_str)

#edgelist <- rbind(edgelist1, edgelist2) #847706
edgelist = edgelist2
edgelist <- subset(edgelist, to_tweet %in% trumptweets$id_str ) #846841

user_ids <- unique(edgelist$from_user); 
user_ids <- samp$id_str[!is.na(match(samp$id_str,user_ids))] #order
i_set <- match(edgelist$from_user, user_ids)

tweets_ids <- unique(edgelist$to_tweet)
idx <- match(tweets_ids, trumptweets$id_str); stopifnot(sum(is.na(idx)) == 0) # no missing tweets
tweets_ids <-  trumptweets$id_str[!is.na(match(trumptweets$id_str, tweets_ids))] #re-order the tweets_ids
j_set <- match(edgelist$to_tweet, tweets_ids)

A <- sparseMatrix(i= i_set, j = j_set, x = rep(1,nrow(edgelist)) )
rownames(A) <- user_ids; colnames(A) <- tweets_ids
tweets <- trumptweets[match(tweets_ids,trumptweets$id_str),]
#retws = rbind(retws1, retws2)
retws = retws2
users <- samp[match(user_ids, samp$id_str),]
#save(A,users, tweets, retws,  file =paste0(RDataPath,"retweet_A_quoted-only.RData"))









#three sub networks  -- PURE RT
#before announcement, before primary, before election.

samp<- read.csv("../data/friends_info/edgelist_Feb27/samp1_info.csv", colClasses = c("character"))
idx1 <- match(retweets$user_id_str, samp$id_str)
retws <- retweets[!is.na(idx1),]
retws <- retws[!is.na(retws$retweet_status_id_str),]
retws <- retws[retws$created_at <'2016-11-09 00:00:00',]
retws <- retws[retws$retweet_status_created_at >'2015-06-16 00:00:00',]

edgelist <- data.frame(from_user = retws$user_id_str, to_tweet = retws$retweet_status_id_str)
user_ids <- unique(edgelist$from_user); user_ids <- samp$id_str[!is.na(match(samp$id_str,user_ids))]
i_set <- match(edgelist$from_user, user_ids)

tweets_ids <- unique(edgelist$to_tweet)
idx <- match(tweets_ids, trumptweets$id_str); stopifnot(sum(is.na(idx)) == 0) # no missing tweets
#tweets_ids <-  trumptweets$id_str[!is.na(match(trumptweets$id_str, tweets_ids))] #re-order the tweets_ids
j_set <- match(edgelist$to_tweet, tweets_ids)
A <- sparseMatrix(i= i_set, j = j_set, x = rep(1,nrow(edgelist)) )
rownames(A) <- user_ids; colnames(A) <- tweets_ids
tweets <- trumptweets[match(tweets_ids,trumptweets$id_str),]
users <- samp[match(user_ids, samp$id_str),]
#save(A,users, tweets,retws, file =paste0(RDataPath,"retweet_A1.RData"))
#retws, retweet happend time, retweet altitudes, some info on the edges


samp<- read.csv("../data/friends_info/edgelist_Feb27/samp2_info.csv", colClasses = c("character"))
idx1 <- match(retweets$user_id_str, samp$id_str)
retws <- retweets[!is.na(idx1),]
retws <- retws[!is.na(retws$retweet_status_id_str),]
retws <- retws[retws$created_at <'2016-11-09 00:00:00',]
retws <- retws[retws$retweet_status_created_at >'2015-06-16 00:00:00',]
edgelist <- data.frame(from_user = retws$user_id_str, to_tweet = retws$retweet_status_id_str)
user_ids <- unique(edgelist$from_user); user_ids <- samp$id_str[!is.na(match(samp$id_str,user_ids))]
i_set <- match(edgelist$from_user, user_ids)

tweets_ids <- unique(edgelist$to_tweet)
idx <- match(tweets_ids, trumptweets$id_str); stopifnot(sum(is.na(idx)) == 0) # no missing tweets
tweets_ids <-  trumptweets$id_str[!is.na(match(trumptweets$id_str, tweets_ids))] #re-order the tweets_ids
j_set <- match(edgelist$to_tweet, tweets_ids)
A <- sparseMatrix(i= i_set, j = j_set, x = rep(1,nrow(edgelist)) )
rownames(A) <- user_ids; colnames(A) <- tweets_ids
tweets <- trumptweets[match(tweets_ids,trumptweets$id_str),]
users <- samp[match(user_ids, samp$id_str),]
#save(A,users, tweets,retws, file =paste0(RDataPath,"retweet_A2.RData")) #note: diff by 2, Jun 8.
#retws, retweet happend time, retweet altitudes, some info on the edges



samp<- read.csv("../data/friends_info/edgelist_Feb27/samp3_info.csv", colClasses = c("character"))
idx1 <- match(retweets$user_id_str, samp$id_str)
retws <- retweets[!is.na(idx1),]
retws <- retws[!is.na(retws$retweet_status_id_str),]
retws <- retws[retws$created_at <'2016-11-09 00:00:00',]
retws <- retws[retws$retweet_status_created_at >'2015-06-16 00:00:00',]
edgelist <- data.frame(from_user = retws$user_id_str, to_tweet = retws$retweet_status_id_str)
user_ids <- unique(edgelist$from_user); user_ids <- samp$id_str[!is.na(match(samp$id_str,user_ids))]
i_set <- match(edgelist$from_user, user_ids)

tweets_ids <- unique(edgelist$to_tweet)
idx <- match(tweets_ids, trumptweets$id_str); stopifnot(sum(is.na(idx)) == 0) # no missing tweets
tweets_ids <-  trumptweets$id_str[!is.na(match(trumptweets$id_str, tweets_ids))] #re-order the tweets_ids
j_set <- match(edgelist$to_tweet, tweets_ids)
A <- sparseMatrix(i= i_set, j = j_set, x = rep(1,nrow(edgelist)) )
rownames(A) <- user_ids; colnames(A) <- tweets_ids
tweets <- trumptweets[match(tweets_ids,trumptweets$id_str),]
users <- samp[match(user_ids, samp$id_str),]
#save(A,users, tweets,retws, file =paste0(RDataPath,"retweet_A3.RData"))
#retws, retweet happend time, retweet altitudes, some info on the edges




