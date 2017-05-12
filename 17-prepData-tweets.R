
#introduction:
# all the retweets, reply, quoted
# retweet_status_id_str, in_reply_to_status_id_str, quoted_status_id_str
# theses are tweets from trump followers and other tweets



RDataPath <- "../data/friends_info/edgelist_Feb27/RData/"
retweets <- read.csv("../data/friends_info/edgelist_Feb27/allReTweets.csv", colClasses = c("character"))
retweets$retweet_status_created_at<- as.POSIXct(retweets$retweet_status_created_at, format = "%a %b %d %H:%M:%S %z %Y")  # local time, -6 timezone
trumptweets <- read.csv("../data/friends_info/edgelist_Feb27/trumptweets.csv",
                       colClasses = c("character"))
trumptweets$created_at <- as.POSIXct(trumptweets$created_at)

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


samp<- read.csv("../data/friends_info/edgelist_Feb27/samp1_idsn.csv", colClasses = c("character"))
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
save(A,users, tweets,retws, file =paste0(RDataPath,"retweet_A1.RData"))
#retws, retweet happend time, retweet altitudes, some info on the edges


samp<- read.csv("../data/friends_info/edgelist_Feb27/samp2_idsn.csv", colClasses = c("character"))
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
save(A,users, tweets,retws, file =paste0(RDataPath,"retweet_A2.RData"))
#retws, retweet happend time, retweet altitudes, some info on the edges



samp<- read.csv("../data/friends_info/edgelist_Feb27/samp3_idsn.csv", colClasses = c("character"))
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
save(A,users, tweets,retws, file =paste0(RDataPath,"retweet_A3.RData"))
#retws, retweet happend time, retweet altitudes, some info on the edges


samp<- read.csv("../data/friends_info/edgelist_Feb27/all_samp.csv", colClasses = c("character"))
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
save(A,users, tweets,retws, file =paste0(RDataPath,"retweet_A123.RData"))
#retws, retweet happend time, retweet altitudes, some info on the edges