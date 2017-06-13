
## yini gave the source
#https://raw.githubusercontent.com/sashaperigo/Trump-Tweets/
trumptweets <-read.csv("https://raw.githubusercontent.com/sashaperigo/Trump-Tweets/master/data.csv", 
                       colClasses = c("character","character","character"))
trumptweets<- trumptweets[,c("Tweet.ID","Date","Text")]
names(trumptweets)  <- c("id_str","created_at","text")
trumptweets$created_at <- as.POSIXct(trumptweets$created_at)
trumptweets$created_at[1:10]


#29886, by July 30, 2016
#time zone is in east coast
# match a tweets with other sets



trumptweets_Hadoop <- read.csv("../data/trump_tweets/tweets_fromHadoop.csv", colClasses = c("character"))
trumptweets2 <- trumptweets_Hadoop[,c("id_str","created_at" ,"text")]
trumptweets2$created_at <- as.POSIXct(trumptweets2$created_at)
trumptweets2$created_at[1:10]


tweets_timeline <- read.csv("../data/friends_info/edgelist_Feb27/allTweets_timeline.csv", colClasses = c("character"))
tweets_timeline$created_at <- as.POSIXct(tweets_timeline$created_at)

#find 255294883680632833 in all of the three
trumptweets[trumptweets$id_str =="255294883680632833",]
trumptweets2[trumptweets2$id_str =="255294883680632833",]
tweets_timeline[tweets_timeline$id_str == "255294883680632833",]

trumptweets$created_at <- trumptweets$created_at-3600   #shift by 1hour east time -> CST



all <- rbind(trumptweets,trumptweets2,tweets_timeline)
ids <- unique(all$id_str)
all <- all[match(ids,all$id_str),]
all <- all[order(all$created_at, decreasing = T),]
all<- all[all$created_at< as.POSIXct('2016-11-09 00:00:00'),]
hist(all$created_at,breaks = 100,freq = T)

dim(all)

#29852 -> 31499
length( grep("^[\"\']", trumptweets$text) ) #potential retweets by trump himself 11773
all[grep('^[\"\']', trumptweets$text)[1:20],]
idx <- grep("^RT", all$text)
trumptweets <- trumptweets[-idx,]
all <- all[-idx,]
write.csv(all, file ="../data/friends_info/edgelist_Feb27/trumptweets.csv", row.names = F)


elec_tweets <- read.csv("../data/friends_info/edgelist_Feb27/trumptweets.csv", colClasses = c("character"))
elec_tweets$created_at <- as.POSIXct(elec_tweets$created_at)
elec_tweets <- elec_tweets[elec_tweets$created_at > as.POSIXct("2015-06-16 00:00:00"),]
write.csv(elec_tweets,file ="../data/friends_info/edgelist_Feb27/trumptweets_election.csv", row.names = F)
hist(as.Date(elec_tweets$created_at), breaks =50, freq = T)





#### back up --- created the allTweets_timeline.csv
tweets <- read.csv("../data/friends_info/edgelist_Feb27/allRetweets_timeline.csv", colClasses = c("character"))
#all the retweets, reply, quoted
#retweet_status_id_str, in_reply_to_status_id_str, quoted_status_id_str
#theses are tweets from trump followers and other tweets

idx1 <- which(!is.na(tweets$retweet_status_id_str)) 
tw1 <- tweets[idx1, c("retweet_status_id_str","retweet_status_text","retweet_status_created_at",
                      "retweet_status_user_id_str")]
names(tw1) <- c("Tweet.ID", "Text","Date","Users")
ids <- unique(tw1$Tweet.ID)
tw2 <- tw1[match(ids, tw1$Tweet.ID),]  #removed duplicated, 1.6M -> 29k
tw2 <- data.frame(tw2)
tw2 <- tw2[,c("Tweet.ID", "Date", "Text")]
tw2$Date <- as.POSIXct(tw2$Date, format = "%a %b %d %H:%M:%S %z %Y")
tw2 <- tw2[tw2$Date<'2016-11-09 00:00:00',]
names(tw2) <- c("id_str","created_at","text")
allTweets_timeline <- tw2[order(tw2$created_at,decreasing = T),]
#write.csv(allTweets_timeline, file ="../data/friends_info/edgelist_Feb27/allTweets_timeline.csv", row.names = F)




# anohter tweets achrive available on Twitter until 2017-1-27
tws <- read.csv("https://raw.githubusercontent.com/bpb27/political_twitter_archive/master/realdonaldtrump/realdonaldtrump.csv",
                colClasses = c("character"))
tws$created_at<- as.POSIXct(tws$created_at, format = "%a %b %d %H:%M:%S %z %Y")  # local time, -6 timezone
tws <- tws[tws$created_at < as.POSIXct("2016-11-09 00:00:00"),] #29963
tws <- tws[-grep("^RT", tws$text),]
hist(trumptweets$created_at, breaks = 100)
#as.Date(trumptweets$created_at, format = "%a %b %d %H:%M:%S %z %Y")

idx <- match(trumptweets$id_str, all$id_str)
sum(is.na(idx))
which(is.na(idx))