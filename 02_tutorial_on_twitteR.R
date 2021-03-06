rm(list=ls())
library(twitteR)
load('credential.RData')


#setup twitter credentials

consumer_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
consumer_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
access_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
access_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

# #setup twitter credentials
#setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

my_oauth <- OAuthFactory$new(consumerKey = consumer_key,
                             consumerSecret = consumer_secret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)
#twitteR::setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)

setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)



######################################################################
#       play around:
######################################################################
#status object:  two methods to download twitters
rdmTweets <- userTimeline("swang282", n=10)  #used the twitteR library to pull in a user stream, most recent statuses
#searchTwitter(searchString = 'swang282', n =25, since = '2013-01-01')
#rdmTweets <- searchTwitter('@realDonaldTrump', n=500, resultType = 'recent')  #search around a hashtag or string
#resultType = "mixed", or "popular"  
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
#Here are the columns, 
names(df)  
#And some example content
head(df,3)

#update/ delete twittes
text = "This is status from Rstudio!"
#tweet(text)

no_retweets = strip_retweets(rdmTweets)
df1 <- do.call("rbind", lapply(no_retweets, as.data.frame))


## user object:
a = getUser(user= 'swang282')
a.followers <- a$getFollowers()
a.followers.info <- do.call('rbind',lapply(a.followers, function(x) as.data.frame(getUser(user = x))))
a.friends <- a$getFriends()
a.friends.info <- do.call('rbind',lapply(a.friends, function(x) as.data.frame(getUser(user = x))))




#jure
# 169 friends; 16.9 K follwers
b = getUser(user= 'jure')
proc.time()
b.followers <- b$getFollowers()
proc.time()
b.followers.info <- do.call('rbind',lapply(b.followers, function(x) as.data.frame(getUser(user = x))))
b.friends <- a$getFriends()
b.friends.info <- do.call('rbind',lapply(b.friends, function(x) as.data.frame(getUser(user = x))))

######################################################################
##### A goal here is to figure out how people twittered this status:
######################################################################

# 1 Hillary Clinton is taking the day off again, she needs the rest. Sleep well Hillary - see you at the debate!     FALSE         32329
# replyToSN             created truncated replyToSID                 id replyToUID
# 1        NA 2016-09-20 14:23:59     FALSE         NA 778238281196662784         NA
# statusSource      screenName retweetCount isRetweet retweeted
# 1 <a href="http://twitter.com/download/iphone" rel="nofollow">Twitter for iPhone</a> realDonaldTrump        14150     FALSE     FALSE
# longitude latitude
# 1      <NA>     <NA>

# containing '@realDonaldTrump'
lst_trumpsample = searchTwitter('@realDonaldTrump', n = 4000)
df_trumpsample = do.call("rbind", lapply(lst_trumpsample, as.data.frame))

length(unique(df_trumpsample$replyToUID))
df_trump_sub <- df_trumpFollower[which(df_trumpsample$replyToSID == '778238281196662784'),]  #96 rows
df_trump_sub$screenName  # these are people who replied to trump is status


#basic infos of people retitted this status
## how to control the rate limit to avoid being blacklisted
followers.info = do.call("rbind", lapply(df_trump_sub$screenName,function(x) as.data.frame(getUser(user = x))))
##



## retweet 
rt.sample <- retweets(id = '778238281196662784', n =100)
rt.sample.df <- rbindlist(lapply(rt.sample,as.data.frame))
as.data.frame(showStatus(i='778238281196662784'))
###########################################################################################
###  study that bipartite relationships between twittes and users
###########################################################################################
# from Trump's timeline
# containing '@realDonaldTrump' to get a set of people of who twitted each status
lst_trump_timeline
lst_trump_timeline = userTimeline(user = 'realDonaldTrump', n= 3200)  # only returned 1329, user_timeline has a cap of 3200
df_trump_timeline = do.call("rbind", lapply(lst_trump_timeline, as.data.frame))

unique(df_trump_timeline$replyToSID) # 1 others' twitte."766741570024534016" and NA
unique(df_trump_timeline$replyToUID) # others:"44945327"  "357606935", and NA

df_trump_sub <- df_trumpFollower[which(df_trump_timeline$replyToSID == '778238281196662784'),]  #96 rows
df_trump_sub$screenName  # these are people who replied to trump is status

for id in df_trumpsa$replyToSID
#basic infos of people retitted this status
## how to control the rate limit to avoid being blacklisted
followers.info = do.call("rbind", lapply(df_trump_sub$screenName,function(x) as.data.frame(getUser(user = x))))
##


# one of trump of likes
dat <- favorites('PartholonSon')
