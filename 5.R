rm(list=ls())
library(twitteR)
library(data.table)
load('credential.RData')
load('follower_samp20000.RData')


#setup twitter credentials
setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)


#
trump <- getUser(user = 'realDonaldTrump')  # rettrumpn a user object

trump.followerIDs <- trump$getFollowerIDs(n = 10000, retryOnRateLimit = 15) ## a list of IDs
trump.followers <- trump$getFollowers(n = 10000, retryOnRateLimit = 15)
system.time ( trump.followers.info <-  do.call('rbind',lapply(trump.followers, as.data.frame)))
system.time(a <- rbindlist(lapply(trump.followers,as.data.frame)))

favorited_twittes <- list()
p1 <- proc.time()
for (user_obj in trump.followers[1:100]){
    favorited_twittes[[user_obj$screenName]] <- favorites(user = user_obj, n = 200, retryOnRateLimit = 15)
}
proc.time()-p1
#



##
tweets_db <- src_sqlite(path = "tweets_db.sqlite3", create = T)
register_sqlite_backend('tweets_db.sqlite3')
search_twitter_and_store("swang282",table_name = 'myself')


favorites(user = 'swang282')

load('trump_followers_id_only.RData')
