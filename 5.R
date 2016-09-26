#download Trump 's follower list
rm(list =ls())
load('credential.RData')


library(dplyr)
library('RSQLite')
if (!require('data.table')){
  install.packages("data.table")
  library(data.table)
}else{
  library(data.table)
}
library(twitteR)


setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)




p1 <- proc.time()
ur <- getUser(user = 'realDonaldTrump')  # return a user object
# lookupUsers(users = list of users ) # return a list of user objects
#
#ur.followerIDs <- ur$getFollowerIDs( retryOnRateLimit = 15) ## n =100, a list of IDs
ur.followers <- ur$getFollowers(n = 1440000, retryOnRateLimit = 15)
proc.time()-p1
# 20k -- 20 mins
save.image(file = "followers_whole.RData" )

#system.time ( ur.followers.info <-  do.call('rbind',lapply(ur.followers, as.data.frame)))
system.time(ur.followers.info <- rbindlist(lapply(ur.followers,as.data.frame)))


proc.time() - p1
# favorited_twittes <- list()
# p1 <- proc.time()
# for (user_obj in ur.followers[1:100]){
#     favorited_twittes[[user_obj$id]] <- favorites(user = user_obj, n = 200, retryOnRateLimit = 15)
# }
# proc.time()-p1
# #
# tweets_db <- src_sqlite(path = "tweets_db.sqlite3", create = T)
# register_sqlite_backend('tweets_db.sqlite3')
# search_twitter_and_store("swang282",table_name = 'myself')
# 
# 
# favorites(user = 'swang282')
