#download Trump 's follower list
library(dplyr)
library('RSQLite')
library(data.table)


ur <- getUser(user = 'realDonaldTrump')  # return a user object
# lookupUsers(users = list of users ) # return a list of user objects
#
ur.followerIDs <- ur$getFollowerIDs(n = 10000, retryOnRateLimit = 15) ## a list of IDs
ur.followers <- ur$getFollowers(n = 10000, retryOnRateLimit = 15)
system.time ( ur.followers.info <-  do.call('rbind',lapply(ur.followers, as.data.frame)))
system.time(a <- rbindlist(lapply(ur.followers,as.data.frame)))

favorited_twittes <- list()
p1 <- proc.time()
for (user_obj in ur.followers[1:100]){
    favorited_twittes[[user_obj$id]] <- favorites(user = user_obj, n = 200, retryOnRateLimit = 15)
}
proc.time()-p1
#
tweets_db <- src_sqlite(path = "tweets_db.sqlite3", create = T)
register_sqlite_backend('tweets_db.sqlite3')
search_twitter_and_store("swang282",table_name = 'myself')


favorites(user = 'swang282')
