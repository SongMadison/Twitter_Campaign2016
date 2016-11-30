#download Trump 's follower list
rm(list =ls())
load('credential.RData')


library(dplyr)
library(RSQLite)
library(twitteR)


setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)




p1 <- proc.time()
trump <- getUser(user = 'realDonaldTrump')  # return a user object
# lookupUsers(users = list of users ) # return a list of user objects

## n =100, a list of IDs
#ur.followerIDs <- ur$getFollowerIDs( retryOnRateLimit = 15)
trump.followers <- trump$getFollowers(n = 1440000, retryOnRateLimit = 15)  # around 10 %

proc.time()- p1


#transform list to data.frame
#system.time ( ur.followers.info <-  do.call('rbind',lapply(trump.followers, as.data.frame)))
system.time(trump.followers.info <- rbindlist(lapply(trump.followers,as.data.frame)))  #faster



# 20k -- 20 mins
save.image(file = "followers_whole.RData")
write.csv(trump_followers_info, file = paste0("./data/trump_followersInfo_",Sys.Date(),".csv"))
proc.time() - p1

