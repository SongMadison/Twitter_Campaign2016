


#download Trump 's follower list
rm(list =ls())

cat("package?", "twitteR:1 or smappR ")


arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) <1) stop("need at least method parameters, twitteR:1 or smappR:2")
k <- as.numeric(arguments[1])
start_i <- as.integer(arguments[2])
experiment_size <- as.numeric(arguments[3])
if (length(arguments) <4) {buffer_size <- 50
}else{buffer_size <- as.integer(arguments[4])  }

#Using twitteR 
if( method == 1 || method=="twitteR"){
  
library(twitteR)
load('../data/credential.RData')
k = 9
consumer_key <- credential$consumer_key[k]
consumer_secret <- credential$consumer_secret[k]
access_token <- credential$access_token[k]
access_secret <- credential$access_secret[k]
# #setup twitter credentials
setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)


#issue 1: rate limit on followers/id/ on each page, not guranteed, how to set cursor
## n =100, a list of IDs
trump <- getUser(user = 'realDonaldTrump')  # return a user object
trump.followerIDs <- trump$getFollowerIDs(n =4000000,retryOnRateLimit = 20)
save(trump.followerIDs, file=paste("../data/followers_Info/trump-followers-IDs.RData"))
trump.follwers <- trump$getFollowers(n =4000000,  retryOnRateLimit  = 20 )
save(trump.followers, file=paste("../data/followers_Info/trump-followers-Info.RData"))
}


### Use smappR
if( method ==2 || method = "smappR"){
  
library(ROAuth)
library(smappR)
#create the folder

if(!file.exists("../data/credentials/my_oauth")){
  
  load('../data/credential.RData')
  k = 9
  consumer_key <- credential$consumer_key[k]
  consumer_secret <- credential$consumer_secret[k]
  access_token <- credential$access_token[k]
  access_secret <- credential$access_secret[k]
  # #setup twitter credentials
  setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)
  
  requestURL <- "https://api.twitter.com/oauth/request_token"
  accessURL <- "https://api.twitter.com/oauth/access_token"
  authURL <- "https://api.twitter.com/oauth/authorize"
  
  my_oauth <- OAuthFactory$new(consumerKey = consumer_key,
                               consumerSecret = consumer_secret,
                               requestURL = requestURL,
                               accessURL = accessURL,
                               authURL = authURL)
  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  save(my_oauth, file = "../data/credentials/my_oauth")

}


fbfollowers <- getFollowers("swang282", oauth_folder = "../data/credentials",sleep = 30)
trump_followers <- getFollowers("realDonaldTrump", cursor = , oauth_folder = "../data/credentials", sleep = 30)
while (current_cursor = 1)
  trump_followers <- getFollowers("realDonaldTrump", cursor = , oauth_folder = "../data/credentials", sleep = 30)
save(list = "trump_followers", file = "../data/trump_followers_id_only_2016-10-05.RData")
getUsersBatch(ids = trump_followers, oauth_folder = "../data/credentials", 
              include_entities = TRUE, verbose = TRUE, 
              output = "../data/trump_followers_screen-names__2016-10-05.json")

}

# as.data.frame(trump)
# description statusesCount followersCount favoritesCount friendsCount
# 1                     33331       11897865             39           41
# url            name             created protected
# 1 https://t.co/mZB2hymxC9 Donald J. Trump 2009-03-18 13:46:38     FALSE
# verified      screenName     location lang       id listedCount
# 1     TRUE realDonaldTrump New York, NY   en 25073877       39124
# followRequestSent
# 1             FALSE

# lookupUsers(users = list of users ) # return a list of user objects





# #transform list to data.frame
# #system.time ( ur.followers.info <-  do.call('rbind',lapply(trump.followers, as.data.frame)))
# system.time(trump.followers.info <- rbindlist(lapply(trump.followers,as.data.frame)))  #faster
# 
# 
# trump_followers_info <- rbindlist(lapply(ur.followers,as.data.frame))
# trump_followers_id_sn <- data.frame(rowid = 1:nrow(trump_followers_info), 
#                             id = trump_followers_info$id,
#                             screenName = trump_followers_info$screenName
#                             )
# write.csv(trump_followers_id_sn, 
#           file = './data/trump_followers_id_sn_top20000.csv', row.names = F)
# # 20k -- 20 mins
# save.image(file = "followers_whole.RData")
# write.csv(trump_followers_info, file = paste0("./data/trump_followersInfo_",Sys.Date(),".csv"))
# proc.time() - p1

