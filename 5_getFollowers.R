
# library(devtools)
# install_github(repo = "mongosoup/rmongodb")
# install_github("SMAPPNYU/smappR") 
# 
# fbfollowers <- getFollowers("FredBoehm128", oauth_folder = "~/Dropbox/credentials")
# trump_followers <- getFollowers("realDonaldTrump", oauth_folder = "~/Dropbox/credentials", sleep = 30)
# save(list = "trump_followers", file = "trump_followers_id_only.RData")
# getUsersBatch(ids = trump_followers, oauth_folder = "~/Dropbox/credentials", 
#               include_entities = TRUE, verbose = TRUE, 
#               output = "data/trump_followers_screen-names.json")
# 
# 
# 
# 
#.libPaths(c("/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-pc-linux-gnu-library/3.2",.libPaths()))

library(ROAuth)
library(smappR)

setwd("~/Stat/Twitter_Campaign2016/code/")
screenName = "realDonalTrump"


load('../data/credential.RData')
arguments <- commandArgs(trailingOnly = TRUE)
k = as.integer(arguments[1])
screenName <- arguments[2]

if(!file.exists(paste0("./credentials/credential",k,"/my_oauth") )){
    consumer_key <- credential$consumer_key[k]
    consumer_secret <- credential$consumer_secret[k]
    access_token <- credential$access_token[k]
    access_secret <- credential$access_secret[k]
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
    
    my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    
    #STOP!!!! for the browser to give you the verifier
}
   ##save in the folder
#save(my_oauth, file = paste0("./credentials/credential",k,"/my_oauth"))


my_oauth_folder <- paste0("./credentials/credential",k)
#load(paste0(my_oauth_folder,"/my_oauth"))

#fbfollowers <- getFollowers("swang282", oauth_folder = my_oauth_folder,sleep = 30) #<5000
followerIDs <- getFollowers(screenName, cursor = -1, oauth_folder = my_oauth_folder, sleep = 60) #mutiple 5k
save(followerIDs, file = paste0("../data/followers_info/ids/", 
                                 screenName,"_id_only.RData"))

followers <- getUsersBatch(ids = followerIDs, oauth_folder = my_oauth_folder, 
              include_entities = TRUE, verbose = TRUE, 
              output = paste0("../data/followers_info/jsons/",screenName,"_screen-names.json"))


