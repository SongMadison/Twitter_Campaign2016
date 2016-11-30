
rm(list =ls())
library(smappR)
## Fred Boehm's script

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



#.libPaths(c("/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-pc-linux-gnu-library/3.2",.libPaths()))



setwd("~/Stat/Twitter_Campaign2016/code/")
screenName = "realDonalTrump"


my_oauth_folder <- "./credentials/credential_mixed2/"
#load(paste0(my_oauth_folder,"/my_oauth"))

swllowers <- getFollowers("swang282", oauth_folder = my_oauth_folder,sleep = 30) #<5000
followerIDs <- getFollowers(screenName, cursor = -1, oauth_folder = my_oauth_folder, sleep = 60) #mutiple 5k
save(followerIDs, file = paste0("../data/followers_info/ids/", 
                                 screenName,"_id_only.RData"))

followers <- getUsersBatch(ids = followerIDs, oauth_folder = my_oauth_folder, 
              include_entities = TRUE, verbose = TRUE, 
              output = paste0("../data/followers_info/jsons/",screenName,"_screen-names.json"))


