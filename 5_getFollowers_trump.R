
rm(list =ls())
library(smappR)

## Fred Boehm's script shared with me

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



my_oauth_folder <- "./credentials/credential_mixed2/"

# sn = 'swang282'
# sn = 'jure'
sn = "realDonaldTrump"
followers_IDs <- getFollowers(sn, cursor = -1, oauth_folder = my_oauth_folder, sleep = 30) #mutiple 5k
writeLines(followers_IDs, con = paste0("/p/stat/songwang/trump_newfollowers/ids/", 
                                sn,"_ids.txt"))


#This takes too much time, output during the downloading
followers_info <- getUsersBatch(ids = followers_IDs, oauth_folder = my_oauth_folder, 
                           include_entities = TRUE, verbose = TRUE, 
                           output = paste0("/p/stat/songwang/trump_newfollowers/jsons/",
                                           sn, "_info.json"))

write.csv(followers_info, file = paste0("/p/stat/songwang/trump_newfollowers/jsons/",
                                        sn, "_info.csv"))
