
rm(list =ls())
library(smappR)

## Fred Boehm's script shared with me, in tutorial folder

# library(devtools)
# install_github(repo = "mongosoup/rmongodb")
# install_github("SMAPPNYU/smappR") 
# 


#.libPaths(c("/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-pc-linux-gnu-library/3.2",.libPaths()))

setwd("~/Stat/Twitter_Campaign2016/code/")

my_oauth_folder <- "./credentials/credential_mixed4/"

# sn = 'swang282'
# sn = 'jure'
sn = "realDonaldTrump"
followers_IDs <- getFollowers(sn, cursor = -1, oauth_folder = my_oauth_folder, sleep = 30, 
                              output = paste0("/p/stat/songwang/trump_newFollowers/ids/", 
                                              sn,"_",Sys.Date(),"_ids.txt")) #mutiple 5k



#This takes too much time, output during the downloading
followers_info <- getUsersBatch(ids = followers_IDs, oauth_folder = my_oauth_folder, 
                           include_entities = TRUE, verbose = TRUE, 
                           output = paste0("/p/stat/songwang/trump_newfollowers/jsons/",
                                           sn, "_info.json"))

write.csv(followers_info, file = paste0("/p/stat/songwang/trump_newfollowers/jsons/",
                                        sn, "_info.csv"))
