
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


## I updated the getBatchUsers function in the original package, add a feature to allow randomness
# library(devtools)
# build("smappR",path = "./smappR/")
# install.packages(pkgs = "./smappR/smappR_0.5.tar.gz", repos = NULL, 
#                  lib = "/afs/cs.wisc.edu/u/s/o/songwang/R/Rlibs/"  )

library(smappR, lib.loc = '~/R/Rlibs')
screenName = "realDonaldTrump"


my_oauth_folder <- "./credentials/credential_mixed2"
#followerIDs <- getFollowers("jure", cursor = -1, oauth_folder = my_oauth_folder,sleep = 10, 
#                           output ="/p/stat/songwang/trump_followers/ids/jure.txt") #<5000


followerIDs <- getFollowers(screenName, cursor = -1, oauth_folder = my_oauth_folder, sleep = 10,
                            output =  paste0("/p/stat/songwang/trump_followers/ids/", 
                                   screenName,"_id_only.txt")) #mutiple 5k

followers <- getUsersBatch(ids = followerIDs, oauth_folder = my_oauth_folder, 
              include_entities = TRUE, verbose = TRUE, 
              output = paste0("/p/stat/songwang/trump_followers/jsons/",
                              screenName,"_screen-names.json"))


