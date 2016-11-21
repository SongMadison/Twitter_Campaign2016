
rm(list =ls())

library(streamR)
#load("../data/credential.RData")

load("./credentials/credential1/my_oauth")  
#load("./credentials/credential15/my_oauth")  #--folder /p/stat/songwang/trumpStream_Nov789-4
while(TRUE){
  filterStream(file.name = paste0("../data/trumpStream/tweets_ing-",Sys.time(),".json"), 
               # save tweets in a json file. 
               track = c('@realDonaldTrump'),
               language = 'en',
               timeout = 300, # running for 60 seconds,
               oauth = my_oauth)
}
