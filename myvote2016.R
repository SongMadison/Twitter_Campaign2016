
rm(list =ls())

library(streamR)

load("./credentials/credential2/my_oauth")  #--folder /p/stat/songwang/trumpStream_Nov789-4
while(TRUE){
  filterStream(file.name = paste0("/p/stat/songwang/myvote2016/tweets_ing-",Sys.time(),".json"), 
               # save tweets in a json file. 
               track = c('#myvote2016'),
               language = 'en',
               timeout = 300, # running for 60 seconds,
               oauth = my_oauth)
}

