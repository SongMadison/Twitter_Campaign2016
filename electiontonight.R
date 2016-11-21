
rm(list =ls())

library(streamR)


keywords = c("#electionday", "#myvote2016","#ivote2016","#voted","electionnight","#VoteTrump")

load("./credentials/credential4/my_oauth")  #--folder /p/stat/songwang/trumpStream_Nov789-4

while(TRUE){
  filterStream(file.name = paste0("/p/stat/songwang/keywords-2/tweets_ing-",Sys.time(),".json"), 
               # save tweets in a json file. 
               track = keywords,
# language = 'en',
               timeout = 300, # running for 60 seconds,
               oauth = my_oauth)
}

