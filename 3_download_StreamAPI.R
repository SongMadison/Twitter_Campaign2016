
rm(list =ls())

library(streamR)
load("credential.RData")

while(TRUE){
  filterStream(file.name = paste0("./data/trumpStream/tweets_ing-",Sys.time(),".json"), # save tweets in a json file. 
               track = c('@realDonaldTrump'),
               language = 'en',
               timeout = 300, # running for 60 seconds,
               oauth = my_oauth)
}


# tweets_df <- parseTweets("trump_tweets.json", simplify = FALSE)
# 
# 
# grep(pattern = '@realDonaldTrump',x =tweets_df$text[1:100])
