
rm(list =ls())

library(streamR)
load("credential.RData")

filterStream(file.name = "tweets_ing-sep25.json", # save tweets in a json file. 
             track = c('@realDonaldTrump'),
             language = 'en',
             timeout = 0, # running for 60 seconds,
             oauth = my_oauth)

# tweets_df <- parseTweets("trump_tweets.json", simplify = FALSE)
# 
# 
# grep(pattern = '@realDonaldTrump',x =tweets_df$text[1:100])
