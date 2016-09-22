

# This script is going to use the streaming API to download trump related twittes

library(streamR)
load("credential.RData")

filterStream(file.name = "tweets.json", # save tweets in a json file. 
             track = c('@realDonaldTrump'),
             language = 'en',
             timeout = 3*60*60, # running for 60 seconds,
             oauth = my_oauth)

#tweets_df <- parseTweets("tweets.json", simplify = FALSE)


