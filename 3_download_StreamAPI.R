
rm(list =ls())

library(streamR)
load("../data/credential.RData")



# #setting up streamAPI
# k =18
# consumer_key <- credential$consumer_key[k]
# consumer_secret <- credential$consumer_secret[k]
# access_token <- credential$access_token[k]
# access_secret <- credential$access_secret[k]
# # #setup twitter credentials
# #setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)
# 
# requestURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "https://api.twitter.com/oauth/access_token"
# authURL <- "https://api.twitter.com/oauth/authorize"
# 
# my_oauth <- OAuthFactory$new(consumerKey = consumer_key,
#                              consumerSecret = consumer_secret,
#                              requestURL = requestURL,
#                              accessURL = accessURL,
#                              authURL = authURL)
# #twitteR::setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)
# 
# #my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

#load("./credentials/credential18/my_oauth") --folder Nov789-1
#load("./credentials/credential17/my_oauth")  #--folder Nov789-2
load("./credentials/credential16/my_oauth")  #--folder Nov789-3
load("./credentials/credential15/my_oauth")  #--folder /p/stat/songwang/trumpStream_Nov789-4
while(TRUE){
  filterStream(file.name = paste0("/p/stat/songwang/trumpStream_Nov789-4/tweets_ing-",Sys.time(),".json"), 
               # save tweets in a json file. 
               track = c('@realDonaldTrump'),
               language = 'en',
               timeout = 300, # running for 60 seconds,
               oauth = my_oauth)
}


# tweets_df <- parseTweets("trump_tweets.json", simplify = FALSE)
# 
# 
# grep(pattern = '@realDonaldTrump',x =tweets_df$text[1:100])




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
