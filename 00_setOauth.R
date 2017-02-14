#credential <- read.csv("../data/twitter_credential.csv", stringsAsFactors =F)
#save(credential, file = '../data/credential.RData')
load('../data/credential.RData')
library(ROAuth)
k = 15
#dir.create("./credentials/credential15")
if(!file.exists(paste0("./credentials/credential",k,"/my_oauth") )){
  consumer_key <- credential$consumer_key[k]
  consumer_secret <- credential$consumer_secret[k]
  access_token <- credential$access_token[k]
  access_secret <- credential$access_secret[k]
  # #setup twitter credentials
  #setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)
  
  requestURL <- "https://api.twitter.com/oauth/request_token"
  accessURL <- "https://api.twitter.com/oauth/access_token"
  authURL <- "https://api.twitter.com/oauth/authorize"
  
  my_oauth <- OAuthFactory$new(consumerKey = consumer_key,
                               consumerSecret = consumer_secret,
                               requestURL = requestURL,
                               accessURL = accessURL,
                               authURL = authURL)
  #twitteR::setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)
  
  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  
  #STOP!!!! for the browser to give you the verifier
}
##save in the folder
save(my_oauth, file = paste0("./credentials/credential",k,"/my_oauth"))
