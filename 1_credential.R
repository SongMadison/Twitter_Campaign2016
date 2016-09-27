rm(list=ls())
packages <- c('twitteR','streamR','RCurl','ROAuth','RJSONIO','stringr')
for (pack in packages ){
  if (!require(pack,quietly = TRUE,character.only = TRUE)){
    install.packages(pack)
    library(pack,character.only = T,quietly = T)
  }
}



#step 0: setup twitter credentials
if(!file.exists('credential.RData')){
  consumer_key ='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
  consumer_secret = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
  access_token = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
  access_secret = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
  

  requestURL <- "https://api.twitter.com/oauth/request_token"
  accessURL <- "https://api.twitter.com/oauth/access_token"
  authURL <- "https://api.twitter.com/oauth/authorize"

  my_oauth <- OAuthFactory$new(consumerKey = consumer_key,
                               consumerSecret = consumer_secret,
                               requestURL = requestURL,
                               accessURL = accessURL,
                               authURL = authURL)
  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  
  
  ### stop here!!!, need to verify with the PIN provided
  # part II
  save(consumer_key, consumer_secret,access_token, access_secret, my_oauth, file = 'credential.RData')
}










