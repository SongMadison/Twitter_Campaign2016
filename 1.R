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
  consumer_key ='QqZQMhTFqF6Ap5p5DTC9AcxBc'
  consumer_secret = 'PPScyl8yoDjB8JBriys19ujfYgbFKMrq1LrmiqXcxu83N2OU1F'
  access_token ='518543036-fJ3IdymSwn3YNrCOc4hR4bADXy5gzfRwkP3Ohlfr'
  access_secret = 'MaNPSGJPyMLysjHcOPIIRvPXYASZMQsePYorVzHg9oXGC'
  

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










