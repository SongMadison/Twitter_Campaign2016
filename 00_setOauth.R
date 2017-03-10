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
if(! file.exists("./credentials/all")){
  dir.create("./credentials/all")
}
save(my_oauth, file = paste0("./credentials/my_oauth",k))
save(my_oauth, file = paste0("./credentials/credential37/my_oauth",k))

getFollowers('swang282',oauth_folder = './credentials/credential37')

files <- NULL
files <- c(files, list.files("./credentials/credential_mixed/"))
files <- c(files, list.files("./credentials/credential_mixed1/"))
files <- c(files, list.files("./credentials/credential_mixed2/"))
files <- c(files, list.files("./credentials/credential_mixed3/"))


cred <- matrix("", 80, 4)
for(f in list.files("./credentials/all/")){
  load(paste0("./credentials/all/",f))
  k <- as.numeric(gsub(pattern = "my_oauth(\\d.*)", replacement = '\\1', f))
  cat(k,'\n')
  item_k <- c(my_oauth$consumerKey, my_oauth$consumerSecret, my_oauth$oauthKey, my_oauth$oauthSecret)
  cred[k,] <- item_k
}




