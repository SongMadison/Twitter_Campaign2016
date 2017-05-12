rm(list=ls())

## step 0 : install packages and its dependent packages
library(smappR)



###################### step 1 ############################################################
## set up the twitter API, you can create multiple applications
## google applications twitter api


consumer_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
consumer_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
access_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
access_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

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

#setup twitter credentials
setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)

dir.create("./credentials/")
save(my_oauth, file = paste0("./credentials/"))







######################################################################
#       step 2: play around:
######################################################################


#download followers based on screen_name/ids
library()
?getFollowers
#getFollowers(screen_name = NULL, oauth_folder, cursor = -1,
#                              user_id = NULL, verbose = TRUE, sleep = 1)
followers <- getFollowers("swang282", oauth_folder = "./credentials/")
friends <- getFriends(screen_name = 'swang282', oauth_folder = "./credentials/")

users <- getUsers(oauth_folder = "./credentials/", screen_names = 'swang282')
users2 <- getUsersBatch( screen_names = followers, oauth_folder = "./credentials/")
users2 <- getUsersBatch( screen_names = friends, oauth_folder = "./credentials/")




### about twitters:

# download timeline
?getTimeline
getTimeline(filename = './swang282.json', n = 3200, oauth_folder = "./credentials",
            screen_name = 'swang282', id = NULL, since_id = NULL, trim_user = "true",
            sleep = 0.5, verbose = FALSE)

## user object:
a = getUser(user= 'swang282')
a.followers <- a$getFollowers()
a.followers.info <- do.call('rbind',lapply(a.followers, function(x) as.data.frame(getUser(user = x))))
a.friends <- a$getFriends()
a.friends.info <- do.call('rbind',lapply(a.friends, function(x) as.data.frame(getUser(user = x))))



#stream API, keep downloading things related to a topic
library(streamR)
# filterStream(file.name = NULL, track = NULL,
#              follow = NULL, locations = NULL, language = NULL,
#              timeout = 0, tweets = NULL, oauth = NULL,
#              verbose = TRUE)
oauth_file = sample(list.files("./credentials/"), 1)
load(oauth_file)
filterStream(file.name = './trump.json',
             track = c('@realDonaldTrump',"Trump"), language = 'en',
             timeout=60, oauth= my_oauth )



