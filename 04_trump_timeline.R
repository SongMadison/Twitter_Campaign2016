

rm(list=ls())
library(twitteR)
library(data.table)
load('credential.RData')


k = 3
consumer_key <- credential$consumer_key[k]
consumer_secret <- credential$consumer_secret[k]
access_token <- credential$access_token[k]
access_secret <- credential$access_secret[k]

setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)


ratelimit <- getCurRateLimitInfo()
ratelimit[ratelimit$resource == "/statuses/home_timeline",]
#/statuses/home_timeline    15        15 2016-09-26 19:25:40
lst_trump_timeline = userTimeline(user = 'realDonaldTrump', n = 3200,  
                                  includeRts = T, retryOnRateLimit = 15)  
# default =20,  user_timeline has a cap of 3200
# without cludeRts = T, returned much fewer than 3200 statuses

head(lst_trump_timeline)
length(lst_trump_timeline) #3199

# convert list to data.frame
# system.time( df_trump_timeline <- do.call("rbind", lapply(lst_trump_timeline, as.data.frame)) )
system.time( df_trump_timeline <- rbindlist(lapply(lst_trump_timeline, as.data.frame)) )

Sys.Date()#10s
write.csv(df_trump_timeline, file = paste0("../data/trump_timeline/trump_timeline_",Sys.Date(),".csv"))


# some exploratory on his timelines
unique(df_trump_timeline$replyToSID) # 1 others' twitte."766741570024534016" and NA
unique(df_trump_timeline$replyToUID) # others:"44945327"  "357606935", and NA


