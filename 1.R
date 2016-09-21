#install.packages('twitteR')
library(twitteR)
consumer_key ='QqZQMhTFqF6Ap5p5DTC9AcxBc'
consumer_secret = 'PPScyl8yoDjB8JBriys19ujfYgbFKMrq1LrmiqXcxu83N2OU1F'
access_token ='518543036-fJ3IdymSwn3YNrCOc4hR4bADXy5gzfRwkP3Ohlfr'
access_secret = 'MaNPSGJPyMLysjHcOPIIRvPXYASZMQsePYorVzHg9oXGC'
setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)


require(twitteR)
#The original example used the twitteR library to pull in a user stream
#rdmTweets <- userTimeline("swang282", n=100)
#Instead, I'm going to pull in a search around a hashtag.
rdmTweets <- searchTwitter('#Trump', n=500)
# Note that the Twitter search API only goes back 1500 tweets (I think?)
length(rdmTweets)
#Create a dataframe based around the results
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))

#Here are the columns
names(df)
#And some example content
head(df,3)


## --who twitted most in the sample we have?
counts=table(df$screenName)
barplot(counts[counts>2])
# Let's do something hacky:
# Limit the data set to show only folk who tweeted twice or more in the sample
cc=subset(counts,counts>2)
barplot(cc,las=2,cex.names =0.3)

## -- pulling out the names of folk who have been retweeted or who have had a tweet sent to them:

#Whilst tinkering, I came across some errors that seemed
# to be caused by unusual character sets
#Here's a hacky defence that seemed to work...
df$text=sapply(df$text,function(row) iconv(row,to='UTF-8'))

#A helper function to remove @ symbols from user names...
trim <- function (x) sub('@','',x)

#A couple of tweet parsing functions that add columns to the dataframe
#We'll be needing this, I think?
library(stringr)
#Pull out who a message is to
df$to=sapply(df$text,function(tweet) str_extract(tweet,"^(@[[:alnum:]_]*)"))
df$to=sapply(df$to,function(name) trim(name))

#And here's a way of grabbing who's been RT'd
df$rt=sapply(df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))

require(ggplot2)
ggplot()+geom_bar(aes(x=na.omit(df$rt)))
#+theme(axis.text.x=theme_text(angle=-90,size=6))+xlab(NULL)
