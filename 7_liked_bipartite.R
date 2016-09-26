

rm(list=ls())
library(twitteR)
library(data.table)
library(Matrix)

load('credential.RData')
#load('follower_samp20000.RData')
load("favor_20_twites.RData")

#setup twitter credentials
setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)


# trump followers features:
#trump.followers.info <- rbindlist(lapply(ur.followers,as.data.frame))

df_trump_timeline <- read.csv("./data/trump_timeline_2016-09-26.csv")
id_trump_timeline <- df_trump_timeline$id

isNA <- which(favor_twittes == 'NA')
favor_twittes <- favor_twittes[-isNA]
isFavorNull <- which( lapply(favor_twittes, length) == 0)
favor_twittes <- favor_twittes[-isFavorNull]

#create edge list
i_set = NULL
j_set = NULL
for (j in 1:length(favor_twittes)){
  id_j <-  sapply(favor_twittes[[j]],function(x) id(x))
  i_j <- match(id_j, id_trump_timeline)
  i_j <- i_set[which(i_j !='NA')]
  i_set <- c(i_set,i_j)
  j_set <- c(j_set, rep(j, length(i_j)))
}

adj <- sparseMatrix(i = i_set, j = j_set, x = rep(1,length(i_set)))


