
#Goal:  construct the retweets network




rm(list = ls ())
source("Head_file.R")
source("function.R")
#template for parallel
# library(doParallel)
# # Find out how many cores are available (if you don't already know)
# detectCores()
# # Create cluster with desired number of cores
# cl <- makeCluster(4) # Register cluster
# registerDoParallel(cl)  # Find out how many cores are being used
# getDoParWorkers()
# 
# results <- foreach (k = 1: nbreaks, .combine = rbind, .packages= c("jsonlite") 
# ) %dopar% { 
#   f(k)
# }
# stopCluster(cl)

###data



# combine all the trump tweets from followers, each with 1200 tweets/retweets
# bigmem04,  ~ 10 mins
# total 3151 different tweets


Tweets_folder <- function(data_folder){
  
  # individual file
  getTrumpTweets <- function(filepath, target_user_id_str){
    #target_user_id_str ="25073877"
    dat <- read.csv(filepath, colClasses = c("character"), stringsAsFactors = F)
    idx1 <- which( ((dat$in_reply_to_user_id_str == target_user_id_str) * 
                      (!is.na(dat$in_reply_to_status_id_str))) >0)      #reply, or mention
    idx2 <- which(( (  dat$quoted_status_user_id_str == target_user_id_str ) * 
                      (!is.na(dat$quoted_status_id_str))) >0)   # quoted + comments
    idx3 <- which(( (  dat$retweet_status_user_id_str == target_user_id_str ) * 
                      (!is.na(dat$retweet_status_id_str))) >0)  # retweet + no comments
    idx <- unique(c(idx1, idx2, idx3))
    tweets<- dat[idx,]
    return(tweets)
  }
  
  files <- list.files(data_folder)
  n_cores = detectCores()
  cl <- makeCluster(floor(n_cores / 2))
  registerDoParallel(cl)
  tweets  <- foreach (i = 1:length(files),
                          .combine = rbind) %dopar% {
                            filepath <- paste0(data_folder, files[i])
                            getTrumpTweets(filepath, "25073877")
                          }
  stopCluster(cl)
  return (tweets)
}  


# construct edgelist from 
createEdgelist <- function(tweets, method ="all"){
  
  if (method == "reply"){
    from_user_id <- tweets$user_id_str
    from_status_id <-  tweets$id_str
    to_status_id <-  tweets$in_reply_to_status_id_str
  }else if (method == "retweet"){
    from_user_id <-  tweets$user_id_str
    from_status_id <- tweets$id_str
    to_status_id <- tweets$retweet_status_id_str
    to_status_id[which(!is.na(tweets$quoted_status_id_str))] <- 
      tweets$quoted_status_id_str[which(!is.na(tweets$quoted_status_id_str))]
  } else if (method == 'all'){
    from_user_id <-  tweets$user_id_str
    from_status_id <- tweets$id_str
    to_status_id <- tweets$in_reply_to_status_id_str
    to_status_id[which(!is.na(tweets$quoted_status_id_str))] <- 
      tweets$quoted_status_id_str[which(!is.na(tweets$quoted_status_id_str))]
    to_status_id[which(!is.na(tweets$retweet_status_id_str))] <- 
      tweets$retweet_status_id_str[which(!is.na(tweets$retweet_status_id_str))]
  }else{
    print("method is wrong")
    return (NULL)
  }
  
  df <- data.frame(from_user_id = from_user_id,
      from_status_id = from_status_id, to_status_id = to_status_id, stringsAsFactors = F)
  df <- df[which(!is.na(df$to_status_id)),]
}



library(doParallel)
tweets1 <-   Tweets_folder( "../data/followers_Network/followers_timeline_0_45k_csv/")
tweets2 <-   Tweets_folder( "../data/followers_Network/followers_timeline_45k_76k_csv/")
tweets <- rbind(tweets1, tweets2)
ids_unique <- unique(tweets$id_str)  
tweets <- tweets[match(ids_unique, tweets$id_str),]  ## match function will return the first index
#74842
tweets <- tweets[tweets$created_at < '2016-11-09 00:00:00',]
tweets <- tweets[tweets$created_at > '2015-01-01 00:00:00',]
#write.csv(tweets, file ="../data/trump_tweets/tweets_fromFollowers_Trump.txt", row.names = F) 
#59905    23
following_cluster <- read.csv("./1209/following/k50/id_sn_cluster.csv", 
                              colClasses = c("character","character"),
                              stringsAsFactors = F)
tweets <- tweets[!is.na (match(tweets$user_id_str, following_cluster$id_str)),]
write.csv(tweets, file = "../data/trump_tweets/followers_tweets/alltweets-followers-Trump.csv", 
          row.names = F)  ## 59904 obs

reply_ids <- tweets$in_reply_to_status_id_str
reply_ids <- reply_ids[!is.na(reply_ids)]  #21952
getStatuses(ids = reply_ids, filename = "../data/trump_tweets/followers_tweets/reply.json", 
            oauth_folder = "./credentials/credential_mixed1/"  )
data.str <- readLines("../data/trump_tweets/followers_tweets/reply.json")
#data.json <- paste0('[', paste0(data.str, collapse= ",") , ']')
data.json <- myToJSON(data.str)
data.df <- jsonlite::fromJSON(data.json, simplifyDataFrame= T)
reply.df <- simplifyTwitterDF(data.df)
write.csv(reply.df, file = "../data/trump_tweets/followers_tweets/reply.csv", row.names = F)


quoted_ids <- tweets$quoted_status_id_str
quoted_ids <- quoted_ids[!is.na(quoted_ids)]    #4573
getStatuses(ids = quoted_ids, filename = "../data/trump_tweets/followers_tweets/quoted.json", 
            oauth_folder = "./credentials/credential_mixed2/"  )

data.str <- readLines("../data/trump_tweets/followers_tweets/quoted.json")
#data.json <- paste0('[', paste0(data.str, collapse= ",") , ']')
data.json <- myToJSON(data.str)
data.df <- jsonlite::fromJSON(data.json, simplifyDataFrame= T)
quoted.df <- simplifyTwitterDF(data.df)
write.csv(quoted.df, file = "../data/trump_tweets/followers_tweets/quoted.csv", row.names = F)



tweetsFromHadoop <- read.csv( "../data/trump_tweets/tweets_fromHadoop.csv",colClasses = c("character"),
                              stringsAsFactors = F)
dim(tweetsFromHadoop)  ## 11305 

idx1 <- which(is.na(samp_tweets$in_reply_to_text) * (!is.na(samp_tweets$in_reply_to_status_id_str)) >0 )
tw_ids <- samp_tweets$in_reply_to_status_id_str[idx1]; length(tw_ids)
cat( sum(!is.na( match(tw_ids, tweetsFromHadoop$id_str))), "out of ",length(tw_ids)," missing replied_twitters are availalbe in Hadoop")

samp_tweets$in_reply_to_created_at[idx1] <- tweetsFromHadoop$created_at[match(tw_ids, tweetsFromHadoop$id_str)]
samp_tweets$in_reply_to_text[idx1] <- tweetsFromHadoop$text[match(tw_ids, tweetsFromHadoop$id_str)]

#for retweeted, either id_str exists with text ; or nothing is avaialbe
idx2 <- which(is.na(samp_tweets$quoted_status_text) * (!is.na(samp_tweets$quoted_status_id_str)) >0 )
length(idx2)


# library(xlsx)
# write.xlsx2(tweets, sheetName ="retweets_reply", file  = "../data/trump_tweets/followers_tweets/follwers-tweets-Trump.xlsx")
# write.xlsx2(reply.df, sheetName ="reply", file  = "../data/trump_tweets/followers_tweets/follwers-tweets-Trump.xlsx")
# write.xlsx2(quoted.df, sheetName ="retweet", file  = "../data/trump_tweets/followers_tweets/follwers-tweets-Trump.xlsx")





  
edgeList <- createEdgelist(tweets, method = "all")
el_reply = createEdgelist(tweets, method = "reply")
el_retweet = edgelist(tweets, method = "retweet")
write.csv(edgeList, file="../data/trump_tweets/followers_tweets/edgelist.csv", row.names = F)
write.csv(el_reply, file = "../data/trump_tweets/followers_tweets/edgelist_reply.csv", row.names = F)
write.csv(el_retweet, file = "../data/trump_tweets/followers_tweets/edgelist_retweet.csv", row.names = F)

#download the tweets
tweets_ids <- unique(edgeList$to_status_id) #3741
# getStatuses(tweets_ids, "../data/trump_tweets/followers_tweets/tweets-Trump.json",
# oauth_folder ="./credentials/credential_mixed3/")

data.str <- readLines("../data/trump_tweets/followers_tweets/tweets-Trump.json")

#data.json <- paste0('[', paste0(data.str, collapse= ",") , ']')
data.json <- myToJSON(data.str)
data.df <- jsonlite::fromJSON(data.json, simplifyDataFrame= T)
#due to double downloading, cuplicate
data.df <- simplifyTwitterDF(data.df)
data.df <- data.df[match(unique(data.df$id_str), data.df$id_str),]
#remove screen name is not realDonaldTrump, don't know why they came up ??
data.df <- data.df[which(data.df$user_id_str == "25073877"),]   #2616

tw_ids <- tweets_ids[is.na(match(tweets_ids, data.df$id_str))]; 
length(tw_ids)
tweetsFromHadoop <- read.csv( "../data/trump_tweets/tweets_fromHadoop.csv",colClasses = c("character"),
                              stringsAsFactors = F)
dim(tweetsFromHadoop)  ## 11305 


idx <- match(tw_ids , tweetsFromHadoop$id_str); sum(is.na(idx))
cat( "out of ", length(tw_ids), "which are not available from API", " only ",sum(is.na(idx)), "are not contained in the database")
idx1 <- match(tweets_ids, tweetsFromHadoop$id_str); 
cat( "out of ", length(tweets_ids), " only ", sum(is.na(idx1)), "are not contained in the database") 

names(tweetsFromHadoop)[12] <-"place_country"
names(data.df)
idx2 <- match( names(tweetsFromHadoop), names(data.df)); 
data2.df <- tweetsFromHadoop[,names(data.df)[idx2[!is.na(idx2)]]][idx[!is.na(idx)],]
data.df <-rbind(data.df[,names(data.df)[idx2[!is.na(idx2)]]], data2.df)
write.csv(data.df, file = "../data/trump_tweets/followers_tweets/tweets-Trump.csv", 
          row.names = F)  # 2900


## 
followers_ids <- unique(edgeList$from_user_id) #8672
# followers_info_new <- getUsersBatch(ids = followers_ids, oauth_folder = "./credentials/credential_mixed3/",
#               output = "../data/trump_tweets/followers_tweets/followersAll_Trump_new.json", verbose = T)
# write.csv(followers_info_new, file ="../data/trump_tweets/followers_tweets/followersAll_Trump_new.csv", 
#           row.names = F)








el <- read.csv("../data/trump_tweets/followers_tweets/edgelist.csv", 
                     colClasses=c("character","character", "character"), stringsAsFactors = F)
el<- el[,c(1,3)]; el$from_user_id <- as.character(el$from_user_id)


tweets <- read.csv("../data/trump_tweets/followers_tweets/tweets-Trump.csv",
                   colClasses=c("character","character"), stringsAsFactors = F)
tweets <- tweets[tweets$created_at < "2016-11-09 00:00:00", ]
tweets <- tweets[tweets$created_at >"2015-01-01 00:00:00", ] 
tweets$id_str <- as.character(tweets$id_str)  #2437 tweeters
tweets <- tweets[tweets$id_str %in% unique(el$to_status_id), ]  #2419
dim(tweets)


users<- read.csv("../data/trump_tweets/followers_tweets/followersAll_Trump_new.csv", 
                 colClasses=c("character"), stringsAsFactors = F)
users$id_str <- as.character(users$id_str)
users <- users[users$id_str %in% el$from_user_id, ]
dim(users)

# el <- el[el$from_user_id%in% users$id_str, ]
# el <- el[el$to_status_id %in% tweets$id_str, ]
# tweets <- tweets[tweets$id_str %in% el$to_status_id, ]
# users <- users[users$id_str %in% el$from_user_id, ]

save(el, tweets, users, file ="../data/trump_tweets/followers_tweets/edgelistAll.RData")
# el: 3741 tweets, 8672 followers, some of tweets/followers cannot be downloaded
# tweets 2417, users, 7897



