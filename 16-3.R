rm(list = ls ())
source("Head_file.R")
#construct the retweets network


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



uniqueTweets <- function(data_folder){
  
  getTrumpTweets <- function(filepath){
    dat <- read.csv(filepath, stringsAsFactors = F)
    dat$in_reply_to_status_id_str <- as.character(dat$in_reply_to_status_id_str);names(dat)[11] <-"place_country"
    tweets<- dat[which(dat$in_reply_to_screen_name =="realDonaldTrump"),]
    return(tweets)
  }
  #sample: some in_reply_to_status_id_str = NA, while screen_name = Trump
  
  files <- list.files(data_folder)
  n_cores = detectCores()
  cl <- makeCluster(floor(n_cores / 2))
  registerDoParallel(cl)
  tweets  <- foreach (i = 1:length(files),
                          .combine = rbind) %dopar% {
                            filepath <- paste0(data_folder, files[i])
                            getTrumpTweets(filepath)
                          }
  stopCluster(cl)
  return (tweets)
}  
library(doParallel)
tweets1 <-   uniqueTweets( "../data/followers_Network/followers_timeline_0_45k_csv/")
tweets2 <-   uniqueTweets( "../data/followers_Network/followers_timeline_45k_76k_csv/")
tweets <- rbind(tweets1, tweets2)
write.csv(tweets, file ="../data/trump_tweets/tweets_fromFollowers_Trump.txt", row.names = F)  # contain replies, not retweet

edgeList_folder <- function(data_folder){
  
  edgeList_file <- function(filepath){
    dat <- read.csv(filepath, stringsAsFactors = F)
    dat$user_id_str <- as.character(dat$user_id_str)
    dat$in_reply_to_status_id_str <- as.character(dat$in_reply_to_status_id_str)
    #names(dat)[11] <-"place_country"
    # checked! two conditions are actually the same  -- (dat$in_reply_to_user_id_str =="25073877")*
    idx <- which( ((dat$in_reply_to_screen_name =="realDonaldTrump") * 
                     (!is.na(dat$in_reply_to_status_id_str))) >0)
    edges <- data.frame( user_id_str = dat$user_id_str[idx], status_id_str = dat$in_reply_to_status_id_str[idx],  stringsAsFactors = F)
    return(edges)
  }
  
  files <- list.files(data_folder)
  n_cores = detectCores()
  cl <- makeCluster(floor(n_cores / 2))
  registerDoParallel(cl)
  edgesall <- foreach (i = 1:length(files),
                          .combine = rbind) %dopar% {
                            filepath <- paste0(data_folder, files[i])
                            edgeList_file(filepath)  }
  stopCluster(cl)
  return(edgesall)
}

library(doParallel)
data_folder1 <-
  "../data/followers_Network/followers_timeline_0_45k_csv/"
data_folder2 <-
  "../data/followers_Network/followers_timeline_45k_76k_csv/"
edgeList <- rbind ( edgeList_folder( data_folder1 ), edgeList_folder (data_folder2) )
write.csv(edgeList, file ="../data/trump_tweets/edgeList_fromFollowers.csv", row.names = F)


#download the tweets
tweets_ids <- unique(edgeList$status_id_str)
getStatuses(tweets_ids, "../data/trump_tweets_fromFollowers.json",oauth_folder ="./credentials/credential_mixed/")

data.str <- readLines("../data/trump_tweets/tweets_fromFollowers.json")
data.json <- paste0('[', paste0(data.str, collapse= ",") , ']')
data.df <- jsonlite::fromJSON(data.json, simplifyDataFrame= T)
data.df <- simplifyTwitterDF(data.df)
#remove screen name is not realDonaldTrump, don't know why they came up ??
data.df <- data.df[which(data.df$user_id_str == "25073877"),]   #2168 /2425
write.csv(data.df, file = "../data/trump_tweets/trump_tweets_fromFollowers.csv", row.names = F)








##################################################################################################################################
#                read the all the retweets about Donald Trump from the Journalism Hadoop
##################################################################################################################################



# collects the tweets from hadoop
# bigmem04,  ~ 10 mins
data_folder = "/p/stat/songwang/trump.retweet.long-nov12/"

tweetsIDs_fromHadoop <- function(data_folder){
  
  files <- list.files(data_folder) #18k files
  interval = 500
  nbreaks = ceiling(length(files)/interval)
  headers = c('tweet_id_str', 'created_at','user_id_str','user_name',
              'user_screen_name','user_description','user_followers_count', 'user_friends_count', 
              'user_verified','geo_type','geo.coordinates','text', 
              'retweet_id_str','retweet_created_at', 'retweet_user_id_str',
              'retweet_user_name','retweet_user_screen_name', 'retweet_user_description',
              'retweet_user_followers_count','retweet_user_friends_count', 'retweet_user_verified',
              'retweet_ugeo_type','retweet_geo_coordinates','retweet_text',
              'user_metion_screen_name')
  
  n_cores = detectCores()
  cl <- makeCluster(floor(n_cores / 2))
  registerDoParallel(cl)
  retweets_ids <- foreach ( i =1 :nbreaks, .combine = c) %dopar% {
    idx_start <- interval*(i-1)+1; idx_end = interval*i
    if (i == nbreaks){ idx_end = length(files) }
    dat <- NULL
    for(j in idx_start:idx_end){
      filepath <- paste0(data_folder, files[j])
      dat <- c(dat, readLines(filepath))
    }
    dat1 <- do.call("rbind", lapply(dat, function(x) unlist(strsplit(x, '\t')))  )
    dat2 <- data.frame(dat1, stringsAsFactors = F)
    colnames(dat2) <- headers
    unique(dat2$retweet_id_str)
  }
  stopCluster(cl)
  return(retweets_ids)
}
retweets_ids <- unique( tweetsIDs_fromHadoop(data_folder))

write(retweets_ids, file = "../data/trump_tweets/tweets_ids_fromHadoop.txt")

edgeList_fromHadoop <- function(data_folder){
  
  files <- list.files(data_folder) #18k files
  interval = 500
  nbreaks = ceiling(length(files)/interval)
  headers = c('tweet_id_str', 'created_at','user_id_str','user_name',
              'user_screen_name','user_description','user_followers_count', 'user_friends_count', 
              'user_verified','geo_type','geo.coordinates','text', 
              'retweet_id_str','retweet_created_at', 'retweet_user_id_str',
              'retweet_user_name','retweet_user_screen_name', 'retweet_user_description',
              'retweet_user_followers_count','retweet_user_friends_count', 'retweet_user_verified',
              'retweet_ugeo_type','retweet_geo_coordinates','retweet_text',
              'user_metion_screen_name')
  
  n_cores = detectCores()
  cl <- makeCluster(floor(n_cores / 2))
  registerDoParallel(cl)
  edges_all<- foreach ( i =1 :nbreaks, .combine = rbind) %dopar% {
    idx_start <- interval*(i-1)+1; idx_end = interval*i
    if (i == nbreaks){ idx_end = length(files) }
    dat <- NULL
    for(j in idx_start:idx_end){
      filepath <- paste0(data_folder, files[j])
      dat <- c(dat, readLines(filepath))
    }
    dat1 <- do.call("rbind", lapply(dat, function(x) unlist(strsplit(x, '\t')))  )
    dat2 <- data.frame(dat1, stringsAsFactors = F)
    colnames(dat2) <- headers
    edges <- data.frame( list(user_id_str = dat2$user_id_str,  status_id_str = dat2$retweet_id_str), stringsAsFactors = F)
    return (edges)
  }
  stopCluster(cl)
  return(edges_all)
}

data_folder = "/p/stat/songwang/trump.retweet.long-nov12/"
edgeList2 <- edgeList_fromHadoop(data_folder)
write.csv(edgeList2, file = "../data/trump_tweets/edgelist_fromHadoop.csv")



tweets_ids <- unique(edgeList2$status_id_str)
getStatuses(tweets_ids, "../data/trump_tweets_fromHadoop.json",oauth_folder ="./credentials/credential_mixed/")
data.df <- jsonlite::fromJSON(data.json, simplifyDataFrame= T)
data.str <- readLines("../data/trump_tweets/tweets_fromHadoop.json")
data.json <- paste0('[', paste0(data.str, collapse= ",") , ']')
data.df <- jsonlite::fromJSON(data.json, simplifyDataFrame= T)
data.df <- simplifyTwitterDF(data.df)
data.df <- data.df[which(data.df$user_id_str == "25073877"),]   #2168 /2425
write.csv(data.df, file = "../data/trump_tweets/trump_tweets_fromHadoop.csv",row.names = F)  #11305/13560/14129



users_ids <- unique(edgesList2$user_id_str)
users.info <- getUsersBatch(ids = users_ids,  output = "../data/trump_tweets/tweets_fromHadoop.json", 
              verbose = T, oauth_folder ="./credentials/credential_mixed/", random = F)
write.csv(users.info, file = "../data/trump_tweets/users_fromHadoop.csv",row.names = F)  #13560/14129   [1] 0.9597282





