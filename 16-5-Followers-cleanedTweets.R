rm(list = ls ())
source("Head_file.R")
source("function.R")
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


Tweets_folder <- function(data_folder){
  
  # individual file
  getTrumpTweets <- function(filepath, target_user_id_str = NULL){
    #target_user_id_str ="25073877"
    dat <- read.csv(filepath, colClasses = c("character"), stringsAsFactors = F)
    if (! is.null(target_user_id_str)){
      idx1 <- which( ((dat$in_reply_to_user_id_str == target_user_id_str) * 
                        (!is.na(dat$in_reply_to_status_id_str))) >0)      #reply, or mention
      idx2 <- which(( (  dat$quoted_status_user_id_str == target_user_id_str ) * 
                        (!is.na(dat$quoted_status_id_str))) >0)   # quoted + comments
      idx3 <- which(( (  dat$retweet_status_user_id_str == target_user_id_str ) * 
                        (!is.na(dat$retweet_status_id_str))) >0)  # retweet + no comments
      idx <- unique(c(idx1, idx2, idx3))
      tweets<- dat[idx,]
      return(tweets)
    }else{
      return (dat)
    }
  }
  
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
tweets1 <-   Tweets_folder( "../data/followers_Network/followers_timeline_0_45k_csv/")
tweets2 <-   Tweets_folder( "../data/followers_Network/followers_timeline_45k_76k_csv/")
tweets <- rbind(tweets1, tweets2)
ids_unique <- unique(tweets$id_str);   length(ids_unique)
#tweets <- tweets[match(ids_unique, tweets$id_str),]  ## match function will return the first index

tweets <- tweets[tweets$created_at < '2016-11-09 00:00:00',]
tweets <- tweets[tweets$created_at > '2015-01-01 00:00:00',]
#write.csv(tweets, file ="../data/trump_tweets/tweets_fromFollowers_Trump.txt", row.names = F) 
#59905    23
write.csv(tweets, file = "../data/trump_tweets/followers_tweets/alltweets-followers.csv", 
          row.names = F)  ## 59904 obs

tweets <- read.csv("../data/followers_Network/tweets_feb06.csv", stringsAsFactors = F, colClasses = c("character"))
merged_texts <- function(tweets){
  user_id_str = unique(tweets$user_id_str)
  merged_text = NULL
  for (i in 1:length(user_id_str)){
    merged_text <- c(merged_text, paste0(tweets$text[which(tweets$screen_name == screenNames[i])], collapse = ' '))
  }
  return( data.frame(user_id_str = user_id_str, merged_text = merged_text))
}
merged_texts_by_screenName <- merged_texts(tweets)
write.csv(merged_texts_by_screenName, 
          file = "../data/trump_tweets/followers_tweets/mergedtweets-followers-recent1200.csv", 
          row.names = F)  ## 59904 obs
