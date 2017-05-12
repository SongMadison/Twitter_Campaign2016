
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
  edges_all  <- 
    foreach ( i =1 :nbreaks, 
              .combine = rbind) %dopar% {
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
#getStatuses(tweets_ids, "../data/trump_tweets/tweets_fromHadoop.json",oauth_folder ="./credentials/credential_mixed/")

data.str <- readLines("../data/trump_tweets/tweets_fromHadoop.json")
data.json <- paste0('[', paste0(data.str, collapse= ",") , ']')
data.df <- jsonlite::fromJSON(data.json, simplifyDataFrame= T)
data.df <- simplifyTwitterDF(data.df)
data.df <- data.df[which(data.df$user_id_str == "25073877"),]   #2168 /2425
write.csv(data.df, file = "../data/trump_tweets/tweets_fromHadoop.csv",row.names = F)  #11305/13560/14129



users_ids <- unique(edgesList2$user_id_str)
users.info <- getUsersBatch(ids = users_ids,  output = "../data/trump_tweets/tweets_fromHadoop.json", 
                            verbose = T, oauth_folder ="./credentials/credential_mixed/", random = F)
write.csv(users.info, file = "../data/trump_tweets/users_fromHadoop.csv",row.names = F)  #13560/14129   [1] 0.9597282



