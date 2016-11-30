rm(list=ls())
library(twitteR)

library(data.table, quietly = T)
load('../data/credential.RData')

trump_followers_info <- read.csv("../data/trump_followers_all_20000.csv",
                                 stringsAsFactors = F)
# load('follower_samp20000.RData')
# 
# system.time( 
#     trump_followers_info <- rbindlist(lapply(ur.followers,as.data.frame))  )
#     
# follower_ids <- trump_followers_info$id
# write.csv(follower_ids, "./data/trump_top20000_id_only.csv",row.names = F)
# 
# write.csv(trump_followers_info, "./data/trump_followers_all_20000.csv", 
#           row.names = F)

CMDLine = TRUE
if (CMDLine == TRUE){
  
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments) <3) stop("need at least three parameters")
  k <- as.numeric(arguments[1]) # range 1-12
  start_i <- as.integer(arguments[2]) # range 1:4302
  experiment_size <- as.numeric(arguments[3])
  if (length(arguments) <4) {buffer_size <- 50
  }else{buffer_size <- as.integer(arguments[4])  }
  
  
}


if (CMDLine == FALSE){
  k = 3
  ## parameters: min_favorite, buffer_size, experiment_size
  experiment_size = 10
  start_i = 1
  buffer_size  = 5
}


consumer_key <- credential$consumer_key[k]
consumer_secret <- credential$consumer_secret[k]
access_token <- credential$access_token[k]
access_secret <- credential$access_secret[k]

#setup twitter credentials
setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)


##exclude uses having protected account and friends <2 
# to save time or ratelimit to deal with what we are interested.
idx <- which(trump_followers_info$protected != TRUE 
             & trump_followers_info$favoritesCount >5 )
SNs <- trump_followers_info[idx,]$screenName



cat("k=",k,'\n')
# in some cases, the account no longer exist any more, tryCatch can avoid that 
# kind of problem
# protected can be removed first
download_favorites <- function(id_user){
  tryCatch(
{ 
  res <- favorites(user = id_user, n = 200, retryOnRateLimit = 20)
  message( paste("\n user", id_user, "has been processed sucessfully \n"))
  return (res)
  
},error = function(cond){
  message(cond)
  cat("\n user", id_user, "has been processed without success! \n")
  return (list())
},warning = function(cond){
  message(cond)
  message( paste("\n user", id_user, "has been processed without success!! \n"))
  return (list())
},finally = {
  #message( paste("\n user", id_user, "has been processed \n"))
}
  ) 
}


i = start_i

lst_favorites = list()
while ( i <=  experiment_size){
  cat("starting to work on i=", i ,"\n")
  ratelimit <- getCurRateLimitInfo()
  limit1 <- ratelimit[ratelimit$resource == "/favorites/list",3]
  limit2 <- ratelimit[ratelimit$resource == "/application/rate_limit_status",3]
  cat("remaining rate:'/favorites/list'; /application/rate_limit_status: ", limit1,limit2,'\n')
  while(min(as.numeric(ratelimit$remaining)) < 2){
    #all the limits not satisfied
    ratelimit <- getCurRateLimitInfo() 
    cat("rate limit exceed, retry in one minute! \n")
    Sys.sleep(60)
  }
  id_user = SNs[i]
  lst_favorites[[id_user]] <- download_favorites(id_user)
  
  
  ###### OUTPUT  FILE----
  if (i %% buffer_size ==0 || i == experiment_size){
    save( lst_favorites, file = paste0("../data/favorites/fravorite50_",
                                    ceiling(i/buffer_size),"_temp.RData") )
    lst_favorites = list()
  }
  i = i+1
}
