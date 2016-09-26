rm(list=ls())
library(twitteR)
library(data.table)
load('credential.RData')
load('follower_samp20000.RData')


#setup twitter credentials
setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)


# trump followers features:
trump.followers.info <- rbindlist(lapply(ur.followers,as.data.frame))
prorectedIdx<- which(trump.followers.info$protected)

download_favorite_twittes <- function( sn ){
    tryCatch(
        {
             favorites(user = user_obj$screenName, n= 200, retryOnRateLimit = 15)
            
        },error = function(cond){
            message(cond)
            return (NA)
        },warning = function(cond){
            message(cond)
            return (NA)
        },finally = {
            message( paste("user", sn, "has been processed sucessfully"))
        }
    ) 
}


p1 <- proc.time()
favor_twittes <- list()
for ( i in 1:1500){
    user_obj <- trump.followers.info[i,]
    if (user_obj$protected){
        favor_twittes[[user_obj$id]] <- list()
    }else{
        favor_twittes[[user_obj$id]] <- download_favorite_twittes( user_obj$screenName )
    }
    cat("i=", i, "\n")
}
proc.time() - p1

sapply(favor_twittes,length)
which(favor_twittes == 'NA')


save(favor_twittes, file = "favaor_twites_1500.RData")



