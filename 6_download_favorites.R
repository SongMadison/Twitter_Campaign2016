rm(list=ls())
library(twitteR)
library(data.table)
load('credential.RData')


load('follower_samp20000.RData')

system.time( 
    trump_followers_info <- rbindlist(lapply(ur.followers,as.data.frame))  )
    
follower_ids <- trump_followers_info$id
write.csv(follower_ids, "./data/trump_top20000_id_only.csv",row.names = F)

write.csv(trump_followers_info, "./data/trump_followers_all_20000.csv", 
          row.names = F)

#setup twitter credentials
setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)

# download favorite twittes up to 200; handles error or warning
## input : id/ screenName of the user
## ouput : the list of twitter objects he/shey liked
download_favorite_twittes <- function( id_user ){
    tryCatch(
        {
            favorites(user = id_user, n= 200, retryOnRateLimit = 15)
            
        },error = function(cond){
            message(cond)
            return (NA)
        },warning = function(cond){
            message(cond)
            return (NA)
        },finally = {
            message( paste("user", id_user, "has been processed sucessfully"))
        }
    ) 
}

# distribution of favorite counts of users in this sample
sum(trump_followers_info$favoritesCount == 0)
dist <- table( trump_followers_info$favoritesCount )
plot(log10( as.numeric(names(dist+1))),log10(dist),
     xlab = "log10 (count+1)", ylab = 'log10 frequency of the count',
     ylim = c(-0.5,4)
     )
p1 <- proc.time()




## parameters: min_favorite, buffer_size, experiment_size
min_favorite = 5
buffer_size  = 5
experiment_size = 15  
## focus on people liked more than 5 twitters before: 
# remove protected account, too
# screen names, easy to check online

row_ids <- which(trump_followers_info$favoritesCount >= min_favorite)
row_ids <- row_ids[which(trump_followers_info[row_ids,]$protected != TRUE)]
SN5  <-  trump_followers_info$screenName[row_ids]
SN5_counts <- trump_followers_info$favoritesCount[row_ids] 
## may not as accurate, changes with time
id5  <- trump_followers_info$id[row_ids]


## download the favorites and save them every buffer_size users are done.
# in this case, I can do something with it, while the code is still running
i = 1
lst_favorite <- list()


while( i <= experiment_size){
    lst_favorite[[ SN5[i] ]] <- download_favorite_twittes( SN5[i] )
    i = i+1      
    if ( (i-1) %% buffer_size == 0 || (i-1) == experiment_size) {
        # write the previous buffer_size items as csv
        df_favorite <- rbindlist(lapply(unlist(lst_favorite), as.data.frame) )
        real_size = length(lst_favorite)
        follower_sn <- rep( x = SN5[(i - real_size) : ( i - 1 )], 
                           sapply(lst_favorite,length))
        df_favorite$follower_sn <- follower_sn
        follower_id <- rep(id5[(i-real_size):(i-1)], 
                           sapply(lst_favorite,length))
        df_favorite$follower_id <- follower_id
        
        write.csv(df_favorite,
                  file = paste0("./data/favorites/favorite_",
                                ceiling((i-1)/buffer_size),".csv"), row.names = F ) 
        cat ("i = ", i , "\n")
        # start another k items
        lst_favorite = list()
    }
}





