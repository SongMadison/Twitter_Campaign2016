

#construct the 
library(twitteR)
library(Matrix)
library(data.table)
load('credential.RData')

setwd("/afs/cs.wisc.edu/u/s/o/songwang/Stat/Twitter_Campaign2016/code")

#load('follower_samp20000.RData')
# trump followers features:
#trump.followers.info <- rbindlist(lapply(ur.followers,as.data.frame))
#write.csv(trump_followers_knfo, file=paste0("./data/trump_timeline_",
#     Sys.Date(),".csv"))

df_trump_timeline <- read.csv("../data/trump_timeline/trump_timeline.csv", 
                              stringsAsFactors = F)
twitterIDs <- df_trump_timeline$id  # status
str(twitterIDs) # intergers
users_favorites_folder <- "../data/favorites/"

adj <- create_LikeBip(twitterIDs, users_favorites_folder)

table(colSums(adj)) #1690/1939 are 0


create_LikeBip <- function(twitterIDs, users_favorites_folder){
  #create edge list
  i_set = NULL
  j_set = NULL
  files = list.files(users_favorites_folder)
  sn_combined = NULL
  nsize = 0
  for (i in 1:length(files)){
    filename <- files[i]
    favor_k <- read.csv(paste0(users_favorites_folder,filename), 
                        stringsAsFactors = F)
    sn_k <- unique(favor_k$follower_sn)
    for (j in 1:length(sn_k)){
      id_j <-  favor_k[favor_k$follower_sn == sn_k[j],]$id
      i_j <- match(id_j, twitterIDs)
      i_j <- i_j[which(i_j !='NA')]
      i_set <- c(i_set,i_j)
      j_set <- c(j_set, rep(nsize+j, length(i_j)))  # there are nsize users ahead
    }
    sn_combined <- c(sn_combined,sn_k)
    nsize = length(sn_combined)
  }
  
  adj <- sparseMatrix(i = i_set, j = j_set, x = rep(1,length(i_set)), 
                              dims =c(length(twitterIDs), length(sn_combined)))
  return(adj)
}







