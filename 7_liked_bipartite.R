
rm(list=ls())
library(twitteR)
library(data.table)
library(Matrix)

load('credential.RData')
#load('follower_samp20000.RData')
# trump followers features:
#trump.followers.info <- rbindlist(lapply(ur.followers,as.data.frame))
#write.csv(trump_followers_info, file=paste0("./data/trump_timeline_",Sys.Date(),".csv"))
df_trump_timeline <- read.csv("./data/trump_timeline_2016-09-27.csv")
id_trump_timeline <- df_trump_timeline$id  # status


#create edge list
i_set = NULL
j_set = NULL
files = list.files("./data/favorites/")
sn_combined = NULL
for (filename in files){
    favor_k <- read.csv(paste0("./data/favorites/",filename), stringsAsFactors = F)
    sn_k <- unique(favor_k$follower_sn)
    for (j in 1:length(sn_k)){
        id_j <-  favor_k[favor_k$follower_sn == sn_k[j],]$id
        i_j <- match(id_j, id_trump_timeline)
        i_j <- i_j[which(i_j !='NA')]
        i_set <- c(i_set,i_j)
        j_set <- c(j_set, rep(j, length(i_j)))
    }
    sn_combined <- c(sn_combined,sn_k)
}


adj <- sparseMatrix(i = i_set, j = j_set, x = rep(1,length(i_set)), 
                    dims =c(length(id_trump_timeline), length(sn_combined)))
                   

save(adj, file= "liked_adj.RData")
