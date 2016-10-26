



#construct the 
library(twitteR)
library(Matrix)
library(data.table)
load('credential.RData')
setwd("/afs/cs.wisc.edu/u/s/o/songwang/Stat/Twitter_Campaign2016/code")


#followers_friends_folder
create_edgelist <-function(friends_files, verbose = TRUE){
  
  #friends_files <- friends_files_samp
  
  #   sn_combined <- c()
  #   i_set <- c()
  #   j_set <-c()
  lst_followed <- list()
  length1 <- length(lst_followed)
  #combine the data, into a single list of screenNames
  for(i in 1:length(friends_files)){
    #i=1
    load(friends_files[i]) 
    #containing lst_friends in the file.
    # list[ list ] - the names of the lists are user_i's
    # in the inner-list will be twitteR user-object
    
    sn_i <- unlist(names(lst_friends)) #
    for ( j in 1:length(sn_i)){
      #j= 1
      sn_ij = sn_i[j]
      lst1 <-lst_friends[[j]] # list of friends of user i
      friends_df <- do.call("rbind",lapply(unlist(lst1), as.data.frame))
      lst_followed[[sn_ij]] <- friends_df$screenName
    } 
    message(i,"-th data set ", length(lst_followed)-length1, "  followers are inserted!")
  }
  return(lst_followed)
}

edgelistToadj <- function(lst_followed){
  xx <- names(lst_followed)
  yy <- unique(unlist(lst_followed))
  dup <- match(yy,xx)
  yy <- yy[is.na(dup)]  # keep non duplicates
  zz <- c(xx, yy)
  ii <- match(xx, zz)
  i_set <- rep(ii, sapply(lst_followed, length))
  j_set <- match(unlist(lst_followed), zz)
  adj <- sparseMatrix(i = i_set, j = j_set, x = rep(1,length(i_set)), 
                      dims =c(length(zz), length(zz)))
  rownames(adj) <- zz
  return(adj)
}



friends_folder <- "../data/friends_info/random1000/"
#followers_files <- list.files(followers_folder)

friends_files <- list.files(friends_folder, full.names = T)

edgelist <- create_edgelist(friends_files)
adj <- edgelistToadj(edgelist)
save(edgelist,adj, file ="../data/friends_info/edgelist_adj_random1000")
#followers <- read.csv("../data/followers_info/trump_followers_all_20000.csv", 
#stringsAsFactors = F)



friends_folder <- "../data/friends_info/batch50/"
#followers_files <- list.files(followers_folder)

friends_files <- list.files(friends_folder, full.names = T)

edgelist <- create_edgelist(friends_files)
adj <- edgelistToadj(edgelist)
save(edgelist,adj, file ="../data/friends_info/edgelist_adj_batch50")

# combined list 

