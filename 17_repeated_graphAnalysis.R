rm (list =ls ())
source("Head_file.R")
source("function.R")


samp <- read.csv(file ="../data/followers_info/jsons/sample100k2.csv", 
                 header = T, stringsAsFactors = F)
SNs <- read.csv(file = "../data/followers_info/jsons/id_counts.csv", header =T, stringsAsFactors = F)
SN1 <- SNs$screen_name[samp[,2]]  #random
SN2 <- SNs$screen_name[samp[,3]]  #non-random

my_oauth_folder <- "./credentials/credential_mixed3"
output <- "../data/followers_info/Bip100k2/non-random100K.txt"
conn = file(output, 'w') 
for ( i in 30012:length(SN1)){  #5607, 8840, 17599 00:01/ Oct18
  #i =1 #sn ="DivinemLee"  #1300 friends
  sn = SN1[i]
  friendIDs <- tryCatch(
    {
      getFriends(screen_name=sn, oauth_folder = my_oauth_folder,
                 cursor=-1, user_id=NULL, verbose=TRUE, sleep=1)
    }, error = function(cond){
      message(cond)
      return (NA)
    }, warning = function(cond){
      message(cond)
      return (NA)
    }
  )                    
  write(paste(c(sn,friendIDs), collapse = ','), file  = output, sep = '\n', append = TRUE) 
  message("i--", i," users have been processed")
}
close(conn) # unitl close ,the data will be written. kind of dangerous if collapsed.







############################# second piece ######

rm (list =ls ())
source("Head_file.R")
source("function.R")


samp <- read.csv(file ="../data/followers_info/jsons/sample100k2.csv", 
                 header = T, stringsAsFactors = F)
SNs <- read.csv(file = "../data/followers_info/jsons/id_counts.csv", header =T, stringsAsFactors = F)
SN1 <- SNs$screen_name[samp[,2]]  #random
SN2 <- SNs$screen_name[samp[,3]]  #non-random

my_oauth_folder <- "./credentials/credential_mixed2"
output <- "../data/followers_info/Bip100k2/random100K.txt"
conn = file(output, 'w') 
for ( i in 26241:length(SN2)){
  #i =1 #sn ="DivinemLee"  #1300 friends
  sn = SN2[i]
  friendIDs <- tryCatch(
    {
      getFriends(screen_name=sn, oauth_folder = my_oauth_folder,
                 cursor=-1, user_id=NULL, verbose=TRUE, sleep=1)
    }, error = function(cond){
      message(cond)
      return (NA)
    }, warning = function(cond){
      message(cond)
      return (NA)
    }
  )                    
  writeLines(paste(c(sn,friendIDs), collapse = ','), con = conn) ## accumulate upto certain point; then write out
  message("i--", i," users have been processed")
}
close(conn)











output_folder <- "../data/followers_Network/createBip-100K-2/"

foll_count <- read.csv(file = paste0(output_folder, "friends_follCount-nonrandom.csv"),
                 header = TRUE)
followers_count <- read.csv("../data/followers_info/jsons/id_counts.csv", stringsAsFactors = T)
samp <- read.csv("../data")
foll_count$friends_id_str <- as.character(foll_count$friends_id_str)


edgelist <- read.csv(paste0(output_folder, "edgelist-non-random100K-2.csv"), 
                     stringsAsFactors = F )  #76431, following 1471141 (>=2)
followers <- unique(edgelist$follower_id_str)
edgelist$friends_id_str <- as.character(edgelist$friends_id_str)

library(igraph)
G <- graph_from_edgelist( as.matrix(edgelist) )
A <- get.adjacency(G)
friends_info <- read.csv(paste0(output_folder, "friends_info.csv"), stringsAsFactors = F)


deg1 <- degree(G,mode = "out")
deg2 <- degree(G, mode = "in")

library(smappR)
followers_info <- getUsersBatch(ids = id_str1,                                 
  include_entities = T, verbose = T, 
  oauth_folder = "./credentials/credential_mixed2/",
  output = paste0(output_folder,"friends_info_nonrandom.jsons"))





