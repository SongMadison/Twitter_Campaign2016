rm(list =ls ())
source("function.R")

library("smappR")
samp <- read.csv(file ="../data/followers_info/jsons/sample100000.csv", 
                 header = T, stringsAsFactors = F)
## indices, for random and non-random

SNs <- read.csv(file = "../data/followers_info/jsons/id_counts.csv", colClasses = c("character"),
                header = T, stringsAsFactors = F)
SN1 <- SNs$screen_name[samp[,2]]  #non-random
SN2 <- SNs$screen_name[samp[,3]]  #random
# samp_sns <- data.frame(list(random = SN1, non_random=SN2) )
# write.csv(samp_sns, file ="../data/followers_info/jsons/sampleSNs100000
   
my_oauth_folder <- "./credentials/credential_mixed"
output <- "../data/friends_info/non-random100K.txt"

### start on Feb 12, 2017, hope to finish on March 15
# total 5.188 millison 
library("smappR")
SNs <- read.csv(file = "id_counts.csv", colClasses = c("character"),
                header = T, stringsAsFactors = F)
set.seed(100)
samp_ids <- sample(nrow(SNs), 1000000)
SN1 <- SNs$screen_name[samp_ids]  
# samp_sns <- data.frame(list(random = SN1, non_random=SN2) )
# write.csv(samp_sns, file ="../data/followers_info/jsons/sampleSNs100000

my_oauth_folder <- "./credentials/credential_mixed"
output <- paste0("total_1000K_", Sys.Date(),".txt")
conn = file(output, 'w') 
for ( i in 1:length(SN1)){  
  #5607, 8840, 17599 00:01/ Oct18
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
  write(paste(c(sn,friendIDs), collapse = ','), file  = output, append= TRUE, sep = '\n')
  message("i--", i," users have been processed")
}
close(conn) # unitl close ,the data will be written. kind of dangerous if
