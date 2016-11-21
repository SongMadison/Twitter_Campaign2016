
library("smappR")
samp <- read.csv(file ="../data/followers_info/jsons/sample100000.csv", 
header = T, stringsAsFactors = F)
SNs <- read.csv(file = "../data/followers_info/jsons/id_counts.csv", header =T, stringsAsFactors = F)
SN1 <- SNs$screen_name[samp[,2]]  #non-random
SN2 <- SNs$screen_name[samp[,3]]  #random
# samp_sns <- data.frame(list(random = SN1, non_random=SN2) )
# write.csv(samp_sns, file ="../data/followers_info/jsons/sampleSNs100000.csv")
   
my_oauth_folder <- "./credentials/credential_mixed3"
output <- "../data/friends_info/non-random100K.txt"
conn = file(output, 'w') 
for ( i in 95496 :length(SN1)){  #5607, 8840, 17599 00:01/ Oct18
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
