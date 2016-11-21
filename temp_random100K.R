
library("smappR")
samp <- read.csv(file ="../data/followers_info/jsons/sample100000.csv", 
header = T, stringsAsFactors = F)
SNs <- read.csv(file = "../data/followers_info/jsons/id_counts.csv", header =T, stringsAsFactors = F)
SN1 <- SNs$screen_name[samp[,2]]  #non-random
SN2 <- SNs$screen_name[samp[,3]]  #random
# samp_sns <- data.frame(list(random = SN1, non_random=SN2) )
# write.csv(samp_sns, file ="../data/followers_info/jsons/sampleSNs100000.csv")
   
my_oauth_folder <- "./credentials/credential_mixed2"
output <- "../data/friends_info/random100K.txt"
conn = file(output, 'w') 
for ( i in 97674:length(SN2)){
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
    writeLines(paste(c(sn,friendIDs), collapse = ','), con = conn) 
    message("i--", i," users have been processed")
}
close(conn)
