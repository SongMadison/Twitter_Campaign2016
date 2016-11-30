# library(devtools)
# install_github(repo = "mongosoup/rmongodb")
# install_github("SMAPPNYU/smappR") 

library(smappR)

myfollowers <- getFollowers("DrDavidDuke", oauth_folder = "./credentials/credential_mixed3")


#trump_followers <- getFollowers("realDonaldTrump", oauth_folder = "~/Dropbox/credentials", sleep = 30)

followers_info <- getUsersBatch(ids = myfollowers, 
              oauth_folder = "./credentials/credential_mixed3", 
              include_entities = TRUE, verbose = TRUE, 
              output = "../data/davidduke/davidduke.json")





# download the top 200 twitters in the timeline for a set of users with screen_name in name1
#dir.create("../data/davidduke")
output_folder <- "../data/davidduke/"
name1 <- followers_info$screen_name
#n = length(name1)
for (i in 1 : length(name1)){
  #i = 1
  file_name <- paste0(output_folder, name1[i],".json")
  tryCatch(
    getTimeline(filename = file_name,n = 200, screen_name = name1[i],   
                oauth_folder = "./credentials/credential_mixed3",
                sleep = 0.5, verbose = TRUE), 
    error = function(e){
       #          message( paste(name1[i], "error occurred"))  
    }) 
  print(paste("XXXXXX -- i = ", i ,' done \n'))   
}




# for each user's screen_name in a set, download the friends list and write them as a string vectors into a file


SN1 <- name1  ## a vector with screen_names of users

my_oauth_folder <- "./credentials/credential_mixed2"
output <- "../data/davidduke/friends_list.txt"
conn = file(output, 'w') 
for ( i in 1:length(SN1)){  
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
})                    
write(paste(c(sn,friendIDs), collapse = ','), file  = output, sep = '\n', append = TRUE) 
message("i--", i," users have been processed")
}
close(conn) 



