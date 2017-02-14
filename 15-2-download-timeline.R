
rm(list =ls() )
source('Head_file.R') #import libraries, self-defined functions


load("../data/followers_Network/data.RData")
## containing A 
dim(A)  #75938 365684

name1 <- rownames(A) #75938
# name2 <- colnames(A)
# sn_description <- fread("../data/followers_info/jsons/sn_descriptions.csv",
#                         colClasses =c("integer","character","character"))
# 
# setkey(sn_description, screen_name)
# followers <- sn_description[name1]

# the code run are Nove 18, obtained 75416x14 data.frame. 
# # note total name1 has 75938
# library(smappR)
# followers_info <- getUsersBatch( 
#   screen_names = name1,                                
#   include_entities = T, 
#   oauth_folder = "./credentials/credential_mixed2/")
# write.csv(followers_info, file = "../data/followers_Network/followers_info.csv", row.names = F)

followers_info <- fread("../data/followers_Network/followers_info.csv",
                        stringsAsFactors = F)
#some user are not available,  75416    14
## decription analysis


## top200 twitters from each follower
output_folder <- "../data/followers_Network/followers_timeline12_3200/"
if (! file.exists( output_folder )) dir.create( output_folder)
Err_users <- NULL
n = length(name1)
for (i in 45001:n){
  #i = 1
  file_name <- paste0(output_folder, name1[i],".json")
  tryCatch(
    getTimeline(filename = file_name, n =3200, screen_name = name1[i],
                oauth_folder = "./credentials/credential_mixed",
                sleep = 0.5, verbose = TRUE), 
    error = function(e){
      Err_users <- c(Err_users, name1[i])
      message( paste(name1[i], "error occurred"))
    }
  )
  Err_users <- cbind(Err_users,name1[i])
  print(paste("XXXXXX -- i = ", i ,'\n'))   
}




output_folder <- "../data/followers_Network/followers_timeline1_3200/"
if (!file.exists( output_folder )) dir.create( output_folder)
Err_users <- NULL
n = length(name1)
for (i in 1129:45000){
  #i = 1
  file_name <- paste0(output_folder, name1[i],".json")
  tryCatch(
    getTimeline(filename = file_name, n =3200, screen_name = name1[i],
                oauth_folder = "./credentials/credential_mixed1",
                sleep = 0.5, verbose = TRUE), 
    error = function(e){
      Err_users <- c(Err_users, name1[i])
      message( paste(name1[i], "error occurred"))
    }
  )
  Err_users <- cbind(Err_users,name1[i])
  print(paste("XXXXXX -- i = ", i ,'\n'))   
}








