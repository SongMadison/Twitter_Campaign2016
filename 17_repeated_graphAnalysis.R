
rm (list =ls ())
library(data.table)


# downloading 1.4 friends
deg_col <- read.csv(file = "../data/followers_Network/createBip-100K-2/friendsIDs-non-random100K.csv",
                 header = TRUE)
deg_col <- as.data.table(deg_col)
deg_col$friends_id_str <- as.character(deg_col$friends_id_str)
id_str1 <- deg_col$friends_id_str

#setkey(deg_col, friends_id_str)

library(smappR)
followers_info <- getUsersBatch(ids = id_str1,                                 
  include_entities = T, verbose = T, 
  oauth_folder = "./credentials/credential_mixed2/",
  output = "../data/followers_Network/createBip-100K-2/friends_info_nonrandom.jsons")


deg_col2 <- read.csv(file = "../data/followers_Network/createBip-100K-2/friendsIDs-random100K.csv",
                    header = TRUE)
deg_col2 <- as.data.table(deg_col2)
deg_col2$friends_id_str <- as.character(deg_col2$friends_id_str)
id_str2 <- deg_col2$friends_id_str

library(smappR)
followers_info <- 
        getUsersBatch(ids = id_str2,                                 
        include_entities = T, verbose = T, 
        oauth_folder = "./credentials/credential_mixed2/",
        output = "../data/followers_Network/createBip-100K-2/friends_info_random.jsons")
