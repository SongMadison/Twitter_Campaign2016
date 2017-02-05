
rm(list = ls())

library(igraph)
library(Matrix)
library(irlba)
library(ggplot2)
library(data.table)
source("function.R")



######
# followers info
ID_SNs <- fread("../data/followers_info/jsons/id_counts.csv", 
                colClasses = c("integer", "integer", "character","character","character") )
setkey(ID_SNs, screen_name);  
head(ID_SNs)  #four have no screen names, 
# V1 followers_count                    id_str protected     screen_name
# 1:  428142               6           80621196     False                
# 2: 2113225               7           76264693     False                
# 3: 2810437               3           72081729     False                
# 4: 5081296               0           82450642     False                
# 5: 3459414             108 728395348515934208     False  0000000000000w
# 6: 3727564              42 745317325914705924     False 000000000000hpo
# 

sn_description <- fread("../data/followers_info/jsons/sn_descriptions.csv", 
                        colClasses = c("character","character"), stringsAsFactors = F)


# index, followers_count, id_str, protected, screen_name
# friends info
friends_ID_SN <- fread('../data/followers_Network/friends_ID_SN_nonrandom.csv',
                       colClasses = c("integer", "integer", "character","character"))

setkey(friends_ID_SN, screen_name)
#index, followers_count    id_str     screen_name


  


edgelist = fread("../data/followers_Network/edgelist-non-random100K.csv",
                 colClasses = c( "character","character") )
followerIds = unique(edgelist$followers_id_str)
friendIds = unique(edgelist$friends_id_str)

ID_SN1 <- ID_SNs[match(followerIds, ID_SNs$id_str),]
sn_description1 <- sn_decription[match(ID_SN1$screen_name, sn_description$screen_name), ]
followers_info <- data.frame(ID_SN1, description = sn_description1$description)
names(followers_info )
# [1] "V1"              "followers_count" "id_str"          "protected"       "screen_name"    
# [6] "description"
write.csv(followers_info[,c(3,5,6,2,4)], file ="../data/followers_Network/followers_info.csv", row.names = F )



idx <- match(edgelist$friends_id_str, friends_ID_SN$id_str)
edgelist <- edgelist[ which(!is.na(idx)),]  ##10105635
friends_ID_SN <- friends_ID_SN[match(unique(edgelist$friends_id_str), friends_ID_SN$id_str), ]


rn <- followers_info$screen_name; cn <- friends_ID_SN$screen_name
i_set <- match(edgelist$followers_id_str, followers_info$id_str); 
j_set <- match(edgelist$friends_id_str, friends_ID_SN$id_str)
A <-  sparseMatrix(i = i_set , j = j_set, dimnames = list(rn, cn))
dim(A)
Dc <-colSums(A); min(Dc)

fcount <- friends_ID_SN$followers_count
names(fcount) <- cn

save(edgelist, followers_info, friends_ID_SN, A, fcount, 
     file = "../data/followers_Network/following.RData")














