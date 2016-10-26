library(jsonlite)
source("function.R")

##data are in non-random1000



friends.json <- readLines("../data/friends_info/non-random1000/friends_info.json")
#large list 126698 items,
N <- length(friends.json)
cat("N=", N, "\n")
friends.df <- fromJSON(myToJSON(friends.json))  #FALSE: 12  TRUE : 126686
# list of 126686 items, each item is an observation
nrow(friends.df)
length(friends.df)
#convert to a .csv
cols <- c('id_str','screen_name','name','description','followers_count'
          ,'statuses_count', 'friends_count', 'created_at', 'location', 'lang'
          , 'time_zone', 'id_str', 'created_at', 'text')
var <- cols[1]
a <- sapply(friends.df, function(x) unlist(x)[var]) # named array
names(a) <- NULL
friendsInfo <- friends.df[,c('id_str','screen_name','name','description','followers_count'
                            ,'statuses_count', 'friends_count', 'created_at', 'location', 'lang'
                            , 'time_zone',  'created_at', 'status')]
#write.csv(friendsInfo, file = "../data/friends_info/non-random1000/friends_info.csv")
id_str <- unlist(lapply(friends.df, "[[",'id_str'))
screen_name <- unlist(lapply(friends.df, "[[", 'screen_name'))
friends_id_sn <- data.frame(list ( id_str = id_str,
                                     screen_name = screen_name))
write.csv(friends_id_sn, 
          file = "../data/friends_info/non-random1000/friends_id_sn.csv", 
          row.names = F)

#id_list[84582][[1]] NULL

## fowlloers info
followers.json <-  readLines("../data/friends_info/non-random1000/followers_info.json")
N <- length(followers.json)
cat("N=", N, "\n")
followers.df <- fromJSON(myToJSON(followers.json))   ## list of vriables
followersInfo <- data.frame(followers.df[,c('id_str','screen_name','name','description','followers_count'
                             ,'statuses_count', 'friends_count', 'created_at', 'location', 'lang'
                             , 'time_zone', 'created_at', 'status')]) # very complidated, not a table
followers_id_sn <- data.frame(list ( id_str = unlist(followers.df['id_str'])
,screen_name = unlist(followers.df['screen_name'])   ))
                                     
write.csv(followers_id_sn, 
          file = "../data/friends_info/non-random1000/followers_id_sn.csv", 
          row.names = F)



#random sample -- toy example
set.seed(123) 
sampIds <- sample(N, 1000)
friends_samp <- friends.json[sampIds]
friends_samp.js <- myToJSON(friends_samp)  #convert to json string
friends_samp.df <- fromJSON(friends_samp.js)
dim(friends_samp.df)



####################################################################################################
# construct the bipartite graph:
friendList <- readLines("../data/friends_info/non-random1000/non-random1000.txt")
followers_SN <- sapply(friendList,  function(x)  {
               a <- unlist(strsplit(x, split = ","))
               return (a[1]) } )
names(followers_SN) <- NULL
## all the followers consiered, out of this 1000, 980 are returned.


friends_Count <- sapply(friendList, function(x) length(unlist(strsplit(x, split = ",")))-1)
names(friends_Count) <- NULL
friends_ID_list <- lapply(friendList, 
                    function(x) {
                      a <- unlist(strsplit(x, split = ","))
                      return(a[-1])
                    } )
friends_ID <-   unlist(friends_ID_list)
length(friends_ID) #7508494 
sum(friends_ID =='NA') ##69 NA, 69/1000 users, their friends not downloaded correctly

freq <- table(friends_ID)  # in R, sort and count, little slow
length(freq)  #5423593
IDs <- names(freq)      #5423593  
freq[which(IDs == 'NA')] #expected 20, in fact 69
IDs <- IDs[which(freq >= 5)]  #126812
IDs <- IDs[-which(IDs == 'NA')]
friends_ID <- IDs

# this set contains about 126811 items -- id_str , is the set we maintained.

#the id_str for the followerSNs: based on the followers_info.csv
followers_id_sn <- read.csv('../data/friends_info/non-random1000/followers_id_sn.csv', 
                           stringsAsFactors = F)
### remove those followers in bipartite graph, but not in followers
row_id_missing <- which(is.na(match(followers_SN, followers_id_sn$screen_name)))
# id/ sn for the rest of 977 followers, order by the terms in barpartite graph
id1 <- as.character(followers_id_sn$id_str[match(followers_SN, followers_id_sn$screen_name)])
id1 <- id1[!is.na(id1)] #remove NA
#977
length(id1)
SN1 <- followers_SN[!is.na(match(followers_SN, followers_id_sn$screen_name))]


#users info:
library(data.table)
friends_id_sn <- read.csv('../data/friends_info/non-random1000/friends_id_sn.csv', 
                          stringsAsFactors = F ) # 126685 rows
friends_id_sn$id_str <- as.character(friends_id_sn$id_str)
col_id_missing <- which(is.na(match(friends_ID, friends_id_sn$id_str))) #398 missing
# id/ sn for the rest of 977 followers, order by the terms in barpartite graph
id2 <- as.character(friends_id_sn$id_str[!is.na(
       match(friends_ID, friends_id_sn$id_str))]) #remove NA
#977
length(id2)
SN2 <- friends_id_sn$screen_name[!is.na(match(friends_ID, friends_id_sn$id_str))]
length(SN2)



ids <- c(id1, id2[is.na(match(id2, id1))])

SNs <- c(SN1, SN2[is.na(match(SN2, SN1))])

i_set = c()
j_set = c()
friends_ID_list2 <- friends_ID_list[-row_id_missing]
for(i in 1:length(friends_ID_list2) ){
  friends_i <- unlist(friends_ID_list2[i]) ## some of them are low frequent users, deleted
  friends_i <- friends_i[!is.na(match(friends_i, ids))]
  i_set <- c(i_set, rep(i, rep(length(friends_i))))
  j_set <- c(j_set, match(friends_i, ids))
  cat("i=", i, "  size: ", length(friends_i), '\n')
}
  
library(Matrix)
adj <- sparseMatrix(i = i_set, j = j_set, x = rep(1,length(i_set)), 
                    dims =c(length(ids), length(ids)))
rownames(adj) <- SNs
colnames(adj) <- SNs
col_deg <- colSums(adj) 
quantile(col_deg, probs = seq(0,1,0.01))



#collected more followers_info, 3 more.
my_oauth_folder ="./credentials/credential_mixed/"
list.files(my_oauth_folder)
#[1] "my_oauth13" "my_oauth14" "my_oauth15"
userIds = followers_SN[is.na(match(followers_SN, followers_id_sn$screen_name))]
userInfo <- getUsersBatch(screen_names = userIds, oauth_folder = my_oauth_folder,
              include_entities = TRUE, verbose = TRUE,
              output = paste0("../data/friends_info/non-random1000/follower_info.json"))
# 1--23 users left
# > length(userInfo)
# [1] 14
# > nrow(userInfo)
# [1] 3
# username changed!, captital letters
userInfo$screen_name
# [1] "MeetThePress" "quantyko"     "iudaismus" 

userIds
# [1] "meetthepress"    "lcwholesalehome" "Belen174"        "dessi_oficial"  
# [5] "NwakalambaJT"    "TomAdelsbach"    "LmsAnthony"      "frankarcuri"    
# [9] "dmendesw"        "goldbloc"        "ImCJY95"         "Quantyko"       
# [13] "radziholmes_"    "pirtle_quintin"  "laura_seidel"    "seekacounsel"   
# [17] "maradona365"     "hghofhv"         "Exaggeratory"    "DanHRothschild" 
# [21] "amir__Yemen"     "Iudaismus"       "_ejmt_" 
# 

