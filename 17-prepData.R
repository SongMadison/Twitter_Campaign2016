
rm (list =ls ())
library(Matrix)
library(data.table)
ResultPath <- "../data/friends_info/edgelist_Feb27/results/"
DataPath <- "../data/friends_info/edgelist_Feb27/"

#Note
#cannog use fread - cuz unbalanced quotation in decription column
# \\\\\\"Or you can just call me 'uMalume', your uncle" ishuu!./// IG & SC - @butter__moon



cleanGraph <- function(edgelist_csv_path, friends_csv_path, followers_csv_path, 
                       RData_path, min.followers = 10, min.friends.total =1000){
  
  el <- fread(input = edgelist_csv_path, colClasses = c("character"))
  names(el) <-c("followers_sn", "friends_id_str")
  friends_info <- read.csv(friends_csv_path, 
                           colClasses = c("character"))
  friends_info$followers_count <- as.numeric(friends_info$followers_count)
  
  followers_info <- read.csv(followers_csv_path,
                             colClasses = c("character"))
  followers_info$followers_count <- as.numeric(followers_info$followers_count)
  
  # sparse A
  followers_sn <- unique(el$followers_sn)
  i_set <- match(el$followers_sn, followers_sn)
  friends_id_str <- unique(el$friends_id_str)
  j_set <- match(el$friends_id_str, friends_id_str)
  
  A <- sparseMatrix(i = i_set, j = j_set, x= rep(1, length(i_set)))
  A@x <-rep(1,length(A@x))   #duplicate edges
  rownames(A) <- followers_sn
  colnames(A) <- friends_id_str
  dr <- rowSums(A); dc <- colSums(A)
  nr = nrow(A); nc = ncol(A)
  
  #remove some rows, some of them col info are not available
  idx1 <- match(tolower(rownames(A)), tolower(followers_info$screen_name))
  sum(is.na(idx1))  
  if (sum(is.na(idx1)) >0 ){ 
    A <- A[!is.na(idx1),]
    followers_info <- followers_info[idx1[!is.na(idx1)],]
  }
    
  #remove some columns, some of them col info are not available
  idx2 <- match(colnames(A), friends_info$id_str)
  sum(is.na(idx2))   
  if ( sum(is.na(idx2)) ){ 
    A <- A[,!is.na(idx2)]
    friends_info <- friends_info[idx2[!is.na(idx2)],]
  }
  colnames(A) <- friends_info$screen_name   #change to column names from id_str to screen_names
  
  #further cleanning, remove those followers having min.friends.total followers
  idx3 <- which(friends_info$followers_count < min.friends.total)  #10000
  if (length(idx3) >0 ){  #single index vector not allowd > about 1.6M
    A <- A[,-idx3]
    friends_info <- friends_info[-idx3,]
  }
    
  #exclude some more rows, having fewer 10 friends  -255 such followers
  dr <- rowSums(A); dc <- colSums(A)
  sum(dr<min.followers)
  #[1] 255
  idx4 <- which(dr < min.followers)
  if(length(idx4)>0){
    A <- A[-idx4,]
    followers_info <- followers_info[-idx4,]
  }
  message("after cleaning, nrow from ", nr, " -> ",nrow(A), "; ncol: from ", nc, ' -> ', ncol(A),'\n nedge: from ', 
          nrow(el), sum(A))
  
  message("starting to save the results ....")
  save(A, friends_info, followers_info, file =RData_path)
}

# readin edgelist, frriends totally followers, and ouput A.RData in the RData folder
edgelist_csv_path = paste0(DataPath,"edgelist_1.csv")
friends_csv_path = "../data/friends_info/edgelist_Feb27/friends_info_1.csv"
followers_csv_path = paste0(DataPath,"samp1_info.csv")
cleanGraph(edgelist_csv_path = edgelist_csv_path, 
           friends_csv_path = friends_csv_path,
           followers_csv_path = followers_csv_path,
           RData_path = paste0(DataPath,"RData/A1.RData"),
           min.followers = 10, min.friends.total = 10000)   

#Note: remove rows, who follows fewer than 100 people; -- trump followers
#      remove columsn, who has totally number of followers <= 10,000  -- features
#Note:#"TaylorGerring", missing in population, but in sample somehow



edgelist_csv_path = paste0(DataPath,"edgelist_2.csv")
friends_csv_path = "../data/friends_info/edgelist_Feb27/friends_info_2.csv"
followers_csv_path = paste0(DataPath,"samp2_info.csv")
cleanGraph(edgelist_csv_path = edgelist_csv_path, 
           friends_csv_path = friends_csv_path,
           followers_csv_path = followers_csv_path,
           RData_path = paste0(DataPath,"RData/A2.RData"),
           min.followers = 10,min.friends.total = 10000)



edgelist_csv_path = paste0(DataPath,"edgelist_3.csv")
friends_csv_path = "../data/friends_info/edgelist_Feb27/friends_info_3.csv"
followers_csv_path = paste0(DataPath,"samp3_info.csv")
cleanGraph(edgelist_csv_path = edgelist_csv_path, 
           friends_csv_path = friends_csv_path,
           followers_csv_path = followers_csv_path,
           RData_path = paste0(DataPath,"RData/A3.RData"),
           min.followers = 10,min.friends.total  = 10000)


# edgelist_csv_path = paste0(DataPath,"edgelist_all.csv")
# friends_csv_path = "../data/friends_info/edgelist_Feb27/friends_info_.csv"
# followers_csv_path = paste0(DataPath,"samp_info.csv")
# cleanGraph(edgelist_csv_path = edgelist_csv_path, 
#            friends_csv_path = friends_csv_path,
#            followers_csv_path = followers_csv_path,
#            RData_path = paste0(DataPath,"RData/A_all.RData"),
#            min.followers = 10, min.friends.total = 10000)





