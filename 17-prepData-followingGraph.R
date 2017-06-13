
rm (list =ls ())
library(Matrix)
library(data.table)
ResultPath <- "../data/friends_info/edgelist_Feb27/results/"
DataPath <- "../data/friends_info/edgelist_Feb27/"

#Note
#cannog use fread - cuz unbalanced quotation in decription column
# \\\\\\"Or you can just call me 'uMalume', your uncle" ishuu!./// IG & SC - @butter__moon



cleanGraph <- function(edgelist_csv_path, friends_csv_path, followers_csv_path, 
                       RData_path, min.followers = 10, min.friends.pop =1000){
  
  el <- fread(input = edgelist_csv_path, colClasses = c("character"))
  names(el) <-c("followers_sn", "friends_id_str")
  followers_sn <- unique(el$followers_sn)
  i_set <- match(el$followers_sn, followers_sn)
  friends_id_str <- unique(el$friends_id_str)
  j_set <- match(el$friends_id_str, friends_id_str)
  
  A <- sparseMatrix(i = i_set, j = j_set, x= rep(1, length(i_set)))
  A@x <-rep(1,length(A@x))# duplicate edges
  rownames(A) <- followers_sn
  colnames(A) <- friends_id_str
  friends_info <- read.csv(friends_csv_path, 
                        colClasses = c("character"))
  friends_info$followers_count <- as.numeric(friends_info$followers_count)
  
  followers_info <- read.csv(followers_csv_path,
                          colClasses = c("character"))
  followers_info$followers_count <- as.numeric(followers_info$followers_count)
  
  dr <- rowSums(A); dc <- colSums(A)
  nr = nrow(A); nc = ncol(A)
  
  #--------------------------------------------------------------------
  #remove some rows, some of them col info are not available
  idx1 <- match(tolower(rownames(A)), tolower(followers_info$screen_name))
  sum(is.na(idx1))  
  if (sum(is.na(idx1)) >0 ){ 
    A <- A[!is.na(idx1),]
    followers_info <- followers_info[idx1[!is.na(idx1)],]
  }
    
    #remove some columns, some of them col info are not available
  idx2 <- match(colnames(A), friends_info$id_str)
  sum(is.na(idx2))   #0, all in 105835 3169691
  if (sum(is.na(idx2)) >0 ){ 
    A <- A[,!is.na(idx2)]
    followers_info <- friends_info[idx2[!is.na(idx1)],]
  }
  #change to column names from id_str to screen_names
  colnames(A) <- friends_info$screen_name
  
  
  
  
    #further cleanning, remove those followers having < 1000 followers
  idx3 <- which(friends_info$followers_count < min.friends.pop)  #1000
  
  if (length(idx3) >0 ){  #single index vector not allowd > about 1.6M
    A <- A[,-idx3]
    friends_info <- friends_info[-idx3,]
  }
    
  
  #exclude some more rows, having fewer 10 friends  -255 such followers
  dr <- rowSums(A); dc <- colSums(A)
  sum(dr<10)
  #[1] 255
  idx4 <- which(dr < min.followers)
  if(length(idx4)>0){
    A <- A[-idx4,]
    followers_info <- followers_info[-idx4,]
  }
  message("after cleaning, nrow from ", nr, " -> ",nrow(A), "ncol: from ", nc, ' -> ', "ncol(A)")
  
  
  message("starting to save the results ....")
  save(A, friends_info, followers_info, file =RData_path)
  
}




edgelist_csv_path = paste0(DataPath,"edgelist_1.csv")
friends_csv_path = "../data/friends_info/edgelist_Feb27/friends_info_1.csv"
followers_csv_path = paste0(DataPath,"all_followers_info.csv")
cleanGraph(edgelist_csv_path = edgelist_csv_path, 
           friends_csv_path = friends_csv_path,
           followers_csv_path = followers_csv_path,
           RData_path = paste0(DataPath,"RData/A1.RData"))

#Note:#"TaylorGerring", missing in population, but in sample somehow

edgelist_csv_path = paste0(DataPath,"edgelist_2.csv")
friends_csv_path = "../data/friends_info/edgelist_Feb27/friends_info_2.csv"
followers_csv_path = paste0(DataPath,"all_followers_info.csv")
cleanGraph(edgelist_csv_path = edgelist_csv_path, 
           friends_csv_path = friends_csv_path,
           followers_csv_path = followers_csv_path,
           RData_path = paste0(DataPath,"RData/A2.RData"))



edgelist_csv_path = paste0(DataPath,"edgelist_3.csv")
friends_csv_path = "../data/friends_info/edgelist_Feb27/friends_info_3.csv"
followers_csv_path = paste0(DataPath,"all_followers_info.csv")
cleanGraph(edgelist_csv_path = edgelist_csv_path, 
           friends_csv_path = friends_csv_path,
           followers_csv_path = followers_csv_path,
           RData_path = paste0(DataPath,"RData/A3.RData"))



'
> summary(dr)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1     119     351    1266    1024  316300 
> summary(dc)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
10.00    12.00    17.00    36.62    31.00 51600.00 
> which.max(dc)
[1] 5
> dim(A)
[1]   56310 1946474
> friends[5]
[1] "25073877"


dim(A2)
[1]  162800 4111145
> summary(dr)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1     160     458    1259    1295  383100 
> summary(dc)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
10.00     12.00     18.00     49.86     35.00 156200.00 
> sum(A)
204996806
> mean(A)
0.0003062879



> dim(A3)
[1]  105835 3169691
> sum(A)
[1] 171608772
> summary(dr)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1     357     848    1621    1601  259000 
> summary(dc)
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
10.00     13.00     19.00     54.14     36.00 103700.00 
> mean(dr)
1621.475
'


## network among 377725 followes themselves. -- "../data/friends_info/edgelist_Feb27/RData/adj_followers_ego.RData"

#adjlistall <- readLines("../data/friends_info/edgelist_Feb27/originalData/adjlist_all.txt")
load("../data/friends_info/edgelist_Feb27/originalData/adjlist_all.RData") #377809 -> 377725 (delete 84)
library(parallel)
sns <- unlist(mclapply(adjlist_all, function(x) gsub("(.*?),.*", replace = "\\1", x), mc.cores = 10))

idx <- match(followers$screen_name, sns)
stopifnot(sum(is.na(idx)) == 0)

sub_ids = followers$id_str  
idx <- match(followers$screen_name, sns) 
adjlist_str <- adjlist_all[idx]

adj_list <-  mclapply(adjlist_str, FUN =function(x) strsplit(x, split = ',') , mc.cores=5) 
#encounter various problems
#Error in sendMaster(try(lapply(X = S, FUN = FUN, ...), silent = TRUE)) : 
#  long vectors not supported yet: fork.c:376

#adj_list = lapply(adjlist_str, function(x) strsplit(x, split = ',')) #single thread
adj_list1 = mclapply(adj_list, function(x) {
  xx = unlist(x); idx = match(sub_ids, xx);
  xx[idx[!is.na(idx)]] #keep friends in sub_ids only. return chr(0) if follows no people; 
}, mc.cores = 5)
adj_list_ids <- mclapply(adj_list1, function(x){
  xx = unlist(x)
  match(xx, sub_ids)
}, mc.cores = 5)
save(adj_list_ids, sns, file = "../data/friends_info/edgelist_Feb27/RData/adj_followers_ego.RData")





#create a full graph among 377725 followers and the accounts followed by 10 of them 
#324933 followers, 9,118,691 friends, links 525,894,469
#this is created from python, one big graph.
load("../data/friends_info/edgelist_Feb27/originalData/edgelist_all.RData")
el<- as.matrix(edgelist_all)
rm(edgelist_all)
library(igraph)
library(Matrix)
rr <- unique(el[,1])
cc <- unique(el[,2])
i_set <- match(el[,1], rr)
j_set <- match(el[,2], cc)+length(rr) # index for c(rr,cc)
rm(el)
save(rr, cc, i_set, j_set, file = "../data/friends_info/edgelist_Feb27/RData/full_graph.RData")
q(); R




