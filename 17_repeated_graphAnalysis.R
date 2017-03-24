rm (list =ls ())
source("Head_file.R")
source("function.R")

DataPath <- "../data/friends_info/edgelist_Feb27/"

### -------------------- followers_info --------------------------------
'
samp1 <- read.csv(paste0(DataPath, "sampleMar20.csv"),colClasses =c("character"))
samp2 <- read.csv(paste0(DataPath,"sampleFeb17.csv"),colClasses =c("character"))
trump <- fread(paste0(DataPath,"trump_followers_info.csv"),
                  colClasses = c("character"))

load(paste0(DataPath,"followers_info.RData"))
'



#------------------------------ create A matrix ---------------
library(data.table)

el<- fread(paste0(DataPath,"edgelist_1.csv"), colClasses = c("character"),
                  header = T)
el<- fread(paste0(DataPath,"edgelist_2.csv"), colClasses = c("character"),
           header = T)
el<- fread(paste0(DataPath,"edgelist_3.csv"), colClasses = c("character"),
           header = T)

names(el) <-c("followers_sn", "friends_id_str")
followers <- unique(el$followers_sn)
i_set <- match(el$followers_sn, followers)
friends <- unique(el$friends_id_str)
j_set <- match(el$friends_id_str, friends)

A <- sparseMatrix(i = i_set, j = j_set, x= rep(1, length(i_set)))
rownames(A) <- followers; colnames(A) <- friends
dr <- rowSums(A); dc = colSums(A) 
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
#every person at least has one friends in common -- trump, 56310 ->51600
#A <- A[,-which.max(dc)] #remove donald trump
save(A, file = paste0(DataPath, "A1.RData"))
save(A, file = paste0(DataPath, "A2.RData"))
save(A, file = paste0(DataPath, "A3.RData"))


###--------------------------- Spectral Analyis ------------


load( paste0(DataPath, "A1.RData") ) #A, friends count
Dr <- rowSums(A)+1; Dc <- colSums(A)+1
tau1 <- sqrt(mean(Dr)); tau2 <- sqrt(mean(Dc))
#Aw <- A %*% Diagonal(m, (Dc/friends_count) * log( (n+1)/(deg_col+1) )  )

L <- Diagonal(length(Dr), (Dr+tau1)^(-1/2)) %*% A %*% Diagonal(length(Dc), (Dc + tau2)^(-1/2)) 
svd <- irlba(L, 50) # iterative method to get top 10 singular eig vectors



## using population col deg + inner product
A1 <- A %*% Diagonal(m, (deg_col/ count2) * log( (n+1)/(deg_col+1) )  ) # min(count2)>=1000, can be ommitted
norm1 <- rowSums(A1 *A1);     norm1 <- sqrt(norm1 + 1e-4)                          #apply cannot work, saying too large

L =  Diagonal(n, norm1^(-0.5)) %*% A1
svd_L <- irlba(L, nv = 50)

Dr <- rowSums(A); Dc <- colSums(A)
tau1 = sqrt(mean(Dr)); tau2 <- sqrt(mean(Dc))
L <- Diagonal(length(Dr), (Dr+tau1)^(-1/2)) %*% A %*% Diagonal(length(Dc), (Dc + tau2)^(-1/2)) 
svd <- irlba(L, 10) # iterative method to get top 10 singular eig vectors


friends_folder <- "/p/stat/songwang/TwitterPython/edgelist_Feb27/friends-info/"
if (!file.exists(friends_folder))   dir.create(friends_folder)




output_file = paste0(friends_folder,"friends_4.json")
friends_info <- getUsersBatch(ids = friends, oauth_folder = './credentials/credential_mixed06', 
                              output = output_file, random = F)
write.csv(friends_info, file = paste0(friends_folder,"/friends_4.csv"))



output_file = paste0(friends_folder,"friends_1.json")
friends_info <- getUsersBatch(ids = friends, oauth_folder = './credentials/credential_mixed04', 
                              output = output_file, random = F)
write.csv(friends_info, file = paste0(friends_folder,"/friends_1.csv"))



edgelist <- read.csv("/p/stat/songwang/TwitterPython/edgelist_Feb27/edgelist_2.csv", colClasses = c("character"))
el = edgelist[,1:2]; names(el) <-c("followers_id_str", "friends_id_str")

followers <- unique(el$followers_id_str)
friends <- unique(el$friends_id_str)
output_file = paste0(friends_folder,"friends_2.json")
friends_info <- getUsersBatch(ids = friends, oauth_folder = './credentials/credential_mixed02', 
                              output = output_file, random = F)
write.csv(friends_info, file = paste0(friends_folder,"/friends_2.csv"))

output_file = paste0(friends_folder,"friends_3.json")
friends_info <- getUsersBatch(ids = friends, oauth_folder = './credentials/credential_mixed06', 
                              output = output_file, random = F)
write.csv(friends_info, file = paste0(friends_folder,"/friends_3.csv"))

foll_count <- read.csv(file = paste0(output_folder, "friends_follCount-nonrandom.csv"),
                 header = TRUE)
followers_count <- read.csv("../data/followers_info/jsons/id_counts.csv", stringsAsFactors = T)
samp <- read.csv("../data")
foll_count$friends_id_str <- as.character(foll_count$friends_id_str)


edgelist <- read.csv(paste0(output_folder, "edgelist-non-random100K-2.csv"), 
                     stringsAsFactors = F )  #76431, following 1471141 (>=2)
followers <- unique(edgelist$follower_id_str)
edgelist$friends_id_str <- as.character(edgelist$friends_id_str)



library(igraph)
G <- graph_from_edgelist( as.matrix(edgelist) )
A <- get.adjacency(G)
friends_info <- read.csv(paste0(output_folder, "friends_info.csv"), stringsAsFactors = F)


deg1 <- degree(G,mode = "out")
deg2 <- degree(G, mode = "in")





###
library(smappR)
library(igraph)
library(data.table)
samp1 <- read.csv(file = '../sampleFeb17.csv', header = T, stringsAsFactors = F)

ID_SN1 <-fread("../id_counts_2.csv", colClasses = c("integer", "integer", "character", "character", "character"),
                header =T, stringsAsFactors = F)
dat <- readLines("/p/stat/songwang/TwitterPython/edgelist_Feb27/adjlist_1234.txt")

idx <- match(samp1$non.random, ID_SN1$screen_name)
idx.order = order(idx)
intervals <- seq(0, length(idx), length.out = 5)
intervals <- round(intervals)
idx1 = idx.order[(intervals[1]+1):intervals[1+1]]; 
writeLines(data1, con = "/p/stat/songwang/TwitterPython/edgelist_Feb27/graph1.txt")
idx2 = idx.order[(intervals[2]+1):intervals[2+1]]; 
writeLines(data2, con = "/p/stat/songwang/TwitterPython/edgelist_Feb27/graph2.txt")
idx3 = idx.order[(intervals[3]+1):intervals[3+1]]; 
writeLines(data3, con = "/p/stat/songwang/TwitterPython/edgelist_Feb27/graph3.txt")
idx4 = idx.order[(intervals[4]+1):intervals[4+1]]; 
writeLines(data4, con = "/p/stat/songwang/TwitterPython/edgelist_Feb27/graph4.txt")


el4 <- read.csv("/p/stat/songwang/TwitterPython/edgelist_Feb27/edgelist/edgelist_1234.csv")
G = graph_from_edgelist(e14[,1:2])




g <- graph_from_edgelist(el, directed = F); A = get.adjacency(g)
