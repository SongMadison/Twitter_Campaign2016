h_name = system("uname -n", intern = T)
h_name = substr(h_name, start = 1, stop = 4)    
if ( h_name == "bigm" || h_name =="desk"){ 
  .libPaths("/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-pc-linux-gnu-library/3.2")
  }







rm (list =ls ())
source("Head_file.R")
source("function.R")

ResultPath <- "../data/friends_info/edgelist_Feb27/results/"
DataPath <- "../data/friends_info/edgelist_Feb27/"


load( paste0(DataPath, "A1.RData"))
ResultPath <- paste0(ResultPath, "result1/")

dr <- rowSums(A)+1; Dc <- colSums(A)+1
tau1 <- sqrt(mean(dr)); tau2 <- sqrt(mean(Dc))


L <- Diagonal(length(dr), (dr+tau1)^(-1/2)) %*% A %*% Diagonal(length(Dc), (Dc + tau2)^(-1/2)) 
svd <- irlba(L, 50) # iterative method to get top 10 singular eig vectors
save(svd, paste0(ResultPath, "svd_L.RData"))
#normalization of rows
k = 50 # or 7
U <- svd$u[,1:k] %*%Diagonal(k, svd$d[1:k]^(1/2)) # first eigenvectors are more important??

rowN <- rowSums(U*U); rowN <- sqrt(rowN +1e-6)
U1 <- Diagonal(length(rowN), rowN^(-1))%*%U
set.seed(123)
km_row = kmeans(U1, k, nstart = 100, iter.max =50)  # most time-consuming steps!

# representative columns
top = 40
keyfriends_ids <- matrix("", top, k)
scores <- matrix(0, top, k)
for(i in 1:k){
  c_i1 <- colMeans( L[which(km_row$cluster==i), ] )
  c_i2 <- colMeans( L[which(km_row$cluster!=i), ] )
  
  #variance stablization transformation
  c_i <- sqrt(c_i1) - sqrt(c_i2)   
  
  names_tmp <- colnames(A)[order(-c_i)]
  c_i <- c_i[order(-c_i)]
  idx <- which(!is.na(names_tmp))[1:top]   #remove NA if they extist
  scores[,i] <- round(c_i[idx],3)
  keyfriends_ids[,i]<- names_tmp[idx] 
  
  print(c_i["25073877"])
}

#get representative followers info
my_oauth_folder <- "./credentials/credential_mixed04/"
library(smappR)
keyfriends_df <- getUsersBatch(ids = unique(as.vector(keyfriends_ids)), oauth_folder = my_oauth_folder, 
                               include_entities = TRUE, verbose = TRUE, 
                               output = paste0(ResultPath, "keyfriends_k50_40.json"))
#keyfriends_df<- read.csv(paste0(ResultPath, "keyfriends_k50_10.csv"))
keyfriends_sn <- keyfriends_df$screen_name[match(as.vector(keyfriends_ids), keyfriends_df$id_str)]
clustering <- cbind(as.vector(keyfriends_ids),keyfriends_sn, rep(1:k, each = top),
                    rep(km_row$size, each = top),  as.vector(scores))
clustering <- data.frame(clustering, stringsAsFactors = F)
names(clustering) <- c("id_str","screen_name","clusters","Sizes","scores")

keyfriends_df <- data.table(keyfriends_df)
setkey(keyfriends_df, id_str)  #order by screen_name, fast match
result <- cbind(clustering,keyfriends_df[clustering$id_str,])
write.csv(result, file =paste0(ResultPath, "keyfriends_k50_40_info.csv"), row.names = F)

savd(A, svd, km_row, result, file =paste0(ResultPath, "result.RData"))

#download friends info
.libPaths(c("/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-pc-linux-gnu-library/3.2", 
            .libPaths()))
library(smappR)
friends1 <- readLines("../data/friends_ids_1.txt")
my_oauth_folder <- "./credentials/credential_mixed01/"
friends_info1<- getUsersBatch(ids = friends1, oauth_folder = my_oauth_folder, 
                  include_entities = TRUE, verbose = TRUE, 
                  output = "../data/friends_1.json")
write.csv(friends_info1, file = "../data/friends1_info.csv")

.libPaths(c("/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-pc-linux-gnu-library/3.2", 
            .libPaths()))
library(smappR)
friends2 <- readLines("../data/friends_ids_2.txt")
my_oauth_folder <- "./credentials/credential_mixed02/"
friends_info2 <- getUsersBatch(ids = friends2, oauth_folder = my_oauth_folder, 
                           include_entities = TRUE, verbose = TRUE, 
                           output = "../data/friends_2.json")
write.csv(friends_info2, file = "../data/friends2_info.csv")

.libPaths(c("/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-pc-linux-gnu-library/3.2", 
            .libPaths()))
library(smappR)
friends3 <- readLines("../data/friends_ids_3.txt")
my_oauth_folder <- "./credentials/credential_mixed03/"
friends_info3 <- getUsersBatch(ids = friends3, oauth_folder = my_oauth_folder, 
                           include_entities = TRUE, verbose = TRUE, 
                           output = "../data/friends_3.json")
write.csv(friends_info3, file = "../data/friends3_info.csv")


#----------------------------------------



processTweets <- function( data_folder, output_folder){
  
  library(doParallel)
  n_cores = detectCores()
  cl <- makeCluster(5)
  registerDoParallel(cl)
  
  if (!file.exists(output_folder)) { dir.create(output_folder)}
  
  files = list.files(data_folder)
  interval = 50
  nbreaks =  ceiling(length(files) / interval)
  
  output <- foreach (i = 1:nbreaks,
                     .combine = rbind,
                     .packages = c("jsonlite", "smappR",'doParallel')) %dopar% {
                       #i =1
                       p1 <- Sys.time()
                       data.str <- NULL
                       idx_set = interval * (i - 1) + (1:interval)
                       if (i == nbreaks) {
                         idx_set = (interval * (i - 1) + 1):length(files)
                       }
                       for (j in idx_set) {
                         filepath <-
                           paste0(data_folder, files[j]) #"./data/trump_followers_screen-names.json"#
                         data.str <- c(data.str, readLines(filepath))
                       }
                       idx <- unlist(lapply(data.str, validate))
                       data.json <-
                         paste0('[', paste0(data.str[idx], collapse = ","), ']')
                       
                       dat <- jsonlite::fromJSON(data.json, simplifyDataFrame = T)
                       data.df <- simplifyTwitterDF(dat)
                       #lst2 <- lapply(lst1, function(x) selected(x))
                       # lst3 <- lapply(lst2, function(x) as.data.frame(x))
                       # data.df <- do.call("rbind", lst3)
                       #data.df <- data.frame(do.call("rbind", lapply(lst2, function(x) as.data.frame(x)) ) )
                       i_str <-
                         paste0(paste0(rep('0', nchar(nbreaks) - nchar(i)), collapse = ""), i, collapse = "")
                       write.csv(data.df,
                                 file = paste0(output_folder, i_str, ".csv"),
                                 row.names = F)
                       cat("i=", i, 'size =', nrow(data.df), "\n")
                       list(i = i, rows = nrow(data.df))
                     }
  
  write.csv(output, file = paste0(output_folder,"log","t",i_str,".txt"))
  stopCluster(cl)
}



##processTweets(data_folder, output folder)




##bigmem04
.libPaths(c("/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-pc-linux-gnu-library/3.2",
            .libPaths()) ) 
source("~/Stat/Twitters/code/function.R")  #change .libPaths()
library(smappR)
library(jsonlite)

for (i in 10:14){
  i_str = ifelse(test = {i>9}, i , paste0("0",i))
  data_folder <-
    paste0("/p/stat/songwang/timelines/","t",i_str,"/")
  #data_folder <- paste0("~/Stat/Twitters/t03/")
  output_folder <-
    paste0("/p/stat/songwang/timelines_csv/", "t",i_str,"/") 
  
  time1 = Sys.time()
  
  processTweets(data_folder, output_folder)
  cat("time:", Sys.time() - time1, '\n')
}


i_str = ifelse(test = {i>9}, i , paste0("0",i))
data_folder <-
  paste0("~/Twitters/code","t",i_str,"/")
output_folder <-
  paste0("~/Stat/code", "t",i_str,"/") 

time1 = Sys.time()

processTweets(data_folder, output_folder)
cat("time:", Sys.time() - time1, '\n')

source("function.R")  #change .libPaths()
library(smappR)
library(jsonlite)
for (i in 1:9){
  i_str = ifelse(test = {i>9}, i , paste0("0",i))
  data_folder <-
  paste0("../data/friends_info/edgelist_Feb27/timelines/","t",i_str,"/")
  output_folder <-
  paste0("../data/friends_info/edgelist_Feb27/timelines_csv/", "t",i_str,"/") 

  time1 = Sys.time()
  
  processTweets(data_folder, output_folder)
  cat("time:", Sys.time() - time1, '\n')
}




rm(list =ls())
#source("Head_file.R")
library(smappR)
library(jsonlite)
source("function.R")
data_folder <-
  "../data/friends_info/edgelist_Feb27/timelines/t06_old/"
output_folder <-
  "../data/friends_info/edgelist_Feb27/timelines_csv/t06/"
if (!file.exists(output_folder)) { dir.create(output_folder)}



files = list.files(data_folder)
files = files
interval = 100
nbreaks =  ceiling(length(files) / interval)
library(doParallel)
n_cores = detectCores()
cl <- makeCluster(4)
registerDoParallel(cl)
results <-
  foreach (i = 1:nbreaks,
           .combine = rbind,
           .packages = c("jsonlite", "smappR")) %dopar% {
             #i =1
             p1 <- Sys.time()
             data.str <- NULL
             idx_set = interval * (i - 1) + (1:interval)
             if (i == nbreaks) {
               idx_set = (interval * (i - 1) + 1):length(files)
             }
             for (j in idx_set) {
               filepath <-
                 paste0(data_folder, files[j]) #"./data/trump_followers_screen-names.json"#
               data.str <- c(data.str, readLines(filepath))
             }
             idx <- unlist(lapply(data.str, validate))
             data.json <-
               paste0('[', paste0(data.str[idx], collapse = ","), ']')
             
             dat <- jsonlite::fromJSON(data.json, simplifyDataFrame = T)
             data.df <- simplifyTwitterDF(dat)
             #lst2 <- lapply(lst1, function(x) selected(x))
             # lst3 <- lapply(lst2, function(x) as.data.frame(x))
             # data.df <- do.call("rbind", lst3)
             #data.df <- data.frame(do.call("rbind", lapply(lst2, function(x) as.data.frame(x)) ) )
             i_str <-
               paste0(paste0(rep('0', nchar(nbreaks) - nchar(i)), collapse = ""), i, collapse = "")
             write.csv(data.df,
                       file = paste0(output_folder, i_str, ".csv"),
                       row.names = F)
             cat("i=", i, 'size =', nrow(data.df), "\n")
             list(i = i,
                  rows = nrow(data.df),
                  time = Sys.time() - p1)
           }
stopCluster(cl)
write.csv(results, file = "results06.csv", row.names = F)



rm(list =ls())
.libPaths(c("/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-pc-linux-gnu-library/3.2",
             .libPaths()) )
#source("function.R")
library(smappR)
library(jsonlite)
library(doParallel)
#source("~/Stat/code/function.R")
data_folder <-
  "/p/stat/songwang/t14/"
output_folder <-
  "/p/stat/songwang/t14_csv/"
if (!file.exists(output_folder)) { dir.create(output_folder)}

source("/u/s/o/songwang/Stat/code/function.R")

files = list.files(data_folder)
files = files
interval = 100
nbreaks =  ceiling(length(files) / interval)

for (i in 122:nbreaks){
  p1 <- Sys.time()
  data.str <- NULL
  idx_set = interval * (i - 1) + (1:interval)
  if (i == nbreaks) {
    idx_set = (interval * (i - 1) + 1):length(files)
  }
  for (j in idx_set) {
    filepath <-
      paste0(data_folder, files[j]) #"./data/trump_followers_screen-names.json"#
    data.str <- c(data.str, readLines(filepath))
  }
  idx <- unlist(lapply(data.str, validate))
  data.json <-
    paste0('[', paste0(data.str[idx], collapse = ","), ']')
  
  dat <- jsonlite::fromJSON(data.json, simplifyDataFrame = T)
  data.df <- simplifyTwitterDF(dat)
  #lst2 <- lapply(lst1, function(x) selected(x))
  # lst3 <- lapply(lst2, function(x) as.data.frame(x))
  # data.df <- do.call("rbind", lst3)
  #data.df <- data.frame(do.call("rbind", lapply(lst2, function(x) as.data.frame(x)) ) )
  i_str <-
    paste0(paste0(rep('0', nchar(nbreaks) - nchar(i)), collapse = ""), i, collapse = "")
  write.csv(data.df,
            file = paste0(output_folder, i_str, ".csv"),
            row.names = F)
  cat("i=", i, 'size =', nrow(data.df), "\n")
  cat("time = ", Sys.time() - p1)
}             



rm(list =ls())
#source("Head_file.R")
library(smappR)
library(jsonlite)
source("function.R")
data_folder <-
  "~/Stat/t03/"
output_folder <-
  "~/Stat/t03_csv/"
if (!file.exists(output_folder)) { dir.create(output_folder)}



files = list.files(data_folder)
files = files
interval = 100
nbreaks =  ceiling(length(files) / interval)
library(doParallel)
n_cores = detectCores()
cl <- makeCluster(floor(n_cores / 2))
registerDoParallel(cl)
results <-
  foreach (i = 1:nbreaks,
           .combine = rbind,
           .packages = c("jsonlite", "smappR")) %dopar% {
             #i =1
             p1 <- Sys.time()
             data.str <- NULL
             idx_set = interval * (i - 1) + (1:interval)
             if (i == nbreaks) {
               idx_set = (interval * (i - 1) + 1):length(files)
             }
             for (j in idx_set) {
               filepath <-
                 paste0(data_folder, files[j]) #"./data/trump_followers_screen-names.json"#
               data.str <- c(data.str, readLines(filepath))
             }
             idx <- unlist(lapply(data.str, validate))
             data.json <-
               paste0('[', paste0(data.str[idx], collapse = ","), ']')
             
             dat <- jsonlite::fromJSON(data.json, simplifyDataFrame = T)
             data.df <- simplifyTwitterDF(dat)
             #lst2 <- lapply(lst1, function(x) selected(x))
             # lst3 <- lapply(lst2, function(x) as.data.frame(x))
             # data.df <- do.call("rbind", lst3)
             #data.df <- data.frame(do.call("rbind", lapply(lst2, function(x) as.data.frame(x)) ) )
             i_str <-
               paste0(paste0(rep('0', nchar(nbreaks) - nchar(i)), collapse = ""), i, collapse = "")
             write.csv(data.df,
                       file = paste0(output_folder, i_str, ".csv"),
                       row.names = F)
             cat("i=", i, 'size =', nrow(data.df), "\n")
             list(i = i,
                  rows = nrow(data.df),
                  time = Sys.time() - p1)
           }
stopCluster(cl)
write.csv(results, file = "results11.csv", row.names = F)




