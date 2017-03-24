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
source("~/Stat/code/function.R")  #change .libPaths()
library(smappR)
library(jsonlite)

for (i in 10:14){
  i_str = ifelse(test = {i>9}, i , paste0("0",i))
  data_folder <-
    paste0("/p/stat/songwang/timelines/","t",i_str,"/")
  output_folder <-
    paste0("/p/stat/songwang/timelines_csv/", "t",i_str,"/") 
  
  time1 = Sys.time()
  
  processTweets(data_folder, output_folder)
  cat("time:", Sys.time() - time1, '\n')
}


i_str = ifelse(test = {i>9}, i , paste0("0",i))
data_folder <-
  paste0("~/Stat/code","t",i_str,"/")
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




