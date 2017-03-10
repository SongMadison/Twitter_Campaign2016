rm(list =ls ())
source("function.R")

library("smappR")
samp <- read.csv(file ="../data/followers_info/jsons/sampleFeb17.csv", 
                 header = T, stringsAsFactors = F)
## indices, for random and non-random

SN1 <- samp[,2]  


#download the friend list from a list of twitter screen names
#SN1: list of screen names
#my_oauth_folder: folder contains oauth objects
#output :usually a .txt file.

DownloadFriendlist <- function (SN1, my_oauth_folder, output){
  conn = file(output, 'w') 
  for ( i in 1:length(SN1)){  
    #5607, 8840, 17599 00:01/ Oct18
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
      }
    )                    
    write(paste(c(sn,friendIDs), collapse = ','), file  = output, append = TRUE, sep = '\n')
    message("i--", i," users have been processed")
  }
  close(conn) # unitl close ,the data will be written. kind of dangerous if
}



my_oauth_folder <- "./credentials/credential_mixed"
output <- "../data/friends_info/Feb17-1.txt"
downloadFriendlist (SN1[1:61798], my_oauth_folder, output)

my_oauth_folder <- "./credentials/credential_mixed1"
output <- "../data/friends_info/Feb17-2.txt"
downloadFriendlist (SN1[61799:122798], my_oauth_folder, output)

my_oauth_folder <- "./credentials/credential_mixed2"
output <- "../data/friends_info/Feb17-3.txt"
downloadFriendlist (SN1[122799:183798], my_oauth_folder, output)


my_oauth_folder <- "./credentials/credential_mixed3"
output <- "../data/friends_info/Feb17-4.txt"
downloadFriendlist (SN1[183799:244798], my_oauth_folder, output)

#Feb 25, the server stop for some reason
#Feb17-1, 31525 lines, 
#last line is uesr_sceen_name FR_Newbrough


#Feb17-2 13074 lines, mistakenly starting from 1 line, wasted !
#last line is  screen name : WinningNiche

#Feb17-3, 45537 lines
#last line screen hhoran80  -- 168335 in SN1

#Feb17-4, 19106 lines 
#lastline is : sceen name:  bmacgolf19 -- 202904 lin SN1



my_oauth_folder <- "./credentials/credential_mixed"
output <- "../data/friends_info/Feb25-1.txt"
downloadFriendlist (SN1[31526:61798], my_oauth_folder, output)


my_oauth_folder <- "./credentials/credential_mixed1"
output <- "../data/friends_info/Feb17-2.txt"
downloadFriendlist (SN1[61799:122798], my_oauth_folder, output)


my_oauth_folder <- "./credentials/credential_mixed2"
output <- "../data/friends_info/Feb25-3.txt"
downloadFriendlist (SN1[168336:183798], my_oauth_folder, output)


my_oauth_folder <- "./credentials/credential_mixed3"
output <- "../data/friends_info/Feb25-4.txt"
downloadFriendlist (SN1[183799:244798], my_oauth_folder, output)


# All finished before Feb27 21:00 -- about 50 hours


s = 0
for (i in 7:15){
  if (i<10){
    i_str = paste0('0',i)
  }else{
    i_str = as.character(i)
  }
  file <- paste0("../data/followers_info/dataframes/trump",i_str,".csv")
  dat <- read.csv(file, 
                  colClasses = c("integer", "integer", "character","character", "character"),
                  stringsAsFactors = F)
  
  s = s + sum(dat$followers_count)
  cat("i=", i, "\n")
}


id_counts <- read.csv("../data/followers_info/jsons/id_counts.csv", 
                      colClasses = c("integer", "integer", "character","character", "character"),
                      stringsAsFactors = F)

sum(id_counts$followers_count)
#[1] 1385568216

set.seed(100)
res <- replicate(n = 1000,expr =   mean(sample(x = 1:100, size = 10, prob = (1:100)^2/sum((1:100)^2), replace = FALSE)))
hist(res)
mean(res); sd(res)

set.seed(100)
res <- replicate(n = 1000,expr =   mean(sample(x = 1:100, size = 10, prob = (1:100)^2), replace = FALSE))
hist(res)
mean(res); sd(res)





output_folder <- "..//data/friends_info/edgelist_Feb27/timelines/t2/"
if (! file.exists( output_folder )) dir.create( output_folder)
Err_users  = NULL
for (i in 65814:122798){
  #i = 1
  file_name <- paste0(output_folder, SN1[i],".json")
  tryCatch(
    getTimeline(filename = file_name, n =3200, screen_name = SN1[i],
                oauth_folder = "./credentials/credential_mixed1",
                sleep = 0.5, verbose = TRUE), 
    error = function(e){
      Err_users <- c(Err_users, SN1[i])
      message( paste(SN1[i], "error occurred"))
    }
  )
  Err_users <- cbind(Err_users,SN1[i])
  print(paste("XXXXXX -- i = ", i ,'\n'))   
}




# starting download on Mar 1

  
library("smappR")
samp <- read.csv(file ="../data/followers_info/jsons/sampleFeb17.csv", 
                 header = T, stringsAsFactors = F)
SN1 <- samp[,2]  #non-random

library(doParallel)
n_cores = detectCores()
cl <- makeCluster(14)
registerDoParallel(cl)

downloadTweets <- function(k, intervals, output_folder, oauth_folder, logFile){
  p1 <- Sys.time()
  for (i in (intervals[k]+1):intervals[k+1]){
    file_name <- paste0(output_folder, SN1[i],".json")
    tryCatch(
      expr = {
        getTimeline(filename = file_name, n =3200, screen_name = SN1[i],
                    oauth_folder = oauth_folder,
                    sleep = 0.5, verbose = FALSE)
        if (!is.null(logFile)){cat( paste0( SN1[i], ", sucess"), file=logFile, append=TRUE, sep = "\n")}
      }, 
      error = function(e){
        if (!is.null(logFile)) {cat(paste0( SN1[i], ", fail"), file=logFile, append=TRUE, sep = "\n")}
      }
    )
  }
  return(data.frame(k = k, nobs = intervals[k+1]-intervals[k],time = Sys.time()-p1))
}

##parallel downloadthe timelines
nfolder = 14
intervals <- seq(0, length(SN1),by= floor(length(SN1)/nfolder))
if (intervals[nfolder+1] < 100) intervals[nfolder+1] = length(SN1)
result <- foreach (k = 1:nfolder,
         .combine = rbind,
         .packages = c("smappR") ) %dopar% {
           
          # download tweets, save  
         k_str = ifelse( k < 10, paste0('0',k), k)
         output <- "../data/friends_info/edgelist_Feb27/timelines/"
         logFile = paste0(output,"log",k_str,".txt")
         output_folder <- paste0(output,"t",k_str,"/")
         if (! file.exists( output_folder )) dir.create( output_folder)
         oauth_folder <- paste0("./credentials/credential_mixed",k_str)
         downloadTweets(k, intervals, output_folder,oauth_folder,logFile)
}

output_folder <- "../data/friends_info/edgelist_Feb27/timelines/t3/"
if (! file.exists( output_folder )) dir.create( output_folder)
Err_users  = NULL
for (i in 122799:183798){
  #i = 1
  file_name <- paste0(output_folder, SN1[i],".json")
  tryCatch(
    getTimeline(filename = file_name, n =3200, screen_name = SN1[i],
                oauth_folder = "./credentials/credential_mixed2",
                sleep = 0.5, verbose = TRUE), 
    error = function(e){
      Err_users <- c(Err_users, SN1[i])
      message( paste(SN1[i], "error occurred"))
    }
  )
  Err_users <- cbind(Err_users,SN1[i])
  print(paste("XXXXXX -- i = ", i ,'\n'))   
}

#127165 -- 4.5k / a day

output_folder <- "../data/friends_info/edgelist_Feb27/timelines/t4/"
if (! file.exists( output_folder )) dir.create( output_folder)
Err_users  = NULL
for (i in 183799:244798){
  #i = 1
  file_name <- paste0(output_folder, SN1[i],".json")
  tryCatch(
    getTimeline(filename = file_name, n =3200, screen_name = SN1[i],
                oauth_folder = "./credentials/credential_mixed3",
                sleep = 0.5, verbose = TRUE), 
    error = function(e){
      Err_users <- c(Err_users, SN1[i])
      message( paste(SN1[i], "error occurred"))
    }
  )
  Err_users <- cbind(Err_users,SN1[i])
  print(paste("XXXXXX -- i = ", i ,'\n'))   
}









## parallel processing the json, to get csv files
# every 100 users, as a .csv file


rm(list =ls())
source("Head_file.R")
source("function.R")
data_folder <-
  "../data/friends_info/edgelist_Feb27/timelines/t01/"
output_folder <-
  "../data/friends_info/edgelist_Feb27/timelines_csv/t01/"
if (!file.exists(output_folder)) { dir.create(output_folder)}



files = list.files(data_folder)
files = files
interval = 100
nbreaks =  ceiling(length(files) / interval)
library(doParallel)
n_cores = detectCores()
cl <- makeCluster(floor(n_cores / 5))
registerDoParallel(cl)
results <-
  foreach (i = 54:nbreaks,
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
             rm(data.df)
             rm(data.json)
             rm(data.str)
             rm(dat)
           }
stopCluster(cl)
write.csv(results, file = "results1.csv", row.names = F)



rm(list =ls())
#source("Head_file.R")
library(smppR)
libary(jsonlite)
source("function.R")
data_folder <-
  "~/Stat/t10/"
output_folder <-
  "~/Stat/t10_csv/"
if (!file.exists(output_folder)) { dir.create(output_folder)}



files = list.files(data_folder)
files = files
interval = 100
nbreaks =  ceiling(length(files) / interval)
library(doParallel)
n_cores = detectCores()
cl <- makeCluster(floor(n_cores / 5))
registerDoParallel(cl)
results <-
  foreach (i = 1:39,
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
write.csv(results, file = "results1.csv", row.names = F)


