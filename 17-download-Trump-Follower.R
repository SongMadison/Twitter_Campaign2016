



# ----- part 0: download the Trump followers and take a sample of sample followers --

sn = "realDonaldTrump"
followers_IDs <- getFollowers(sn, cursor = -1, oauth_folder = my_oauth_folder, sleep = 30, 
                              output = paste0("../data/friends_info/ids/", 
                                              sn,"_",Sys.Date(),"_ids.txt")) #mutiple 5k

followers_info <- getUsersBatch(ids = followers_IDs, oauth_folder = my_oauth_folder, 
                                include_entities = TRUE, verbose = TRUE, 
                                output = paste0("../data/friends_info/jsons/",
                                                sn, Sys.Date(),"_info.json"))
# jsons files are processed in Python, if followers_info cannot be generated.
# the following data are created and sample 



# This research started from last Sept, we first did a smalle experiment on followers from Mar 2016 - Oct 10 2016.
# first
# 
# 
# 
# started to download the information for the first 6.5 million, the time effects got compromised 
#already.  i.e. the data not downloaded at the same time.
# 
# follower ids most before Oct 10, about 12.2 million, 0.8 millions added later after election
# based on ids on Jan -8  -- toal 13 millions
# samped 377809 samples from them. first 



# of 244798 followers sampled screen names as below
# sampleTrumpFollowers.ipynb


## ----- part 1: download the friends list of sampled set of followers --

#id_counts_2 <- read.csv('../data/followers_info/jsons/id_counts_2.csv') 

rm(list =ls ())
source("function.R")
library("smappR")


samp <- read.csv(file ="../data/followers_info/jsons/sampleFeb17.csv", 
                 header = T, stringsAsFactors = F)
## indices, for random and non-random

SN1 <- samp$non.random




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



my_oauth_folder <- "./credentials/credential_mixed01"
output <- "../data/friends_info/Feb25-1.txt"
downloadFriendlist (SN1[31526:61798], my_oauth_folder, output)


my_oauth_folder <- "./credentials/credential_mixed02"
output <- "../data/friends_info/Feb17-2.txt"
downloadFriendlist (SN1[61799:122798], my_oauth_folder, output)


my_oauth_folder <- "./credentials/credential_mixed03"
output <- "../data/friends_info/Feb25-3.txt"
downloadFriendlist (SN1[168336:183798], my_oauth_folder, output)


my_oauth_folder <- "./credentials/credential_mixed04"
output <- "../data/friends_info/Feb25-4.txt"
downloadFriendlist (SN1[183799:244798], my_oauth_folder, output)


# All finished before Feb27 21:00 -- about 50 hours

## redownload the four parts, to check whether there is anything wrong
my_oauth_folder <- "./credentials/all/part1/"
output <- "../data/friends_info/Mar10-4.txt"
downloadFriendlist (SN1[183799:244798], my_oauth_folder, output)

## download the 100k old followers 
samp100K <- read.csv("../data/followers_info/jsons/sampleSNs100000.csv",header = T, stringsAsFactors = F)
my_oauth_folder <- "./credentials/credential_mixed02/"
output <- "../data/friends_info/old100K.txt"
downloadFriendlist (samp100K$non_random, my_oauth_folder, output)

## download a news ample from recent followers
my_oauth_folder <- "./credentials/credential_mixed"
output <- "../data/friends_info/Feb17-1.txt"
downloadFriendlist (SN1[1:61798], my_oauth_folder, output)


## total 133011 rows
my_oauth_folder <- "/p/stat/songwang/credentials/credential_mixed01"
output <- "/p/stat/songwang/post-Mar20-1.txt"
downloadFriendlist (SN1[40001:40000], my_oauth_folder, output)

my_oauth_folder <- "/p/stat/songwang/credentials/credential_mixed02"
output <- "/p/stat/songwang/post-Mar20-2.txt"
downloadFriendlist (SN1[40001:80000], my_oauth_folder, output)

my_oauth_folder <- "/p/stat/songwang/credentials/credential_mixed03"
output <- "/p/stat/songwang/post-Mar20-3.txt"
downloadFriendlist (SN1[80001:110000], my_oauth_folder, output)

my_oauth_folder <- "./credentials/credential_mixed04"
output <- "/p/stat/songwang/post-Mar20-4.txt"
downloadFriendlist (SN1[110001:133011], my_oauth_folder, output)

#### ------------------PART 3:  download timeline ------------------------

#use parallel process to download
# The code stop after running out of space. 
# after cleanning and deleted those unnecessary files, redownloaded from the place it stopped 
# newStartIdx -- total number of finished items +1

# analyze the logs, to concinue downloading from where the code failed
folder <- "../data/friends_info/edgelist_Feb27/timelines/logs_finished1/"
newSNs <- character(14)
for (i in 1:14){
  i_str = ifelse (i<10 , paste0('0',i), as.character(i)) 
  log_i <-  read.table(paste0(folder, "log",i_str,"_time.txt"), skip = 1)
  log_i$time = paste0(log_i$V6, log_i$V7,'-',log_i$V8)
  idx1 = which(log_i$time < 'Mar4-12:51')
  newSNs[i] <- gsub(pattern = "(.*).json", replacement = "\\1", log_i$V9[max(idx1)+1])
}

library("smappR")
# samp <- read.csv(file ="../data/followers_info/jsons/sampleFeb17.csv", 
#                 header = T, stringsAsFactors = F)
samp <- read.csv("../data/friends_info/edgelist_Feb27/sampleMar20.csv",colClasses =c("character"))

SN1 <- samp[,2]  


library(doParallel)
n_cores = detectCores()
cl <- makeCluster(14)
registerDoParallel(cl)

##split SN1 in to pieces, this function download the k-th piece.
downloadTweets <- function(SNs, startIdx, endIdx, output_folder, oauth_folder, logFile){
  p1 <- Sys.time()
  for (i in (startIdx):endIdx){
    file_name <- paste0(output_folder, SNs[i],".json")
    tryCatch(
      expr = {
        getTimeline(filename = file_name, n =3200, screen_name = SNs[i],
                    oauth_folder = oauth_folder,
                    sleep = 0.5, verbose = FALSE)
        if (!is.null(logFile)){cat( paste0( SNs[i], ", sucess"), file=logFile, append=TRUE, sep = "\n")}
      }, 
      error = function(e){
        if (!is.null(logFile)) {cat(paste0( SNs[i], ", fail"), file=logFile, append=TRUE, sep = "\n")}
      }
    )
  }
  return(data.frame(k = k, nobs = endIdx - startIdx, time = Sys.time()-p1))
}

nfolder = 14
intervals <- seq(0, length(SN1),by= floor(length(SN1)/nfolder))
intervals[15] <- length(SN1)
newStartIdx <- match(newSNs, SN1)
cbind(newStartIdx,intervals[-1], intervals[-1] - newStartIdx ); 


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
                     idx1 = newStartIdx[k]; idx2 <- intervals[k+1]
                     downloadTweets(SNs = SN1, startIdx = idx1 , endIdx = idx2 , 
                                    output_folder, oauth_folder, logFile)
                   }


#mistenly delete first part t05

#download a 13.3 followers' timeline, Mar 25
library("smappR")
# samp <- read.csv(file ="../data/followers_info/jsons/sampleFeb17.csv", 
#                 header = T, stringsAsFactors = F)
samp <- read.csv("../data/friends_info/edgelist_Feb27/sampleMar20.csv",colClasses =c("character"))

SN1 <- samp[,2]  


library(doParallel)
n_cores = detectCores()
cl <- makeCluster(14)
registerDoParallel(cl)

##split SN1 in to pieces, this function download the k-th piece.
downloadTweets <- function(SNs, startIdx, endIdx, output_folder, oauth_folder, logFile){
  p1 <- Sys.time()
  for (i in (startIdx):endIdx){
    file_name <- paste0(output_folder, SNs[i],".json")
    tryCatch(
      expr = {
        getTimeline(filename = file_name, n =3200, screen_name = SNs[i],
                    oauth_folder = oauth_folder,
                    sleep = 0.5, verbose = FALSE)
        if (!is.null(logFile)){cat( paste0( SNs[i], ", sucess"), file=logFile, append=TRUE, sep = "\n")}
      }, 
      error = function(e){
        if (!is.null(logFile)) {cat(paste0( SNs[i], ", fail"), file=logFile, append=TRUE, sep = "\n")}
      }
    )
  }
  return(data.frame(k = k, nobs = endIdx - startIdx, time = Sys.time()-p1))
}

nfolder = 14
intervals <- seq(0, length(SN1),by= floor(length(SN1)/nfolder))
intervals[15] <- length(SN1)
startIdx <- intervals[1:14]+1
endIdx <- intervals[-1]

# stop in the middle, restart # Mar26
tmp1 <- read.csv("../data/friends_info/edgelist_Feb27/timelines_2/n_lines.csv", header = F)
startIdx1 <- intervals[1:14]+as.numeric(gsub("(.*) log.*", "\\1", tmp1$V1) )+1
#cbind(startIdx1, endIdx)

tmp2<- read.csv("../data/friends_info/edgelist_Feb27/timelines/line2.csv", header =F)
startIdx2 <- startIdx1 + as.numeric(gsub("(.*) log.*", "\\1", tmp2$V1) )
#cbind(startIdx1, startIdx2, endIdx)

tmp3 <- read.csv("../data/friends_info/edgelist_Feb27/timelines/line3.csv", header =F)
startIdx3 <- startIdx2 + as.numeric(gsub("(.*):.*", "\\1", tmp3$V1) )
#cbind(startIdx1, startIdx2, endIdx)cbind(startIdx1, startIdx, endIdx)	

tmp4 <- read.csv("../data/friends_info/edgelist_Feb27/timelines/line4.csv", header =F)
startIdx <- startIdx3 + as.numeric(gsub("(.*):.*", "\\1", tmp4$V1) )
cbind(startIdx1, startIdx2, startIdx, endIdx)	

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
                   if (k ==3){
                     oauth_folder <- paste0("./credentials/credential_mixed",15)
                   }
                   idx1 = startIdx[k]; idx2 <- endIdx[k]
                   downloadTweets(SNs = SN1, startIdx = idx1 , endIdx = idx2 , 
                                  output_folder, oauth_folder, logFile)
                 }
stopCluster(cl)
Sys.time()
#error we have, try catch will continue. 
#{"request":"\/1.1\/statuses\/user_timeline.json","error":"Not authorized."}  -- privacy
#{"errors":[{"code":34,"message":"Sorry, that page does not exist."}]}  -- pate not exist



###----------------------------PART 4: cleanning the timeline tweets----------
## every 100 .jsons  -->  one .csv file
## this part assumes a lot of memory. 6 processes, takes about 30-40G memory


rm(list =ls())
library(doParallel)
n_cores = detectCores()
cl <- makeCluster(3)
registerDoParallel(cl)
#source("~/Stat/Twitters/code/Head_file.R")

source("~/Stat/Twitters/code/function.R")
library(smappR)
library(jsonlite)
result <- 
  foreach(i = c(1,10,11),
          .combine = rbind, 
          .packages= c("jsonlite", "smappR")) %dopar% {
      time1 = Sys.time()          
      i_str = ifelse(test = {i>9}, i , paste0("0",i))
      data_folder <-
        paste0("/p/stat/songwang/timelines/","t",i_str,"/")
      #data_folder <- paste0("~/Stat/Twitters/t03/")
      output_folder <-
        paste0("/p/stat/songwang/timelines_csv/", "t",i_str,"/") 
      processTweets(data_folder, output_folder)
      
      elpase_time <- Sys.time() - time1
      list(i = i,  time = paste0(elpase_time, " ", attr(elpase_time,"units"))) 
}
result



rm(list =ls())
library(doParallel)
n_cores = detectCores()
cl <- makeCluster(4)
registerDoParallel(cl)
#source("~/Stat/Twitters/code/Head_file.R")

source("~/Stat/Twitters/code/function.R")
library(smappR)
library(jsonlite)
result <- 
  foreach(i = 1:14,
          .combine = rbind, 
          .packages= c("jsonlite", "smappR")) %dopar% {
            time1 = Sys.time()          
            i_str = ifelse(test = {i>9}, i , paste0("0",i))
            data_folder <-
              paste0("../data/friends_info/edgelist_Feb27/timelines/","t",i_str,"/")
            #data_folder <- paste0("~/Stat/Twitters/t03/")
            output_folder <-
              paste0("../data/friends_info/edgelist_Feb27/timelines_csv/", "t",i_str,"/") 
            processTweets(data_folder, output_folder)
            
            elpase_time <- Sys.time() - time1
            list(i = i,  time = paste0(elpase_time, " ", attr(elpase_time,"units"))) 
          }

#another parallel schema, this will cost a lot of memory - due switch core very often
results <-
  foreach (i = 1:nbreaks,
           .combine = rbind,
           .packages = c("jsonlite", "smappR")) %dopar% {
             #i =1
             p1 <- Sys.time()
             data.str <- NULL
             idx_set = interval *(i-1) +(1:interval)
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
write.csv(results, file = "results05.csv", row.names = F)





## --------------------PART 5: partition adjlists, create edgelist ------------

#parition the graph into three parts.

#updated on April 13, 2017
'
samp <- read.csv("../data/friends_info/edgelist_Feb27/all_samp_idsn_counts.csv", colClasses = c("character"))
all <- read.csv("../data/friends_info/edgelist_Feb27/all_followers_info.csv", colClasses = c("character"))
ids <-readLines("../data/friends_info/edgelist_Feb27/originalData/ids_13M.txt")
idx <- match(all$id_str, ids)
ids <-readLines("../data/friends_info/edgelist_Feb27/originalData/ids_13M.txt")
idx <- match(all$id_str, ids)

#early 3 million
idx3 <- which(idx >10e6) ##9900774 - 12.8 
#middle
idx2 <- which((idx > 3.2e6) * (idx<=10e6) >0)  ##9900774
#early 3.2 million
idx1 <- which(idx <=3.2e6)  #1-3128869 

i1 = 3128868
i2 = 99007743

idx11 <- match(samp$screen_name, all$screen_name[1:i1])
idx11 <- which(!is.na(idx11))
idx22 <- match(samp$screen_name, all$screen_name[(i1+1):i2])
idx22 <- which(!is.na(idx22))
idx33 <- match(samp$screen_name, all$screen_name[(i2+1):length(idx)])
idx33 <- which(!is.na(idx33))

i11 <- 60886
i22 <- 252614
samp1 <- samp[1:i11,]
samp2 <- samp[(i11+1):i22,]
samp3 <- samp[(i22+1):nrow(samp),]

adjlist_all <- readLines("../data/friends_info/edgelist_Feb27/originalData/adjlist_all.txt")
sns_adj  <- gsub("^(.*?),.*", "\\1", adjlist_all) #? important, match the shortest
idx_a<- match(sns_adj, samp$screen_name)
idx_a <- idx_a[!is.na(idx_a)]
adjlist_all <- adjlist_all[idx_a]
length(adjlist_all) #377649
adjlist_1 <- adjlist_all[1:i11]
adjlist_2 <- adjlist_all[(i11+1):i22]
adjlist_3 <- adjlist_all[(i22+1):length(adjlist_all)]

length(adjlist_1 )
[1] 60886
length(adjlist_2)
[1] 191728
length(adjlist_3)
[1] 125035

writeLines(adjlist_1, con = "../data/friends_info/edgelist_Feb27/originalData/adjlist_1.txt")
writeLines(adjlist_2, con = "../data/friends_info/edgelist_Feb27/originalData/adjlist_2.txt")
writeLines(adjlist_3, con = "../data/friends_info/edgelist_Feb27/originalData/adjlist_3.txt")
writeLines(adjlist_all, con = "../data/friends_info/edgelist_Feb27/originalData/adjlist_all.txt")

'

all_samp <- read.csv("../data/friends_info/edgelist_Feb27/all_samp_idsn_counts.csv", 
                     colClasses = c("character"))
ids_13m <- readLines("../data/friends_info/edgelist_Feb27/originalData/ids_13M.txt")

#cut point at 3.2 m down the 13m list
#cut point at 10 m

idx1 <- match( all_samp$id_str, ids_13m[2.8e6:3.5e6])
which(idx1 > 0.4e6)  #value at 0.4e6
i1 <- 60916  # 1:61028 lines, most of values before this are in [0, 3.2m]
 #1- 60916

idx2 <- match(all_samp$id_str, ids_13m[9.4e6:10.1e6])
which(idx2> 0.4e6)   
i2 <- 243608 #252772; #243608 #values beyond this are consistently greater 10m
# 60917 - 243607

#243608: 377651
#1-61028
#61029 : 252772
#252773 : 377809  belong to three groups
# nrow(all_samp)

#head -61028 adjlist_all.txt >> adjlist_1.txt
#head -2552772 adjlist_all.txt|tail - 191744 >> adjlist_2.txt
#tail -125037 adjlist_all.txt >> adjlist_3.txt

samp1 <- all_samp[1:60916,]
samp2 <- all_samp[60917:243607,]
samp3 <- all_samp[243608: 377651,]

# we allow some mismatches. due to account activity. unfriend, friend again and so on.
# there are less than 100 mismatches.
# potentially some overlapping between ids. especially among 1:0.809e6 and 0.809e6 13e6.
# check carefully, there are about 1944 duplicates in terms of id_str

length(unique(ids_13m)) #12998006


#Based on the three sets of adjacency list, we python script is used to create the edgelist
#save as edgelist_1.csv, edgelist_2.csv, edgelit_3.csv
# remove those friends having less thant 20 folllosers among the networks graph1, graph2, graph3





#----------------- download friends info  especially their followers_count ------------
#after process the three graphs using python code 

#manually parallel download friends info -- easy to check and pick from where failed

library(smappR)
friends1 <- readLines("../data/friends_ids_1.txt")
my_oauth_folder <- "./credentials/credential_mixed01/"
friends_info1<- getUsersBatch(ids = friends1, oauth_folder = my_oauth_folder, 
                              include_entities = TRUE, verbose = TRUE, 
                              output = "../data/friends_1.json")
close(output) # 20 tokens, takes about 2 days
write.csv(friends_info1, file = "../data/friends1_info.csv") 
#some times we cannot do this, because we lose the connect or what.
# wait for a whole, cannot write out.



library(smappR)
friends2 <- readLines("../data/friends_ids_2.txt")
my_oauth_folder <- "./credentials/credential_mixed02/"
friends_info2 <- getUsersBatch(ids = friends2, oauth_folder = my_oauth_folder, 
                               include_entities = TRUE, verbose = TRUE, 
                               output = "../data/friends_2.json")
close(output)
write.csv(friends_info2, file = "../data/friends2_info.csv")


library(smappR)
friends3 <- readLines("../data/friends_ids_3.txt")
my_oauth_folder <- "./credentials/credential_mixed03/"
friends_info3 <- getUsersBatch(ids = friends3, oauth_folder = my_oauth_folder, 
                               include_entities = TRUE, verbose = TRUE, 
                               output = "../data/friends_3.json")
close(output)
write.csv(friends_info3, file = "../data/friends3_info.csv")

## due to exceed memory limit. redownload the following
'
grep -n "820335577" friends_ids_2.txt 
4109621:820335577
wc -l friends_ids_2.txt 
4111145 friends_ids_2.txt

grep -n "253513562" friends_ids_3.txtson
2942333:253513562
bigmem04:data$ wc -l friends_ids_3.txt 
3169691 friends_ids_3.txt
'

library(smappR)
friends <- readLines("../data/friends_ids_2.txt")
my_oauth_folder <- "./credentials/credential_mixed03/"
friends_info <- getUsersBatch(ids = friends[4109622:length(friends)], oauth_folder = my_oauth_folder, 
                               include_entities = TRUE, verbose = TRUE, 
                               output = "../data/friends_2_2.json")
close(output)
write.csv(friends_info, file = "../data/friends2_2_info.csv")


library(smappR)
friends <- readLines("../data/friends_ids_3.txt")
my_oauth_folder <- "./credentials/credential_mixed03/"
friends_info <- getUsersBatch(ids = friends[2942334:length(friends)], oauth_folder = my_oauth_folder, 
                              include_entities = TRUE, verbose = TRUE, 
                              output = "../data/friends_3_2.json")
close(output)
write.csv(friends_info, file = "../data/friends3_2_info.csv")









## ----- clean up tweets, apply time constraint, related to trump ---
## some issues about parallel -- encounter memory issue. 
# a lot of cashe if you constant change cores. Better run a long time a single core.
# each folder many small files, good to use parallel.  I decide parallel there. once one folder 
# finishes, I will close manually to clean up the cashe on the those cores. Otherwise, cashe will explode.
# R doesn't release memory when first job is done.
source("Head_file.R")
source("function.R")
#csv_folder <- "../data/friends_info/edgelist_Feb27/timelines_csv_1/t01_part1/"
#tweets1 <-   processTweets_time(csv_folder,end_time = '2016-11-09 00:00:00')


# indivalually 
#given folder name
source("function.R")
library(doParallel)
i=3#7
i_str <- ifelse(i <10, paste0('0',i), i)
csv_folder <- paste0("../data/friends_info/edgelist_Feb27/timelines_csv/t",i_str,"_part2/")
tweets <-   processTweets_time(csv_folder, start_time = '2015-01-01 00:00:00', end_time = '2016-11-09 00:00:00', 
                               n_cores = 1)
write.csv(tweets, file = paste0("../data/friends_info/edgelist_Feb27/timelines_csv_Nov8/t",
                                i_str,"_part2.csv"))
cat("i=", i , "is done! \n\n")

source("function.R")
library(doParallel)
for( i in c(3,7)){
  i_str <- ifelse(i <10, paste0('0',i), i)
  csv_folder <- paste0("../data/friends_info/edgelist_Feb27/timelines_csv/t",i_str,"_part2/")
  tweets <-  processTweets_people(csv_folder,user_id_str = "25073877", n_cores = 2)
  write.csv(tweets, file = paste0("../data/friends_info/edgelist_Feb27/timelines_csv_trump/t",
                                  i_str,"_part2.csv"))
  cat("i=", i , "is done! \n\n")
}


i_str <- ifelse(i <10, paste0('0',i), i)
csv_folder <- paste0("../data/friends_info/edgelist_Feb27/timelines_csv/t",i_str,"_part2/")
tweets <-   processTweets_time(csv_folder,end_time = '2016-11-09 00:00:00', n_cores = 2)
write.csv(tweets, file = paste0("../data/friends_info/edgelist_Feb27/timelines_csv_Nov8/t",
                                i_str,"_part2.csv"))
cat("i=", i , "is done! \n\n")


for (i in 11:14 ){
  i_str <- ifelse(i <10, paste0('0',i), i)
  csv_folder <- paste0("../data/friends_info/edgelist_Feb27/timelines_csv_1/t",i_str,"_part1/")
  tweets <-   processTweets_time(csv_folder,end_time = '2016-11-09 00:00:00', n_cores = 8)
  write.csv(tweets, file = paste0("../data/friends_info/edgelist_Feb27/timelines_csv_1_Nov8/t",
            i_str,"_part1.csv"))
  cat("i=", i , "is done! \n\n")
  rm(tweets)
}

# second part tweets
source("Head_file.R")
source("function.R")
for (i in c(3:14) ){
  i_str <- ifelse(i <10, paste0('0',i), i)
  csv_folder <- paste0("../data/friends_info/edgelist_Feb27/timelines_csv/t",i_str,"_part1/")
  tweets <-   processTweets_time(csv_folder,end_time = '2016-11-09 00:00:00', n_cores = 4)
  write.csv(tweets, file = paste0("../data/friends_info/edgelist_Feb27/timelines_csv_Nov8/t",
                                  i_str,"_part1.csv"))
  cat("i=", i , "is done! \n\n")
  rm(tweets)
}

#trump user_id_str = "25073877"
source("Head_file.R")
source("function.R")
for (i in 1:14 ){
  i_str <- ifelse(i <10, paste0('0',i), i)
  csv_folder <- paste0("../data/friends_info/edgelist_Feb27/timelines_csv_1/t",i_str,"_part1/")
  tweets <-   processTweets_people(csv_folder,user_id_str = "25073877", n_cores = 8)
  write.csv(tweets, file = paste0("../data/friends_info/edgelist_Feb27/timelines_csv_1_trump/t",
                                  i_str,"_part1.csv"))
  cat("i=", i , "is done! \n\n")
  rm(tweets)
}


#trump related -- part3
source("Head_file.R")
source("function.R")
for (i in 1:14 ){
  i_str <- ifelse(i <10, paste0('0',i), i)
  csv_folder <- paste0("../data/friends_info/edgelist_Feb27/timelines_csv_2/t",i_str,"_part3/")
  tweets <-   processTweets_people(csv_folder,user_id_str = "25073877", n_cores = 8)
  write.csv(tweets, file = paste0("../data/friends_info/edgelist_Feb27/timelines_csv_1_trump/t",
                                  i_str,"_part3.csv"))
  cat("i=", i , "is done! \n\n")
  rm(tweets)
}

source("Head_file.R")
source("function.R")
for (i in c(1:14) ){
  i_str <- ifelse(i <10, paste0('0',i), i)
  csv_folder <- paste0("../data/friends_info/edgelist_Feb27/timelines_csv_2/t",i_str,"_part3/")
  tweets <-   processTweets_time(csv_folder,end_time = '2016-11-09 00:00:00', n_cores = 4)
  write.csv(tweets, file = paste0("../data/friends_info/edgelist_Feb27/timelines_csv_1_Nov8/t",
                                  i_str,"_part3.csv"))
  cat("i=", i , "is done! \n\n")
  rm(tweets)
}

#trump related -- part4
source("Head_file.R")
source("function.R")
for (i in 1:14 ){
  i_str <- ifelse(i <10, paste0('0',i), i)
  csv_folder <- paste0("../data/friends_info/edgelist_Feb27/timelines_csv_2/t",i_str,"_part4/")
  tweets <-   processTweets_people(csv_folder,user_id_str = "25073877", n_cores = 4)
  write.csv(tweets, file = paste0("../data/friends_info/edgelist_Feb27/timelines_csv_1_trump/t",
                                  i_str,"_part4.csv"))
  cat("i=", i , "is done! \n\n")
  rm(tweets)
}

source("Head_file.R")
source("function.R")
for (i in c(1:14) ){
  i_str <- ifelse(i <10, paste0('0',i), i)
  csv_folder <- paste0("../data/friends_info/edgelist_Feb27/timelines_csv_2/t",i_str,"_part4/")
  tweets <-   processTweets_time(csv_folder,end_time = '2016-11-09 00:00:00', n_cores = 4)
  write.csv(tweets, file = paste0("../data/friends_info/edgelist_Feb27/timelines_csv_1_Nov8/t",
                                  i_str,"_part4.csv"))
  cat("i=", i , "is done! \n\n")
  rm(tweets)
}
for (i in c(3,7) ){
  i_str <- ifelse(i <10, paste0('0',i), i)
  csv_folder <- paste0("../data/friends_info/edgelist_Feb27/timelines_csv_2/t",i_str,"_part4/")
  tweets <-   processTweets_time(csv_folder,end_time = '2016-11-09 00:00:00', n_cores = 4)
  write.csv(tweets, file = paste0("../data/friends_info/edgelist_Feb27/timelines_csv_1_Nov8/t",
                                  i_str,"_part4.csv"))
  cat("i=", i , "is done! \n\n")
  rm(tweets)
}

