

DataPath <- "../data/friends_info/edgelist_Feb27/"

# ----- part 0: download the Trump followers and take a sample of sample followers --

sn = "realDonaldTrump"
followers_IDs <- getFollowers(sn, cursor = -1, oauth_folder = my_oauth_folder, sleep = 30, 
                              output = paste0("/p/stat/songwang/trump_newFollowers/ids/", 
                                              sn,"_",Sys.Date(),"_ids.txt")) #mutiple 5k

followers_info <- getUsersBatch(ids = followers_IDs, oauth_folder = my_oauth_folder, 
                                include_entities = TRUE, verbose = TRUE, 
                                output = paste0("/p/stat/songwang/trump_newfollowers/jsons/",
                                                sn, "_info.json"))

# jsons files are processed in Python, the following data are created and sample 
# of 244798 followers sampled screen names as below
# sampleTrumpFollowers.ipynb


# '../data/followers_info/jsons/id_counts_2.csv'  6.5 M followers -part 1
# '../data/followers_info/jsons/id_counts.csv'  5.4 M followers - part2





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
samp <- read.csv(file ="../data/followers_info/jsons/sampleFeb17.csv", 
                 header = T, stringsAsFactors = F)
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





###----------------------------PART 4: cleanning the timeline tweets----------
## every 100 .jsons  -->  one .csv file
## this part assumes a lot of memory. 6 processes, takes about 30-40G memory


rm(list =ls())
source("Head_file.R")
source("function.R")
data_folder <-
  "../data/friends_info/edgelist_Feb27/timelines/t051/"
output_folder <-
  "../data/friends_info/edgelist_Feb27/timelines_csv/t05/"
if (!file.exists(output_folder)) { dir.create(output_folder)}


files = list.files(data_folder)
files = files
interval = 100
nbreaks =  ceiling(length(files) / interval)
library(doParallel)
n_cores = detectCores()
cl <- makeCluster(floor(2))
registerDoParallel(cl)
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
write.csv(results, file = "results05.csv", row.names = F)



## --------------------PART 5: -- cleanning and construct graph analyze ------------
#in python, saved as edgelist and adjlist
edgelist <- read.csv("edgelist_1.csv", colClasses = c("character"))
el = edgelist[,1:2]; names(el) <-c("followers_id_str", "friends_id_str")

followers <- unique(el$followers_id_str)
friends <- unique(el$friends_id_str)
i_set <- match(el$followers_id_str, followers)
j_set <- match(el$friends_id_str, friends)

library(Matrix)
A <- sparseMatrix(i = i_set, j = j_set, x= rep(1, length(i_set)))
svd50_1 <- irlba(A, nv =52); print(Sys.time())


followers <- unique(edgelist[,1])

samp1 <- read.csv("../data/friends_info/edgelist_Feb27/sample_Mar20.csv",colClasses =c("character"))
samp2 <- read.csv("../data/friends_info/edgelist_Feb27/sampleFeb17.csv",colClasses =c("character"))
trump <- read.csv("../data/friends_info/edgelist_Feb27/trump_followers_info.csv",colClasses = c("character"))
# id_counts_all <- read.csv("followers_info/jsons/id_counts_all.csv")  
#12877541, Mar22
#id_str, screen_name, protected, follower_count  # removed duplicates
all <- readLines("../data/friends_info/edgelist_Feb27/ids_13M.txt")
id_sn <- trump[,4:5]
rm(trump) #save memory

i1 <- match(all[3.2e6], id_sn$id_str)
# 3130834
i2 <- match(all[10e6], id_sn$id_str)
# 9902771

id_sn1 <- id_sn[1:i1,]
id_sn2 <- id_sn[(i1+1):i2,]
id_sn3 <- id_sn[(i2+1):nrow(id_sn),]

# note that there are some duplicate id_strs
length(ids)
#[1] 12877541
nrow(id_sn)
#[1] 12879486

#Among the newly 0.8M people, about 1900 have ids same as before
#-- 1, those people unfollow and then follow
#-- 2, previous account with that ids suspended, a new account created. 2 in our sample are like this.
'> id_sn[which(id_sn$id_str == "72542082"),]
id_str screen_name
108020  72542082   mithoMaya
1274102 72542082     popup2k
> id_sn[which(id_sn$id_str == "3250613910"),]
id_str    screen_name
167355   3250613910       Neskuler
10056146 3250613910 AdanFashion511
'
samp_sn <- c(samp1$screen_name, samp2$non.random)
idx1 <- match(samp_sn, id_sn1$screen_name)
idx2 <- match(samp_sn, id_sn2$screen_name)
idx3 <- match(samp_sn, id_sn3$screen_name)

idx1[is.na(idx1)] <- 0
idx2[is.na(idx2)] <- 0
idx3[is.na(idx3)] <- 0


sum(idx1*idx2*idx3>0)

'[1] 0
sum(idx1*idx2>0)  # show up in graph1, graph2
[1] 136
sum(idx1*idx3>0) 
[1] 29
sum(idx2*idx3>0)
[1] 1
'


'
nrow(graph3)+nrow(graph1)+nrow(graph2)
[1] 377803
> length(samp_sn)  #6 are missing
[1] 377809
which(idx1+idx2+idx3==0)
[1]    16009 104706
graph1 <- rbind(graph1,samp1[which(idx1+idx2+idx3==0),1:2])
> nrow(graph1)
[1] 60951

> f[,1:2]
id_str   screen_name
1   14295211 TaylorGerring
2 2358442358   iprayanyway
'
library(smappR)
#f<- getUsersBatch(screen_names = samp_sn[which(idx1+idx2+idx3==0)], 
#                  oauth_folder = './credentials/credential_mixed01/' )
f <- data.frame(id_str=c("14295211","2358442358"), screen_name = c("TaylorGerring","iprayanyway"))
graph1 <-rbind( graph1,f[,1:2])
nrow(graph1)+ nrow(graph2)+nrow(graph3)

'> nrow(graph1)
[1] 61001
> nrow(graph2)
[1] 191769
> nrow(graph3)
[1] 125039
'

ii1 <- match(samp1$screen_name, id_sn$screen_name)
which(ii1>i1) #i is the seprating point 3130834
# 61029,61030...

ii2 <- match(samp2$non.random, id_sn$screen_name[(5e6+1):nrow(id_sn)])
samp3 <- samp2[order(ii2),]
ii3 <- match(samp3$non.random, id_sn$screen_name)
which(ii3>i2) #125022
graph3 <- adj1[119762:nrow(samp2)]  #12537 rows

## 61028
## 119762, length(ii3)-119762+1 tail - 125037


## first 61208 lines; last 12537 lines, the middle 61029:252772  191744

