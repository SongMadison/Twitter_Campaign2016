rm(list = ls())

library(data.table)
library(Matrix)
library(tm)
library(rARPACK)  ## fast eigenvector computation
library(irlba)   ## fast svd computation


load("../data/followers_Network/data.RData")
## containing A 

name1 <- rownames(A)
name2 <- colnames(A)
sn_description <- fread("../data/followers_info/jsons/sn_descriptions.csv",
                        colClasses =c("integer","character","character"))

setkey(sn_description, screen_name)
followers <- sn_description[name1]

# the code run are Nove 18, obtained 75416x14 data.frame. 
# # note total name1 has 75938
# library(smappR)
# followers_info <- getUsersBatch( 
#   screen_names = name1,                                
#   include_entities = T, 
#   oauth_folder = "./credentials/credential_mixed2/")
# write.csv(followers_info, file = "../data/followers_Network/followers_info_status.csv", row.names = F)

followers_info <- fread("../data/followers_Network/followers_info_status.csv",
                        stringsAsFactors = F)


## top200 twitters from each follower
output_folder <- "../data/followers_Network/followers_timeline/"
n_Error = 0
n = length(name1)
for (i in 1: 40548){
  #i = 1
  file_name <- paste0(output_folder, name1[i],".json")
  tryCatch(
    getTimeline(filename = file_name,n =200, screen_name = name1[i],
                oauth_folder = "./credentials/credential_mixed3",
                sleep = 0.5, verbose = TRUE), 
    error = function(e){
#       print(n_Error)
#       eval.parent(substitute(n_Error <- n_Error +1))
      message( paste(name1[i], "error occurred"))
      write(name1[i], file = conn, append = TRUE )    
    }) 
  print(paste("XXXXXX -- i = ", i ,'\n'))   
}








text <- followers$'description' # from the downloading file
#text <- paste( followers$'description', followers$'status.text')  #downloaded around Nov 15

#toy.data <- read.csv("toy.csv", stringsAsFactors = F)
#text <- toy.data[,'description']  # use specific set

text <- removeMostPunctuation(text, preserve_intra_word_dashes = T)
text <- removeMostNumbers(text)
vc <- VCorpus( VectorSource(text) ) # just change the vector of strings to corpus
ctrl <- list(#removePunctuation = list(preserve_intra_word_dashes = TRUE),
             stopwords = TRUE,
             removeNumbers = FALSE
             #, stemming = TRUE                    # remove prefix or postfix
             #, bounds = list(global= c(15,Inf))   # remove low-frequency/high frequency words
             #, wordLengths = c(4, 20) # remove short words like 'a' 
             #, weighting = function(x) weightSMART(x, spec = "nnn")
)
tdm <- TermDocumentMatrix(vc, control =  ctrl)  ## term - ducoment matrix
terms <- tdm$dimnames$Terms
print ( sprintf( "after initial cleaning: %s words remains in %s docuemnts",
                 dim(tdm)[1], dim(tdm)[2], '\n') )  


B = spMatrix(i = tdm$i, j = tdm$j, x = tdm$v, nrow = tdm$nrow, ncol  = tdm$ncol)         # frequency count
rownames(B)  = tdm$dimnames$Terms

# remove 's' 
# for word ends with 's', whether the word without 's' is in terms. 
# like designs is in, check the posiition of design, all the locations of design alike are returned
# some are NA, means like "boss" exists, but "bos" not.
idx <- match( gsub(pattern = '(.*)s$', replacement = '\\1', 
                   x= terms[grep('s$',terms)]), terms)
idx1 <- match(paste0(terms[idx[!is.na(idx)]],'s'), terms)    # location of plural terms
idx2 <- match(terms[idx[!is.na(idx)]], terms)   #location of single terms with out s
B[idx2,] <- B[idx1,]+B[idx2,]
terms <- terms[-idx1];  B<- B[terms,]; #update terms, tdm

# remvoe 'ed'
idx <- match( gsub(pattern = '(.*)ed$', replacement = '\\1', x= terms[grep('ed$',terms)]), terms)
idx1 <- match(paste0(terms[idx[!is.na(idx)]],'ed'),terms)
idx2 <- match(terms[idx[!is.na(idx)]], terms)
B[idx2,] <- B[idx1,]+B[idx2,]
terms <- terms[-idx1];  B<- B[terms,]; #update terms, tdm
print (sprintf( "after combining 's','ed' cleaning: %s words remains in %s docuemnts",
                dim(B)[1], dim(B)[2], '\n') )


## keep words that appears in less than 10 document
rownames(B)  = terms
kw = (B>0)+0  # converte counts in term document matrix to  {0,1}
B1 = B[rowSums(kw)>=5,]   
B1 <- t(B1)

load('data4.RData') # vlustering results
Z <- matrix(0, nrow(A),k)
for(i in 1:k){
  Z[which(km_row$cluster == i), i] <- 1  
}
words_by_cluster <- t(Z)%*%B1
terms <- colnames(B1)
library(wordcloud)
i = 3
wordcloud(words = terms, freq = words_by_cluster[i,], 
          min.freq = quantile( words_by_cluster[i,],0.98), random.order = F)



#timeline text data
library(doParallel)
# Find out how many cores are available (if you don't already know)
detectCores()
## [1] 4
# Create cluster with desired number of cores
cl <- makeCluster(8)
# Register cluster
registerDoParallel(cl)
# Find out how many cores are being used
getDoParWorkers()


source('function.R')
library(jsonlite)
path = "../data/followers_Network/followers_timeline2"
files <- list.files(path,full.names = T)

interval = 1000
nbreaks = ceiling(length(files)/interval)
results <- foreach (k = 1: nbreaks, .combine = rbind, .packages= c("jsonlite") 
  ) %dopar% {
  # k= 1
  idx_set = interval*(k-1)+(1:interval)
  if (k == nbreaks){
    idx_set = (interval*(k-1)+1):length(files)
  }
    
  alldata <- NULL
  user_id = NULL
  user_screenName = NULL
  id_str = NULL
  created_at = NULL
  text = NULL
  for (i in idx_set){
    #i=3
    data <- readLines(files[i])
    if (length(data) != 0){
      data.js <- myToJSON(data)
      data.df <- jsonlite::fromJSON(data.js)
      user_id <- c(user_id, unlist(data.df$user$id_str))
      #user_screenName = c(user_screenName, 
      #          rep(gsub(paste0(path,"/(.*).json"), replacement = '\\1',files[i]),
      #              length(data.df$id_str)) )
      #id_str <- c(id_str, data.df$id_str)
      #created_at = c(created_at, data.df$created_at)
      user_screenName = c(user_screenName, 
                   gsub(paste0(path,"/(.*).json"), replacement = '\\1',files[i]))
      text <- c(text, paste(data.df$text, collapse =" "))
    }
    #print(paste("i = ", i, "is done!\n"))
  }
#   alldata <- cbind(user_id ,  user_screenName,
#                    id_str,created_at, text)
  alldata <- cbind(user_screenName, text)
  return (alldata)
}
stopCluster(cl)

#results is not a data.frame yet
results <- data.frame(results)
res= list()
res$user_id <- unlist(results$user_id)
res$user_screenName <- unlist(results$user_screenName)
res$id_str <- unlist(results$id_str)
res$created_at <- unlist(results$created_at)
res$text <- unlist(results$text)
res <- as.data.frame(res, stringsAsFactors = F)
#write.csv(res, file ="../data/followers_Network/tweets_2.csv", row.names = F)


N = length(unique(res$user_screenName))
sns <- character(N)
text <- character(N)
sns[1] <- res$user_screenName[1]

start_i = 1; end_i = 1
i = 1; j = 1
while (j <= length(res$id_str)){
  if (res$user_screenName[j] == sns[i]){
    end_i = j
  }else{
    text[i] <- paste(res$text[start_i:end_i],collapse = " ")
    i = i+1
    sns[i] <- res$user_screenName[j]  #new screen Name[j]
    start_i = j
    end_i = j
    print(end_i)
  }
  j = j+1
  #if (i %%1000 == 0)message("i = " , i)
}
text[i] <- paste(res$text[start_i:end_i], collapse = " ")
data <- data.frame(list(sns =sns, text = text), stringsAsFactors = F)

## combinded two such sets, remove the duplicates
#write.csv(dat, file ="../data/followers_Network/followers_sns_text_combined.csv", row.names = F )

#use the text to explain whether the results  make senses or not
dat <- read.csv("../data/followers_Network/followers_timeline_top200.csv", stringsAsFactors = F)

#break the up file
ss = 2000
nbreaks = ceiling(dim(dat)[1]/ss)
for (i in 1:nbreaks){
   #i =1
   idx = ((i-1)*ss+1):min(ss*i,dim(dat)[1])
   dat1 <- dat[idx,]
   if (i <10) {
     i_str = paste0('0',i)
   }else{
     i_str = i
   }
   filename <- paste0("../data/followers_Network/followers_timeline_combined/",i_str,".csv")
   write.csv(dat1, file = filename, row.names = F)
}                 







dat$text <- gsub("[\t\n]", " ", x = dat$text )
dat$text <- gsub("\\s+", " ", x = dat$text )
dat$text <- removeMostPunctuation(dat$text, preserve_intra_word_dashes = T)
dat$text <- removeMostNumbers(dat$text)

vc <- VCorpus( VectorSource(dat$text) ) # just change the vector of strings to corpus
ctrl <- list(#removePunctuation = list(preserve_intra_word_dashes = TRUE),
  stopwords = TRUE,
  removeNumbers = FALSE
  #, stemming = TRUE                    # remove prefix or postfix
  , bounds = list(global= c(20,Inf))   # remove low-frequency/high frequency words
  , wordLengths = c(4, 25) # remove short words like 'a' 
  #, weighting = function(x) weightSMART(x, spec = "nnn")
)


tdm <- TermDocumentMatrix(vc, control =  ctrl)  ## term - ducoment matrix
terms <- tdm$dimnames$Terms
print ( sprintf( "after initial cleaning: %s words remains in %s docuemnts",
                 dim(tdm)[1], dim(tdm)[2], '\n') )  

B = spMatrix(i = tdm$i, j = tdm$j, x = tdm$v, nrow = tdm$nrow, ncol  = tdm$ncol)         
# frequency count
rownames(B)  = tdm$dimnames$Terms
colnames(B) = dat$user_screenName

tfidf <- weightTfIdf(tdm, normalize = TRUE) #some empty documents
B1 <- spMatrix(i = tfidf$i, j = tfidf$j, x = tfidf$v, nrow = tfidf$nrow, ncol  = tfidf$ncol)         # frequency count
rownames(B1)  = tfidf$dimnames$Terms
colnames(tfidf) = dat$user_screenName
## remove those empty documents <=2
col_sum <- colSums(B)
idx <- which(col_sum <= 5)  ; length(idx) #[1] 0: 2624, total 4371
B <- B[,-idx]
B1 <- B1[,-idx]
dim(B)

clustering <- read.csv("../data/followers_Network/results_20_clusters.csv", 
                       stringsAsFactors = F)
k = max(clustering$cluster)
Z <- matrix(0, nrow(clustering), k)
for(i in  1:k){
  Z[which(clustering$cluster == i), i] <-1
}
x <- match(colnames(B), clustering$sceenNames)
Z1 <- Z[x[which(!is.na(x))], ]
csize <- colSums(Z1)
Z1 <- Z1 %*% Diagonal(k,csize^(-1))
totalwords <- t(Z1)%*%t(B1)
diffwords <- scale(totalwords,center = T, scale = F)
topwords<- matrix("", 20 ,k)
for(i in 1:k){
  topwords[,i] <- terms[order(-diffwords[i,])[1:20]]
}
topwords 

