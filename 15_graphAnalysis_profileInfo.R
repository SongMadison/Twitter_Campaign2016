rm(list = ls())



#### download timeline of each follower
# library(data.table)
# library(Matrix)
# #Nov 18, 2016
# 
# load("../data/followers_Network/data.RData")
# name1 <- rownames(A)
# name2 <- colnames(A)
# sn_description <- fread("../data/followers_info/jsons/sn_descriptions.csv",
#                         colClasses =c("integer","character","character"))
# 
# setkey(sn_description, screen_name)
# followers <- sn_description[name1]
# 
# ## the code run are Nove 18, obtained 75416x14 data.frame. 
# ## note total name1 has 75938
# # library(smappR)
# # followers_info <- getUsersBatch( 
# #   screen_names = name1,                                
# #   include_entities = T, 
# #   oauth_folder = "./credentials/credential_mixed2/")
# write.csv("../data/followers_Network/followers_info_status.csv", row.names = F)
# 
# 
# ## top200 twitters from each person
# output_folder <- "../data/followers_Network/followers_timeline/"
# ERR <- "../data/followers_Network/followers_timeline/00_ERR.txt"
# conn <- file(ERR)
# n_Error = 0
# n = length(name1)
# for (i in 36529: 40549){
#   #i = 1
#   file_name <- paste0(output_folder, name1[i],".json")
#   tryCatch(
#     getTimeline(filename = file_name,n =200, screen_name = name1[i],
#                 oauth_folder = "./credentials/credential_mixed3",
#                 sleep = 0.5, verbose = TRUE), 
#     error = function(e){
# #       print(n_Error)
# #       eval.parent(substitute(n_Error <- n_Error +1))
#       message( paste(name1[i], "error occurred"))
#       write(name1[i], file = conn, append = TRUE )
#       
#     }) 
# print(paste("XXXXXX -- i = ", i ,'\n'))   
# }
# 
# #nError # not updated in the middle, weird
# 
# ## backward
# 
# library(smappR)
# library(Matrix)
# load("../data/followers_Network/data.RData")
# 
# name1 <- rownames(A)
# name2 <- colnames(A)
# 
# output_folder <- "../data/followers_Network/followers_timeline2/"
# ERR <- "../data/followers_Network/followers_timeline2/00_ERR.txt"
# conn <- file(ERR)
# n_Error = 0
# n = length(name1)
# for (j in 1:n){
#     i = n-j+1
#     file_name <- paste0(output_folder, name1[i],".json")
#   tryCatch(
#     getTimeline(filename = file_name,n =200, screen_name = name1[i],
#                 oauth_folder = "./credentials/credential_mixed3",
#                 sleep = 0.5, verbose = TRUE), 
#     error = function(e){
#       #       print(n_Error)
#       #       eval.parent(substitute(n_Error <- n_Error +1))
#       message( paste(name1[i], "error occurred"))
#       write(name1[i], file = conn,append = TRUE )
#       
#     }) 
#   print(paste("XXXXXX -- j = ", j ,'\n'))   
# }
# 
# file1 <- readLines("../data/followers_Network/file1.txt")
# sn1 <- gsub(pattern = ".*\\s(.*).json$",replacement = "\\1", file1)
# sn1= sn1[-1]
# file2 <- readLines("../data/followers_Network/file2.txt")
# sn2 <- gsub(pattern = ".*\\s(.*).json$",replacement = "\\1", file2)
# sn2= sn2[-1]
# #common files
# x <- sn2[which(!is.na(match(sn2, sn1) ))]
# deleted <- paste0(x,'.json')
# write(deleted,file = "../data/followers_Network/deleted.txt")
# #shell script:
# #  rm -f <$(deleted.txt)
# 
# x <- match(sn2, sn1)
# 
# 




library(tm)
library(Matrix)  ## sparse matrix computation
library(rARPACK)  ## fast eigenvector computation
library(irlba)   ## fast svd computation



followers$text <- paste( followers$'description', followers$'status.text')
data <- read.csv("toy.csv", stringsAsFactors = F)
## bag of words matrix text analysis
text <- data[,'description']  # use specific set
removeMostPunctuation<-
function (x, preserve_intra_word_dashes = FALSE) 
{
  rmpunct <- function(x) {
    x <- gsub("#", "\002", x)
    x <- gsub("[[:punct:]]+", "", x)
    gsub("\002", "#", x, fixed = TRUE)
  }
  if (preserve_intra_word_dashes) { 
    x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
    x <- rmpunct(x)
    gsub("\001", "-", x, fixed = TRUE)
  } else {
    rmpunct(x)
  }
}
removeMostNumbers <- function(x){
      gsub(' \\d ', "", x)
} 
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
# some are NA, means like "boss" exi
sts, but "bos" not.
idx <- match( gsub(pattern = '(.*)s$', replacement = '\\1', x= terms[grep('s$',terms)]), terms)
idx1 <- match(paste0(terms[idx[!is.na(idx)]],'s'),terms)    # location of plural terms
idx2 <- match(terms[idx[!is.na(idx)]], terms)   #location of single terms with out s
B[idx1,] <- B[idx1,]+B[idx2,]
terms <- terms[-idx1];  B<- B[terms,]; #update terms, tdm

# remvoe 'ed'
idx <- match( gsub(pattern = '(.*)ed$', replacement = '\\1', x= terms[grep('ed$',terms)]), terms)
idx1 <- match(paste0(terms[idx[!is.na(idx)]],'ed'),terms)
idx2 <- match(terms[idx[!is.na(idx)]], terms)
B[idx1,] <- B[idx1,]+B[idx2,]
terms <- terms[-idx1];  B<- B[terms,]; #update terms, tdm
print (sprintf( "after combining 's','ed' cleaning: %s words remains in %s docuemnts",
                dim(B)[1], dim(B)[2], '\n') )


## keep words that appears in less than 10 document
rownames(B)  = terms
kw = (B>0)+0  # converte counts in term document matrix to  {0,1}
B1 = B[rowSums(kw)>=10,]   
B1 <- t(B1)

load('data4.RData')
for(i in 1:k){
  Z[which(km_row$cluster == i), i] <-1  
}
words_by_cluster <- t(Z)%*%B1
terms <- colnames(B1)
i = 1
wordcloud(words = terms, freq = word_total[i,], 
          min.freq = quantile( word_total[i,],0.98), random.order = F)




