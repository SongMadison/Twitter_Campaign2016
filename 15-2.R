
rm(list =ls() )
source('Head_file.R') #import libraries, self-defined functions


load("../data/followers_Network/data.RData")
## containing A 
dim(A)  #75938 365684

name1 <- rownames(A) #75938
# name2 <- colnames(A)
# sn_description <- fread("../data/followers_info/jsons/sn_descriptions.csv",
#                         colClasses =c("integer","character","character"))
# 
# setkey(sn_description, screen_name)
# followers <- sn_description[name1]

# the code run are Nove 18, obtained 75416x14 data.frame. 
# # note total name1 has 75938
# library(smappR)
# followers_info <- getUsersBatch( 
#   screen_names = name1,                                
#   include_entities = T, 
#   oauth_folder = "./credentials/credential_mixed2/")
# write.csv(followers_info, file = "../data/followers_Network/followers_info.csv", row.names = F)

followers_info <- fread("../data/followers_Network/followers_info.csv",
                        stringsAsFactors = F)
#some user are not available,  75416    14
## decription analysis


## top200 twitters from each follower
output_folder <- "../data/followers_Network/followers_timeline11/"
Err_users <- NULL
n = length(name1)
for (i in 45001:n){
  #i = 1
  file_name <- paste0(output_folder, name1[i],".json")
  tryCatch(
    getTimeline(filename = file_name, n =1200, screen_name = name1[i],
                oauth_folder = "./credentials/credential_mixed1",
                sleep = 0.5, verbose = TRUE), 
    error = function(e){
      Err_users <- c(Err_users, name1[i])
      message( paste(name1[i], "error occurred"))
    }
  )
  Err_users <- cbind(Err_users,name1[i])
  print(paste("XXXXXX -- i = ", i ,'\n'))   
}


for (i in 1:45000){
  #i = 1
  file_name <- paste0(output_folder, name1[i],".json")
  tryCatch(
    getTimeline(filename = file_name, n =1200, screen_name = name1[i],
                oauth_folder = "./credentials/credential_mixed",
                sleep = 0.5, verbose = TRUE), 
    error = function(e){
      Err_users <- c(Err_users, name1[i])
      message( paste(name1[i], "error occurred"))
    }
  )
  Err_users <- cbind(Err_users,name1[i])
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
  sdistinctive_words = TRUE,
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


A = spMatrix(i = tdm$i, j = tdm$j, x = tdm$v, nrow = tdm$nrow, ncol  = tdm$ncol)         # frequency count
rownames(A)  = tdm$dimnames$Terms

# remove 's' 
# for word ends with 's', whether the word without 's' is in terms. 
# like designs is in, check the posiition of design, all the locations of design alike are returned
# some are NA, means like "boss" exists, but "bos" not.
idx <- match( gsub(pattern = '(.*)s$', replacement = '\\1', 
                   x= terms[grep('s$',terms)]), terms)
idx1 <- match(paste0(terms[idx[!is.na(idx)]],'s'), terms)    # location of plural terms
idx2 <- match(terms[idx[!is.na(idx)]], terms)   #location of single terms with out s
A[idx2,] <- A[idx1,]+A[idx2,]
terms <- terms[-idx1];  A<- A[terms,]; #update terms, tdm

# remvoe 'ed'
idx <- match( gsub(pattern = '(.*)ed$', replacement = '\\1', x= terms[grep('ed$',terms)]), terms)
idx1 <- match(paste0(terms[idx[!is.na(idx)]],'ed'),terms)
idx2 <- match(terms[idx[!is.na(idx)]], terms)
A[idx2,] <- A[idx1,]+A[idx2,]
terms <- terms[-idx1];  A<- A[terms,]; #update terms, tdm
print (sprintf( "after combining 's','ed' cleaning: %s words remains in %s docuemnts",
                dim(A)[1], dim(A)[2], '\n') )


## keep words that appears in less than 10 document
rownames(A)  = terms
kw = (A>0)+0  # converte counts in term document matrix to  {0,1}
A1 = A[rowSums(kw)>=5,]   
A1 <- t(A1)
