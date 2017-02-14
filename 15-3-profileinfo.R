
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


text <- followers_info$'description' # from the downloading file
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


B = spMatrix(i = tdm$i, j = tdm$j, x = tdm$v, nrow = tdm$nrow, ncol  = tdm$ncol)         # frequency count
rownames(B)  = tdm$dimnames$Terms

# remove 's' 
# for word ends with 's', whether the word without 's' is in terms. 
# like designs is in, check the posiition of design, all the locations of design alike are returned
# some are NA, means like "boss" exi
#sts, but "bos" not.
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

