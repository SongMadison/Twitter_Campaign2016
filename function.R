

myToJSON <- function(data.json, verbose = T){
  # input = data.json == text.file, contains collections of json string. 
     # e.g user json, twitter json etc.
  # output: type : a concatenated string
  # functionality: remove the invalid json entires, concatenate all the individual jsons togehter
     # the ouput string of form will be '[ {}, {}, {}]'

  validateIds <- unlist(lapply(data.json, function(x) validate(x)))
  a <- integer(2)
  a[1] = sum(validateIds == TRUE); a[2] <- sum(validateIds = FALSE)
  if (verbose){
    message("valid json string", a[1], "invalid json string ", a[2])
  }
  if (a[1] >0){
    data.json <- data.json[which(validateIds == TRUE)] 
  }else{
    message("invalid json files, no vlid json strings")
  }
  return (data.json)
  return (sprintf("[%s]", paste(data.json, collapse = ',')))
}


processTweets <- function( data_folder, output_folder){
  
  if (!file.exists(output_folder)) { dir.create(output_folder)}
  
  files = list.files(data_folder)
  interval = 50
  nbreaks =  ceiling(length(files) / interval)
  
  for ( i in 1:nbreaks){
    #i =1
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
    message("i=", i, ', size =', nrow(data.df), "\n")
  }
  
}

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

## missing then filled with 'NA', put in the function.R
# unlistWithNA <- function(field) 


unlistWithNA <- function(field) {
  notnulls <- unlist(lapply(field, function(x)
    ! is.null(x)))
  vect <- rep(NA, length(field))
  vect[notnulls] <- unlist(field)
  return (vect)
}

#dat is output of  #jsonlite::fromJSON(, simplifyDataFrame = T)
simplifyTwitterDF <- function(dat) {
  created_at = unlistWithNA(dat$created_at)
  id_str = unlistWithNA(dat$id_str)
  text = unlistWithNA(dat$text)
  truncated = unlistWithNA(dat$truncated)
  #entities = unlist(dat$entities),
  source = unlistWithNA(dat$source)
  
  in_reply_to_status_id_str = unlistWithNA(dat$in_reply_to_status_id_str)
  in_reply_to_user_id_str = unlistWithNA(dat$in_reply_to_user_id_str)
  #in_reply_to_screen_name = unlistWithNA(dat$in_reply_to_screen_name)
  if (is.null(dat$user)) {
    user_id_str = rep(NA, nrow(dat))
    #user_screen_name = rep(NA, nrow(dat))
  } else{
    user_id_str = unlistWithNA(dat$user$id_str)
    #user_screen_name = unlistWithNA(dat$user$screen_name)
  }
  
  if(is.null(dat$quoted_status)){
    quoted_status_created_at = rep(NA, nrow(dat))
    quoted_status_id_str = rep(NA, nrow(dat))
    quoted_status_text = rep(NA, nrow(dat))
    quoted_status_user_id_str <- rep(NA, nrow(dat))
  }else{
    quoted_status_created_at <- unlistWithNA(dat$quoted_status$created_at)
    quoted_status_id_str <- unlistWithNA(dat$quoted_status$id_str)
    quoted_status_text <- unlistWithNA(dat$quoted_status$text)
    quoted_status_user_id_str <- unlistWithNA(dat$quoted_status$user$id_str)
  }
  if(is.null(dat$retweeted_status)){
    retweet_status_created_at = rep(NA, nrow(dat))
    retweet_status_id_str = rep(NA, nrow(dat))
    retweet_status_text = rep(NA, nrow(dat))
    retweet_status_user_id_str = rep(NA, nrow(dat))
  }else{
    retweet_status_created_at  <- unlistWithNA(dat$retweeted_status$created_at)
    retweet_status_id_str <- unlistWithNA(dat$retweeted_status$id_str)
    retweet_status_text <- unlistWithNA(dat$retweeted_status$text)
    retweet_status_user_id_str = unlistWithNA(dat$retweeted_status$user$id_str)
  }
  if (is.null(dat$place) ||
      is.null(names(dat$place))) {
    # no place obj
    place_name = rep(NA, nrow(dat))
    place_country = rep(NA, nrow(dat))
  } else{
    place_name = unlistWithNA(dat$place$name)
    place_country = unlistWithNA(dat$place$country)
  }
  retweet_count = unlistWithNA(dat$retweet_count)
  favorite_count = unlistWithNA(dat$favorite_count)
  favorited = unlistWithNA(dat$favorited)
  retweeted = unlistWithNA(dat$retweeted)
  lang = unlistWithNA(dat$lang)
  
  lst <-
    list(
      created_at = formatTwDate(created_at),
      id_str = id_str,
      text = gsub("[\n\t\r]", " ", text),
      truncated = truncated,
      source = gsub( ".* rel=\"nofollow\">(.*)</a>", "\\1", source),
      in_reply_to_status_id_str = in_reply_to_status_id_str,
      in_reply_to_user_id_str = in_reply_to_user_id_str,
      #in_reply_to_screen_name = in_reply_to_screen_name,
      quoted_status_created_at = quoted_status_created_at,
      quoted_status_id_str = quoted_status_id_str,
      quoted_status_text = quoted_status_text,
      quoted_status_user_id_str = quoted_status_user_id_str,
      retweet_status_id_str  = retweet_status_id_str ,
      retweet_status_created_at = retweet_status_created_at,
      retweet_status_text = retweet_status_text,
      retweet_status_user_id_str = retweet_status_user_id_str ,
      user_id_str = user_id_str,
      #user_screen_name = user_screen_name,
      place_name = place_name,
      place_country = place_country,
      retweet_count = retweet_count,
      favorite_count = favorite_count,
      favorited = favorited,
      retweeted = retweeted,
      lang = lang
    )
  return(data.frame(lst, stringsAsFactors = F))
}




#----------------------------------


#---------------------------- text cleaning --------------------------------------
removeMostPunctuation<-
  function (x, preserve_intra_word_dashes = FALSE) 
  { ## keep # and @, intra -
    rmpunct <- function(x) {
      x <- gsub("@", "\001", x)
      x <- gsub("#", "\002", x)
      x <- gsub("[[:punct:]]+", "", x)
      x <- gsub("\001", "@", x, fixed = TRUE)
      x <- gsub("\002", " #", x, fixed = TRUE)
      return (x)
    }
    if (preserve_intra_word_dashes) { 
      x <- gsub("(\\w)-(\\w)", "\\1\003\\2", x)
      x <- rmpunct(x)
      gsub("\003", "-", x, fixed = TRUE)
    } else {
      rmpunct(x)
    }
  }

removeMostNumbers <- function(x){
  #remove pure numbers
  gsub(' \\d+ ', " ", x)
} 



removePluralPast<- function(tdm, terms){
  # remove 's' 
  # for word ends with 's', whether the word without 's' is in terms. 
  # like designs is in, check the posiition of design, all the locations of design alike are returned
  # some are NA, means like "boss" exists, but "bos" not.
  idx <- match( gsub(pattern = '(.*)s$', replacement = '\\1', 
                     x= terms[grep('s$',terms)]), terms)
  idx1 <- match(paste0(terms[idx[!is.na(idx)]],'s'), terms)    # location of plural terms
  idx2 <- match(terms[idx[!is.na(idx)]], terms)   #location of single terms with out s
  tdm[idx2,] <- tdm[idx1,]+tdm[idx2,]
  terms <- terms[-idx1];  tdm<- tdm[-idx1,]; #update terms, tdm
  
  # remvoe 'ed'
  idx <- match( gsub(pattern = '(.*)ed$', replacement = '\\1', x= terms[grep('ed$',terms)]), terms)
  idx1 <- match(paste0(terms[idx[!is.na(idx)]],'ed'),terms)
  idx2 <- match(terms[idx[!is.na(idx)]], terms)
  tdm[idx2,] <- tdm[idx1,]+tdm[idx2,]
  terms <- terms[-idx1];  tdm<- tdm[-idx1,]; #update terms, tdm
  
  print (sprintf( "after combining 's','ed' cleaning: %s words remains in %s docuemnts",
                  dim(tdm)[1], dim(tdm)[2], '\n') )
  
}




bag_of_word_tweets <- function(textvector, bigram = F){
  
  text <- removeMostPunctuation(text, preserve_intra_word_dashes = T)
  text <- removeMostNumbers(text)
  
  #remove some special characters or emoji
  text <- gsub("\U0001f1fa", " " ,text); text <- gsub("\U0001f1f8", " ", text)
  text <- gsub("\u274c", " ", text)
  text <- gsub("\\s+", " ", text)
  vc <- VCorpus( VectorSource(text) ) # just change the vector of strings to corpus
  if(bigram){
    BigramTokenizer <- function(x){
      bigram <- unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
      words <- unlist(words(x)) ;tokens <- c(words,bigram)
      return (tokens)  
    }
    ctrl <- list( #removePunctuation = list(preserve_intra_word_dashes = TRUE),
      stopwords = TRUE,
      removeNumbers = FALSE
      #, stemming = TRUE                    # remove prefix or postfix
      , bounds = list(global= c(2,Inf))   # remove low-frequency/high frequency words
      #, wordLengths = c(4, 20) # remove short words like 'a' 
      #, weighting = function(x) weightSMART(x, spec = "nnn")
      ,tokenize = BigramTokenizer
    )
  }else{
    ctrl <- list( #removePunctuation = list(preserve_intra_word_dashes = TRUE),
      stopwords = TRUE,
      removeNumbers = FALSE
      #, stemming = TRUE                    # remove prefix or postfix
      , bounds = list(global= c(2,Inf))   # remove low-frequency/high frequency words
      #, wordLengths = c(4, 20) # remove short words like 'a' 
      #, weighting = function(x) weightSMART(x, spec = "nnn")
      #,tokenize = BigramTokenizer
    )
  }
  tdm <- TermDocumentMatrix(vc, control = ctrl)
  return (tdm)
}



#-----------------------------  evaluate clustering results  ---------------------

VI <- function(confMatrix){
  n <- sum(confMatrix)
  P <- rowSums(confMatrix)/n;  Q <- colSums(confMatrix)/n
  s1 = 0 
  confMatrix <- confMatrix/n
  for (i in 1:length(P)){
    if (P[i] == 0) next
    s1 = s1 - P[i]*log(P[i])
  }
  s2 =0
  for (i in 1:length(Q)){
    if (Q[i] == 0) next
    s2 = s2 - Q[i]*log(Q[i])
  }
  I = 0
  for (i in 1:length(P)){
    for (j in 1:length(Q[i])){
      if (confMatrix[i,j] == 0) next # this will also skip the case P[i], Q[j] ==0
      I = I+  confMatrix[i,j] * log(confMatrix[i,j]/P[i]/Q[j])
  }}
  return  (1 - 0.5* I/s1- 0.5* I/s2)
}

NMI <- function(confMatrix){
  n <- sum(confMatrix)
  P <- rowSums(confMatrix)/n;  Q <- colSums(confMatrix)/n
  s1 = 0 
  confMatrix <- confMatrix/n
  for (i in 1:length(P)){
    if (P[i] == 0) next
    s1 = s1 - P[i]*log(P[i])
  }
  s2 =0
  for (i in 1:length(Q)){
    if (Q[i] == 0) next
    s2 = s2 - Q[i]*log(Q[i])
  }
  I = 0
  for (i in 1:length(P)){
    for (j in 1:length(Q[i])){
      if (confMatrix[i,j] == 0) next # this will also skip the case P[i], Q[j] ==0
      I = I+  confMatrix[i,j] * log(confMatrix[i,j]/P[i]/Q[j])
    }}
  return  (I/sqrt(s1*s2))
}

high.deg.cluster <- function(A, label_row, label_col = NULL){
  
  if (!isSymmetric(A) && !is.null(label_col)){
    #print (is.null(km_col))
    deg.row = rowSums(A); name1 <- rownames(A)
    deg.col = colSums(A); name2 <- colnames(A)
    high.deg.rows = matrix("", 20, k)
    high.deg.cols = matrix("", 20, k)
    for ( i in 1:k){
      deg = deg.row[which(label_row ==i)]
      high.deg.rows[,i] <- name1[ order(-deg)[1:20] ]
      
      deg2 = deg.col[which(label_col ==i)]
      high.deg.cols[,i] <- name2[ order(-deg2)[1:20] ]
    }
    result <- list()
    result$high.deg.rows = high.deg.rows
    result$high.deg.cols = high.deg.cols
    return (result)
  }else{
    deg.row = rowSums(A); name1 <- rownames(A)
    high.deg.rows = matrix("", 20, k)
    for ( i in 1:k){
      deg = deg.row[which(label_row ==i)]
      high.deg.rows[,i] <- name1[order(-deg)[1:20]]
    }
    result = high.deg.rows
    return (result)
  }
}


high.innerProd<- function(X, X.normed, label){
  #X has rownames
  if (is.null(rownames(X)) ){
    stop( "X has no row names!")
  }
  
  k = length(unique(label))  
  #colMeans need this and %*5 needs to be a matrix
  if (min(table(label)) <2){stop("smalleset cluster has size <2") }
  
  high.deg.inner = matrix("", 20, k)
  for( i in 1:k){
    center_i <- colMeans(X.normed[which(label == i),])
    inner_i  =  X[which(label==i),] %*% matrix(center_i)
    name_i = rownames(X)[which(label==i)]
    high.deg.inner[,i] <- name_i[order(-inner_i)[1:20]]
  }
  result = high.deg.inner
  return (result)
}


# ---- ----------------- graph analysis ----------------------------------------
###create a graph from edgeslit
### depends on package igraph
createGraph <- function(el){
  id1 <- unique(el[,1])
  id2 <- unique(el[,2])
  id_all <- c(id1, id2[is.na(match(id2, id1))])
  g <- graph_from_el(as.matrix(el),directed = T)
  A <- get.adjacency(g)
  idx_row <- match(id1, V(g)$name)
  idx_col <- match(id2, V(g)$name )
  A <- A[idx_row, idx_col] 
  res <- list(graph = g, adj = A)
  return (res)
}

#flattenMatrix to i,j, value
flattenMatrix <- function(adjM){
  n = dim(adjM)[1]
  m = dim(adjM)[2]
  data<- data.frame(list(
    row = rep(1:n, times = m),
    col = rep(1:m, each = n),
    value = as.vector(adjM)))
  return (data)
}

DisimA <- function(A, k, taus = c(0,0), rowNorm = T, simultaneous =T, verbose = T, seed = NULL){
  if(!is.null(seed)) {set.seed(seed)
    }else {set.seed(123)}
  nr <- dim(A)[1]; nc <- dim(A)[2]
  labs = list(); labs$row <- rep(NA, nr); labs$col <- rep(NA, nc);
  labs$U<- matrix(0, nr, k); labs$V <- matrix(0, nc, k)
  
  Dr <- rowSums(A); Dc <- colSums(A)
  goodr <- which(Dr != 0); #out degree is zero
  goodc <- which(Dc != 0);

  A <- A[goodr, goodc]
  nr <- dim(A)[1]; nc <- dim(A)[2]
  Dr <- rowSums(A); Dc <- colSums(A)
  tau1 = taus[1]; tau2 <- taus[2]
  L <- Diagonal(nr, (Dr+tau1)^(-1/2)) %*% A %*% Diagonal(nc, (Dc +tau2)^(-1/2))
  if (verbose){
    svd <- irlba::irlba(L, nv = max(40,k+2))
    plot(svd$d, main ="scree plot"); abline(v = k, col= "red", lty=2)
  }else{
    svd <- irlba::irlba(L, nv = k+2)
  } 
  U <- svd$u[,1:k];   labs$U[goodr,] = U
  V <- svd$v[,1:k];   labs$V[goodc,] = V
  if (rowNorm) {norm1 <- rowSums(U*U); norm1 <- sqrt(norm1+1e-6); U <- Diagonal(nr, norm1^(-1)) %*% U}
  if (rowNorm) {norm2 <- rowSums(V*V); norm2 <- sqrt(norm2+1e-6); V <- Diagonal(nc, norm2^(-1)) %*% V}
  if (simultaneous){ 
    X <- rbind(U, V);  km <- kmeans(X, k , iter.max = 30, nstart = 30);
    labs$row[goodr] <- km$cluster[1:nr]; labs$col[goodc] <- km$cluster[(nr+1):(nr+nc)]
  }else{
    km1 <- kmeans(U, k , iter.max = 30, nstart = 30); km2 <- kmeans(V, k , iter.max = 30, nstart = 30)
    labs$row[goodr] <- km1$cluster; labs$column[goodc] <- km2$cluster
  }
  labs
  return(labs)
}
membershipM <- function(labs){
  k <- max(labs,na.rm = T); m <- length(labs)
  Z <- matrix(0, m, k)
  for(i in 1:k){
    Z[which(labs == i), i] <- 1 
  }
  return(Z)
}




graph_component <- function(el, connected = T){
  user_ids <- unique(el[,1]); tweets_ids <- unique(el[,2])
  graph <- graph_from_edgelist(as.matrix(el), directed = T)
  if (connected){
    clustg <- clusters(graph)
    giant_id <- which.max(clustg$csize)
    giant <- induced_subgraph(graph, vids = which(clustg$membership==giant_id)) 
    nodes <- names( V(giant) ); 
    idx_users <- match(user_ids, nodes); idx_users <- idx_users[which(!is.na(idx_users))];
    idx_tweets <- match(tweets_ids, nodes); idx_tweets <- idx_tweets[which(!is.na(idx_tweets))];
    A <- get.adjacency(giant) ; dim(A);sum(A)/nrow(A)
  }else{
    A <- get.adjacency(graph)
    idx_users <- match(user_ids, names(V(graph))); idx_tweets <- match(tweets_ids, names(V(graph)))
  }
  A <- A[idx_users,idx_tweets]; 
  return(A)
}


representative_rows <- function(X, clust, top = 20){
  nr <- dim(X)[1]; k <- dim(X)[2]
  norm1 <- sqrt(1e-6 + rowSums(X*X))
  X1 <- Diagonal(nr,norm1^(-1))%*%X
  Z <- membershipM(clust)
  NZ <- Z %*% Diagonal(k, 1/colSums(Z))
  centers <- t(NZ) %*% X1
  goodrows <- matrix(NA, top, k)
  for( i in 1:k ){
    top1 <- min(top, sum(clust == i))
    values <- X[clust == i, ]%*%as.vector(centers[i,])
    ord <- order(-values)[1:top1]
    goodrows[1:top1,i] <- which(clust == i)[ord]
  }
  return(goodrows)
}

#lab = 1:k; expected = k:1
swichLables <- function(x, lab, expected){
  x1 <- x
  for ( i in 1:length(lab)){
    x1[x == lab[i]] <- expected[i]
  }
  return (x1)
}
