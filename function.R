myValidJSON <- function(data.json){
  
  #validate()
  #Test if a string contains valid JSON. Characters vectors will be 
  #collapsed into a single string
  
  validateIds <- unlist(lapply(data.json, function(x) validate(x)))
  a <- table(validateIds) ##FALSE  TRUE :12 126686 
  message(names(a), " : ", a)
  data.json <- data.json[which(validateIds == TRUE)] 
  return (data.json)
}

myToJSON <- function(data.json){
  
  #validate()
  #Test if a string contains valid JSON. Characters vectors will be 
  #collapsed into a single string
  
  validateIds <- unlist(lapply(data.json, function(x) validate(x)))
  a <- table(validateIds) ##FALSE  TRUE :12 126686 
  nitem = length(validateIds)
  if (nitem >0){
    message(names(a)[1]," : ", a[1], ", ", names(a)[2]," : ", a[2])
  }else{
    message("there is 0 observation")
  }
  data.json <- data.json[which(validateIds == TRUE)] 
  return (sprintf("[%s]", paste(data.json, collapse = ',')))
}

# input:json.data is a list of {  }. having the following format.
#  ouput: '[ {},{} ]'  -- add ',' and []

myConvertJson <- function(json.data){
  json.data <- paste0( json.data[1:(length(json.data)-1)], ",")
  json.data[length(json.data)] <- paste0(json.data[length(json.data)], "]")
  json.data[1] <- paste0("[",json.data[1])
  return(json.data)
}




# kmans diganosis
balloonplot <- function(A, label_row, label_col, probility =F){
  k_ = max(label_row)
  followersZ <- matrix(0, dim(A)[1], k_)
  friendsZ <- matrix(0, dim(A)[2], k_)
  for ( i in 1:k_){
    followersZ[which(label_row ==i), i] = 1
    friendsZ[which(label_col== i), i] = 1
  }
  
  blockM <- t(followersZ) %*% A %*% friendsZ
  if(probility){
    blockM <- Diagonal(k_, colSums(followersZ)^(-1))%*% blockM  %*%
              Diagonal(k_, colSums(friendsZ)^(-1))
  }
  
  data1 <- flattenMatrix(blockM)
  s <- ggplot(data1,  aes(factor(col), factor(row)),fill = factor(row))
  p1 <- s + geom_point(aes(size = sqrt(value), col = factor(row)))
  p1
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

###create a graph from edgeslit
### depends on package igraph
createGraph <- function(edgelist){
  id1 <- unique(edgelist[,1])
  id2 <- unique(edgelist[,2])
  id_all <- c(id1, id2[is.na(match(id2, id1))])
  g <- graph_from_edgelist(as.matrix(edgelist),directed = T)
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


removeMostPunctuation<-
  function (x, preserve_intra_word_dashes = FALSE) 
  { ## keep # and @
    rmpunct <- function(x) {
      x <- gsub("@", "\001", x)
      x <- gsub("#", "\002", x)
      x <- gsub("[[:punct:]]+", "", x)
      x <- gsub("\001", "@", x, fixed = TRUE)
      x <- gsub("\002", "#", x, fixed = TRUE)
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
  gsub(' \\d+ ', " ", x)
} 



removeplural_past<- function(tdm, terms){
  
  
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
