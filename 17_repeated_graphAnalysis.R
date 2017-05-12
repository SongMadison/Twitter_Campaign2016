
rm (list =ls ())

library(Matrix)
library(irlba)
library(igraph)
library(ggplot2)

#source("function.R")

ResultPath <- "../data/friends_info/edgelist_Feb27/results/"
DataPath <- "../data/friends_info/edgelist_Feb27/"


###-----  Change data and output folder ----------------------------
load("../data/friends_info/edgelist_Feb27/RData/A1.RData")
ResultPath <- paste0(ResultPath, "result3/")
if (!file.exists(ResultPath))  { dir.create(ResultPath)}
#-------------------------------------------------------------------------

dr <- rowSums(A)+1; Dc <- colSums(A)+1
tau1 <- sqrt(mean(dr)); tau2 <- sqrt(mean(Dc))

L <- Diagonal(length(dr), (dr+tau1)^(-1/2)) %*% A %*% Diagonal(length(Dc), (Dc + tau2)^(-1/2)) 
svd <- irlba(L, 50+2) # iterative method to get top 10 singular eig vectors
#normalization of rows
k = 50 # or some other numbers
U <- svd$u[,1:k] %*%Diagonal(k, svd$d[1:k]^(1/2)) # first eigenvectors are more important??

rowN <- rowSums(U*U); rowN <- sqrt(rowN +1e-6)
U1 <- Diagonal(length(rowN), rowN^(-1))%*%U
set.seed(123)
km_row = kmeans(U1, k, nstart = 100, iter.max =50)  # most time-consuming steps!

# representative columns
top = 40
keyfriends_ids <- matrix("", top, k)
scores <- matrix(0, top, k)
for(i in 1:k){
  c_i1 <- colMeans( L[which(km_row$cluster==i), ] )
  c_i2 <- colMeans( L[which(km_row$cluster!=i), ] )
  
  #variance stablization transformation
  c_i <- sqrt(c_i1) - sqrt(c_i2)   
  
  names_tmp <- colnames(A)[order(-c_i)]
  c_i <- c_i[order(-c_i)]
  idx <- which(!is.na(names_tmp))[1:top]   #remove NA if they extist
  scores[,i] <- round(c_i[idx],3)
  keyfriends_ids[,i]<- names_tmp[idx] 
  
  print(c_i["25073877"])
}

#get representative followers info
my_oauth_folder <- "./credentials/credential_mixed04/"
library(smappR)
keyfriends_df <- getUsersBatch(ids = unique(as.vector(keyfriends_ids)), oauth_folder = my_oauth_folder, 
                               include_entities = TRUE, verbose = TRUE, 
                               output = paste0(ResultPath, "keyfriends_k50_40.json"))
#keyfriends_df<- read.csv(paste0(ResultPath, "keyfriends_k50_10.csv"))
keyfriends_sn <- keyfriends_df$screen_name[match(as.vector(keyfriends_ids), keyfriends_df$id_str)]
clustering <- cbind(as.vector(keyfriends_ids),keyfriends_sn, rep(1:k, each = top),
                    rep(km_row$size, each = top),  as.vector(scores))
clustering <- data.frame(clustering, stringsAsFactors = F)
names(clustering) <- c("id_str","screen_name","clusters","Sizes","scores")

keyfriends_df <- data.table(keyfriends_df)
setkey(keyfriends_df, id_str)  #order by screen_name, fast match
result <- cbind(clustering,keyfriends_df[clustering$id_str,])
write.csv(result, file =paste0(ResultPath, "keyfriends_k50_40_info.csv"), row.names = F)

save(list =ls(), file =paste0(ResultPath,"result.RData"))













#igraph approach, node need to be numbers
library(igraph)
G <- graph_from_edgelist( as.matrix(edgelist) )
A <- get.adjacency(G)

friends_info <- read.csv(paste0(output_folder, "friends_info.csv"), stringsAsFactors = F)
deg1 <- degree(G,mode = "out")
deg2 <- degree(G, mode = "in")
g <- graph_from_edgelist(el, directed = F); A = get.adjacency(g)
