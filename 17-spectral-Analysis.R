
rm (list =ls())
library(Matrix)
library(irlba)
library(igraph)
library(ggplot2)
library(data.table)

load("../data/friends_info/edgelist_Feb27/RData/A2.RData")
ResultPath <- paste0("../results/", "result2/")
if(!file.exists(ResultPath)){ 
           dir.create(ResultPath)
           cat(ResultPath ," is created  ....\n")
}




friends_count <- friends_info$followers_count
dr <- rowSums(A); dc <- colSums(A)
nr = nrow(A); nc = ncol(A)

pc <- (dc+1)/(friends_count+1)

pdf(paste0(ResultPath,"sqrt_ratio_weight.pdf"), height = 6, width = 8)
pc.hist <- ggplot(data = data.frame(rt_pc = sqrt(pc)), aes(rt_pc))+
                geom_histogram(binwidth=0.01)+ggtitle("sqrt of columns weights")
pc.hist
dev.off()


# normalized frequency * rownormlized
# borrowed from text mining.
#global weighting, give higher weighting
# based their proportion,
# low occurrence in the sample
#-- help distinguish clusters from each others.
#-- word1, word2, same likelihood to be follower, the by smaller group win

L <-  Diagonal(nr, (dr+sqrt(mean(dc)))^(-1)) %*% A %*% Diagonal(nc, pc^(0.5) * log( nc/(dc+1) )  ) 
#global weighting on columns
svd_L <- irlba(L, nv = 50+2)


#km_row1, L,



# representative columns features
k=10
top = 40
keyfriends_sns <- matrix("", top, k)
scores <- matrix(0, top, k)
for(i in 1:k){
  c_i1 <- colMeans( L[which(km_row1$cluster==i), ] )
  c_i2 <- colMeans( L[which(km_row1$cluster!=i), ] )
  
  #variance stablization transformation
  c_i <- sqrt(c_i1) - sqrt(c_i2)   
  names_tmp <- colnames(L)[order(-c_i)]
  c_i <- c_i[order(-c_i)]
  idx <- which(!is.na(names_tmp))[1:top]   #remove NA if they extist
  scores[,i] <- round(c_i[idx],3)
  keyfriends_sns[,i]<- names_tmp[idx] 
  
  print(c_i["realDonaldTrump"])
}


library(smappR)
if(file.exists(paste0(ResultPath, "keyfriends_k10_40.json"))){
  file.remove(paste0(ResultPath, "keyfriends_k10_40.json"))
  cat(paste0(ResultPath, "keyfriends_k10_40.json") , "is removed! \n")
}
# keyfriends_df <- getUsersBatch(screen_names = unique(as.vector(keyfriends_sns)), 
#                                oauth_folder = "./credentials/credential_mixed01/", 
#                                include_entities = TRUE, verbose = TRUE, 
#                                output = paste0(ResultPath, "keyfriends_k10_40.json"))
# write.csv(keyfriends_df, file = paste0(ResultPath,"keyfriends_df_k10.csv" ),row.names = F)

keyfriends_df <- read.csv(paste0(ResultPath,"keyfriends_df_k10.csv"))
keyfriends_df <- data.table(keyfriends_df)
setkey(keyfriends_df, screen_name)  #order by screen_name, fast match
#keyfriends_df<- read.csv(paste0(ResultPath, "keyfriends_k50_10.csv"))
keyfriends_ids <- keyfriends_df$id_str[match(as.vector(keyfriends_sns), keyfriends_df$screen_name)]
clustering <- cbind(keyfriends_ids,as.vector(keyfriends_sns), rep(1:k, each = top),
                    rep(km_row1$size, each = top),  as.vector(scores))
clustering <- data.frame(clustering, stringsAsFactors = F)
names(clustering) <- c("id_str","screen_name","clusters","Sizes","scores")

result <- cbind(clustering,keyfriends_df[clustering$screen_name])
write.csv(result, file =paste0(ResultPath, "keyfriends_k10_40_info.csv"), row.names = F)

write.csv(x = data.frame(cluser = 1:k, size = km_row1$size), file = paste0(ResultPath,"k10.csv"))


k=50
X <- svd_L$u[,1:k]
X <- t(  apply(X, 1, function(x) x/sqrt(sum(x*x)+1e-4)) )
set.seed(123)
km_row2 <- kmeans(X, centers = k,iter.max = 50, nstart = 200)

'
km_row$size  -- normalization
[1]  2409    84     1     4  2824  6294     4   275   783   158   468     4
[13]   150   144   388  1002    36   704    72   168    34  1448     3     1
[25]   120   694    63 26592  2371     1     1     1     1    93    25    79
[37]  1097   134   701   219   356    10  1191     1   124  2772   877    31
[49]     1   710
'

# representative columns features
top = 40
keyfriends_sns <- matrix("", top, k)
scores <- matrix(0, top, k)
for(i in 1:k){
   c_i1 <- colMeans( L[which(km_row2$cluster==i), ] )
   c_i2 <- colMeans( L[which(km_row2$cluster!=i), ] )

   #variance stablization transformation
   c_i <- sqrt(c_i1) - sqrt(c_i2)   
   names_tmp <- colnames(L)[order(-c_i)]
   c_i <- c_i[order(-c_i)]
   idx <- which(!is.na(names_tmp))[1:top]   #remove NA if they extist
   scores[,i] <- round(c_i[idx],3)
   keyfriends_sns[,i]<- names_tmp[idx] 
     
   print(c_i["realDonaldTrump"])
}


library(smappR)
if(file.exists(paste0(ResultPath, "keyfriends_k50_40.json"))){
   file.remove(paste0(ResultPath, "keyfriends_k50_40.json"))
   cat(paste0(ResultPath, "keyfriends_k50_40.json") , "is removed! \n")
}
keyfriends_df <- getUsersBatch(screen_names = unique(as.vector(keyfriends_sns)),
                               oauth_folder = "./credentials/credential_mixed01/",
                               include_entities = TRUE, verbose = TRUE,
                               output = paste0(ResultPath, "keyfriends_k10_40.json"))
write.csv(keyfriends_df, file = paste0(ResultPath,"keyfriends_df_k50.csv" ),row.names = F)

keyfriends_df <- data.table(keyfriends_df)
setkey(keyfriends_df, screen_name)  #order by screen_name, fast match
#keyfriends_df<- read.csv(paste0(ResultPath, "keyfriends_k50_10.csv"))
keyfriends_ids <- keyfriends_df$id_str[match(as.vector(keyfriends_sns), keyfriends_df$screen_name)]
clustering <- cbind(keyfriends_ids,as.vector(keyfriends_sns), rep(1:k, each = top),
                     rep(km_row2$size, each = top),  as.vector(scores))
clustering <- data.frame(clustering, stringsAsFactors = F)
names(clustering) <- c("id_str","screen_name","clusters","Sizes","scores")
 
result <- cbind(clustering,keyfriends_df[clustering$screen_name])
write.csv(result, file =paste0(ResultPath, "keyfriends_k50_40_info.csv"), row.names = F)
write.csv(x = data.frame(cluser = 1:k, size = km_row1$size), file = paste0(ResultPath,"k50.csv"))


save(L, svd_L, km_row1, km_row2, file = paste0(ResultPath, "result.RData"))






