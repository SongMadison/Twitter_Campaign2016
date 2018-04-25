
rm (list =ls())
library(Matrix)
library(irlba)
library(igraph)
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))
library(data.table)

load("../data/friends_info/edgelist_Feb27/RData/A3.RData")
ResultPath <- paste0("../results_following/", "result3/")

if(!file.exists(ResultPath)){ 
           dir.create(ResultPath)
           cat(ResultPath ," is created  ....\n")
}



#---------------------------- exploratory --------------------------------
# look at the degree distribtuion, potential weighting the graph
# deterimine the eigenvectors and the number of clusters


friends_count <- friends_info$followers_count
dr <- rowSums(A); dc <- colSums(A)
nr = nrow(A); nc = ncol(A)
pc <- (dc+1)/(friends_count+1)

pc.hist <- ggplot(data = data.frame(rt_pc = sqrt(pc)), aes(rt_pc))+
  geom_histogram(binwidth=0.01)+ggtitle("sqrt of columns weights")

dr.hist <- ggplot(data = data.frame(log_row_deg = log2(dr)), aes(log_row_deg))+
  geom_histogram(binwidth=0.5)+ggtitle("log2 row degree")

dc.hist <- ggplot(data = data.frame(log_col_deg = log2(dc)), aes(log_col_deg))+
  geom_histogram(binwidth=0.5)+ggtitle("log2 col degree")


# normalized frequency * rownormlized,  borrowed tfidf stuff from text mining.
# global weighting, give higher columns 
   # related to trump, high following trump ratio of his/her followers
   # give higher weight to those relative low overall frequencies
# normalize the row, 
   # could be normalized to 1
   # introduced a regularizer, with value of sqrt(mean(dr))

L <-  Diagonal(nr, (dr+sqrt(mean(dr)))^(-1)) %*% A %*% Diagonal(nc, pc^(0.5) * log( nc/(dc+1) )  ) 
svd_L <- irlba(L, nv = 50+2)

scree.plot <- ggplot(data = data.frame(x = 1:length(svd_L$d), y = svd_L$d), mapping = aes(x =x, y = y)) +
  geom_point()+
  xlab("index") + ylab("singular values")+ ggtitle("scree plot")



######################## some parameters #################################
k = 50  # n clusters
top = 40 # 40 select features
savePath <- paste0(ResultPath, "analysis.RData")
##########################################################################


# ---- Spectral clustering clustering  -----------------------
X <- svd_L$u[,1:k]
X <- t(  apply(X, 1, function(x) x/sqrt(sum(x*x)+1e-6)) )

set.seed(123)
km_row <- kmeans(X, centers = k, iter.max = 50, nstart = 200)
# order the cluster in the order of second eigenvectors
# km_row <- kmeans(X, centers= km_row$centers[ord(-km_row$centers[,2])],iter.max = 1)


size.plot <- ggplot(data =data.frame(x = 1:k, y = km_row$size), mapping = aes(x,y))+
              geom_bar(stat='identity')+xlab("cluster_id")+ylab("cluster size")

#partition result
clustering <- data.frame(screen_name= rownames(A), cluster = km_row$cluster)
head(clustering)



#-----------------------------------------------------
# representative columns features


#copied from email exchange with Chris Wells, on June 1, 2017
deplorables_str <- c("Mike Cernovich: @Cernovich"
, "Richard Spencer: @RichardBSpencer"
,"Daily Stormer: @rudhum"
,"David Duke: @DrDavidDuke"
,"Paul Joseph Watson: @PrisonPlanet"
,"Alex Jones: @RealAlexJones"
,"Breitbart: @BreitbartNews"
,"Paul Ray Ramsey: @ramzpaul"
,"Gen. Robert E Lee: @Suthen_boy"
,"Ann Kelly: @LadyAodh")
screen_name <- gsub(".*@(.*)", '\\1', deplorables_str)
names <- gsub("(.):.*", '\\', deplorables_str)
deplorables <- data.frame(screen_name, names)

#probportion of followers
deplorable_features <- matrix(0, nrow = k, nrow(deplorables))
idx <- match(deplorables$screen_name, colnames(A))
for (i in 1:k){
  deplorable_features[i,which(!is.na(idx))] <- colMeans(A[km_row$cluster==i, idx[!is.na(idx)]])
}

colnames(deplorable_features) <- paste0(deplorables$screen_name, '-', 
                    friends_info$followers_count[match(deplorables$screen_name, friends_info$screen_name)])
deplorable_features <- data.frame(cluster_id = 1:k, deplorable_features)


keyfriends_sns <- matrix("", top, k)
trump_score <- numeric(k)
score1 <- matrix(0, top, k)
score2 <- matrix(0, top, k)
for(i in 1:k){
  
   c_i0 <- colMeans(L)
   c_i1 <- colMeans( L[which(km_row$cluster==i), ] )
   c_i2 <- colMeans( L[which(km_row$cluster!=i), ] )

   #variance stablization transformation 
   # -- taking into account part of the sparsity
   c_i <- sqrt(c_i1) - sqrt(c_i2)   
   ord <- order(-c_i)[1:top]                    # first top idx
   
   score1[,i] <- round(c_i[ord],4)
   score2[,i] <- round(c_i1[ord]/c_i0[ord], 4)  # proportion over grand mean
   keyfriends_sns[,i]<- colnames(L)[ord]
 
   print(trump_score[i] <- round(c_i1["realDonaldTrump"]/c_i0["realDonaldTrump"],4))
}


library(smappR)
keyfriends_df <- getUsersBatch(screen_names = unique(as.vector(keyfriends_sns)),
                               oauth_folder = "./credentials/credential_mixed02/",
                               include_entities = TRUE, verbose = TRUE)
keyfriends_df$"status.text" <- gsub("\\s+", " ", keyfriends_df$"status.text")  #\s space classes
keyfriends_df$description <- gsub("\\s+", " ", keyfriends_df$description)
keyfriends_df <- data.table(keyfriends_df)
setkey(keyfriends_df, screen_name)  #order by screen_name, fast match
keyfriends_ids <- keyfriends_df$id_str[match(as.vector(keyfriends_sns), keyfriends_df$screen_name)]
result <- data.frame(id_str = keyfriends_ids,
                      screen_name = as.vector(keyfriends_sns), 
                      cluster_id = rep(1:k, each = top),
                      clust_size = rep(km_row$size, each = top), 
                      trump_ratio = rep(trump_score, each = top),  #prop over mean for Trump
                      tran_score = as.vector(score1),
                      proportion = as.vector(score2)
                      )
                               
result50 <- cbind(result,keyfriends_df[result$screen_name])
idx <- c(3,4,5,1,2,6,7,15:18,10:14,19:21)
result50 <- result50[,idx]
'> names(result50)
[1] "cluster_id"        "clust_size"        "trump_ratio"      
[4] "id_str"            "screen_name"       "tran_score"       
[7] "proportion"        "created_at"        "location"         
[10] "lang"              "time_zone"         "name"             
[13] "description"       "followers_count"   "statuses_count"   
[16] "friends_count"     "status.id_str"     "status.created_at"
[19] "status.text
'




#------------------------ update followers_info.csv ---------------------
followers_info <- read.csv("../data/friends_info/edgelist_Feb27/samp3_info.csv", colClasses = c("character"))
headers <- c("id_str","screen_name", "name","description","created_at",
             "lang","location","time_zone", "verified",
             "favourites_count","followers_count", "friends_count","listed_count", "statuses_count")
followers_info <- followers_info[,headers]

#add time order to follow Trump
ids <- readLines("../data/friends_info/edgelist_Feb27/ids_13M_cleaned.txt")
N_ids = length(ids) #12998006 # after remove  some duplicates
idx <- match(followers_info$id_str, ids)
stopifnot(sum(is.na(idx)) == 0)
followers_info$time_order <- N_ids - idx

#add cluster_id 
#load("../results/result3/analysis.RData")  #L, svd_L, km_row, result50,
followers_info$cluster<-rep(NA, nrow(followers_info))
followers_info$cluster[-idx_following] <- km_row$cluster
table(followers_info$cluster,exclude = F)


#add friend_count in the following graph
followers_info$friends_count_samp <- rep(NA, nrow(followers_info))
followers_info$friends_count_samp[-idx_following] <- rowSums(L>0)



#--------- select some representative followers(rows) in clusters --------
#load("../results_following/result1/analysis.RData")
selected_followers <- select_rows(svd_L$u,top = 40,labs = km_row$cluster,verbose = T)
selected_followers$screen_name = rownames(L)[selected_followers$idx]



#--------------------ouput---------------------
clustering <- data.frame(screen_name= rownames(A), cluster = km_row$cluster)
write.csv(clustering, file = paste0(ResultPath,"membership.csv"),row.names = F)
write.csv(result50, file = paste0(ResultPath, "keyfriends_k50_40.csv"), row.names = F)
write.csv(followers_info, file = paste0(ResultPath,"followers_info.csv"), row.names = F)
write.csv(selected_followers, file = paste0(ResultPath,"selected_followers.csv"), row.names = F)
write.csv(deplorable_features, file =paste0(ResultPath, "deplorable_features.csv"), row.names = F)



pdf(paste0(ResultPath,"EDA.pdf"), height = 6, width = 8)
pc.hist
dc.hist
dr.hist
scree.plot
size.plot
dev.off()

save(L, svd_L, km_row, result50, file = savePath)




