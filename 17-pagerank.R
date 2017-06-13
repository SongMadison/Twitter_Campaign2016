

rm(list =ls())

library(Matrix)
library(igraph)
library(tidyverse)



#R = 0.85*(K^{-1}A)^T R + 0.15/N *1_N
# R = 0.85 * *(K^{-1}A)^T R + 0.15 / N * N/(length(vids)) 1_{vids}
my_pagerank <- function(A, vids = NULL, tol = 1e-6, iter.max =100, verbose = T){
  N  <- dim(A)[1]
  D <- rowSums(A);
  zero_ids <- which(D==0); 
  D1 <- D; D1[D>0] <- D[D>0]^(-1)
  P <- t(A) %*% Matrix::Diagonal(N, D1)
  rm(D, Dr, A)
  #P[,which(D==0)] <- 1/N will be dense!!
  if (is.null(vids)){
    J <- as.matrix(rep(1/N, N))
  }else{
    J <- matrix(0, N, 1); J[vids] = 1/(length(vids))
  }
  i = 0
  err = 1
  R <- as.matrix(rep(1/N, N))
  while (i < iter.max && err >tol){
    R1 <- P %*% (0.85 *R)+ 0.85*sum(R[zero_ids])/N +0.15 *J
    #R1 <- R1/sum(R1) #normalization are equivalent
    err = sum(abs(R-R1))
    if (verbose){cat("i=", i,", the L1 error is ", err, "!\n")}
    i = i+1
    R <- R1
  }
  if (err> tol) cat("exceed the maximum", iter.max, "iterations")
  #R <- R/sum(R)
  return (as.numeric(R))
}



## part 1, toy example about my_pagerank
g <- graph(c(
  1, 2, 1, 3, 1, 4, 
  2, 3, 2, 6, 3, 1, 
  3, 5, 4, 2, 4, 1, 
  4, 5, 5, 2, 5, 6, 
  6, 3, 6, 4, 6,7), 
  directed=TRUE)

M0 = get.adjacency(g, sparse = FALSE)

M = t(M0) %*% Diagonal(dim(M0)[1], rowSums(M0)^(-1))
M[,7] <- 1/dim(M)[1]
n = nrow(M)
U = matrix(data=rep(1/n, n^2), nrow=n, ncol=n)
beta=0.85
A1 = beta*M+(1-beta)*U
e = eigen(A1)
v <- e$vec[,1]
v <- as.numeric(v) / sum(as.numeric(v))
v

my_pagerank(M0)

page_rank(g)$vector

#library(expm)  matrix power A %^% 100
n = nrow(M)
U = matrix(data=rep(1/n, n^2), nrow=n, ncol=n)
beta=0.85
A = beta*M+(1-beta)*U
r = matrix(data=rep(1/n, n), nrow=n, ncol=1)
for (i in 1:100) r = A%*% r
as.numeric(r)







#verify ids info between followers and samp
followers <- read.csv("../results_following/followers_with_cluster_info.csv", colClasses = c("character"))
all_samp <- read.csv("../data/friends_info/edgelist_Feb27/all_samp_info.csv", colClasses = c("character"))

idx1 <- match(followers$screen_name, all_samp$screen_name);  stopifnot(sum(is.na(idx1)) == 0)
idx2 <- match(followers$id_str, all_samp$id_str); stopifnot(sum(is.na(idx2))==0)
which(idx1 != idx2)




#Part 2 : network among trump followers: 277725 x 277725
load("../data/friends_info/edgelist_Feb27/RData/adj_followers_ego.RData") #adj_list_ids, sns
library(igraph)
G <- graph_from_adj_list(adj_list_ids)  #max(unlist(adj_list_ids))
ecount(G) #20766474
vcount(G) #377725
Dr = degree(G, mode = 'out'); sum(Dr == 0)/length(Dr) #65.8% are zero
Dc = degree(G, mode = 'in'); sum(Dc == 0)/length(Dc)  #18.2% are zero
D = degree(G, mode = 'all'); sum(D == 0)/length(D)    #12.6 %

pg1 <- page_rank(G)
p0 = rep(0, vcount(G))
p0[which(all_samp$verified =='True')]=1; p0 <- p0/sum(p0)
pg_personalized = page_rank(G, personalized = p0)
output <- data.frame(id = sns, pagerank = pg1$vector, pg_personalized = pg_personalized$vector)
#write.csv(output, file = "../results_following/pagerank1.csv",row.names = F)



## some analysis on the page ranks using verified account.
output <- read.csv("../results_following/pagerank1.csv", colClasses = c("character"))
output$pagerank <- as.numeric(output$pagerank)   
output$pg_personalized <- as.numeric(output$pg_personalized)

#followers <- read.csv("../results_following/followers_with_cluster_info.csv", colClasses = c("character"))
dat = followers[,c("screen_name", "verified")] %>% left_join(output, by = "screen_name") %>% 
  mutate(logpg = log10(pagerank)) %>% mutate(logpg_personalized = log10(pg_personalized))
dat$verified <- as.factor(dat$verified)

ggplot(dat, aes(logpg)) + stat_ecdf(geom="step",aes( color = verified )) +
  labs(x= "pagerank (in log10)", y = "cumulative probability")+facet_grid(verified~.)
ggplot(dat, aes(logpg, ..density..))+geom_histogram(aes(color=verified), 
                                                    bins =100, position = "stack") +facet_grid(verified~.)
ggplot(dat, aes(logpg, color=verified, fill = verified ))+
  geom_density( alpha =0.1,   position = "stack") +facet_grid(verified~.)
#aes(  group = verified),


ggplot(dat, aes(logpg_personalized)) + stat_ecdf(geom="step",aes(group = verified, color = verified )) + 
  labs(x= "log10(personalized pagerank)", y = "cumulative probability")
ggplot(dat, aes(logpg_personalized, ..density..))+geom_histogram(aes(color=verified,  group = verified), 
                                                                 bins =100, position = "stack")+
  labs(x= "log10(personalized pagerank)")
ggplot(dat, aes(logpg_personalized, color=verified, fill = verified ))+
  geom_density(aes(  group = verified), alpha =0.1,   position = "stack") +
  labs(x= "log10(personalized pagerank)")



#followers <- read.csv("../results_following/followers_with_cluster_info.csv", colClasses = c("character"))




#----------------------------------------------
#Part 3 : network among trump followers and their friends, keep(friends followed by >=10)
#324923 x 9118691
load("../data/friends_info/edgelist_Feb27/RData/full_graph.RData") #rr, cc, i_set, j_set
A <- sparseMatrix(i = i_set, j = j_set, x = rep(1, length(i_set)), symmetric = T) 
#A <- sparseMatrix(i = i_set, j = j_set, x = rep(1, length(i_set)))
#save(A, rr, cc, file = "../results_following/full_following.RData")
#use igraph converting is so slow, use my_pagerank
friends <- read.csv("../data/friends_info/data_friends/friends_321_first.csv.csv", colClasses = c("character"))


library(Matrix)
load( "../results_following/full_following.RData")
verified1 <- read.csv("followers_verified.csv", colClasses = c("character"))
verified2 <- read.csv("friends_verified.csv", colClasses = c("character"))

idx1 <- match(verified1$screen_name, c(rr,cc))
idx2 <- match(verified2$id_str, c(rr,cc))
vid1 <- idx1[which(verified1$verified == 'True')]
vid2 <- idx2[which(verified2$verified == 'True')]

#using function, the memory keep increasing
R <- my_pagerank(A, vids = vids, verbose= T)
#i= 0 , the L1 error is  1.855959 !
#i= 1 , the L1 error is  1.367573 !
#i= 2 , the L1 error is  1.151904 !
#i= 3 , the L1 error is  0.9798834 !






R = my_pagerank (A, vids = verbose = T)
output <- data.frame(id = c(rr, cc), pagerank = as.matrix(R))
#write.csv(output, file = "../results_following/pagerank2.csv",row.names = F)
pg2 <- read.csv("../results_following/pagerank2.csv", colClasses = c("character"))
pg2$pagerank <- as.numeric(pg2$pagerank)
#324933, 324934-9443624
pg2[324930:324940,]

#G <- graph_from_edgelist(el,directed = T)
#pg <- page_rank(G)









ggplot(dat, aes(logpg2)) + stat_ecdf(geom="step",aes(group = verified, color = verified )) + 
  labs(x= "pagerank (in log10)", y = "cumulative probability")
ggplot(dat, aes(logpg2, ..density..))+geom_histogram(aes(color=verified,  group = verified), 
                                                     bins =100, position = "stack")
ggplot(dat, aes(logpg2, color=verified, fill = verified ))+
  geom_density(aes(  group = verified), alpha =0.1,   position = "stack")



#some exploratory on 
load("../results_following/result2/analysis.RData")
cluster_info <- read.csv("../results_following/cluster_summary.csv")
cluster_info <- cluster_info[51:100,]
#cat_names <- unique(cluster_info$clust_cat)
cat_names <-c("Trump supporters","conservatives","anti-feminist conservatives","liberals","news&regional",
              "international","non-political", "other countries")
ord <- NULL
for( nn in cat_names){
  ord <- c(ord, which(cluster_info$clust_cat == nn))
}

Z <- membershipM(km_row$cluster)
NZ <- Z%*% Diagonal(dim(Z)[2],km_row$size^(-1))
S <- t(NZ) %*% L%*%t(L)%*% NZ   

S1 <- S[ord, ord]
heatmap(as.matrix(S1))
balloon.plot(S1, text = F)
image(S1)
library(sparseAHC)
hclust = sparseAHC(S, linkage = "average")
plot(hclust)
nclust = 10
clus = cutree(hclust, nclust )
for( i in 1:nclust ){
  cat("i=",i,"\t", cluster_info$cluster_label[which(clus ==i)], "\n")
}

table( match(cluster_info$clust_cat, cat_names))
# 1  2  3  4  5  6  7  8 
# 4  6  2  6  1  2 14 15
ii <- cumsum(table( match(cluster_info$clust_cat, cat_names)))
balloon.plot(round(S1*10^4,4),text = F)+geom_abline(slope =0, intercept = 50-ii[1], color = "red") +
  geom_vline(xintercept=ii[1], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[2], color = "red") +
  geom_vline(xintercept=ii[2], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[3], color = "red") +
  geom_vline(xintercept=ii[3], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[4], color = "red") +
  geom_vline(xintercept=ii[4], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[5], color = "red") +
  geom_vline(xintercept=ii[5], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[6], color = "red") +
  geom_vline(xintercept=ii[6], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[7], color = "red") +
  geom_vline(xintercept=ii[7], color ="red")

rm(list =ls())

membershipM <- function(labs){
  k <- max(labs,na.rm = T); m <- length(labs)
  Z <- matrix(0, m, k)
  for(i in 1:k){
    Z[which(labs == i), i] <- 1 
  }
  return(Z)
}

load("../results_following/result1/analysis.RData")
Z <- membershipM(km_row$cluster)
NZ <-Z%*% Diagonal(dim(Z)[2],km_row$size^(-1))
S1 <- t(Z) %*% L%*%t(L)%*% Z  

save(Z, L, S1, file )



load("../results_following/result2/analysis.RData")
cluster_info <- read.csv("../results_following/cluster_summary.csv")
cluster_info <- cluster_info[51:100,]
#cat_names <- unique(cluster_info$clust_cat)
cat_names <-c("Trump supporters","conservatives","anti-feminist conservatives","liberals","news&regional",
              "international","non-political", "other countries")
ord <- NULL
for( nn in cat_names){
  ord <- c(ord, which(cluster_info$clust_cat == nn))
}

Z <- membershipM(km_row$cluster)
NZ <-Z%*% Diagonal(dim(Z)[2],km_row$size^(-1))
S2 <- t(NZ) %*% L%*%t(L)%*% NZ  
S2 <- t(Z) %*% L%*%t(L)%*% Z  



load("../results_following/result3/analysis.RData")
cluster_info <- read.csv("../results_following/cluster_summary.csv")
cluster_info <- cluster_info[101:150,]
#cat_names <- unique(cluster_info$clust_cat)
cat_names <-c("Trump supporters","conservatives","anti-feminist conservatives","liberals","news&regional",
              "international","non-political", "other countries")
ord <- NULL
for( nn in cat_names){
  ord <- c(ord, which(cluster_info$clust_cat == nn))
}

Z <- membershipM(km_row$cluster)
NZ <-Z%*% Diagonal(dim(Z)[2],km_row$size^(-1))
#S2 <- t(NZ) %*% L%*%t(L)%*% NZ  
S3 <- t(Z) %*% L%*%t(L)%*% Z


save(S1,S2, S3, file = "similarity50.RData")
