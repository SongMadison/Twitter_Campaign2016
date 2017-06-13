library(Matrix); library(tidyverse)

load("../results_following/result3/analysis.RData")
cluster_info <- read.csv("../results_following/cluster_summary.csv")
cluster_info <- cluster_info[1:50,]
#cat_names <- unique(cluster_info$clust_cat)
cat_names <-c("Trump supporters","conservatives","anti-feminist conservatives","liberals","news&regional",
              "international","non-political", "other countries")
ord <- NULL
for( nn in cat_names){
  ord <- c(ord, which(cluster_info$clust_cat == nn))
}

Z <- membershipM(km_row$cluster)
NZ <-Z%*% Diagonal(dim(Z)[2],km_row$size^(-1))
S <- t(NZ) %*% L%*%t(L)%*% NZ   #averaged similarity
S <- t(Z) %*% L%*%t(L)%*% Z   #total similarity

heatmap(as.matrix(S),main = "using original cluster id")   #exploring more below!!

library(sparseAHC)
hclust = sparseAHC(S, linkage = "average")
plot(hclust)
nclust = 10
clus = cutree(hclust, nclust )
for( i in 1:nclust ){
  cat("i=",i,"\t", cluster_info$cluster_label[which(clus ==i)], "\n")
}

S1 <- S[ord, ord]
image(S1) 
table( match(cluster_info$clust_cat, cat_names))
# 1  2  3  4  5  6  7  8 
# 4  6  2  6  1  2 14 15
ii <- cumsum(table( match(cluster_info$clust_cat, cat_names)))
balloon.plot(round(S1*10^4,4),text = F)+
  geom_abline(slope =0, intercept = 50-ii[1]+1, color = "red") +
  geom_vline(xintercept=ii[1], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[2]+1, color = "red") +
  geom_vline(xintercept=ii[2], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[3]+1, color = "red") +
  geom_vline(xintercept=ii[3], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[4]+1, color = "red") +
  geom_vline(xintercept=ii[4], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[5]+1, color = "red") +
  geom_vline(xintercept=ii[5], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[6]+1, color = "red") +
  geom_vline(xintercept=ii[6], color ="red")+
  geom_abline(slope =0, intercept = 50-ii[7]+1, color = "red") +
  geom_vline(xintercept=ii[7], color ="red")+
  scale_size(range = c(0, 4))+ labs(title = "similariy among clusters (in 1e-4)")





#S1, S2, S3, totally similarity, corresponding to graph 1,2,3
# t(Z) %*% t(L) L %*% Z
load("./similarity50.RData")
cs <- read.csv(file = "../results_following/cluster_summary.csv", colClasses = c("character"))
cs

summary(S1)

a = as.matrix(S1)
rownames(a) = cs$label_note[101:150]; colnames(a) = rownames(a)
rem = c(13,50,46,44,26)
a = a[-rem, -rem]
# sqrt(mean(rowSums(a)))
d = diag(1/sqrt(rowSums(a +1)))
x = d%*%a%*%d
rownames(x) = rownames(a); colnames(x) = colnames(a)
heatmap(x%*%x, symm = T)


a = as.matrix(S2)
rownames(a) = 1:50; colnames(a) = rownames(a)
rownames(a) = cs$label_note[51:100]; colnames(a) = rownames(a)
rownames(a) = cs$clust_cat[51:100]; colnames(a) = rownames(a)
rem = c(37,18,2,27,15,22)
a = a[-rem, -rem]
# sqrt(mean(rowSums(a)))
d = diag(1/sqrt(rowSums(a +1)))
x = d%*%a%*%d
rownames(x) = rownames(a); colnames(x) = colnames(a)
heatmap(x%*%x, symm = T)





a = as.matrix(S3)
rownames(a) = 1:50; colnames(a) = rownames(a)
rownames(a) = cs$label_note[1:50]; colnames(a) = rownames(a)
rownames(a) = cs$clust_cat[51:100]; colnames(a) = rownames(a)
rem = c(41,32,21,11,35,38,30,23,24)
a = a[-rem, -rem]
# sqrt(mean(rowSums(a)))
d = diag(1/sqrt(rowSums(a +1)))
x = d%*%a%*%d
rownames(x) = rownames(a); colnames(x) = colnames(a)
heatmap(x%*%x, symm = T)
