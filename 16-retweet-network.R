
data1 <-  read.csv("bip1.csv", stringsAsFactors =F)
data1 <- data1[,c("X0","X1")]
data4 <- read.csv("bip4.csv", stringsAsFactors = F)
data1 <- data4[,c('X0',"X1")]

followers <- unique(data1$X0)
tweets <- unique(data1$X1)
length(followers)
length(tweets)
nrow(data1)



library(Matrix)
library(igraph)
i_set = match(data1$X0, unique(data1$X0))
j_set = match(data1$X1, unique(data1$X1))

# g <- graph_from_edgelist (as.matrix(data1), directed = T)
# vcount(g)
# ecount(g)
#A <- get.adjacency(g)

A <- sparseMatrix(i = i_set,j = j_set)
dim(A)
row_deg = rowSums(A)
col_deg = colSums(A)
summary(row_deg)
summary(col_deg)


fs <- follower2[idx[!is.na(idx)]]
commonOrnot <- !is.na(match(follower2, fs))
t.test(row_deg~commonOrnot)

