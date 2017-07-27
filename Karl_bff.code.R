rm(list = ls())
library(igraph)
library(Matrix)
load(file = url("http://pages.stat.wisc.edu/~karlrohe/classes/data/HighEnergyPhysics.RData"))
# load(file = "data/HighEnergyPhysics.RData")

ls()
getURL(rownames(dw)[100])  
# data comes from high energy physics arxiv papers between January 1993 to April 2003 (124 months)
# https://snap.stanford.edu/data/cit-HepPh.html

summary(citeGraph)  # this is the citation graph using library(igraph)'s representation.
# http://igraph.org/r/
# demo(package="igraph"); demo("centrality")
# http://link.springer.com/book/10.1007%2F978-1-4939-0983-4

dim(dw)  #dw = document x word matrix.  This is a *sparse matrix* using library(Matrix)
# I often call this the "bag of words matrix"
colnames(dw)[1:10]
rownames(dw)[100] %>% getURL
str(dw)


# only some of the papers in the citation graph have abstracts:
which(foundAbs)[1]
sum(foundAbs) == nrow(dw)
length(foundAbs) == length(V(citeGraph))

# I have done a lot of prep to make two files dw and citeGraph.
#   I'm not sharing my code because I wrote it a long time ago 
#   and it is not taking advantage of any of the nice packages for "text processing" 
#   that are now available:
library(tm)
# also:
# library(tidytext) # http://tidytextmining.com

stopwords()  # this is from tm
mean(colnames(dw) %in% stopwords())

# words often have several "forms" and it is often useful to "preprocesses" the text.
# plural, upper/lower case, different forms (e.g "-ed"... use "stems")
# hard way (by hand):
x = colnames(dw)[4]
plural = max(gregexpr("s",x)[[1]]) == nchar(x)
# easy way: use a proper text processing package!

wd = colSums(dw)
hist(wd)  # bah!  (if needed, dev.off() will remove previous plot settings)
hist(wd %>% sqrt)
hist(wd %>% log)
sw = names(wd) %in% stopwords()
boxplot(log(wd)~sw)


#  why are we talking about bag-of-words and graphs in the same day?
#    they are a fundamentally similar in many ways.
#    graphs can be stored as sparse matrices:
Acite = get.adjacency(citeGraph)
str(Acite)
isSymmetric(Acite)
# for what comes next, symmetric graphs are "nicer"...
Acite = Acite + t(Acite)

tmp  = as(dw, "dgTMatrix")
edgeList = cbind(rownames(dw)[tmp@i + 1], colnames(dw)[tmp@j+1])
wordsGraph = from_edgelist(edgeList)  
# this is a special kind of graph... 
#   no edges between documents. 
#   no edges between words.
#   only edges from documents to words. 
#   such graphs are *bipartite graphs*.

#  these types of matrices often have "long tailed degree distributions"
hist(rowSums(dw))
hist(colSums(dw))
hist(colSums(Acite))
# this makes things tricky to deal with!  
#   "normalization" (of some sort) is often a key processing step to deal with this!

# also, these big matrices often reveal "clusters"
#   in the WikiCountyGraph, these aligned with states.
# what could they align with here?
source("https://raw.githubusercontent.com/karlrohe/regSpec/master/regSpec.R")

citeClusters = regspec(Acite,k = 5)
citeClusters %>% str  # lots of stuff... focus on uclst.  it gives the cluster labels.

# a key diagnostic
citeClusters$uclst %>% table  # balanced... nice.  If unbalanced... it is awkward.

wordClusters = regspec(dw,k = 5)
# uclst are clust labels on the *rows* of dw. vclst are cluster labels on the *columns*
str(wordClusters)  

wordClusters$uclst %>% table  # balanced... nice.
wordClusters$vclst %>% table  # balanced... nice.


# to make sense of the citation clusters, use the words!
source('http://pages.stat.wisc.edu/~karlrohe/classes/code/bff.R') # this loads a function bff which gets "the most important words" for each cluster
library(tidyverse)

cc = data_frame(as.character(rownames(Acite)),as.character(citeClusters$uclst)) 
colnames(cc) = c("paper","citeCluster")
cc = cc %>% right_join(data_frame(paper = rownames(dw)))
bff(clusters = cc$citeCluster,dw)


bff(clusters = wordClusters$uclst,dw)

# these things can be very sensitive to processing!
#  we have found that making bag-of-words "binary" instead of a count
#    works better.
#    bff does something slightly different in that case. 
#    finds something which, to my eye, looks more interesting.  
#    I could be wrong!
str(dw)
dwBinary = dw
dwBinary@x = rep(1,length(dwBinary@x))
bff(clusters = cc$citeCluster,dwBinary)
bff(clusters = wordClusters$uclst,dwBinary)

# these partitions find totally different things...
cbind(cc$citeCluster,wordClusters$uclst) %>% as.data.frame %>% table
cbind(cc$citeCluster,wordClusters$uclst) %>% as.data.frame %>% table %>% as.matrix %>% chisq.test
# null model:
cbind(cc$citeCluster,sample(wordClusters$uclst)) %>% as.data.frame %>% table %>% as.matrix %>% chisq.test
# these are so far different that it makes me concerned that perhaps the indexing is off...



