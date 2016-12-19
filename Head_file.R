
.libPaths(c(.libPaths(), "~/Rlibs", "/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-pc-linux-gnu-library/3.3",
            lib.loc = "/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-redhat-linux-gnu-library/3.2"))
#.libPaths()
library(data.table)
library(Matrix)
library(tm)
#library(rARPACK,lib.loc = "/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-redhat-linux-gnu-library/3.2")
## fast eigenvector computation
library(irlba) 
library(ggplot2)
library(jsonlite)
library(wordcloud)
library(smappR)
library(igraph)



source("function.R")