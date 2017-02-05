## set the library path
.libPaths(c("/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-pc-linux-gnu-library/3.3","~/R/Rlibs_ubuntu/") ) 

#'/afs/cs.wisc.edu/u/s/o/songwang/R/x86_64-redhat-linux-gnu-library/3.2’ for desk11, bigmem03

# ## Choose USA (IA) as the CRAN mirror
# 
# Mirrors <- getCRANmirrors(all = FALSE, local.only = FALSE)
# chooseCRANmirror(graphics = F, ind = which(Mirrors$Name == 'USA (IA)'))
# 


Packages <- c(
  'boot',
  'car',
  'ggplot2',
  'gridExtra',
  'lattice',
  'plyr',
  'reshape',
  #'reshape2',
  'randomForest', 
  'stats',
  'xts',
  "data.table", "Matrix", "tm", "ggplot2", 'jsonlite',
     'irlba', 'igraph', 'smappR', 'wordcloud', 
    'xlsx'
)
#'rARPACK'
## For loop for requiring packages and installing them if something doesnt exist
for(Package in Packages){
  if(require(package=Package, character.only=T) == F){
    print(paste('Installing', Package))
    try(install.packages(Package, dependencies = TRUE))
  } else{
    #print(paste(Package, 'already exists'))
    #suppressMessages(suppressWarnings(require(xyz)))
    suppressWarnings(suppressMessages(library(package=Package, character.only=T, 
                                               warn.conflicts = FALSE, quietly = T)))  # load quietly
  }
}

## For parallel processing, when passing the list of packages to load
## in all the cores. Could be different from Packages
MyAutoLoads <- Packages



