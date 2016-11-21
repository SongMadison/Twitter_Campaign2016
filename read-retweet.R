

datStr <- readLines("outputfile.txt")
datLst <- lapply(datStr, function(x) strsplit(x, split ='\t'))
dat_length <- sapply(datLst, function(x) length(unlist(x))) # check everything is OK, length == 25
dat_df <- as.dataframe(datLst, col)
