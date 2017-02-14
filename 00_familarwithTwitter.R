
library(smappR)
getTimeline(filename = "../data/followers_timeline/songwang.json", oauth_folder = "./credentials/credential_mixed2/", 
            screen_name = 'swang282')

source("function.R")
dat.str <- readLines("../data/followers_timeline/songwang.json")
dat.json <- myToJSON(dat.str)
dat.df <- jsonlite::fromJSON(dat.json)
data.df <- simplifyTwitterDF(dat.df)



getStatuses("778051770740125696", filename = "tmp1.json", oauth_folder = "./credentials/credential_mixed2/")