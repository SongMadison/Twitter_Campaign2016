
# this code is utilizing Fred Boehm's data about trump's followers up to Jun, 2016
# there are 8.8M followers in his data sets


setwd("~/Stat/Twitter_Campaign2016/code/")

rm( list = ls() )

library(jsonlite)
#library(rjson)

if(FALSE){
dat1 <-fromJSON("https://api.github.com/users/hadley/orgs")
#toy
toy.json <- '[
{"first":"Song","last":"Wang","age":27},
{"first":["松"],"last":null, "age":30}, 
{"first" : "Jared", "last":"Huling", "age":26}
]'
dat2 <- data.frame(fromJSON(toy.json))
# json_file <- lapply(json_file, function(x){
#   x[sapply(x,is.null)] <- NA
#   unlist(x)
# })
# do.call("rbind",json_file)
}

## example1

# load('./data/trump_followers_id_only.RData')
# length(trump_followers)  #8.8M



trump.json <- readLines("../data/trump_followers_screen-names.json")
#validate()
#Test if a string contains valid JSON. Characters vectors will be 
#collapsed into a single string
validateIds <- unlist(lapply(trump.json, function(x) validate(x)))
table(validateIds) ##FALSE 4, TRUE 358309
trump.json <- trump.json[which(validateIds == TRUE)]
N <- length(trump.json)
cat("N=", N, "\n")
#random sample
set.seed(123) 
sampIds <- sample(N, 1000)
trump_samp <- trump.json[sampIds]
write(trump_samp, file="../data/trump_samp.json")
myToJSON <- function(x){
    return (sprintf("[%s]", paste(x, collapse = ',')))
}
trump_samp.js <- myToJSON(trump_samp)
trump_samp.df <- fromJSON(trump_samp.js)


#sample proportional to its followers
trump.json <-myToJSON(trump.json)
trump_df <- fromJSON(trump.json)
foll_count <- unlist(trump_df$followers_count)
sampleId2 <- sample(x = 1:length(foll_count),size = 1000, prob = foll_count)
cat(length(foll_count),"\n")

SNs <- cbind(rnd_samp = trump_samp.df$screen_name, info_samp = trump_df$screen_name[sampleId2])
write.csv(SNs, file = "../data/SNs.txt",row.names = F)

trump_df 