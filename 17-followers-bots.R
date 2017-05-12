

input_folder <- "../data/friends_info/edgelist_Feb27/timelines_csv_Nov8/"
output_folder <-"../data/friends_info/edgelist_Feb27/timelines_csv_Nov8/bots/"
#dir.create(output_folder)
files = list.files(input_folder)
files <- files[grep('^t', files)]
dat <- read.csv("../data/friends_info/edgelist_Feb27/timelines_csv_Nov8/t01_part1.csv", colClasses = c("character"))
colnames(dat)
headers = c("id_str","created_at",'source',"user_id_str")
dat1 <- dat[,headers]

length(unique(dat$user_id_str))
#dates -- from 2015-01-01, 2016-11-08
#user_id_str, date, counts

dat1$created_at <- as.Date(dat1$created_at)
i
idx <- which((dat1$created_at >= as.Date('2015-01-01')) * (dat1$created_at <= as.Date('2016-11-08')) >0)

dat$created_at <- as.Date(dat$created_at)
output <- NULL
user_ids <- unique(dat$user_id_str)
for ( id_str in user_ids){
  dat1 <- dat[dat$user_id_str == id_str,]
  counts <- table(dat1$created_at)
  tmp <- data.frame(user_id_str = rep(id_str, length(counts)), date = as.Date(names(counts)), 
                                      tweets_count = as.numeric(counts))
  output <- rbind(output, tmp)
}


dates <- seq(from = as.Date('2015-01-01'), to = as.Date('2017-11-08'), by = 'day')
counts <- 
dates[match(names(counts), dates)]