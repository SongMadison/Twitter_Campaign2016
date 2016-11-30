library(twitteR)
library('RSQLite')
library(DBI)

con <- dbConnect(RSQLite::SQLite(), "../data/twitters_db")
register_db_backend(con)


## after download, save the file into database
load('../follower_samp20000.RData')
store_users_db(ur.followers, table_name = "foll_20k")
store_twitts_db(tweets, table_name ="twitts_1")
load_users_db(as.data.frame= FALSE, table_name = "users")
load_tweets_db(as.data.frame = FALSE, table_name = "tweets")



##how to download the twitts/users and save them into the database directly?