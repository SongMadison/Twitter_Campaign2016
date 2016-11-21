library(readr)
library(dplyr)
library(stringr)
colnames <- c("id_str",
              "created_at",
              "user.id_str",
              "user.name",
              "user.screen_name",
              "user.description",
              "user.followers_count",
              "user.friends_count",
              "user.verified",
              "geo.type",
              "geo.coordinates",
              "text",
              "retweeted_status.id_str",
              "retweeted_status.created_at",
              "retweeted_status.user.id_str",
              "retweeted_status.user.name",
              "retweeted_status.user.screen_name",
              "retweeted_status.user.description",
              "retweeted_status.user.followers_count",
              "retweeted_status.user.friends_count",
              "retweeted_status.user.verified",
              "retweeted_status.geo.type",
              "retweeted_status.geo.coordinates",
              "retweeted_status.text",
              "retweeted_status.source", 
              "entities.user_mentions.screen_name")
library(lubridate)
dat <- read_delim("onefile.txt", col_names = colnames, escape_backslash = FALSE,
                  delim = "\t", trim_ws = TRUE, escape_double = FALSE) %>% 
  mutate(date = as_date(created_at), hour = hour(created_at), minute = minute(created_at), 
         retweet_date = as_date(retweeted_status.created_at), 
         retweet_hour = hour(retweeted_status.created_at),
         retweet_minute = minute(retweeted_status.created_at))
write_csv(dat, "onefile1.csv")
