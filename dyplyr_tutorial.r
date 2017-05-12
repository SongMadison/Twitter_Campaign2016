temp %>% left_join(kmd) %>% gather(`X2015.01.15`: X2016.11.15, key = time, value = tweets) %>% group_by(value, time) %>% summarise(ave_tweets = mean(tweets)) %>% 
  mutate(date = as.Date(substr(time, 2, 11), format = "%Y.%m.%d")) %>%
  ggplot(mapping = aes(x = date, y = ave_tweets, color = value)) + geom_line()