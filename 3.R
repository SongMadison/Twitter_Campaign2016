
library(dplyr)
library(purrr)
library(twitteR)



load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))
#trump_tweets  #trump_tweets_df

library(tidyr)
names(trump_tweets_df)
tweets <- trump_tweets_df %>%
    select(id, statusSource, text, created) %>%
    extract(statusSource, "source", "Twitter for (.*?)<") %>%   #extract function from tidyr
    filter(source %in% c("iPhone", "Android"))




library(lubridate)
library(scales)

tweets %>%
    count(source, hour = hour(with_tz(created, "EST"))) %>%
    mutate(percent = n / sum(n)) %>%
    ggplot(aes(hour, percent, color = source)) +
    geom_line() +
    scale_y_continuous(labels = percent_format()) +
    labs(x = "Hour of day (EST)",
         y = "% of tweets",
         color = "")


tweet_picture_counts <- tweets %>%
    filter(!str_detect(text, '^"')) %>%
    count(source,
          picture = ifelse(str_detect(text, "t.co"),
                           "Picture/link", "No picture/link"))

ggplot(tweet_picture_counts, aes(source, n, fill = picture)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "", y = "Number of tweets", fill = "")



library(tidytext)

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- tweets %>%
    filter(!str_detect(text, '^"')) %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = reg) %>%
    filter(!word %in% stop_words$word,
           str_detect(word, "[a-z]"))

tweet_words



android_iphone_ratios <- tweet_words %>%
    count(word, source) %>%
    filter(sum(n) >= 5) %>%
    spread(source, n, fill = 0) %>%
    ungroup() %>%
    mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
    mutate(logratio = log2(Android / iPhone)) %>%
    arrange(desc(logratio))

# top 10 in android, top 10 in Iphone
plot1 <- android_iphone_ratios %>%
    filter(logratio<=sort(android_iphone_ratios$logratio)[10]  
           | logratio >= sort(android_iphone_ratios$logratio,decreasing = T)[10] ) 

ggplot(plot1, aes( word, logratio, fill=logratio>0 ))+
    geom_bar(stat = "identity", position = "dodge" ) +
    labs(x = "", y = "Number of tweets", fill = "")+
    coord_flip()




nrc <- sentiments %>%
    filter(lexicon == "nrc") %>%
    dplyr::select(word, sentiment)

nrc
