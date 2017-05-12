

topics_info <-  read.csv("../results_topics/unigram/cluster_names_k28_updated.csv", colClasses = c(character))
idx <- grep('^X', colnames(topics_info))
for( i in idx){
  topics_info[,i] <- as.integer(topics_info[,i])
}
ntopics <- nrow(topics_info)
topics_x_time = topics_info[,idx]
dates <- colnames(topics_info)[idx]
p1 <- balloon.plot(as.matrix(topics_x_time),
                   xlabel = dates,
                   ylabel = paste0(1:ntopics,"-",topics_info$cluster_size))+
                  labs(title = paste0("different topics of Trump's tweets in graph 1\n (total ", 
                      nrow(tweets_with_cluster),") during 2015-06-16 and 2016-11-08"))
