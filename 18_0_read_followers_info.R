#Read in followers_info
readFollowersData <- function(file_name){
    #file_name = "../combined_data/followers_with_cluster_retweeting_features.csv"
    followers_info <- read.csv(file_name, colClasses = c("character"))
    names(followers_info)[names(followers_info)=='clust_label'] = "cluster_label"
    names(followers_info)[names(followers_info)=='clust_cat'] = "cluster_type"
    #correct variable types
    #factor
    followers_info$cluster <- as.factor(followers_info$cluster)
    #followers_info$cluster_type = as.factor(followers_info$cluster_type)
    followers_info$cluster_type <- factor(followers_info$cluster_type, levels = c("alt right","far right conservatives","Trump supporters","mainstream conservatives",
                                                                                  "mainstream politics", "men's interest","liberals",
                                                                                  "apolitical","other countries"))
    followers_info$period <- factor(followers_info$period, levels =c("pre announcement", "primary election","general election"))
    #data time
    followers_info$created_at <- as.POSIXct(followers_info$created_at) #GMT, not sure.
    followers_info$first_date_timeline <- as.Date(followers_info$first_date_timeline)
    followers_info$last_date_timeline <- as.Date(followers_info$last_date_timeline)
    #non-numerical
    non_numerical <- c("period","cluster","cluster_label","cluster_type", "id_str",  "screen_name", "name", 
                       "description","created_at","lang",
                       "location", "time_zone", "verified", 
                       "first_date_timeline","last_date_timeline")
    non_numerical_ids <- match(non_numerical, names(followers_info))
    for( i in 1:ncol(followers_info)){
        if (!(i %in% non_numerical_ids)){
            followers_info[,i] <- as.numeric(followers_info[,i]) 
        }
    }
    rm(non_numerical,non_numerical_ids)
    return(followers_info)
    
}


followers_info <- readFollowersData("../combined_data/followers_with_cluster_retweeting_features.csv")

