library(jsonlite)
#data_folder <- "../data/followers_Network/followers_timeline1/"
# data_folder <- "../data/followers_Network/followers_timeline_0_45k/"
# output_folder <- "../data/followers_Network/followers_timeline_0_45k_csv/"

data_folder <-
  "../data/followers_Network/followers_timeline_45k_76k/"
output_folder <-
  "../data/followers_Network/followers_timeline_45k_76k_csv/"
files = list.files(data_folder)
## missing then filled with 'NA'
unlistWithNA <- function(field) {
  notnulls <- unlist(lapply(field, function(x)
    ! is.null(x)))
  vect <- rep(NA, length(field))
  vect[notnulls] <- unlist(field)
  return (vect)
}
#selected certain variables

simplify <- function(dat) {
  created_at = unlistWithNA(dat$created_at)
  id_str = unlistWithNA(dat$id_str)
  text = unlistWithNA(dat$text)
  truncated = unlistWithNA(dat$truncated)
  #entities = unlist(dat$entities),
  source = unlistWithNA(dat$source)
  in_reply_to_status_id_str = unlistWithNA(dat$in_reply_to_status_id_str)
  in_reply_to_user_id_str = unlistWithNA(dat$in_reply_to_user_id_str)
  in_reply_to_screen_name = unlistWithNA(dat$in_reply_to_screen_name)
  if (is.null(dat$user)) {
    user_id_str = rep(NA, nrow(dat))
  } else{
    user_id_str = unlistWithNA(dat$user$id_str)
  }
  
  if (is.null(dat$place) ||
      is.null(names(dat$place))) {
    # no place obj
    place_name = rep(NA, nrow(dat))
    place_country = rep(NA, nrow(dat))
  } else{
    place_name = unlistWithNA(dat$place$name)
    place_country = unlistWithNA(dat$place$country)
  }
  is_quote_status = unlistWithNA(dat$is_quote_status)
  retweet_count = unlistWithNA(dat$retweet_count)
  favorite_count = unlistWithNA(dat$favorite_count)
  favorited = unlistWithNA(dat$favorited)
  retweeted = unlistWithNA(dat$retweeted)
  lang = unlistWithNA(dat$lang)
  
  lst <-
    list(
      created_at = created_at,
      id_str = id_str,
      text = text,
      truncated = truncated,
      source = source,
      in_reply_to_status_id_str = in_reply_to_status_id_str,
      in_reply_to_user_id_str = in_reply_to_user_id_str,
      in_reply_to_screen_name = in_reply_to_screen_name,
      user_id_str = user_id_str,
      place_name = place_name,
      place_country,
      is_quote_status = is_quote_status,
      retweet_count =
        retweet_count,
      favorite_count = favorite_count,
      favorited = favorited,
      retweeted = retweeted,
      lang = lang
    )
  return(data.frame(lst, stringsAsFactors = F))
}


interval = 100
nbreaks =  ceiling(length(files) / interval)
library(doParallel)
detectCores()
cl <- makeCluster(5)
registerDoParallel(cl)
results <-
  foreach (i = 1:nbreaks,
           .combine = rbind,
           .packages = c("jsonlite")) %dopar% {
             #i =1
             p1 <- Sys.time()
             data.str <- NULL
             idx_set = interval * (i - 1) + (1:interval)
             if (i == nbreaks) {
               idx_set = (interval * (i - 1) + 1):length(files)
             }
             for (j in idx_set) {
               filepath <-
                 paste0(data_folder, files[j]) #"./data/trump_followers_screen-names.json"#
               data.str <- c(data.str, readLines(filepath))
             }
             idx <- unlist(lapply(data.str, validate))
             data.json <-
               paste0('[', paste0(data.str[idx], collapse = ","), ']')
             
             dat <- fromJSON(data.json, simplifyDataFrame = T)
             data.df <- simplify(dat)
             
             
             #lst2 <- lapply(lst1, function(x) selected(x))
             # lst3 <- lapply(lst2, function(x) as.data.frame(x))
             # data.df <- do.call("rbind", lst3)
             #data.df <- data.frame(do.call("rbind", lapply(lst2, function(x) as.data.frame(x)) ) )
             i_str <-
               paste0(paste0(rep('0', nchar(nbreaks) - nchar(i)), collapse = ""), i, collapse = "")
             write.csv(data.df,
                       file = paste0(output_folder, i_str, ".csv"),
                       row.names = F)
             cat("i=", i, 'size =', nrow(data.df), "\n")
             list(i = i,
                  rows = nrow(data.df),
                  time = Sys.time() - p1)
           }
stopCluster(cl)

write.csv(results, file = "results2.csv", row.names = F)
