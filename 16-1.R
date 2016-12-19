library(jsonlite)
data_folder <-
  "../data/followers_Network/followers_timeline_0_45k/"
output_folder <-
  "../data/followers_Network/followers_timeline_0_45k_csv/"

# data_folder <- "../data/followers_Network/followers_timeline_45k_76k/"
# output_folder <- "../data/followers_Network/followers_timeline_45k_76k_csv/"
files = list.files(data_folder)
## missing then filled with 'NA', put in the function.R
# unlistWithNA <- function(field) 

#selected certain variables, put in the function.R
#simplify <- function(dat) , 


interval = 100
nbreaks =  ceiling(length(files) / interval)
library(doParallel)
n_cores = detectCores()
cl <- makeCluster(floor(n_cores / 2))
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
write.csv(results, file = "results1.csv", row.names = F)
