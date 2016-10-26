rm(list=ls())
	library(twitteR)
	library(data.table, quietly = T)
	load("../data/credential.RData")


	screenNames <- read.csv("../data/SNs.txt",header = T, stringsAsFactors = F)


	CMDLine = TRUE
	if (CMDLine == TRUE){
		  
		  arguments <- commandArgs(trailingOnly = TRUE)
			    if (length(arguments) <4) stop("need at least three parameters")
				      k <- as.numeric(arguments[1])
					        start_i <- as.integer(arguments[2])
						  experiment_size <- as.numeric(arguments[3])
						    rnd_column <- as.integer((arguments[4]))
						      if (length(arguments) <5) {buffer_size <- 50
							        }else{buffer_size <- as.integer(arguments[5])  }
		    
		    
	}



if (CMDLine == FALSE){
	  k = 11
		    ## parameters: min_favorite, buffer_size, experiment_size
		    experiment_size = 503
		      start_i = 501
		        buffer_size  = 2
			  rnd_column <-2
}


consumer_key <- credential$consumer_key[k]
consumer_secret <- credential$consumer_secret[k]
access_token <- credential$access_token[k]
access_secret <- credential$access_secret[k]

#setup twitter credentials
setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_secret)


	SNs <- screenNames[,rnd_column]




	cat("k=",k,'\n')
# in some cases, the account no longer exist any more, tryCatch can avoid that 
# kind of problem
# protected can be removed first
	download_friends <- function(id_user){
		  tryCatch(
				      { 
			            user_obj <- getUser(id_user)
				          res <- user_obj$getFriends(retryOnRateLimit = 20)
					        message( paste("\n user", id_user, "has been processed sucessfully \n"))
						      return (res)
							            
		          },error = function(cond){
			             message(cond)
				           cat("\n user", id_user, "has been processed without success! \n")
		               return (list())
			        },warning = function(cond){
				          message(cond)
				          message( paste("\n user", id_user, "has been processed without success!! \n"))
					       return (list())
						  },finally = {
						            #message( paste("\n user", id_user, "has been processed \n"))
						  }
		    ) 
	}


i = start_i

lst_friends = list()
	while ( i <=  experiment_size){
		  cat("star to work on i=", i ,"\n")
			    ratelimit <- getCurRateLimitInfo()
			      limit1 <- ratelimit[ratelimit$resource == "/friends/ids",3]
			        limit2 <- ratelimit[ratelimit$resource == "/account/verify_credentials",3]
				  cat("remaining rate:", limit1,limit2,'\n')
				    while(min(as.numeric(ratelimit$remaining)) < 2){
					        #all the limits not satisfied
					        ratelimit <- getCurRateLimitInfo() 
							    cat("rate limit exceed, retry in one minute! \n")
							        Sys.sleep(60)
								  }
		    id_user = SNs[i]
			      lst_friends[[id_user]] <- download_friends(id_user)
			        
			        if (i %% buffer_size ==0 || i == experiment_size){
					    save(lst_friends, file = paste0("../data/friends_info/friends_",rnd_column,"_",buffer_size,
								                                        ceiling(i/buffer_size),".RData") )
						        lst_friends = list()
							  }
		      i = i+1
	}


