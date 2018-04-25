library(smappR)
my_author_folder = './credentials/credential_mixed01/'

sns <- c("wsmath1", "lovely_pinetree", "swang282")
sn <- sns[2]
getTimeline(paste0("../data/followers_info/jsons/",sn,Sys.Date(),".json"),
            oauth_folder = './credentials/credential_mixed01/',screen_name = sn
                          )

getUsersBatch(screen_names = c("TEN_GOP", "Crystal1Johnson"), oauth_folder = my_author_folder)




#behaviour on Twitter
#retweeting with no comment: RT, retweeted_status
#retweeting with comments - tweets with quoted :  XXX quoted status.   #quoted_status{}
#reply_to_status                  in_reply_to_status{}
#mention sb

#"status_id", "in_reply_to_status_id", "quoted_status_id, "retweeted_status_id"
#EXTRA:
#retweet of {a tweet with quoted status}, in_reply_to = NULL,  quoted_status of the retweet.