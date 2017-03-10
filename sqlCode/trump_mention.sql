

insert overwrite local directory 'trump.mention-22'
row format delimited
fields terminated by "\t"

select *
from gh_rc2
where  ( year =2015 or (year =2016 and month <= 10) or
             (year =2016 and month =11 and day <=4) ) 
and ( (lower(text) like "%@realdonaldtrump%")
       or (lower(retweeted_status.user.screen_name) like "%realdonaldtrump%") 
       or (lower(user.screen_name) like "%realdonaldtrump%")
       );
