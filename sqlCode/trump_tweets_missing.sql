

insert overwrite local directory 'trump.retweet.long-oct_missing'
row format delimited
fields terminated by "\t"

select regexp_replace(id_str, "[ \t\r\n]+", " "),
regexp_replace(created_at, "[ \t\r\n]+", " "),
regexp_replace(user.id_str, "[ \t\r\n]+", " "), 
regexp_replace(user.name, "[ \t\r\n]+", " "), 
regexp_replace(user.screen_name, "[ \t\r\n]+", " "), 
regexp_replace(user.description, "[ \t\r\n]+", " "), 
regexp_replace(user.followers_count, "[ \t\r\n]+", " "), 
regexp_replace(user.friends_count, "[ \t\r\n]+", " "), 
user.verified, regexp_replace(geo.type, "[ \t\r\n]+", " "), 
geo.coordinates, regexp_replace(text, "[ \t\r\n]+", " "), 
regexp_replace(retweeted_status.id_str, "[ \t\r\n]+", " "), 
regexp_replace(retweeted_status.created_at, "[ \t\r\n]+", " "), 
regexp_replace(retweeted_status.user.id_str, "[ \t\r\n]+", " "), 
regexp_replace(retweeted_status.user.name, "[ \t\r\n]+", " "), 
regexp_replace(retweeted_status.user.screen_name, "[ \t\r\n]+", " "), 
regexp_replace(retweeted_status.user.description, "[ \t\r\n]+", " "), 
regexp_replace(retweeted_status.user.followers_count, "[ \t\r\n]+", " "), 
regexp_replace(retweeted_status.user.friends_count, "[ \t\r\n]+", " "), 
retweeted_status.user.verified, 
regexp_replace(retweeted_status.geo.type, "[ \t\r\n]+", " "), 
retweeted_status.geo.coordinates, 
regexp_replace(retweeted_status.text, "[ \t\r\n]+", " "), 
entities.user_mentions.screen_name

from gh_rc2
where (
	((year = 2016 and month = 10)
	  or (5<= day and day <= 15) ) 
and 
	(	
		(lower(text) like "%rt @realdonaldtrump%") 
		or (lower(retweeted_status.user.screen_name) like "%realdonaldtrump%")
	) );
