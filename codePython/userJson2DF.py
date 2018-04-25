# extract the given column info, produce a dataframe
#Note
##some elements of the downloaded json items are not valid, use try except to get rid of them

import json
import pandas as pd #toy experiment
import csv
   

def userJson2DF(json_file, cols = None):

    # if cols are not specified, select all the fiels			
    if cols == None:
    	json_list = []
    	with open (json_file) as f1:
            for line in f1:
            	try:
                    line = json.loads(line)
                    json_list.append(line)
            	except:
                    continue
    	data_df = pd.DataFrame(json_list)           
        
    	return data_df    
    
    # selected some specific c=fields	
    data = {}
    for c in cols:
        data[c] = []
    with open (json_file) as f1:
        for line in f1:
            try:
                line = json.loads(line)
            except:
                continue
                
            for c in cols:
                if line[c] =={} or c not in line:
                    data[c].extend([''])
                else:
                    data[c].extend(line[c])
        data_df = pd.DataFrame.from_dict(data)  
        return data_df




#example on small json file

path ="./followers10.json"
toys = [json.loads(line) for line in open(path)]  # list, in the memory , works for small file
#print(toys[1])

cols = ['id_str', 'screen_name', 'name','created_at', 'description', 
        'location', 'time_zone',  'favourites_count','followers_count',  'friends_count',
        'lang', 'listed_count' ,'statuses_count', 'geo_enabled', 'protected' ,'verified'] 

toys_df = userJson2DF(path, cols)
#print(toys_df.columns)
print("numbers of column of dataframe:" + str(len(toys_df.columns)))

toys_df2 = pd.DataFrame(toys)
#print(toys_df2.columns)
print("numbers of column of dataframe:" + str(len(toys_df2.columns)))

toys_df3 = userJson2DF(path)
#print(toys_df3.columns)
print("numbers of column of dataframe:" + str(len(toys_df3.columns)))
