import pandas as pd
import numpy as np
from os import listdir
from os.path import isfile, join
from datetime import datetime
import sys

def dailyCount(tws, verbose = True):
    '''
    input_file: data frame, which contains columns: user_id_str, created_at
    verobse: bool: print out some extra info 
    rtype:   data frame with three columns -- users_id_str, created_date, count
    '''
    
    tws = tws.loc[:,['user_id_str','created_at']]
    tws['created_at'] = pd.to_datetime(tws['created_at']).dt.date
    
    #loop through the file, count of tweets based on key (user_id, date)
    all_users = {}
    for i in range(len(tws)):
        (u,k) = tws.iloc[i]
        if u in all_users:
            if k in all_users[u]:
                all_users[u][k] += 1
            else:
                all_users[u][k] =1
        else:
            all_users[u] = {}
            all_users[u][k] = 1
            
    #convert dictionary to dataframe      
    all_users_df = None
    for u in all_users:
        aa = all_users[u]
        df = pd.DataFrame.from_dict(aa, orient='index')
        df['user_id_str'] =  u
        df['created_date'] = df.index
        df['count'] = df.iloc[:,0]
        df = df.loc[:,["user_id_str","created_date","count"]]
        all_users_df = pd.concat([all_users_df,df])           
        
    #output as csv file
    if verbose:
        print ("dataframe shape:", all_users_df.shape,
               "number of users:" + str(len(all_users_df.loc[:,'user_id_str'].unique())), 
               "number of tweets:" + str(sum(all_users_df.loc[:,'count'])))    
    return all_users_df    
def dailyCount_helper(files, i):
    start_time = datetime.now()
    f  = files[i]
    input_file = join("../../data/friends_info/edgelist_Feb27/timelines_csv_simplified/", f)
    tws = pd.read_csv(input_file, dtype =str, index_col = None) # no index
    #tws = tws.iloc[:1000]
    result = dailyCount(tws)
    result.to_csv("../../data/friends_info/edgelist_Feb27/timelines_csv_dailycounts/"+f, index = False, header=False) 
    #without header, index, easier to combine.
    print(files[i]+" is done.  time lapse: "+str(datetime.now() - start_time))
    
if __name__ == '__main__':
    i = int(sys.argv[1])
    files = listdir("../../data/friends_info/edgelist_Feb27/timelines_csv_simplified/")
    files.sort()    
    dailyCount_helper(files, i) 
