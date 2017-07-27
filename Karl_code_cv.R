library(data.table)
library(tidyverse)
library(tidytext)
library(Matrix)
# install.packages("glmnet")
library(glmnet)
library(randomForest)


data = fread("https://raw.githubusercontent.com/bpb27/political_twitter_archive/master/realdonaldtrump/realdonaldtrump.csv") %>% as.tbl
str(data)

text_df <- data_frame(tweet = 1:nrow(data), text = data$text)

# this does a lot of processing! 
#  to lower, remove @ # , . 
#  often these make sense on a first cut.
#  worth revisiting before "final results"!
tt  = text_df %>% unnest_tokens(word, text)
str(tt)

# make the document-term matrix.  
#   I sometimes call this the bag-of-words-matrix.
dt = cast_sparse(tt, tweet, word)
cs = colSums(dt)

# can we predict which phone it came from?
# http://varianceexplained.org/r/trump-tweets/ 
table(data$source) %>% sort
y = data$source == "Twitter for Android"
sum(cs>20)
x = dt[,cs>20]  # is it "cheating" to select variables with cs?  
#no, it's not peaking at the y-s, you can look at x-s
# the risk is that i might not build the right model because I could be throwing away all the good variables 

leaveout = sample(1:nrow(dt), 10000)
# family = "binomial" > binary outcome variables
fit = glmnet(x[-leaveout,], y[-leaveout] , family = "binomial", alpha  = .9)
Yhat = predict.glmnet(fit, x[leaveout,],s = fit$lambda)
(y[leaveout] != (Yhat>0)) %>%  colMeans  %>%plot
plot(colSums(fit$beta != 0), colMeans(y[leaveout] != (Yhat>0)))

cvfit = cv.glmnet(x,y, family = "binomial",
                  type.measure = "class", nfolds = 5, 
                  alpha = .1)
plot(cvfit)
# predict.cv.glmnet
#usually nfoldes = 5 or 10
# At the end of the day, you need to make predictions on a hold outset!  here is how:
# yhat = predict(cvfit, xnew) #  make sure that xnew has the same structure as x in cv.glmnet.

# First mode is a huge step up from null model.  
#   what are the important variables?
modelSeq = 30
fit$beta[fit$beta[,modelSeq]!=0,modelSeq] %>% sort


# repeat the above analysis, trying to predict the tweets that come on saturdays.
y = grepl("Sat",data$created_at) #search through the vector, does the pattern appearn in the element?

cvfit = cv.glmnet(x,y, family = "binomial",type.measure = "class", nfolds = 5, alpha = .1)
plot(cvfit)  # what does this suggest?
mean(y)
#no relationship, not able to predict the Saturday tweets. You can always guess it's not saturday


leaveout = sample(1:nrow(dt), 10000)
fit = glmnet(x[-leaveout,], y[-leaveout] , family = "binomial", alpha  = 0)
Yhat = predict.glmnet(fit, x[leaveout,],s = fit$lambda)
(y[leaveout] != (Yhat>0)) %>%  colMeans  %>%plot
plot(colSums(fit$beta != 0), colMeans(y[leaveout] != (Yhat>0)))

# repeat the analysis for predicting "late-night tweeting"
y = substr(data$created_at, start = 12, stop = 13) %>% 
  as.numeric %in% c(6,11+12)  # this is {daytime = TRUE}.  So,
y = !y
mean(y)
cvfit = cv.glmnet(x,y, family = "binomial",type.measure = "class", nfolds = 5, alpha = .1)
plot(cvfit)  # what does this suggest? no evidence we can tell through tweets


# sentiment analysis!
#   http://tidytextmining.com/sentiment.html
#   sentiment analysis is a way of creating new features from words. 
#   words are assigned values, eg whether they belong to these types:
sentiments$sentiment %>% table %>% names


str(sentiments)
sentiments$sentiment %>% table
get_sentiments("afinn")$score %>% table
get_sentiments("bing")$sentiment %>% table
# there are ~13 different values (T/F, valued, or +/-) assigned to each word.  
#   Lets make a matrix:
#     each row is a tweet
#     one column for each of these 13 "sentiment" features

# > sentiments$sentiment %>% table
# .
# anger anticipation      disgust         fear          joy     negative 
# 1247          839         1058         1476          689         8106 
# positive      sadness     surprise        trust 
# 4318         1191          534         1231 
# > sentiments$sentiment %>% table %>% names
# [1] "anger"        "anticipation" "disgust"      "fear"         "joy"         
# [6] "negative"     "positive"     "sadness"      "surprise"     "trust"       
# > get_sentiments("bing")$sentiment %>% table
# .
# negative positive 
# 4782     2006 
# > get_sentiments("afinn")$score %>% table
# .
# -5  -4  -3  -2  -1   0   1   2   3   4   5 
# 16  43 264 965 309   1 208 448 172  45   5 

# words used in tweets.  aligned with columns of dt!
dictionary = data.frame(word = colnames(dt)) %>% as.tbl



sdic = sentiments$word %>% table %>% sort %>% names %>% rev 
sdesc = sentiments$sentiment %>% table %>% sort %>% names %>% rev 
sdesc = c(NA,sdesc)
tmp = sparseMatrix(i = match(sentiments$word, sdic),
                   j=match(sentiments$sentiment, sdesc))
sdesc[1] = "NA"
sentMat = as.matrix(tmp); colnames(sentMat) = sdesc
sentFrame = data.frame(word = sdic, sentMat) %>% as.tbl %>% 
  left_join(get_sentiments("afinn")) %>% left_join(get_sentiments("bing"))
sentFrame[is.na(sentFrame)] = 0
sentFrame$sentiment[sentFrame$sentiment=="positive"] = 1
sentFrame$sentiment[sentFrame$sentiment=="negative"] = -1
sentFrame$sentiment = as.numeric(sentFrame$sentiment)

feels = left_join(dictionary, sentFrame)

goodWords = which(rowSums(is.na(feels))==0)

x = dt[,goodWords]
feels = feels[goodWords,]
sx = x%*%as.matrix(feels[,-1])
colnames(sx) = c(colnames(feels)[2:12], "afinn", "bing")
sx = as.matrix(sx)


yAndroid = data$source == "Twitter for Android"
fitAndroid = glm(yAndroid~sx, family = binomial(link = "logit")) 
fitAndroid%>% summary

yWeekDay = !grepl("Sat",data$created_at)
# So, positive elements are more likely during weekdays
fitWeekDay = glm(yWeekDay~sx, family = binomial(link = "logit")) 
fitWeekDay %>% summary

yDayTime = substr(data$created_at, start = 12, stop = 13) %>% 
  as.numeric %in% c(6,11+12)  # this is {daytime = TRUE}.  
# So, positive elements are more likely during daytime.
fitDayTime= glm(yDayTime~sx, family = binomial(link = "logit")) 
fitDayTime %>% summary

# misclassification error function:
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

dataDayTime = data.frame(yDayTime = yDayTime, sx =sx)
fitDayTime= glm(yDayTime~., family = binomial(link = "logit"), data = dataDayTime) 
library(boot)
cv.glm(dataDayTime, fitDayTime, cost, K = 5)$delta[1]  
# so is that good?  what is "baseline"????