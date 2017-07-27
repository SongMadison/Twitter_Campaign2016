library(data.table)
library(tidyverse)
library(tidytext)
library(Matrix)
# install.packages("glmnet")
library(glmnet)
library(randomForest)


data = fread("https://raw.githubusercontent.com/bpb27/political_twitter_archive/master/realdonaldtrump/realdonaldtrump.csv") %>% as.tbl
# save(file="TrumpTweets.RData", data)
# rm(list  =ls())
# load(file="TrumpTweets.RData")
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
sum(cs>20)
x = dt[,cs>20]  # is it "cheating" to select variables with cs?  


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

yRt = data$retweet_count
fitRt = lm(yRt~sx)
fitRt%>% summary
plot(fitRt)

ssx = scale(sx)  # this centers and scales the columns of the design matrix.  How does this change the betas????


fitRt = lm(log(yRt+1)~ssx)
fitRt%>% summary
plot(fitRt)
#sd in the output should not be bad, especially with a large amount of observations

# How much should we trust those p-values???
#   bootstrap!  
#    Note: bootstrap doesn't always work!  
#          In particular, if estimator is "bad", then bootstrap will also be "bad"  ("bad" = does not have variance ~ 1/n).  For example, the mean of a cauchy distribution...

B = 1000  # this is the number of bootstraps that we should do.  Typically B = 1000 (or more!).  
bootBetas = matrix(NA, nrow = B, ncol = length(fitRt$coef))
colnames(bootBetas) = c("intercept", colnames(ssx))
for(b in 1:B){
  bootSamp = sample(nrow(ssx), nrow(ssx), replace =T)
  bootBetas[b,] = lm(log(yRt[bootSamp]+1)~ssx[bootSamp,])$coef
  
}


par(mfrow = c(3,5), mar = rep(2,4))
for(j in 2:ncol(bootBetas)) hist(bootBetas[,j], main = colnames(ssx)[j-1])
for(j in 2:ncol(bootBetas)) qqnorm(bootBetas[,j], main = colnames(ssx)[j-1])

m = min(bootBetas[,-1])
M = max(bootBetas[,-1])
par(mfrow = c(1,1), las = 1, mar = c(4,5,2,2))
boxplot(bootBetas[,-(1:2)], border = "white", horizontal = T)
for(j in 3:ncol(bootBetas)) points( bootBetas[,j], rnorm(B,j-2,.01), pch  =".")
lines(c(0,0), c(-99,99),col = "red")



# the bootstrap provides a way to estimate the variance
#   of betaHat...
sdBetaHat = apply(bootBetas,2, sd)
meanBetaHat = fitRt$coef # better than: apply(bootBetas,2, mean)

zIntervals = rbind(meanBetaHat - 1.96*sdBetaHat, meanBetaHat + 1.96*sdBetaHat)
Zscores = meanBetaHat / sdBetaHat
2*pnorm(-abs(Zscores))

# I prefer to use the percentile bootstrap.

pIntervals = apply(bootBetas, 2, function(x) return(quantile(x, c(.025, .975))))

# we can also get confidence intervals from lm

lmIntervals  = t(confint(fitRt))

# lets add them to our figure.  
#  z intervals with red
#  p intervals with blue
for(j in 3:ncol(bootBetas)){
  points( zIntervals[,j], rep(j-2,2),col = "red")
  points( pIntervals[,j], rep(j-2,2),col = "blue")
  points( lmIntervals[,j], rep(j-2,2),col = "orange")	
}

#  Compare "theoretical" and "bootstrap" intervals.  
#   What are some advantages of theoretical intervals? 
#   What are some advantages of the bootstrap intervals?


library(splines)
library(gam)
good = c(2,3,8:12)
dat = data.frame(y = log(yRt+1), x = sx[,good])
amfit = gam(y~ 
              ns(x.negative,	df = 3) + 
              ns(x.positive,	df = 3) + 
              ns(x.disgust,	df = 3) + 
              ns(x.anticipation,	df = 3) + 
              ns(x.joy,	df = 3) + 
              ns(x.surprise,	df = 3) + 
              ns(x.afinn,	df = 3) 
            , data =dat)


par(mfrow=c(1,7))
plot(amfit, se=TRUE,col="blue")

interFit = lm(y~.*., data= dat)


sd(amfit$resid)^2 #gets better, but not much
sd(interFit$resid)^2
sd(fitRt$resid)^2
sd(dat$y)^2 #linear model does not explain much in y


# can these measures be used to compare the different models? 
#   why or why not? 
#   If so, which one is the best model?  
#   If not, could you create a better measure?

#no cross validation, it's only training data
#overfitting is a problem when the dimensions/features are much bigger than sample size (when n is small and p is big), 
#when observations are way more than features. 



library(randomForest)

rf =randomForest(y~., data = dat, ntree = 50)
plot(rf)
quartz()

#rf is good when you have a lot of data points, not many features, interactions between features, etc.
