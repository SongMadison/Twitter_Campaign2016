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
str(dt)
dim(dt)
hist(rowSums(dt))
cs = colSums(dt)
hist(log(cs[cs>1]))

# which words get the most "favorites"?
#   put on your scientist hat.  
#   what is a meaningful way to measure this?
#   it is not "trivial" because each tweet has lots of words... 
#   we want to say which *words* get the most favorites.



#  we are going to play with a hold out set.
#  trying to predict:
Y = log(data$favorite_count + 1) 

# before going on, we are going to have a hold out test set...

leaveout = sample(1:nrow(dt), 20000)
cs = colSums(dt[-leaveout,])
# select some good words:
D = Diagonal(ncol(dt), 1/(cs+2)) ###???

# without the +2 ...  score = {average value of Y} over tweets with that word.
#   why +2???
score = t(Y[-leaveout])%*%dt[-leaveout,]%*%D %>% as.vector
score %>% hist

colnames(dt)[which.max(score)]  # what is the "most favorited word"?
colnames(dt)[order(-score)[1:10]]  # what are the top 10?

# how many tweets use these words?  (we might be concerned if less than ~3 tweets used the word...)
cs[order(-score)[1:10]]  


# If only given bag-of-words for a tweet, can we predict how many favorites it will get?
# THIS IS A PLAYGROUND WITH 4 KNOBS.  (you could imagine making this a shiny app!)
# to predict Y, we will change:
# 1) number of features (chosen by top score!)
# 2) ridge vs lambda vs elastic net vs randomForest
# 3) number of "leave out"
# 4) instead of "favorites", use "retweets"!
# 5) incorporate other data in dat to do the prediction! time etc.

# we will compare based on MSE


### LASSO:
d = 10 # num of features
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout]) 
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot

# how many nonzero betas?
colSums(fullSolution$beta != 0) %>% plot 

plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2), type = 'l')


d = 100
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout])
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot
colSums(fullSolution$beta != 0) %>% plot 
plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2), type = 'l')



d = 200
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout])
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot
colSums(fullSolution$beta != 0) %>% plot 
plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))



d = 2000
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout])
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot
plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))


d = 20000
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout])
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot
plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))




### Random forest;  what is "fair comparison" for leaveout
d = 10
rf = randomForest(x = as.matrix(dt[-leaveout,order(-score)[1:d]]), y = Y[-leaveout], ntree = 50)
plot(rf)

# this takes ~1 minute:
d = 100
rf = randomForest(x = as.matrix(dt[-leaveout,order(-score)[1:d]]), y = Y[-leaveout], ntree = 50)
plot(rf)

## sparse data > random forest
##






#### RIDGE
#ridge: alpha = 0
d = 10
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = 0)
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot
colSums(fullSolution$beta != 0) %>% plot 

d = 1000
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = 0)
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot
colSums(fullSolution$beta != 0) %>% plot 

d = 20000
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = 0)
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot


# ELASTIC NET: 
# both ridge and lasso penalty, alpha: ratio of lasso to ridge penalty

d = 20000
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = .1)
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot
plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))

d = 20000
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = .5)
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot
plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))

d = 10000
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = .9)
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot
plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))





# increase the training set!
leaveout = sample(1:nrow(dt), 5000)

d = 20000
fullSolution = glmnet(x = dt[-leaveout,order(-score)[1:d]], y = Y[-leaveout], alpha = .9)
Yhat = predict.glmnet(fullSolution, dt[leaveout,order(-score)[1:d]],s = fullSolution$lambda)
(Y[leaveout] - Yhat)^2 %>%  colMeans  %>%plot
plot(colSums(fullSolution$beta != 0),colMeans((Y[leaveout] - Yhat)^2))


cutt  = 200
lines(cutt*c(1,1), c(-9999,999999))
modelNumber = which(colSums(fullSolution$beta!=0)>cutt)[1]
model = which(fullSolution$beta[,modelNumber]!=0)
best = order(-score)[1:d][model]
length(best)


# what would happen if we used:  as.matrix(dt)[-leaveout,best]?  (hint: don't try it)
leaveout = sample(1:nrow(dt), 10000)
rf = randomForest(x = as.matrix(dt[-leaveout,best]), y = Y[-leaveout], ntree = 50)
plot(rf)



# this is me trying to get fancy... (and utterly failing to dramatically improve predictions)
# let's take the svd to make "better features"
library(rARPACK)
A = dt[-leaveout,best]; Dl = Diagonal(nrow(A), 1/sqrt(rowSums(A)+10)); Dr = Diagonal(ncol(A), 1/sqrt(colSums(A)+10))
s = svds(Dl%*%A%*%Dr, k = 50)
plot(s$d[-1])
u  = t(apply(s$u[,1:35],1,function(x) return(x/sqrt(sum(x^2) + 1/(sqrt(nrow(A))*20)  ))))
rfSVD = randomForest(x = u, y = Y[-leaveout], ntree = 50)
plot(rfSVD)

# because I was curious, I also tried rotating with fastICA 
#   (which is cool, but not at all clear why it might help here.)
# install.packages("fastICA")
library(fastICA)
fica= fastICA(u, 35)
str(fica)
rfICA = randomForest(x = fica$S, y = Y[-leaveout], ntree = 50)
plot(rfICA)

# SQRT Lasso picks a lambda for you without cross-validation!
source("https://raw.githubusercontent.com/cran/RPtests/master/R/sqrt_lasso.R")
sqrtLasso = sqrt_lasso(x = dt[,order(-score)[1:10]], y = Y)

