

library(rtweet)
library(igraph)
library(stringr)
tweets = searchTwitter("Tom Brady", n=100)
tweets_txt = sapply(tweets, function(x) x$getText())
grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweets, ignore.case=TRUE, value=TRUE)
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweets_txt, ignore.case=TRUE)
head(tweets_txt[rt_patterns],10)
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))

for (i in 1:length(rt_patterns))
{ 
  # get tweet with retweet entity
  twit = tweets[[rt_patterns[i]]]
  # get retweet source 
  poster = str_extract_all(twit$getText(),
                           "(RT|via)((?:\\b\\W*@\\w+)+)") 
  #remove ':'
  poster = gsub(":", "", unlist(poster)) 
  # name of retweeted user
  who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  # name of retweeting user 
  who_retweet[[i]] = rep(twit$getScreenName(), length(poster)) 
}

who_post = unlist(who_post)
who_retweet = unlist(who_retweet)
userList <- lookupUsers(who_retweet)


who_retweet_id = list()
for(i in 1:length(userList))
{
  who_retweet_id[[i]] = rep(userList[[i]]$getId())
}
user_follower_ids=list()


