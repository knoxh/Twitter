install.packages("rtweet")
library(rtweet)
library(httpuv)

source("rtweet_authenticate.R")

tweetsdf <- search_tweets("Tom Brady", n=180, include_rts = TRUE)

tweetstext <- tweetsdf$text
#for (i in 1:nrow(tweetsdf)) {
#  tweetstext[[i]] <- tweetsdf[i,5]
#}

# READ ME: length=200, but I searched for 180 tweets? Is it a data frame?
length(tweetstext)

# READ ME: Can use lines 19-28 instead of 30-50?

who_retweet = list()
who_post = list()
# Do i have to specify the element of the list? Is an if-else statement not the best?
for(i in 1:nrow(tweetsdf)){
  if (tweetsdf[i,11] == TRUE) {
    who_retweet <- append(who_retweet, tweetsdf[i,]$screen_name)
  } else {
    who_post <- append(who_post, tweetsdf[i,]$screen_name)
  }
}

tweets_txt = sapply(tweetstext, function(x) x$getText())
grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweetstext, ignore.case=TRUE, value=TRUE)
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweets_txt, ignore.case=TRUE)
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))

for (i in 1:length(rt_patterns))
{ 
  # get tweet with retweet entity
  twit = tweets[[rt_patterns[i]]]
  twitinfo <- tweets_data(twit)
  # get retweet source 
  poster = str_extract_all(twit$getText(),
                           "(RT|via)((?:\\b\\W*@\\w+)+)") 
  #remove ':'
  poster = gsub(":", "", unlist(poster)) 
  # name of retweeted user
  who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  # name of retweeting user 
  who_retweet[[i]] = rep(twit$screen_name, length(poster)) 
}

who_post = unlist(who_post)
who_retweet = unlist(who_retweet)
userList <- lookup_users(who_retweet)

# get id's of the retweeters
who_retweet_id = list()
for(i in 1:length(userList))
{
  who_retweet_id[[i]] = userList[i,1]$user_id
}
user_follower_ids=list()

# see who is in common between the retweeters and their followers
# returning the wrong number of followers
index <- 1:25
edgeList <- NULL
for (i in index){
  
  cat("User number ", i, "\n")
  user <- who_retweet_id[[i]]
  user_follower_ids[[i]] <- get_followers(user, n=20, retryonratelimit = TRUE)
}
  common <- intersect(who_retweet_id, user_follower_ids)
  cat("common followers: ")
  cat(common)
  cat('\n')
  
  if (length(common) >0) {
    e <- cbind(user$user_id, common)
    edgeList <- rbind(edgeList, e)
  }